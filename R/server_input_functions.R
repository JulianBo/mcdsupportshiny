
# Server: INPUT-Funktionen --------------------------------------------------------

# Zusammenspiel von Daten und Konfiguration testen ---------

#' Test validity of mcdsuppportshiny configuration
#'
#' @param configList
#' @param dtAlternativen
#'
#' @return
#' @export
#'
#' @examples
validateConfig <- function (configList, dtAlternativen){

  #Flatten list
  # See https://stackoverflow.com/questions/16300344/how-to-flatten-a-list-of-lists-in-r
  flatlist <- unlist(configList)


  ## Existieren alle verbundenen Attribute?
  Attribnames <- flatlist[grep("Attribname",names(flatlist) )]

  colnames_in_Attribnames <- colnames(dtAlternativen) %in% Attribnames

  # message(paste0("Sie benutzen die Attribute: ",
  #                paste(colnames(dtAlternativen)[colnames_in_Attribnames], collapse=", "),
  #                ". Sie benutzen im Moment nicht: ",
  #                paste(colnames(dtAlternativen)[!colnames_in_Attribnames], collapse=", "),
  #                ". Nicht zugeordnet ist: ",
  #                paste(names(Attribnames)[is.na(Attribnames)], collapse=", ")
  # ))
  if(!(all(Attribnames %in% colnames(dtAlternativen)|is.na(Attribnames) )) )
    stop (paste(Attribnames[!(Attribnames %in% colnames(dtAlternativen))], collapse=", " ), " nicht in Daten enthalten oder NA")

  #TODO: weitere Tests einbauen: mappings ohne Attribnames
  #TODO: mindestens ein Mapping muss vorhanden sein.
}

#' Helper function. Standard configuration of utility function
#'
#' @param util_func
#' @param util_fit_x2
#' @param y2
#' @param util_fit_x1
#' @param y1
#'
#' @return list of five:
#' list(util_func=util_func,
#' util_fit_x1=util_fit_x1,
#' y1=y1,
#' util_fit_x2=util_fit_x2,
#' y2=y2)
#'
#'
#' @export
#'
#' @examples
utility_settings<-function( util_func="prop",
                            util_fit_x1=NA, y1=NA,
                            util_fit_x2="mean",
                            y2=100){
  list(util_func=util_func,
       util_fit_x1=util_fit_x1,
       y1=y1,
       util_fit_x2=util_fit_x2,
       y2=y2)
}

#' Get Settings of Indicators
#'
#' @param x  Configuration list, see \code{\link{validateConfig}}.
#' @param positive_utility_settings standard utitily function settings, \code{\link{utility_settings}()}.
#' @param negative_utility_settings standard utitily function settings for negative weights, \code{\link{utility_settings}(util_func="negprop")}.
#' @param agg_func standard aggregation function.
#' @param include_parent Should new settings apply also to parent itself, or only to children?
#' @param minweight only to determine, if negative settings should be included
#' @param open.maxdepth
#'
#' @return
#' @export
#'
#' @examples
getIndikatorensetting<- function(x,
                                 positive_utility_settings= utility_settings(),
                                 negative_utility_settings= utility_settings(util_func="negprop"),
                                 agg_func="weighted.sum", #alternative: weighted.mean
                                 include_parent=TRUE,
                                 open.maxdepth=Inf,
                                 standardweight=30){



  this.positive_utility_settings<-positive_utility_settings
  if("util_func" %in% names(x)) this.positive_utility_settings$util_func<-x$util_func
  if("util_fit_x1" %in% names(x)) this.positive_utility_settings$util_fit_x1<-x$util_fit_x1
  if("y1" %in% names(x)) this.positive_utility_settings$y1<-x$y1
  if("util_fit_x2" %in% names(x)) this.positive_utility_settings$util_fit_x2<-x$util_fit_x2
  if("y2" %in% names(x)) this.positive_utility_settings$y2<-x$y2
  #Negative Settings
  this.negative_utility_settings<-negative_utility_settings
  if("negative_util_func" %in% names(x)) this.negative_utility_settings$util_func<-x$negative_util_func
  if("negative_util_fit_x1" %in% names(x)) this.negative_utility_settings$util_fit_x1<-x$negative_util_fit_x1
  if("negative_y1" %in% names(x)) this.negative_utility_settings$y1<-x$negative_y1
  if("negative_util_fit_x2" %in% names(x)) this.negative_utility_settings$util_fit_x2<-x$negative_util_fit_x2
  if("negative_y2" %in% names(x)) this.negative_utility_settings$y2<-x$negative_y2
  #Further Settings
  this.agg_func <-if("agg_func" %in% names(x))x$agg_func else agg_func
  this.include_parent <- if("include_parent" %in% names(x) ) x$include_parent else include_parent
  this.minweight <- if("minweight" %in% names(x) ) x$minweight else minweight
  this.standardweight <- if("standardweight" %in% names(x) ) x$standardweight else standardweight


  dtIndikatorensetting <- rgetIndikatorensetting (x,depth=0, parent="Szenarioergebnis",
                                                  positive_utility_settings= this.positive_utility_settings,
                                                  negative_utility_settings= this.negative_utility_settings,
                                                  agg_func=this.agg_func,
                                                  include_parent=this.include_parent,
                                                  open.maxdepth = open.maxdepth,
                                                  minweight = this.minweight,
                                                  standardweight = this.standardweight)

  return(dtIndikatorensetting)

}

#' Internal version of getIndikatorensetting
#'
#' @param x
#' @param depth
#' @param parent
#' @param positive_utility_settings standard utitily function settings, \code{\link{utility_settings}()}.
#' @param negative_utility_settings standard utitily function settings for negative weights, \code{\link{utility_settings}(util_func="negprop")} .
#' @param agg_func
#' @param include_parent
#' @param open.maxdepth
#' @param minweight only to determine, if negative settings should be included, see \code{\link{rSliderGuiInput}}.
#' @param color
#'
#' @return
#'
#' @examples
rgetIndikatorensetting<- function(x, depth=0, parent="Szenarioergebnis",
                                  positive_utility_settings= utility_settings(),
                                  negative_utility_settings= utility_settings(util_func="negprop"),
                                  agg_func="weighted.sum", #alternative: weighted.mean
                                  include_parent=TRUE,
                                  open.maxdepth=Inf,
                                  color=NA,
                                  minweight=0,
                                  standardweight=30){

  stopifnot(depth>=0)
  stopifnot(is.list(x))

  ret <- data.table()

  #Ebene, ab wo mit Ausklapppanels gearbeitet wird. Kann nur kleiner werden, nie größer
  open.maxdepth<- getOpen.Maxdepth(x,open.maxdepth)

  for(i in 1:length(x) ){
    list.elem <- x[[i]]
    elem.name<- names(x)[i]
    #print(elem.name)

    ##Attribute parsen
    #Positive Settings
    this.positive_utility_settings<-positive_utility_settings
    if("util_func" %in% names(list.elem)) this.positive_utility_settings$util_func<-list.elem$util_func
    if("util_fit_x1" %in% names(list.elem)) this.positive_utility_settings$util_fit_x1<-list.elem$util_fit_x1
    if("y1" %in% names(list.elem)) this.positive_utility_settings$y1<-list.elem$y1
    if("util_fit_x2" %in% names(list.elem)) this.positive_utility_settings$util_fit_x2<-list.elem$util_fit_x2
    if("y2" %in% names(list.elem)) this.positive_utility_settings$y2<-list.elem$y2
    #Negative Settings
    this.negative_utility_settings<-negative_utility_settings
    if("negative_util_func" %in% names(list.elem)) this.negative_utility_settings$util_func<-list.elem$negative_util_func
    if("negative_util_fit_x1" %in% names(list.elem)) this.negative_utility_settings$util_fit_x1<-list.elem$negative_util_fit_x1
    if("negative_y1" %in% names(list.elem)) this.negative_utility_settings$y1<-list.elem$negative_y1
    if("negative_util_fit_x2" %in% names(list.elem)) this.negative_utility_settings$util_fit_x2<-list.elem$negative_util_fit_x2
    if("negative_y2" %in% names(list.elem)) this.negative_utility_settings$y2<-list.elem$negative_y2
    #Further Settings
    this.agg_func <-if("agg_func" %in% names(list.elem))list.elem$agg_func else agg_func
    this.include_parent <- if("include_parent" %in% names(list.elem) ) list.elem$include_parent else include_parent
    this.minweight <- if("minweight" %in% names(list.elem) ) list.elem$minweight else minweight
    this.standardweight <- if("standardweight" %in% names(list.elem) ) list.elem$standardweight else standardweight


    #Falls Element. GGf. Child-Elemente parsen
    if("class" %in% names(list.elem)){
      retvalue <- data.table(name=elem.name,
                             is_mapping = list.elem$class=="mapping",
                             negative=FALSE,
                             Attribname=if(list.elem$class=="mapping" & "Attribname" %in% names(list.elem) ) list.elem$Attribname else NA,
                             level=depth,
                             util_func=if(this.include_parent)this.positive_utility_settings$util_func else positive_utility_settings$util_func,
                             util_fit_x1=if(this.include_parent) this.positive_utility_settings$util_fit_x1 else positive_utility_settings$util_fit_x,
                             y1=if(this.include_parent) this.positive_utility_settings$y1 else positive_utility_settings$y1,
                             util_fit_x2=if(this.include_parent) this.positive_utility_settings$util_fit_x2 else positive_utility_settings$util_fit_x2,
                             y2=if(this.include_parent) this.positive_utility_settings$y2 else positive_utility_settings$y2,
                             agg_func=if(this.include_parent) this.agg_func else agg_func,
                             parent=parent,
                             bscName=if(depth>= open.maxdepth )NS(parent)("bsc") else NA,
                             standardweight= if(this.include_parent) {this.standardweight} else standardweight
      )

      #Falls negativ nötig
      if ( this.minweight<0)  {
        retvalue<-rbind(
          retvalue,
          data.table(name=elem.name,
                     is_mapping = list.elem$class=="mapping",
                     negative=TRUE,
                     Attribname=if(list.elem$class=="mapping"& "Attribname" %in% names(list.elem)) {
                       ##Falls anderes Attribut für negative Einstellungen.
                       if("negative_Attribname" %in% names(list.elem) ) list.elem$negative_Attribname else list.elem$Attribname
                       } else NA,
                     level=depth,
                     util_func=if(this.include_parent)this.negative_utility_settings$util_func else negative_utility_settings$util_func,
                     util_fit_x1=if(this.include_parent) this.negative_utility_settings$util_fit_x1 else negative_utility_settings$util_fit_x1,
                     y1=if(this.include_parent) this.negative_utility_settings$y1 else negative_utility_settings$y1,
                     util_fit_x2=if(this.include_parent) this.negative_utility_settings$util_fit_x2 else negative_utility_settings$util_fit_x2,
                     y2=if(this.include_parent) this.negative_utility_settings$y2 else negative_utility_settings$y2,
                     agg_func=if(this.include_parent) this.agg_func else agg_func,
                     parent=parent,
                     bscName=if(depth>= open.maxdepth )NS(parent)("bsc") else NA,
                     standardweight = if(this.include_parent) {this.standardweight} else standardweight
          )
        )
      }

      #Rekursion
      if(list.elem$class=="elements")
        retvalue <-rbind(retvalue, rgetIndikatorensetting (list.elem,depth=depth+1, parent=elem.name,
                                                           positive_utility_settings= this.positive_utility_settings,
                                                           negative_utility_settings= this.negative_utility_settings,
                                                           agg_func=this.agg_func,
                                                           include_parent=this.include_parent,
                                                           open.maxdepth = open.maxdepth,
                                                           minweight = this.minweight,
                                                           standardweight = this.standardweight)
        )

      # print("")
      # print(paste0("*** ", elem.name))
      # print("*****ret****")
      # print(ret)
      # print("*****retvalue****")
      # print(retvalue)


      ret<-rbind(ret,retvalue)
    } # if "class" in names

  }#for

  return(ret)


}
