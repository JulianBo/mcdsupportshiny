
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

  message(paste0("Sie benutzen die Attribute: ",
                 paste(colnames(dtAlternativen)[colnames_in_Attribnames], collapse=", "),
                 ". Sie benutzen im Moment nicht: ",
                 paste(colnames(dtAlternativen)[!colnames_in_Attribnames], collapse=", "),
                 ". Nicht zugeordnet ist: ",
                 paste(names(Attribnames)[is.na(Attribnames)], collapse=", ")
  ))
  if(!(all(Attribnames %in% colnames(dtAlternativen)|is.na(Attribnames) )) )
    stop (paste(Attribnames[!(Attribnames %in% colnames(dtAlternativen))], collapse=", " ), " nicht in Daten enthalten oder NA")

  #TODO: weitere Tests einbauen: mappings ohne Attribnames
  #TODO: mindestens ein Mapping muss vorhanden sein.
}


#' Get Settings of Indicators
#'
#' @param x  Configuration list, see \code{\link{validateConfig}}.
#' @param util_func standard utitily function.
#' @param util_mean standard utility mean.
#' @param util_offset standard utility offset.
#' @param util_scale standard utility scale.
#' @param agg_func standard aggregation function.
#' @param include_parent Should new settings apply also to parent itself, or only to children?
#' @param open.maxdepth
#'
#' @return
#' @export
#'
#' @examples
getIndikatorensetting<- function(x,
                                 util_func="prop",util_mean="mean",
                                 util_offset=ifelse(util_func=="antiprop", 10,0),
                                 util_scale=100,
                                 agg_func="weighted.sum", #alternative: weighted.mean
                                 include_parent=TRUE,
                                 open.maxdepth=Inf ){




  dtIndikatorensetting <- rgetIndikatorensetting (x,depth=0, parent="Szenarioergebnis",
                                                  util_func=util_func, util_mean = util_mean,
                                                  util_offset = util_offset, util_scale = util_scale,
                                                  agg_func=agg_func,
                                                  include_parent=include_parent                                                  ,
                                                  open.maxdepth=open.maxdepth )

  return(dtIndikatorensetting)

}

#' Internal version of getIndikatorensetting
#'
#' @param x
#' @param depth
#' @param parent
#' @param util_func
#' @param util_mean
#' @param util_offset
#' @param util_scale
#' @param agg_func
#' @param include_parent
#' @param open.maxdepth
#' @param color
#'
#' @return
#'
#' @examples
rgetIndikatorensetting<- function(x, depth=0, parent="Szenarioergebnis",
                                  util_func="prop",util_mean="mean",
                                  util_offset=ifelse(util_func=="antiprop", 10,0),
                                  util_scale=100,
                                  agg_func="weighted.sum", #alternative: weighted.mean
                                  include_parent=TRUE,
                                  open.maxdepth=Inf,
                                  color=NA){

  stopifnot(depth>=0)
  stopifnot(is.list(x))

  ret <- data.table()

  #Ebene, ab wo mit Ausklapppanels gearbeitet wird. Kann nur kleiner werden, nie größer
  open.maxdepth<- getOpen.Maxdepth(x,open.maxdepth)

  for(i in 1:length(x) ){
    list.elem <- x[[i]]
    names <- names(x)[i]

    #Attribute parsen
    this.util_func <- ifelse("util_func" %in% names(list.elem), list.elem$util_func, util_func)
    this.util_mean <- ifelse("util_mean" %in% names(list.elem), list.elem$util_mean, util_mean)
    this.util_offset <- ifelse("util_offset" %in% names(list.elem), list.elem$util_offset, util_offset)
    this.util_scale <- ifelse("util_scale" %in% names(list.elem), list.elem$util_scale, util_scale)
    this.agg_func <- ifelse("agg_func" %in% names(list.elem),  list.elem$agg_func, agg_func)
    this.include_parent <- ifelse("include_parent" %in% names(list.elem),
                                  list.elem$include_parent, include_parent)


    #Falls Element. GGf. Child-Elemente parsen
    if("class" %in% names(list.elem)){
      retvalue <- data.table(name=names,
                             is_mapping = list.elem$class=="mapping",
                             Attribname=ifelse(list.elem$class=="mapping",list.elem$Attribname, NA),
                             level=depth,
                             util_func=ifelse(this.include_parent, this.util_func, util_func),
                             util_mean=ifelse(this.include_parent, this.util_mean, util_mean),
                             util_offset=ifelse(this.include_parent, this.util_offset, util_offset),
                             util_scale=ifelse(this.include_parent, this.util_scale, util_scale),
                             agg_func=ifelse(this.include_parent,this.agg_func, agg_func),
                             parent=parent,
                             bscName=ifelse(depth>= open.maxdepth,
                                            paste0("bsc",parent,  collapse="_") ,
                                            NA)
      )

      #Rekursion
      if(list.elem$class=="elements")
        retvalue <-rbind(retvalue, rgetIndikatorensetting (list.elem,depth=depth+1, parent=names,
                                                           util_func=this.util_func,
                                                           util_mean = this.util_mean,
                                                           util_offset = this.util_offset,
                                                           util_scale = this.util_scale,
                                                           agg_func=this.agg_func,
                                                           include_parent=this.include_parent,
                                                           open.maxdepth = open.maxdepth)
        )


      ret<-rbind(ret,retvalue)
    } # if "class" in names

  }#for

  return(ret)


}
