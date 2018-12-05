
# GUI-Functions -----------------------------------------------------------

#' Creates List of Sliders.
#'
#' @param x Configuration list, see \code{\link{validateConfig}}.
#' @param breaking Should SliderGui be split up into a list of several pages, or deliver
#'                 one single list of sliders?
#'                 \describe{
#'                    \item{Null or FALSE}{No pagination.}
#'                    \item{TRUE}{Put all subordinate elements of first order
#'                                on extra pages, add main page containing only first order elements.}
#'                    \item{Integer}{Put all subordinate elements of nth order on extra pages, add
#'                                   main page containing elements up to nth order.
#'                                   1 corresponds to TRUE.}
#'                 }
#'                 Maybe in future versions possibilities for putting special Elements on
#'                 extra pages by name will be added, right now this is not possible.
#' @param mainpageposition One of "first", "last", "none".
#'                         Should main page be first or last or be omitted?
#' @param reusingvalues Should old slider values be used? If yes, provide list of values to be accessed by name
#'                      Usually this is done by \code{isolate(input)} or a variation thereof. Values which are not
#'                      in this list will be set to the default value.
#'                      Needed when creating sliderGuis on several pages, and Sliders should be set to old values again.
#' @param parents_name Label used to name root when creating fold-out-panel for root.
#' @param minweight Standard minimum slider weight.
#' @param maxweight Standard maximum slider weight.
#' @param standardweight Standard slider weight.
#' @param open.maxdepth From which depth on to create fold-out-panels, if not declared otherwise in x.
#'                      Maximum:12 (see \code{\link{shiny:column}}).Must be larger than breaking (if provided).
#' @param cb_title Default description label for checkbox.
#'
#' @return Slidergui: List of Sliders (breaking== NULL or breaking==FALSE)
#'                    or list of SliderGui-pages (breaking==TRUE or Breaking>0).
#' @export
#'
#' @examples
rSliderGui<-function(x,
                     breaking=NULL,mainpageposition=c("first","last", "none"),
                     reusingvalues=!is.null(breaking),
                     parents_name="genauer",
                     minweight=0,maxweight=100, standardweight=30,
                     open.maxdepth=Inf,
                     cb_title= "I don't know"
                     ){


  stopifnot(is.list(x))

  if (is.null(breaking)){
    breaking <-0
  } else  if (is.logical(breaking)){breaking<-as.integer(breaking)
  } else  stopifnot(breaking>=0)


  slGui <- recSliderGui(x,depth=0,
                        breaking=breaking,
                        reusingvalues = reusingvalues,
                        parents_name = parents_name, minweight = minweight,
                        maxweight = maxweight, standardweight = standardweight,
                        open.maxdepth = open.maxdepth, cb_title=cb_title)
  slGui
}

#' Internal Version of rSliderGui
#'
#' @param x
#' @param depth actual depth - root is 0
#' @param breaking
#' @param mainpageposition
#' @param reusingvalues
#' @param parents_name
#' @param minweight
#' @param maxweight
#' @param standardweight
#' @param open.maxdepth
#' @param cb_title
#'
#'
#' @return a list of rows as UIoutput - Sliders and collapsebars.

recSliderGui<-function(x, depth=0,
                       breaking=0,
                       reusingvalues=!is.null(breaking),
                       parents_name="genauer",
                     minweight=0,maxweight=100, standardweight=30,
                     open.maxdepth=Inf, cb_title= "I don't know"
){
  # Ziel: Elemente für innerhalb sidebarPanel bzw. für ganze Seite zurückliefern.

  #Erste Ebene: Offen
  #Zweite Ebene: Eingerückt. Offen. (ggf Korrigiert um breaking)
  # Danach: CollapsePanel.

  #Alle Sliders und Collapsepanels für sich als tagList (das meint: alle fluidrows, und collapsePanelGroups)
  #Dann  jeweils als Geschachtelte Liste.


  #Ebene, ab wo mit AusklappPanels gearbeitet wird. Kann nur kleiner werden, nie größer
  open.maxdepth<- getOpen.Maxdepth(x,open.maxdepth)

  ##weitere TESTS sind bereits in rCollapsePanel
  stopifnot(depth>=0)
  stopifnot(open.maxdepth>=breaking)



  ret <- list()

  ##Durch Elemente des aktuellen Knotens iterieren
  for(i in 1:length(x) ){
    list.elem <- x[[i]]
    elem.name <- names(x)[i]

    #Attribute parsen
    this.minweight <- ifelse("minweight" %in% names(list.elem), list.elem$minweight, minweight)
    this.maxweight <- ifelse("maxweight" %in% names(list.elem),  list.elem$maxweight, maxweight)
    this.standardweight <- ifelse("standardweight" %in% names(list.elem),
                                  list.elem$standardweight, standardweight)
    this.description <- ifelse("description" %in% names(list.elem),
                               list.elem$description, elem.name)

    #Falls Element. GGf. Child-Elemente parsen
    if("class" %in% names(list.elem)){

      ## Hier Slider selbst - Taglist!
      ## In Abhängigkeit davon, ob alte Werte wiederbenutzt werden sollen oder nicht.
      if (is.list(reusingvalues)) {
        returnvalue <-tagList(sliderCheckboxInput(elem.name,
                                                  description = paste0(this.description, collapse=""),
                                                  min = this.minweight,
                                                  max = this.maxweight,
                                                  value =reusingvalues[elem.name] %||%this.standardweight,
                                                  cb_title = cb_title)
        )
      } else {
        returnvalue <-tagList(sliderCheckboxInput(elem.name,
                                                  description = paste0(this.description, collapse=""),
                                                  min = this.minweight,
                                                  max = this.maxweight,
                                                  value = this.standardweight,
                                                  cb_title = cb_title)
        )
      }

      ##Je nach Tiefe zusammenfügen
      ##TODO: HIER LIEGT FEHLER! Alles muss in einzelne Reihe gesetzt werden!
      # Falls noch offen. Je nach Tiefe eingerückt
      if (depth<open.maxdepth){

        ##Falls Mainpage
        #Ganz normal einrücken
        if (depth<=breaking) {

          #zurückgeben
          ret<- c(ret,
                  indentedRow(indention = depth,
                              returnvalue)
          )

          ##Falls keine Mainpage
          #Einrückung um Tiefe des Breaks bereinigen
        } else {
          ret<- c(ret,
                  indentedRow(indention = depth -breaking,
                              returnvalue)
          )

        }

        #Falls nicht mehr offen; nicht einrücken
      } else  if (depth>= open.maxdepth){
        ret<- c(ret,list(returnvalue) )
      }#  if (depth>= open.maxdepth)


      ##Ggf. REKURSION
      ##Slider (Element selbst) mit evtl. Child-Knoten zusammensetzen. mit c(): sollte list() sein.
      if(list.elem$class=="elements")
        ret <-c(ret,
                # tagList(
                  recSliderGui(list.elem,depth+1,
                                     breaking = breaking,
                                     reusingvalues = reusingvalues,
                                     elem.name,
                                     minweight = this.minweight, maxweight=this.maxweight,
                                     standardweight = this.standardweight,
                                     open.maxdepth = open.maxdepth,
                                     cb_title = cb_title)
                # )
        )




    } # if "class" in names

  }#for

  #Falls nicht mehr offen, muss komplete Sliderliste in ein bsCollapsePanel gepackt werden
  if (depth>= open.maxdepth){
    result <- bsCollapse(id=paste0("bsc",parents_name,  collapse="_"),
                         bsCollapsePanel(title=paste0("Faktor ",parents_name," einstellen",  collapse=""), ##Hier beschreibung einstellen
                                         tagList(ret)
                                         )#bsCollapsePanel
                         )#bsCollapse
  } else result <- ret

  return(result)



}


#' Creation of indented fluidrows
#'
#' Creates an indented fluidrow
#'
#' @param ... Elements to be included (each on several row).
#' @param indention Integer between 0 and 11. Indention.
#' @param tagListWrap Should fluidrow be wrapped into \code{\link{shiny:tagList}}.
#'
#' @return \code{\link{shiny:fluidRow}}/\code{\link{shiny:column}} with indented elements.
#'
#' @examples
indentedRow<-function(...,indention=0, tagListWrap=FALSE){
  stopifnot(indention>=0 & indention<12)
  ret<-fluidRow(column(
    width= 12-indention,
    ...,
    offset=indention
  ))
  if (tagListWrap) { return(tagList(ret))
  } else return (ret)
}


#' Recursively turn Configurations list into a vector of colors
#'
#' @param x Configuration list, see \code{\link{validateConfig}}.
#' @param color Default color.
#' @param color_parent Should parent itself also be colored in new color or only the children?
#' @param collapse should all styles be collapsed into a single style-tag (default), or not (collapse=NULL)
#'
#' @return named vector of colors, corresponding to flatted list. names: element names.
#'
#' @note See \url{https://stackoverflow.com/questions/36906265/how-to-color-sliderbar-sliderinput.}
#'
#' @export
#'
#' @examples
#'
rColorVector <- function(x, color="", color_parent=TRUE){


  #Color einfügen.
  #Siehe: https://stackoverflow.com/questions/36906265/how-to-color-sliderbar-sliderinput

  stopifnot(is.list(x))

  recColorVector(x, color=color, color_parent =color_parent)
}


#' Internal Version of rColorVector
#'
#' @param x
#' @param num Position in flatted List, because recursive list is processed iteratively
#' @param color
#' @param color_parent
#'
#' @return

recColorVector<-function(x,num=0, color="", color_parent=TRUE){
  #TODO
  ret <- vector(mode="character")

  for(i in 1:length(x) ){


    list.elem <- x[[i]]
    elem.name <- names(x)[i]


    #

    if("class" %in% names(list.elem)){
      this.color <- ifelse("color" %in% names(list.elem),list.elem$color, color)
      this.color_parent <- ifelse("color_parent" %in% names(list.elem),
                                  list.elem$color_parent, color_parent)

      #message(sprintf("num = %i, color= %s", num, color) )

      ret <- c(ret,setNames( ifelse(this.color_parent, this.color,color),elem.name ) )
      num<-num+1

      ##Recursion
      if(list.elem$class=="elements"){
        recvalue <- recColorVector(list.elem,num ,this.color, color_parent = this.color_parent)
        #message(length(recvalue))

        ret<-c(ret,recvalue)
        num <- num+length(recvalue)
      }
    }

  } #for

  return(ret)
}



#' @describeIn rColorVector Vector of colors converted into a vector of CSS-Styles to use for ION.rangeSlider.
#'
#' @export

rColorSliders<-function(x, color_parent=TRUE, collapse="\n"){
  ## TODO: Do this directly in SliderCheckbox-Module!

  colorsVector <- rColorVector(x, color_parent =color_parent)

  ret<-lapply(1:length(colorsVector), function(x){

      sprintf("[for=\"%1$s\"]+span>.irs>.irs-single, [for=\"%1$s\"]+span>.irs-bar-edge, [for=\"%1$s\"]+span>.irs-bar {background: %2$s}"
              ,#Correcting for Namespace of module!
              paste0(names(colorsVector)[x],"-sl"), colorsVector[x])
  })
  tags$style(HTML(paste(ret, collapse = collapse)))

}
