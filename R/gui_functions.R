
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
#' @param reusingvalues Should old slider values be used? Needed when creating sliderGuis on
#'                      several pages, and Sliders should be set to old values again.
#' @param parents_name Label used to name root when creating fold-out-panel for root.
#' @param minweight Standard minimum slider weight.
#' @param maxweight Standard maximum slider weight.
#' @param standardweight Standard slider weight.
#' @param open.maxdepth From which depth on to create fold-out-panels, if not declared otherwise in x.
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
  recSliderGui(x,depth=0,
               breaking=breaking,mainpageposition=mainpageposition,
               reusingvalues = reusingvalues,
               parents_name = parents_name, minweight = minweight,
               maxweight = maxweight, standardweight = standardweight,
               open.maxdepth = open.maxdepth, cb_title=cb_title)
}

#' Internal Version of rSliderGui
#'
#' @param x
#' @param depth actual depth
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
#' @return

recSliderGui<-function(x, depth=0,
                       breaking=NULL, mainpageposition=c("first","last", "none"),
                       reusingvalues=!is.null(breaking),
                       parents_name="genauer",
                     minweight=0,maxweight=100, standardweight=30,
                     open.maxdepth=Inf, cb_title= "I don't know"
){
  # Ziel: Elemente für innerhalb sidebarPanel zurückliefern.

  #Erste Ebene: Offen
  #Zweite Ebene: Eingerückt. Offen.
  # Danach: CollapsePanel.

  stopifnot(depth>=0)
  stopifnot(is.list(x))

  #Ebene, ab wo mit Ausklapppanels gearbeitet wird. Kann nur kleiner werden, nie größer
  open.maxdepth<- getOpen.Maxdepth(x,open.maxdepth)



  ret <- tagList()

  #Durch Elemente des aktuellen Knotens iterieren
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
      ## Hier Slider selbst
      returnvalue <-tagList(sliderCheckboxInput(elem.name,
                                        description = paste0(this.description, collapse=""),
                                        min = this.minweight,
                                        max = this.maxweight,
                                        value = this.standardweight,
                                        cb_title = cb_title)
                            )

      #Ggf. Rekursion
      if(list.elem$class=="elements")
        returnvalue <-tagList(returnvalue, recSliderGui(list.elem,depth+1,elem.name,
                                                      minweight = this.minweight, maxweight=this.maxweight,
                                                      standardweight = this.standardweight,
                                                      open.maxdepth = open.maxdepth,
                                                      cb_title = cb_title) )


      ##Je nach Tiefe zusammenfügen
      # Falls noch offen. Je nach Tiefe eingerückt
      if (depth<open.maxdepth){

        #zurückgeben
        ret<- tagList(ret,
                      fluidRow(column(width= 12-depth,
                                      offset=depth,
                                      returnvalue

                      ))
        )

      }#if (depth<open.maxdepth)

      #Falls nicht mehr offen; nicht einrücken
      if (depth>= open.maxdepth){
        ret<- tagList(ret,returnvalue)
      }#  if (depth>= open.maxdepth)

    } # if "class" in names

  }#for

  result <- ret

  #Falls nicht mehr offen, muss komplete Sliderliste in ein bsCollapsePanel gepackt werden
  if (depth>= open.maxdepth)
    result <- bsCollapse(id=paste0("bsc",parents_name,  collapse="_"),
                         bsCollapsePanel(title=paste0("Faktor ",parents_name," einstellen",  collapse=""), ##Hier beschreibung einstellen
                                         ret
                         )#bsCollapsePanel
    )#bsCollapse

  return(result)



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
