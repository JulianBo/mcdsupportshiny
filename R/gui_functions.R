
# GUI-Functions -----------------------------------------------------------

#' Creates List of Sliders.
#'
#' @param x Configuration list, see \code{\link{validateConfig}}.
#' @param parents_name Label used to name root when creating fold-out-panel for root.
#' @param minweight Standard minimum slider weight.
#' @param maxweight Standard maximum slider weight.
#' @param standardweight Standard slider weight.
#' @param open.maxdepth From which depth on to create fold-out-panels, if not declared otherwise in x.
#' @param cb_title Default description label for checkbox.
#'
#' @return List of Sliders.
#' @export
#'
#' @examples
rSliderGui<-function(x, parents_name="Genauer",
                     minweight=0,maxweight=100, standardweight=30,
                     open.maxdepth=Inf,
                     cb_title= "I don't know"
                     ){
  recSliderGui(x,depth=0, parents_name = parents_name, minweight = minweight,
               maxweight = maxweight, standardweight = standardweight,
               open.maxdepth = open.maxdepth, cb_title=cb_title)
}

#' Internal Version of rSliderGui
#'
#' @param x
#' @param depth actual depth
#' @param parents_name
#' @param minweight
#' @param maxweight
#' @param standardweight
#' @param open.maxdepth
#'
#' @return

recSliderGui<-function(x, depth=0, parents_name="Genauer",
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

  for(i in 1:length(x) ){
    list.elem <- x[[i]]
    names <- names(x)[i]

    #Attribute parsen
    this.minweight <- ifelse("minweight" %in% names(list.elem), list.elem$minweight, minweight)
    this.maxweight <- ifelse("maxweight" %in% names(list.elem),  list.elem$maxweight, maxweight)
    this.standardweight <- ifelse("standardweight" %in% names(list.elem),
                                  list.elem$standardweight, standardweight)
    this.description <- ifelse("description" %in% names(list.elem),
                               list.elem$description, names)

    #Falls Element. GGf. Child-Elemente parsen
    ## Hier Slider selbst
    if("class" %in% names(list.elem)){
      returnvalue <-tagList(sliderCheckboxInput(names,
                                        description = paste0(this.description, collapse=""),
                                        min = this.minweight,
                                        max = this.maxweight,
                                        value = this.standardweight,
                                        cb_title = cb_title)
                            )

      #Rekursion
      if(list.elem$class=="elements")
        returnvalue <-tagList(returnvalue, recSliderGui(list.elem,depth+1,names,
                                                      minweight = this.minweight, maxweight=this.maxweight,
                                                      standardweight = this.standardweight,
                                                      open.maxdepth = open.maxdepth,
                                                      cb_title = cb_title) )


      #Je nach Tiefe zusammenfügen

      if (depth<open.maxdepth){

        #zurückgeben
        ret<- tagList(ret,
                      fluidRow(column(width= 12-depth,
                                      offset=depth,
                                      returnvalue

                      ))
        )

      }#if (depth<open.maxdepth)


      if (depth>= open.maxdepth){
        ret<- tagList(ret,returnvalue)
      }#  if (depth>= open.maxdepth)

    } # if "class" in names

  }#for

  result <- ret

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
#'
#' @return Vector of colors, corresponding to flatted list.
#'
#' @note See \url{https://stackoverflow.com/questions/36906265/how-to-color-sliderbar-sliderinput.}
#'
#' @export
#'
#' @examples
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
    names <- names(x)[i]


    #

    if("class" %in% names(list.elem)){
      this.color <- ifelse("color" %in% names(list.elem),list.elem$color, color)
      this.color_parent <- ifelse("color_parent" %in% names(list.elem),
                                  list.elem$color_parent, color_parent)

      #message(sprintf("num = %i, color= %s", num, color) )

      ret <- c(ret, ifelse(this.color_parent, this.color,color) )
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
rColorSliders<-function(x, color_parent=TRUE){

  colorsVector <- rColorVector(x, color_parent =color_parent)

  lapply(1:length(colorsVector), function(x){
    tags$style(HTML(
      sprintf(".js-irs-%1$i .irs-single, .js-irs-%1$i .irs-bar-edge, .js-irs-%1$i .irs-bar {background: %2$s}",
              x-1, colorsVector[x])) )
  })

}
