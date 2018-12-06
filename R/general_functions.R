
# Allgemeine Funktionen ---------------------------------------------------

#' Get minimum of open.maxdepth and x$open.maxdepth
#'
#' Compares open.maxdepth and x$open.maxdepth and returns lower one, if present.
#'
#' @param x fgh
#' @param open.maxdepth dfgh
#'
#' @return returns min( x$open.maxdepth, open.maxdepth) if x$open.maxdepth exists, if not open.maxdepth
#'
#' @examples
#' getOpen.Maxdepth( NULL, 12)
#' getOpen.Maxdepth( list(open.maxdepth=10), 12)
#'
#' @export
getOpen.Maxdepth<- function (x,open.maxdepth=Inf){

  stopifnot(is.list(x))

  if("open.maxdepth" %in% names(x)) {
    new.open.maxdepth <- x$open.maxdepth
  }else return(open.maxdepth)

  if(!is.numeric(new.open.maxdepth)) stop("x$open.maxdepth must be numeric")

  ifelse(new.open.maxdepth< open.maxdepth,
         new.open.maxdepth,
         open.maxdepth)

}

#Cross join with Data.tables
# see https://stackoverflow.com/questions/10600060/how-to-do-cross-join-in-r
# For problem in Package see: https://github.com/tidyverse/dplyr/issues/548 and
# https://stackoverflow.com/questions/10527072/using-data-table-package-inside-my-own-package/10529888#10529888
#' Crossjoin of two data.tables
#'
#' @param x1 first data.table
#' @param x2 second data.table
#'
#' @return crossjoin of data.tables x1 and x2
#' @export
#'
#'
#' @examples
#'
crossjoinFunc<-  function(x1,x2)
  setkey(x1[,c(k=1,.SD)],k)[x2[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]


#' Even spread of difference
#'
#' @param x1 lower value
#' @param x2 higher value
#' @param share number of partitions
#'
#' @return width of each partition
#' @export
#'
#' @examples
#'getwidth(1,10,9)
#'getwidth(1,10,4)
#'
getwidth <- function(x1,x2,share=20)  (x2-x1)/share

#' Fallback for Null-Value.
#'
#' Useful for example for reinitialising inputs.
#'
#' @param a First value.
#' @param b Fallback.
#'
#' @return if (!is.null(a)) a else b
#' @export
#'
#' @note see \url{https://stackoverflow.com/questions/33137546/switch-between-layouts-reactively-with-shiny?noredirect=1&lq=1}.
#' @examples
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}


#' Setting Attributes of Elements within sliderGrui
#'
#' Returns x with attributes element_name, depth, and parent_name set to values
#'
#' @param x object
#' @param element_name value for element_name-attribute
#' @param depth value for depth-attribute
#' @param parent_name value for parent_name-attribute
#'
#' @return x with attributes element_name, depth, and parent_name
#'
#' @note Setting attribute "names" (with \code{\link{stats:setNames}}) produces an errors, therefore
#'       attribute "element_name" is used.
#'
#' @examples
setSliderGuiAttribs<-function(x,element_name,depth, parent_name){
  attr(x,"depth")<-depth
  attr(x,"element_name")<-element_name
  attr(x,"parent_name")<-parent_name

  x
  #setNames(x,name)
}
