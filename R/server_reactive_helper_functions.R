# Server: REACTIVES-Helper-Funktionen --------------------------------------------------------



#' Title
#'
#' @param centervalue
#' @param x
#' @param offset
#' @param trim
#' @param na.rm
#'
#' @return defaults to mean
#'
#' @examples
#'
#' @export
calculatecenterfunc <- function(centervalue, x,offset, trim = 0, na.rm = TRUE){
  #defaults to mean
  if (is.character(centervalue) ) {
    if(centervalue=="mean"){
      mean(x+offset, trim , na.rm)
    } else {
      as.numeric(centervalue)
    }

  } else {
    if(is.numeric(centervalue) ){
      centervalue
    }else {
      mean(x+offset, trim , na.rm)
    }

  }

}

#' Convert Values of Indicators to utility-values.
#'
#' @param x Numeric. Value to be converted.
#' @param type One of c("prop", "negprop", antiprop", "identity")
#' @param offset Offst to be added to x.
#' @param centervalue Numeric. Mean of utility values.
#' @param scale Scale.
#'
#' @return   switch(as.character(type),
#'                  prop = scale* (x+offset)/centervalue ,
#'                  negprop = 2*scale - scale*(x +offset)/centervalue ,
#'                  antiprop=scale* centervalue/(x+offset),
#'                  identity= x)
#' @export
#'
#' @examples
utilityfunc <- function(x, type, offset=ifelse(type=="antiprop", 50 ,0),
                        centervalue=calculatecenterfunc("mean", x,offset), scale=100) {
  #cat(as.character(type))
  # print(str(x))
  # print(str(offset))
  switch(as.character(type),
         prop = scale* (x+offset)/centervalue ,
         negprop = 2*scale - scale*(x +offset)/centervalue ,
         antiprop=scale* centervalue/(x+offset),
         identity= x)
}

