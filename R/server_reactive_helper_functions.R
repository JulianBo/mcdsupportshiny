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
calculatecenterfunc <- function(centervalue, x,offset=0, trim = 0, na.rm = TRUE){
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
utilityfunc <- function(x, type,
                        x1=0,
                        y1=switch(as.character(type),
                                  prop=0,
                                  negprop=200,
                                  antiprop=200,
                                  0),
                        x2=calculatecenterfunc("mean", x,offset=0),
                        y2=100) {
  #cat(as.character(type))
  # print(str(x))
  # print(str(offset))

  if (x1==x2) {

    if (y1==y2) return(y1) else return (NA_real_)

  } else if (y1==y2) {
    return (y1)
  } else {

  # if(x2>x1 &y2<y1){
  #   x3=x1
  #   x1=x2
  #   x2=x2
  #
  #   y3=y1
  #   y1=y2
  #   y2=y3
  # }

  switch(as.character(type),
         prop = x*(y2-y1)*1./(x2-x1) + (y1*x2 -y2*x1)*1./(x2-x1),
         negprop = x*(y2-y1)*1./(x2-x1) + (y1*x2 -y2*x1)*1./(x2-x1),
         antiprop= y1*(x1+ (y1*x1-y2*x2)*1./(y2-y1))/(x+ (y1*x1-y2*x2)*1./(y2-y1)),
         identity= x)
  }
}

