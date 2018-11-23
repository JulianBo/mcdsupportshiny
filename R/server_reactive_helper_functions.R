# Server: REACTIVES-Helper-Funktionen --------------------------------------------------------



#' Title
#'
#' @param centervalue
#' @param x
#' @param offset
#' @param trim
#' @param na.rm
#'
#' @return
#'
#' @examples
#'
#' @export
calculatecenterfunc <- function(centervalue, x,offset, trim = 0, na.rm = FALSE){
  #defaults to mean
  ifelse (is.character(centervalue),
          ifelse(centervalue=="mean",
                 mean(x+offset, trim , na.rm),
                 as.numeric(centervalue)),
          ifelse(is.numeric(centervalue),
                 centervalue,
                 mean(x+offset, trim , na.rm)
          )
  )
}

#' Convert Values of Indicators to utility-values.
#'
#' @param x Numeric. Value to be converted.
#' @param type One of c("prop","antiprop", "identity")
#' @param offset Offst to be added to x.
#' @param centervalue Numeric. Mean of utility values.
#' @param scale Scale.
#'
#' @return   switch(as.character(type),
#'                  prop = scale* (x+offset)/centervalue ,
#'                  antiprop=scale* centervalue/(x+offset),
#'                  identity= x)
#' @export
#'
#' @examples
utilityfunc <- function(x, type, offset=ifelse(type=="antiprop", 50 ,0),
                       centervalue=calculatecenterfunc(centervalue, x,offset), scale=100) {


  #cat(as.character(type))
  switch(as.character(type),
         prop = scale* (x+offset)/centervalue ,
         antiprop=scale* centervalue/(x+offset),
         identity= x)
}

