# Server: REACTIVES-Helper-Funktionen --------------------------------------------------------



#' Title
#'
#' @param centervalue one of mean, min, max, median, q1, q3; numeric value; NA
#' @param x
#' @param offset
#' @param trim works only for mean
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
    switch(centervalue,
      mean=mean(x+offset, trim =trim,  na.rm=na.rm),
      min=min(x+offset,   na.rm=na.rm),
      max=max(x+offset,   na.rm=na.rm),
      median=median(x+offset,   na.rm=na.rm),
      q1=quantile(x+offset,probs=0.25,  na.rm=na.rm),
      q3=quantile(x+offset,probs=0.75,  na.rm=na.rm),
      as.numeric(centervalue)
      )

  } else {
    if(is.numeric(centervalue) ){
      centervalue
    }else if(is.na(centervalue)){
      centervalue
    }else  {
      mean(x+offset, trim , na.rm)
    }

  }

}

#' Convert Values of Indicators to utility-values.
#'
#' @param x Numeric. Value to be converted.
#' @param x1 if (replace_NA=TRUE & is.na(x1)): 0
#' @param y1 if (replace_NA=TRUE & is.na(y1)): switch(as.character(type),prop=0,negprop=200,antiprop=200,0)
#' @param x2 if (replace_NA=TRUE & is.na(x2)): calculatecenterfunc("mean", x,offset=0)
#' @param y2 if (replace_NA=TRUE & is.na(y2)): 100
#' @param type One of c("prop", "negprop", antiprop", "identity")
#' @param replace_NA  should NA be replaced at x1,y1,x2,y2 ??
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
                        y2=100,
                        replace_NA=TRUE) {
  #cat(as.character(type))
  # print(str(x))
  # print(str(offset))

  if(replace_NA){
    if (is.na(x1)) x1<- 0
    if (is.na(y1)) y1<-switch(as.character(type),prop=0,negprop=200,antiprop=200,0)
    if (is.na(x2)) x2<- calculatecenterfunc("mean", x,offset=0)
    if (is.na(y2)) y1<- 100
  }

  if (x1==x2) {

    if (y1==y2) return(y1) else return (NA_real_)

  } else if (y1==y2) {
    return (y1)
  } else {


  switch(as.character(type),
         prop = x*(y2-y1)*1./(x2-x1) + (y1*x2 -y2*x1)*1./(x2-x1),
         negprop = x*(y2-y1)*1./(x2-x1) + (y1*x2 -y2*x1)*1./(x2-x1),
         antiprop= y1*(x1+ (y1*x1-y2*x2)*1./(y2-y1))/(x+ (y1*x1-y2*x2)*1./(y2-y1)),
         identity= x)
  }
}

