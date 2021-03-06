% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gui_functions.R
\name{recSliderGuiInput}
\alias{recSliderGuiInput}
\title{Internal Version of rSliderGui}
\usage{
recSliderGuiInput(id, x, depth = 0, breaking = 0,
  parents_name = "genauer", minweight = 0, maxweight = 100,
  standardweight = 30, open.maxdepth = Inf,
  cb_title = "I don't know", sliderlabel = "", sliderstate = "")
}
\arguments{
\item{depth}{actual depth - root is 0}

\item{sliderlabel}{}

\item{sliderstate}{Default class of slider.}
}
\value{
a list of rows as UIoutput - Sliders and collapsebars
        with attributes "depth" and "element_name". See code{\link{setNameDepth}}
}
\description{
Internal Version of rSliderGui
}
\examples{
test<-recSliderGuiInput("slg1", configList)
sapply(test, function(x){setNames(attr(x,"depth"),attr(x,"element_name"))})
}
