
# Slider with Checkbox ------------------------------------------

#' Module sliderCheckbox: Slider with Checkbox
#'
#' Creates a Slider with a Checkbox to mark "I don't know" and corresponding functionality.
#'
#' @param id Modules id.
#' @param description Label for Slider.
#' @param min Slider minimum value.
#' @param max Slider maximum value.
#' @param value Slider initial value. If NA, Checkbox will be activated and default returned.
#' @param default Value to be returned if Checkbox is active.
#' @param cb_title Description Label for Checkbox.
#' @param width The width of the \code{\link[shiny]{sliderInput}}, e.g. '400px', or '100\%'; see \code{\link[shiny]{validateCssUnit}}.
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#'
#' @return fluidrow with slider and checkbox for Input, Values as described below.
#' @export
#'
#' @note For Usage of Modules, see \url{https://shiny.rstudio.com/articles/modules.html}.
#'       To use the functionality, \code{\link[shinyjs]{shinyjs}} must have been loaded and
#'       \code{\link[shinyjs:useShinyjs]{initialised}}.
#'
#' @examples
sliderCheckboxInput <- function(id,description="",
                                min = 0,
                                max = 100,
                                value = 30,
                                default=30 ,
                                cb_title = "I don't know",
                                width = "100%"){
  ns <- NS(id)

  if(!is.na(default)&!is.numeric(default)){
    warning("default must be numeric or NA; set to NA")
    default<-NA_real_
  } else if(is.na(default)) default<-NA_real_

  fluidRow(
    column(width=9,
           sliderInput(ns("sl"),
                       paste0(description, collapse=""),
                       min = min,
                       max = max,
                       value = ifelse(is.na(value),default,value),
                       width = width),
           hidden(numericInput(
             ns("defaultNumeric"), "If you can see this, you forgot useShinyjs()", default)
           )

    ),
    column(width=2,
           checkboxInput(ns("active"),
                         cb_title, value=is.na(value) )
    )
  )
}

#'
#' @describeIn sliderCheckboxInput Returns numeric. Slidervalue if checkbox is FALSE, else default.
#'                                 Value which sider had when checkbox was last checked will be returned in attribute "oldvalue".
#' @export
sliderCheckbox<- function(input, output, session) {

  oldvalue<- reactiveVal(NA)

  observeEvent(input$active, {
    #message(paste(input$active, name, collapse=";") ) #For Development
    if (input$active){
      oldvalue(input$sl)
      disable("sl")
      updateSliderInput(session, "sl", value=input$defaultNumeric)
    }else {
      updateSliderInput(session, "sl", value=oldvalue())
      enable("sl")
    }

    #toggleState("sl", !input$active)
  })

  onclick("sl",
          if(input$active) updateCheckboxInput(session, "active", value=FALSE)
  )


  return ( reactive({
    ##Implement Fallback, if notyet initialised
    if (is.null(input$active) ){
      ret<-NULL
    } else {if (input$active){
      ret<-input$defaultNumeric
    }else {
      ret<-input$sl
    }
      attr(ret, "oldvalue")<-oldvalue()
    }



    ret
    }))

}

#'
#' @describeIn sliderCheckboxInput Updates sliderCheckboxInput, ignoring all NULL-values.
#' @export
updateSliderCheckboxInput<- function( session, id,
                                      description=NULL,
                                      min = NULL,
                                      max = NULL,
                                      value = NULL,
                                      default=NULL ,
                                      cb_title = NULL){

  ns<-NS(id)

  #Updating Checkbox- without values
  if(!is.null(cb_title))updateCheckboxInput(session,ns("active"), label=cb_title)

  #Updating Slider - without values
  updateSliderInput(session,ns("sl"), label=description, min=min, max=max)

  #Updating Default Value
  if(!is.null(default))updateNumericInput(session,ns("defaultNumeric"), value=default)

  #Updating Value itself
  if(!is.null(value)){ if(is.na(value) ) {
    updateCheckboxInput(session,ns("active"), value=TRUE)
  }else {
    updateCheckboxInput(session,ns("active"), value=FALSE)
    updateSliderInput(session,ns("sl"),value=value)
  }
  }

}
