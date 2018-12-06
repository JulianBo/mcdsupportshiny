
# Slider with Checkbox ------------------------------------------

#' Module sliderCheckbox: Slider with
#'
#' Creates a Slider with a Checkbox to mark "I don't know" and corresponding functionality.
#'
#' @param id Modules id.
#' @param description Label for Slider.
#' @param min Slider minimu value.
#' @param max Slider maximum value.
#' @param value Slider initial value. If NA, Checkbox will be activated and default returned.
#' @param default Value to be returned if Checkbox is active. #TODO: use default when
#'                initialising module, instead of having to pass it another time to sliderCheckbox().
#' @param cb_title Description Label for Checkbox.
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
                                cb_title = "I don't know"){
  ns <- NS(id)

  fluidRow(
    column(width=9,
           sliderInput(ns("sl"),
                       paste0(description, collapse=""),
                       min = min,
                       max = max,
                       value = ifelse(is.na(value),default,value))
    ),
    column(width=2,
           checkboxInput(ns("active"),
                         cb_title, value=is.na(value) )
    )
  )
}

#'
#' @describeIn sliderCheckboxInput returns slider Value if Checkbox is FALSE, else default.
#' @export
sliderCheckbox<- function(input, output, session,
                          default=NA #TODO: use default when initialising module
                          #, name=NULL
                          ) {

  oldvalue<- reactiveVal()

  observeEvent(input$active, {
    #message(paste(input$active, name, collapse=";") ) #For Development
    if (input$active){
      oldvalue(input$sl)
      disable("sl")
      updateSliderInput(session, "sl", value=default)
    }else {
      updateSliderInput(session, "sl", value=oldvalue())
      enable("sl")
    }

    toggleState("sl", !input$active)
  })

  onclick("sl",
    if(input$active) updateCheckboxInput(session, "active", value=FALSE)
 )

  return ( reactive({
    ##Implement Fallback, if notyet initialised
    if (is.null(input$active) ){
      NULL
    } else if (input$active){
      default
    }else {
      input$sl
    }
  }))

}

