
# Slider with Not Known-Checkbox ------------------------------------------

#' Module sliderCheckbox: Slider with
#'
#' Creates a Slider with a Checkbox to mark "I don't know" and corresponding functionality.
#'
#' @param id Modules id.
#' @param description Label for Slider.
#' @param min Slider minimu value.
#' @param max Slider maximum value.
#' @param value Slider initial value.
#' @param default Value to be returned if Checkbox is active.#TODO: set default when initialising module
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
                                cb_title = "I don't know"){
  ns <- NS(id)

  fluidRow(
    column(width=9,
           sliderInput(ns("sl"), ##TODO: Change rest to Namespace
                       paste0(description, collapse=""),
                       min = min,
                       max = max,
                       value = value)
    ),
    column(width=2,
           checkboxInput(ns("active"), ##TODO: Change rest to Namespace
                         cb_title, value=FALSE )
    )
  )
}

#'
#' @describeIn sliderCheckboxInput returns slider Value if Checkbox is FALSE, else default.
#' @examples
sliderCheckbox<- function(input, output, session,
                          default=NA) {
  observeEvent(input$active, {
    message(input$active ) #For Development
    toggleState("sl", !input$active)
  })

  onclick("sl",updateCheckboxInput(session, "active", value=FALSE))

  return ( reactive({
    if (input$active){
      NA
    }else {
      input$sl
    }
  }))

}
