
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
#' @param sliderlabel Default class of slidercheckboxes, to use with \href{https://stackoverflow.com/a/42887905}{custom
#' prettify-function to have individual labels}.
#' @param sliderstate Default state of slider,see \code{\link{rSliderGuiInput}}.
#' @param width The width of the \code{\link[shiny]{sliderInput}}, e.g. '400px', or '100\%'; see \code{\link[shiny]{validateCssUnit}}.
#' @param sliderclass Default class of slider, see \code{\link{rSliderGuiInput}}.
#'
#' @return fluidrow with slider and checkbox for Input, values as described below.
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
                                default=value ,
                                cb_title = "I don't know",
                                sliderlabel="",sliderstate="",sliderclass="",
                                width = "100%"){
  id_replaced<-gsub("[^A-Za-z0-9-]", "", id)
  ns <- NS(id_replaced)

  if(!is.numeric(default)){
    warning("default must be numeric; set to min")
    default<-min
  }


  sl<-sliderInput(ns("sl"),
                  description,
                  min = min,
                  max = max,
                  value = if(is.na(value) )default else value,
                  width = width)


  #message(paste0("inside sliderCheckboxInput. id=", id, " siderlabel=", sliderlabel))
  fluidRow(id=id_replaced, sliderlabel=sliderlabel,sliderstate=sliderstate,
    column(width=9,
           if(is.na(value)) disabled(sl) else sl,
           hidden(
             numericInput(ns("defaultNumeric"),
                          "If you can see this, you forgot useShinyjs()", default)
           )

    ),
    column(width=2,
           checkboxInput(ns("disabled"),
                         cb_title, value=is.na(value) )
    )
  )
}

#'
#' @describeIn sliderCheckboxInput Returns list of reactives and functions.
#'                                 #TODO: Documentation.
#'                                 Slidervalue if checkbox is FALSE, else default.
#'                                 Value which sider had when checkbox was last checked will be returned in attribute "oldvalue".
#' @param check_changeDefaultNumeric Should defaultNumeric be observed for changes? Enables updating DefaultNumeric
#'                                   when slider is disabled, but is time-consuming
#' @param check_enableOnClick   Should disabled slider be activated when clicked on? Time-consuming.
#'
#' @export
sliderCheckbox<- function(input, output, session,
                          oldvalue =NULL,
                          check_changeDefaultNumeric=FALSE,
                          check_enableOnClick=FALSE) {

  #see: https://stackoverflow.com/questions/54560439/shiny-update-input-without-reactives-getting-triggered
  rv<-reactiveValues(storedvalue=oldvalue,
                     update=TRUE)

  if(check_changeDefaultNumeric) observeEvent(input$defaultNumeric,ignoreInit = TRUE, {
    if(input$disabled) updateSliderInput(session, "sl", value=input$defaultNumeric)
  })

  if(check_enableOnClick)   onclick("sl",
            if(input$disabled) updateCheckboxInput(session, "disabled", value=FALSE)
            )


  #To isolate update and reactivity. See https://stackoverflow.com/a/54560830/4177265
  observeEvent(input$sl, {
    if (rv$update) rv$storedvalue<-input$sl else rv$update<-TRUE
  })

  observeEvent(input$disabled,
               ignoreInit = TRUE, #ignoreInit, otherwise creation of Checkbox will set slider to oldvalue.
               {

    # message(paste("Event input$disabled", input$disabled,
    #               "storedvalue:", rv$storedvalue,
    #               "defaultvalue:",input$defaultNumeric, collapse=";") ) #For Development

    if (input$disabled){
      if(is.null(rv$storedvalue)) rv$storedvalue<-input$sl
      disable("sl")
      if(!is.null(input$defaultNumeric) &!(input$defaultNumeric==input$sl)) {
        rv$update<-FALSE
        updateSliderInput(session, "sl", value=input$defaultNumeric)
      }
    }else {

      #see https://github.com/rstudio/shiny/issues/2250
      if(!is.null(rv$storedvalue) & !(rv$storedvalue==input$sl)) {
        rv$update<-FALSE
        updateSliderInput(session, "sl", value=rv$storedvalue)
      }
      enable("sl")
    }

  })



#
# @param checkbox_active checkbix active? overrides value
# @param value New value.
# @param oldvalue
#
# @return
  setState<-function( checkbox_active=NULL,value=NULL,oldvalue=NULL){
    if(!is.null(checkbox_active)) {

      if(!is.null(oldvalue) ) rv$storedvalue<-oldvalue
      updateCheckboxInput(session, "disabled", value=checkbox_active)


      }else if(!is.null(value)){
        if(is.na(value)){
          updateCheckboxInput(session, "disabled", value=TRUE)
          if(!is.null(oldvalue) ) rv$storedvalue<-oldvalue
        }else {
          if(input$disabled){
            rv$storedvalue<-value
            updateCheckboxInput(session, "disabled", value=FALSE)
          } else updateSliderInput(session, "sl", value=value)
        }

      } else if(!is.null(oldvalue) ) rv$storedvalue<-oldvalue


  }

  value<-reactive({
    # isolate(message(paste0("GETVALUE. disabled:",input$disabled,
    #                "; sl:", input$sl,"; oldvalue:", rv$storedvalue,
    #                "; update:", rv$update) )
    # )

    ##Implement Fallback, if notyet initialised
    if (is.null(input$disabled) ){
      NULL
    } else if (input$disabled){
      ret<-input$defaultNumeric
    }else {
      ret<-rv$storedvalue
    }
  })

  return ( list(
    value= value,
    checkbox_active=reactive(input$disabled),
    oldvalue=reactive(rv$storedvalue),
    setState=setState,
    print=reactive(paste0("VALUE: ", value(),
                          "; sl: ", input$sl,
                          "; checkbox: ", input$disabled,
                          "; oldvalue: ", rv$storedvalue) ),
    syncModules=function(oldmodule){
      setState(oldmodule$checkbox_active(),oldmodule$value(),oldmodule$oldvalue())
    }

    ))

}

#'
#' @describeIn sliderCheckboxInput Updates static parts ofsliderCheckboxInput (not values!), ignoring all NULL-values.
#' @export
updateSliderCheckboxInput<- function( session, id,
                                      description=NULL,
                                      min = NULL,
                                      max = NULL,
                                      default=NULL ,
                                      cb_title = NULL){

  ns<-NS(gsub(" ", "", id, fixed = TRUE))

  #Updating Checkbox- without values
  if(!is.null(cb_title))updateCheckboxInput(session,ns("disabled"), label=cb_title)

  #Updating Slider - without values
  if (!(is.null(description)&is.null(min)&is.null(max)) ) updateSliderInput(session,ns("sl"), label=description, min=min, max=max)

  #Updating Default Value
  if(!is.null(default))updateNumericInput(session,ns("defaultNumeric"), value=default)

  }
