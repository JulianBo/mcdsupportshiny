#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(shinyjs)


library(mcdsupportshiny)

source("Setup.R", encoding="UTF-8") #local=FALSE, auch in ui.R sichtbar
#source("Setup_INOLA.R", encoding="UTF-8") #local=FALSE, auch in ui.R sichtbar



# Define UI for application
shinyUI(fluidPage(
      useShinyjs(),
      ## See https://stackoverflow.com/questions/16970989/sliderinput-in-shiny-how-to-control-the-length-width-of-the-slider-in-shiny
      ## TODO: Warum sind Slider nicht auf kompletter Fensterbreite
      tags$head(tags$style(HTML('irs {max-width: 1000px}'))),
      rColorSliders(configList) ,

  # Application title
  titlePanel("Gewichtungen"),
  uiOutput("ui"),
  br(),
  actionButton("prevBtn", "< Previous"),
  actionButton("nextBtn", "Next >")



)) #shinyUI(fluidPage
