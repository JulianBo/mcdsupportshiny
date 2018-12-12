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

slGui1<-rSliderGuiInput("slGui1",configList,breaking=1,title_text=NULL,
                   cb_title="Ich weiß nicht")
NUM_PAGES <- length(slGui1)

# Define UI for application
shinyUI(fluidPage(
      useShinyjs(),
      rColorSliders(configList,"slGui1") ,
      rColorSliders(configList,"slGui2") ,

  ## Application title ----
  titlePanel("Gewichtungen"),

  ## List of pages - Main Part    ----


  hidden(
    list( #list, weil diverse "divs" in einer Liste kombiniert werden - obwohl rSliderGuiInput schon Liste von Pages liefert.

      #### SliderGuis

      ##TODO: Namespaces in rSliderGUIInput
      ##TODO: Set SliderGui2 to values of SliderGui1 at last page before mainpage.
      ##        First step: Implement SliderGui as module, implement updateSliderGui()

      ##SliderGui1 - several pages
      lapply(seq(NUM_PAGES),
             function(i)div(class = "page",
                            id = paste0("page", i),slGui1[[i]])),


      ######Final Page
      ### Sidebar with a slider inputs
      div(class = "page", id = paste0("page", NUM_PAGES+1),
        sidebarLayout(
          sidebarPanel(
            tags$p("Bitte stellen sie ein, wie wichtig Ihnen die einzelnen Indikatoren im Verhältnis zu den anderen Indikatoren sind."),
            rSliderGuiInput("slGui2",configList,breaking=0,title_text=NULL,
                       cb_title="Ich weiß nicht"
                       #, reusingvalues = lapply(sliderCheckboxModules,function(x)x() )#input ##TODO
            )
          ),#end of sidebarPanel

          # Show Results, Description, ...
          mainPanel(
            tabsetPanel(id="MainTabset",
                        tabPanel("Informationen",
                                 tags$p(texte$begruessungstext),tags$p(texte$begruessungstext2),
                                 h3("Weiter zur Auswertung"),
                                 fluidRow(
                                   #Weitere Abfragen
                                   column(width=6,
                                          selectInput("PlaceSlct",texte$ortstext ,choices=texte$ortslist,
                                                      selected = "Nein, woanders"),
                                          selectInput("FirsttimeSlct","Haben Sie dieses Tool schon einmal benutzt?" ,
                                                      choices=list("Nein", "Ja")),
                                          selectInput("GenderSlct","Welches ist Ihr Geschlecht?" ,
                                                      choices=list("Nicht angegeben/weitere", "Weiblich", "Männlich")),
                                          sliderInput("AgeSl", "Wie alt sind Sie?", min=0, max=100, value=0)

                                   ),
                                   #Speicher-Button
                                   column(width=6,
                                          selectInput("ChoiceSlct","Welche Alternative gefällt ihnen spontan am Besten?" ,
                                                      choices=levels(dtAlternativen$titel) ),
                                          br(),
                                          tags$p("Sind Sie auf der linken Seite mit den Einstellungen zufrieden? Dann können Sie diese absenden und das Ergebnis ansehen"),
                                          actionButton("speichernBtn", "Fertig? Speichern und Ergebnis ansehen")
                                   )
                                 )
                                 ##TODO: Alternativen beschreiben
                                 # ,
                                 # h3("Informationen zu den Alternativen"),
                                 # textOutput("InformationenText")
                                 ,
                                 textOutput("Aux_to_initialise")

                        ),
                        tabPanel("Entscheidungen",
                                 h2("Gesamtergebnis"),
                                 fluidRow(column(width=6,
                                                 plotOutput("ErgebnisPlot")
                                 ),
                                 column(width=6,
                                        tags$p("Mit den aktuellen Einstellungen präferieren Sie:",
                                               textOutput("ErgebnisText")),
                                        tags$p("Ursprünglich ausgewählt hatten Sie:",
                                               textOutput("ChoiceText")),
                                        tableOutput("ErgebnisTable"),
                                        tags$p("Mit den Einstellungen zufrieden? Dann auch diese speichern!"),
                                        actionButton("addBtn", "Aktuelle Einstellungen speichern")


                                 )
                                 )
                                 ,
                                 h2("Ergebnisse im Einzelnen und nach Szenario"),
                                 plotOutput("EntscheidungenPlot"),
                                 tableOutput("EntscheidungenTable")

                        ),
                        tabPanel("Endgültige Gewichtungen",
                                 tableOutput("DirGewichtungenTable")
                        ),
                        tabPanel("Alternativen",
                                 tableOutput("AlternativenTable")
                        ),
                        tabPanel("Nutzen-Funktionen",
                                 plotOutput("NutzenPlot")
                        )
                        ,
                        tabPanel("Bisherige Gewichtungen",
                                 plotOutput("BisherigeDecsPlot"),
                                 plotOutput("BisherigeHistsPlot")
                                 # ,
                                 # tableOutput("BisherigeTable")
                        ),
                        tabPanel("Einstellungen für die Indikatoren",
                                 tableOutput("Indikatorensettings")
                        ),
                        tabPanel("Über dieses Programm",
                                 tags$div(tags$p("Dieses Programm wurde im Rahmen des Projektes INOLA erstellt.
                                                 Weitere Informationen unter: ", tags$a(href="http://inola-region.de", "inola-region.de") ),
                                          tags$p("Technische Umsetzung: Julian Bothe") )
                                 )
                        ,
                        tabPanel("R Helper",
                                 textOutput("RoutputPrint"),
                                 tableOutput("RoutputTable1"),
                                 tableOutput("RoutputTable")
                        )
                        ) #tabsetpanel
            ) #mainPanel
        ) #sidebarLayout
    )#list (final Page)


    )#c
  ), #hidden

  ## Bottom Part ----
  br(),
  actionButton("prevBtn", "< Zurück"),
  actionButton("nextBtn", "Weiter >"),
  hidden(actionButton("saveBtn", "Speichern und Ergebnis ansehen >" ))

)) #shinyUI(fluidPage
