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

#source("Setup.R", encoding="UTF-8") #local=FALSE, auch in ui.R sichtbar
source("Setup_INOLA.R", encoding="UTF-8") #local=FALSE, auch in ui.R sichtbar

slGui1<-rSliderGuiInput("slGui1",configList,breaking=1,
                        beschreibungs_text=texte$begruessungstext2,
                        title_text=TRUE,
                   cb_title="Ich weiß nicht")
slGui2<-rSliderGuiInput("slGui2",configList,breaking=0,
                        beschreibungs_text = "Wenn sie jetzt die Einstellungen verändern, können sie verfolgen, wie sich dies auf das Ergebnis auswirkt.",
                        title_text="Gewichtungen",
                        cb_title="Ich weiß nicht")

NUM_PAGES_slGUI <- length(slGui1)

# Define UI for application
shinyUI(fluidPage(
      useShinyjs(),
      rColorSliders(configList,"slGui1") ,
      rColorSliders(configList,"slGui2") ,

  ## Application title ----
  titlePanel("Multikriterieller Pfadvergleich"),

  ## List of pages - Main Part    ----


  hidden(
    list( #list, weil diverse "divs" in einer Liste kombiniert werden - obwohl rSliderGuiInput schon Liste von Pages liefert.

      #### SliderGuis
      ##Begrüßung und Alternativen beschreiben
      div(class = "page", id = paste0("page", 1), #change according to paging. TODO: automatically

          tags$p(texte$begruessungstext),

          tags$p(texte$auswahlaufforderungstext),
          selectInput("ChoiceSlct","Welche Alternative gefällt ihnen spontan am Besten?" ,
                      choices=levels(dtAlternativen$Titel) ),

          h3("Informationen zu den Alternativen"),
          textOutput("InformationenText")
      ),


      ##SliderGui1 - several pages
      lapply(seq(NUM_PAGES_slGUI),
             function(i)div(class = "page",
                            id = paste0("page", i+1),slGui1[[i]])),#change according to paging

      #### Demographic Data
      div(class = "page", id = paste0("page", NUM_PAGES_slGUI+2), #change according to paging
          p("Bitte geben Sie zum Abschluss noch einige Demographische Daten ein."),
          h2("Bitte geben sie uns ein paar Informationen über sich."),
          selectInput("FirsttimeSlct","Haben Sie dieses Tool schon einmal benutzt?" ,
                      choices=list("Nicht angegeben","Nein", "Ja")),
          selectInput("PlaceSlct",texte$ortstext ,choices=c("Nicht angegeben",texte$ortslist)),
          selectInput("GenderSlct","Welches ist Ihr Geschlecht?" ,
                      choices=list("Nicht angegeben", "Weiblich", "Männlich","Weitere/Divers")),
          sliderInput("AgeSl", "Wie alt sind Sie?", min=0, max=100, value=0)

          ),

      ######Final Page
      ### Sidebar with a slider inputs
      div(class = "page", id = paste0("page", NUM_PAGES_slGUI+3), #change according to paging
          hidden(numericInput("NUM_PAGES", label=NULL,value=NUM_PAGES_slGUI+3)), #CHANGE to maximum number of Pages
        sidebarLayout(
          sidebarPanel(
            slGui2,

            textOutput("Aux_to_initialise")
          ),#end of sidebarPanel

          # Show Results, Description, ...
          mainPanel(
            tabsetPanel(id="MainTabset",
                        tabPanel("Ergebnis",
                                 h2("Ergebnis"),
                                 tags$b(
                                   tags$p("Mit den aktuellen Einstellungen präferieren Sie:",
                                          textOutput("ErgebnisText")),
                                   tags$p("Ursprünglich ausgewählt hatten Sie:",
                                          textOutput("ChoiceText"))
                                 )
                                 ,
                                 tags$p("Sie können jetzt sowohl die Einstellungen/Gewichtungen als auch die präferierte Alternative verändern, und dabei sehen, wie sich die Ergebnisse anpassen. Beachten sie, dass manche Indikatoren nicht in die Berechnung eingehen, weil sie nicht ohne weiteres quantifizierbar bzw. mit Zahlen abbildbar sind."),

                                 div(id="abstimmungsDiv",
                                   tags$p("Wenn Sie mit den Einstellungen zufrieden sind, können Sie diese abspeichern. Damit gehen diese Werte in das Ergebnis ein."),
                                   selectInput("ChoiceFinalSlct","Welche Alternative präferieren Sie, nachdem Sie diese Ergebnisse gesehen haben?" ,
                                               choices=levels(dtAlternativen$Titel)
                                               #TODO: Add changecount!
                                   ),
                                   actionButton("addBtn", "Zufrieden? Dann aktuelle Einstellungen speichern und damit abstimmen!")
                                 ),
                                 hidden(
                                   div(id="dankeDiv",
                                       tags$p(tags$b("Vielen Dank. Ihre Präferenzen wurden gespeichert"))
                                   )
                                 ),
                                 h3("Gesamtergebnis"),
                                 fluidRow(
                                   column(width=6, plotOutput("ErgebnisPlot")),
                                   column(width=6,tableOutput("ErgebnisTable"))
                                   ),
                                 h3("Ergebnisse im Einzelnen und nach Szenario"),
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
                        # ,
                        # tabPanel("R Helper",
                        #          textOutput("RoutputPrint"),
                        #          tableOutput("RoutputTable1"),
                        #          tableOutput("RoutputTable2"),
                        #          tableOutput("RoutputTable"),
                        #          textOutput("RoutputText2")
                        # )
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
  hidden(actionButton("saveBtn", "Speichern und Ergebnis ansehen >" )),
  br(),
  textOutput("pageNrText")


)) #shinyUI(fluidPage
