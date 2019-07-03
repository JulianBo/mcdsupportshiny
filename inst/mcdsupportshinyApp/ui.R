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

library(DT)
library(ggplot2)
#library(plotly)


library(mcdsupportshiny)

#source("Setup.R", encoding="UTF-8") #local=FALSE, auch in ui.R sichtbar
#source("Setup_INOLA.R", encoding="UTF-8") #local=FALSE, auch in ui.R sichtbar
source("Setup_INOLA_neu.R",local=TRUE, encoding="UTF-8") #local=FALSE, auch in ui.R sichtbar

dtIndikatorensettings<-getIndikatorensetting(configList)

slGui1<-rSliderGuiInput("slGui1",configList,breaking=1,
                        beschreibungs_text=texte$begruessungstext2,
                        title_text=TRUE,
                   cb_title="Ich weiß nicht")
slGui2<-rSliderGuiInput("slGui2",configList,breaking=0, open.maxdepth =1,
                        beschreibungs_text = "Wenn sie jetzt die Einstellungen verändern, können sie verfolgen, wie sich dies auf das Ergebnis auswirkt.",
                        title_text="Gewichtungen",
                        cb_title="Ich weiß nicht")

NUM_PAGES_slGUI <- length(slGui1)

# Define UI for application
shinyUI(fluidPage(theme="mcdsupportshiny.css",
      useShinyjs(),
      includeScript("labels.js"),
      rColorSliders(configList,"slGui1") ,
      rColorSliders(configList,"slGui2") ,

  ## Application title ----
  a(href="http://www.inola-region.de",
     img(src="Inola_Logo-rgb_web_transparent.png", style="height: 4em; float:right"),
     style="text-align: right"),
  titlePanel(title_text),


  ## List of pages - Main Part    ----



    list( #list, weil diverse "divs" in einer Liste kombiniert werden - obwohl rSliderGuiInput schon Liste von Pages liefert.

      #### SliderGuis
      ##Begrüßung und Alternativen beschreiben
      div(class = "page", id = paste0("page", 1), #change according to paging. TODO: automatically

          tags$p(texte$begruessungstext),

          tags$p(texte$auswahlaufforderungstext),
          selectInput("ChoiceSlct", texte$choiceSlctText ,
                      choices=c("Bitte Auswählen", AlternativenLevels  )
                      ),

          h3(texte$alternativentitel),
          textOutput("InformationenText"),
          pfadbeschreibungen,
          rahmenbeschreibungen
      ),


      ##SliderGui1 - several pages
      hidden(
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
            sidebarLayout(position="left",
                          sidebarPanel( width = 5,
                                        div(id="slGui2",slGui2 ),

                                        textOutput("Aux_to_initialise")
                          ),#end of sidebarPanel

                          # Show Results, Description, ...
                          mainPanel(width = 7,
                                    tabsetPanel(id="MainTabset",
                                                tabPanel("Ergebnisse",
                                                         tabsetPanel(
                                                           tabPanel("Hauptergebnisse",

                                                                    h2("Ergebnis"),
                                                                    div(class="results",
                                                                        tags$div("Mit den aktuellen Einstellungen hat folgender Pfad die höchste Punktzahl:",
                                                                                 textOutput("ErgebnisText", inline=TRUE)),
                                                                        tags$div("Ursprünglich ausgewählt hatten Sie:",
                                                                                 textOutput("ChoiceText", inline=TRUE))
                                                                    ),
                                                                    tags$div("Hier können sie sehen, welche ",
                                                                             tags$a(href="http://inola-region.de", target="_blank", "Geschichte hinter den einzelnen Pfaden steckt."))

                                                                    ,
                                                                    h2("Jetzt ausprobieren und endgültig abstimmen!"),
                                                                    tags$p("Sie können jetzt sowohl die Einstellungen/Gewichtungen als auch die präferierte Alternative verändern, und dabei sehen, wie sich die Ergebnisse anpassen. Beachten sie, dass manche Indikatoren nicht in die Berechnung eingehen, weil sie nicht ohne weiteres quantifizierbar bzw. mit Zahlen abbildbar sind."),

                                                                    div(id="abstimmungsDiv",
                                                                        tags$p("Wenn Sie mit den Einstellungen zufrieden sind, können Sie diese abspeichern. Damit gehen diese Werte in das Ergebnis ein."),
                                                                        selectInput("ChoiceFinalSlct","Welchen Pfad präferieren Sie, nachdem Sie diese Ergebnisse gesehen haben?" ,
                                                                                    choices=c("Bitte Auswählen", AlternativenLevels  ),
                                                                                    width = "100%"
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
                                                                    "Je höher die Punktzahl, desto mehr entspricht der Pfad ihren Präferenzen. Die gestrichelte Linie gibt den Mittelwert der einzelnen Energiepfade unter verschiedenen Rahmenbedingungen an: Je höher, desto besser ist der Pfad unter verschiedenen Rahmenbedingungen.",
                                                                    fluidRow(
                                                                      column(width=8, plotOutput("ErgebnisPlot")),
                                                                      column(width=4,
                                                                             h4("Mittelwert pro Pfad"),
                                                                             tableOutput("ErgebnisTable")
                                                                             )
                                                                    )
                                                                    #,
                                                                    # h3("Ergebnisse nach Rahmen"),
                                                                    # plotOutput("SzenarioPlot")

                                                           ),
                                                           tabPanel("Detailergebnisse",
                                                                    ##dtGewichtungen[,.N, by=.(parent, level)][order(level)]
                                                                    h3("Punktwerte der Kategorien"),
                                                                    h4("Bereiche"),
                                                                    plotOutput("BereichPlot"),

                                                                    h4("Einzelne Bereiche genauer erkunden"),

                                                                    selectInput("BereichDetailPlotSelect",
                                                                                "Bitte Bereich auswählen",choices=unique(dtIndikatorensettings[level>0,parent])
                                                                    ),
                                                                    plotOutput("BereichDetailPlot"),
                                                                    h3("Punktwerte als Tabelle"),

                                                                    DT::dataTableOutput("EntscheidungenTable")

                                                           )
                                                         )



                                                ),
                                                tabPanel("Bisherige Abstimmungen",
                                                         AnalysisPreviousUI("AnalysisPrevious",
                                                                            copy(dtIndikatorensettings)[,name_new:=paste0(name, #gsub("[ ()]", ".", name),
                                                                                                                          ".originalweights")
                                                                                                        ]
                                                                            ),
                                                         actionButton("renewBisherige",
                                                                      "Bisherige Abstimmungen neu laden")


                                                ),
                                                # tabPanel("Endgültige Gewichtungen",
                                                #          tableOutput("DirGewichtungenTable")
                                                # ),


                                                # tabPanel("Einstellungen für die Indikatoren",
                                                #          tableOutput("Indikatorensettings")
                                                # ),
                                                tabPanel("Über dieses Programm",
                                                         tags$div(tags$p("Dieses Programm wurde im Rahmen des Projektes INOLA erstellt.
                                                 Weitere Informationen unter: ", tags$a(href="http://inola-region.de", target="_blank", "inola-region.de") ),
                                                                  tags$p("Technische Umsetzung: Julian Bothe"),
                                                                  tags$p("Der Quellcode des Programms kann unter ", tags$a(href="https://github.com/JulianBo/mcdsupportshiny",
                                                                                                                           target="_blank", "https://github.com/JulianBo/mcdsupportshiny"),
                                                                         "  heruntergeladen und eingesehen werden.")),
                                                         h3("Funktionsweise"),
                                                         tags$p("Dieses Programm rechnet die einzelnen Indikatoren in Punktzahlen um. Je nach Gewichtung werden diese Schrittweise anteilig aufaddiert - Je höher die Gewichtung ist, desto mehr der Punkte wird benutzt. "),
                                                         h4("Benutzte Punktzahlen der Indikatoren"),
                                                         plotOutput("NutzenPlot"),
                                                         selectInput(
                                                           "NutzenPlotOptions",
                                                           "Bitte Variablen zur Ansicht auswählen",
                                                           choices= names(dtAlternativen)[-(1:2)],
                                                           selected=names(dtAlternativen)[-(1:2)],
                                                           multiple=TRUE,
                                                           width="100%"


                                                         ),

                                 h4("Tabelle der Alternativen"),
                                 DT::dataTableOutput("AlternativenTable")
                        )

                        ) #tabsetpanel
            ) #mainPanel
        ) #sidebarLayout
    )#list (final Page)


    )#c
  ), #hidden

  ## Bottom Part ----
  br(),
  disabled(actionButton("prevBtn", "< Zurück")),
  disabled(actionButton("nextBtn", "Weiter >") ),
  hidden(actionButton("saveBtn", "Speichern und Ergebnis ansehen >" )),
  #see: https://stackoverflow.com/questions/6345549/right-align-link
  #see: https://stackoverflow.com/questions/25062422/restart-shiny-session
  #see: https://stackoverflow.com/questions/2906582/how-to-create-an-html-button-that-acts-like-a-link
  div( style="float: right;",
       HTML("<input type=\"button\"
            class=\"btn btn-default action-button shiny-bound-input\";
            onclick=\"location.href='javascript:history.go(0)';\" value=\"[Neustart]\" /> ") ),
  br(),
  textOutput("pageNrText")


)) #shinyUI(fluidPage
