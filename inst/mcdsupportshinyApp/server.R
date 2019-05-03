
# Vorbereiten  ------------------------------------------------------------


library(shiny)
library(shinyjs)

library (data.table)
library(plotly)
library(googlesheets)


library(promises)
library(future)
plan(multisession)

library(mcdsupportshiny)

# Initialisieren#########

#source("Setup.R", encoding="UTF-8") #local=FALSE, auch in ui.R sichtbar
source("Setup_INOLA.R", encoding="UTF-8") #local=FALSE, auch in ui.R sichtbar


validateConfig(configList,dtAlternativen)



# Globale Variablen berechnen ---------------------------------------------

dtIndikatorensettings<-getIndikatorensetting(configList)
vColors<-rColorVector(configList, color="blue")
dtIndikatorensettings<-dtIndikatorensettings[data.table(name=names(vColors),colors=vColors), on="name"]

dtIndikatorensettings[,slname:=paste("slGui2", gsub("[^A-Za-z0-9-]", "", name),"sl", sep = ns.sep)] #shiny:ns.sep; NS() not vectorised
dtIndikatorensettings[, number:=1:length(name)]

setcolorder(dtIndikatorensettings, "number")
setkey(dtIndikatorensettings,number)

# columns: bscName bscName.parent - Get each parent-child-combination.Only of collapsePanels.
# Parent=NA --> Top-level.
dtBscCombinations <- unique(merge(dtIndikatorensettings ,
                                 dtIndikatorensettings[,.(name, bscName )],
                                 by.x="parent", by.y = "name", suffixes = c("", ".parent"))
                           [!is.na(bscName), .(bscName,bscName.parent) ], na.rm=TRUE)
dtBscCombinations[,timesClicked:=0]
dtBscCombinations[,opened:=FALSE]
dtBscCombinations[,lastState:=""]
#visible:=FALSE, Visible lässt sich nur hinterher über opened berechnen, aufgrund BUG

##TODO: Warning.
dtAlternativen_long <- merge(melt(dtAlternativen, id.vars=c("Titel", "Rahmenszenario")),
                             dtIndikatorensettings, by.x="variable", by.y="Attribname" )

#Berechne Mittelwert
#Default: Mittelwert der Variablen, über alle Rahmenszenarien
dtAlternativen_long[,
                    centervar:=calculatecenterfunc(first(util_mean),value, first(util_offset)
                                                   ),
                    by=.(variable, negative)]

#Berechne Nutzen. Gruppiert, weil utilityfunc  einen single character vector für type erwartet (liegt am switch)
# TODO: utilityfunc vektorisieren???
#Ausgehend von Attributen, nicht von Indikatoren.
dtAlternativen_long[,nutzen:=utilityfunc(x=value,
                                        type=first(util_func),
                                        offset = util_offset,
                                        centervalue = centervar,
                                        scale=util_scale ),
                    by=.(variable, negative)]

#Füge Minimum und MAximum hinzu
#nötig um Nutzenfunktionen zu plotten; inkl. 5% außerhalb
dtAlternativen_long[,`:=`(   value_min=min(value)*0.95,
                             value_max=max(value)*1.05,
                             nutzen_min=min(nutzen)*0.95,
                             nutzen_max=max(nutzen)*1.05
                             ),
                    by=.(variable,negative,util_func, util_offset,util_offset, util_scale, centervar)]

#FÜge width hinzu, um es beim Plotten benutzen zu können (position_dodge)
#Siehe: https://stackoverflow.com/questions/48946222/ggplot-with-facets-provide-different-width-to-dodge-with-each-facet
dtAlternativen_long[,width_dodge:= getwidth(value_min, value_max)]

#Dodging
dtAlternativen_long[,`:=`( I_group=1:.N,
                 N_group=.N,
                 group=.GRP,
                 #Here the actual dodging is done
                 value_dodgedx = value - ( (1:.N-0.5) - .N/2) *width_dodge ),
          , by=.(variable, negative, value, nutzen)]

## Nutzenfunktionen, zum Plotten
dtNutzenFuncs <-  copy(dtAlternativen_long)[,.N,
                                            by=.(variable,negative, util_func, util_offset,util_offset, util_scale, centervar,value_min,value_max)]
dtNutzenFuncsList <- crossjoinFunc(dtNutzenFuncs,data.table(n=seq_len(101)-1))
dtNutzenFuncsList[,`:=`(
  x= value_min  +(value_max-value_min  )*n*1./100,
  y=utilityfunc(value_min  +(value_max-value_min  )*n*1./100,
  # x= value_min -10 +(value_max-value_min +20 )*n*1./100,
  # y=utilityfunc(value_min -10 +(value_max-value_min +20 )*n*1./100,
               type=first(util_func),
               offset = util_offset,
               centervalue = centervar,
               scale=util_scale)

), by=.(variable,negative )]



# Reactive data.tables vorbereiten ----------------------------------------


dtGewichtungen <- copy(dtIndikatorensettings[,.(colors=first(colors),
                                                number=first(number)
                                                ),
                                             by=.(name, is_mapping, level, parent, bscName, slname)])
setkey(dtGewichtungen, name)


##Nutzenwerte, um sie später reactive füllen zu können

# benutze "name" anstatt "variable", um auf Indikatoren(Mappings)
# anstatt auf Attribute(Alternativen) zu kommen
# dtNutzen <- dcast(dtAlternativen_long,Titel +Rahmenszenario~name,  value.var = "nutzen",
#                   fun.aggregate=max) ##muss später nochmal gefüllt werden, wegen negativen Zellen

dtNutzen<-copy(dtAlternativen[,.(Titel,Rahmenszenario)])
#Spalte für Szenarioergebnis
dtNutzen[,dtIndikatorensettings[level==0,first(parent)]  :=NA_real_]

##Füge weitere Spalten hinzu, um sie später zu füllen
# Alle Indikatoren
dtNutzen[,unique(dtIndikatorensettings[,name])  :=NA_real_]
# Alle Spalten von Gruppierungen von nicht zugeordneten Attributen
#NA= Nicht gewusst; 0 = Ausschließen
dtNutzen[,dtIndikatorensettings[is_mapping& is.na(Attribname),name]  :=0]

NutzenWerte<- as.matrix(dtNutzen[,.SD,
                                 .SDcols = names(dtNutzen)[-(1:2)] #!(names(dtNutzen) %in% c( "Titel", "Rahmenszenario"))]
                                 ]
)


# Connection to Database --------------------------------------------------


# pool <- dbPool(
#   drv = RMySQL::MySQL(),
#   dbname = "inola_test",
#   host = "db4free.net",
#   username = "inola_test_admin",
#   password = "test2035test"
# )


datastorage <- future(initialize_datastorage( speicher_template, speichersettings$method, speichersettings$place) )




# Define server logic ##########
shinyServer(function(input, output, session) {

# Session-Data -----------------------------

  session_start= date()
  session_id = as.integer(runif(n=1, max=1000000) )

  dtBisherige <- future({ loadData(speichersettings$method, speichersettings$place ) })

  ##Bisherige Daten laden.


# Load Modules -----------------------------
  # sliderCheckboxModules <-sapply(dtGewichtungen$name,
  #                                function(x) callModule(sliderCheckbox,x)
  #                                )

  #copy(dtBscCombinations) to separate Counting. Otherwise call-by-reference, to same data.table.
  # time1<- system.time(
    slGui1<-callModule(rSliderGui,"slGui1", dtGewichtungen$name,copy(dtBscCombinations) )
  # )
  # time2<- system.time(
    slGui2<-callModule(rSliderGui,"slGui2", dtGewichtungen$name,copy(dtBscCombinations) )
  # )

  #message(time1)

# Reactives berechnen -----------------------------------------------------

  rv_dtGewichtungen <- reactive({

    #Zum Testen:
    #dtGewichtungen[,originalweights:= c(-10,10,1:10*10, 10,10)]

    dtGewichtungen[,originalweights:=slGui2$sliderCheckBoxValues()]

    ##HIER EIGENTLICHE LOGIK
    #getrennt nach allen leveln aufaggregieren
    #Summe aller Einstellungen pro Level
    dtGewichtungen[ ,sum_in_level:=sum(abs(originalweights), na.rm = TRUE) ,
                    by=.(parent, level)]

    dtGewichtungen[,finalweight_in_level :=
                     #alle 0 ausschließen
                     ifelse(sum_in_level==0, 0, abs(originalweights)/sum_in_level) ]

    ## Korrigierte Gewichtungen, wo nicht zugeordnete Variablen und Äste,
    ## in denen alle Gewichtungen auf 0 gesetzt werden, nicht berücksichtigt werden
    ## Muss absteigend geschehen, weil sich 0-Werte von den Blättern propagieren könnten,
    ## falls dort auch nicht zugeordnete Variablen wären.

    dtGewichtungen[,finalweight_in_level_corrected:=0]

    for( i in max(  dtIndikatorensettings$level):0){

      ##TODO: Childssumcorrected anpassen!!!

      dtGewichtungen[!(is_mapping )&level==i,
                     sum_in_level_corrected := sum(abs(originalweights), na.rm = TRUE) ,
                     by=.(parent, level)]


      dtGewichtungen[dtGewichtungen[level==i,
                                    .(name=first(parent),childs_sum_corrected=first(sum_in_level_corrected) ),
                                    by=.(name)] ,
                     childs_sum_corrected:=childs_sum_corrected
                      ]
      #print(dtGewichtungen[level==i])

      dtGewichtungen[,finalweight_in_level_corrected :=
                       #alle 0 und NA ausschließen
                       ifelse(is.na(sum_in_level_corrected) | sum_in_level_corrected==0,
                              0,
                              abs(originalweights)/sum_in_level_corrected) ]

    }



    #Nur relevanten Spalten zurückliefern
    dtGewichtungen[,
                   .(name,slname, is_mapping,level,
                     parent,
                     originalweights,
                     sum_in_level, finalweight_in_level, sum_in_level_corrected,
                     finalweight_in_level_corrected)]


    }) #end of rv_dtGewichtungen





  rv_dtSzenarioergebnis <- reactive({


    ## Blätter in Abhängigkeit von NEgativität füllen
    for (x in dtIndikatorensettings[is_mapping&!is.na(Attribname),unique(name)] ){
      # print(x)
      # print(rv_dtGewichtungen()[name==x])
      # print(dtAlternativen_long[name==x & negative==(rv_dtGewichtungen()[name==x]$originalweight<0)])
      NutzenWerte[,x]<-dtAlternativen_long[name==x & negative==(rv_dtGewichtungen()[name==x]$originalweight<0)]$nutzen
    }



    ## #Eigentliche Logik.
   for( i in max(  dtIndikatorensettings$level):0)
     for (x in dtIndikatorensettings[level==i,.N, by=parent]$parent){
      # print(paste0("---- i=",i,";x= ",x, " ---"))
      # print(NutzenWerte[,dtIndikatorensettings[parent==x &level==i,unique(name)] ])
      # print(rv_dtGewichtungen()[parent==x &level==i])

        NutzenWerte[,x]<-
          NutzenWerte[,dtIndikatorensettings[parent==x &level==i,unique(name)] ] %*%
          as.matrix(rv_dtGewichtungen()[parent==x &level==i, finalweight_in_level])
        # Added as.matrix for 1x1 matrices, else treated as scalar (with error)
           #dtGewichtungen[parent==x &level==i,finalweight_in_level]## Zum Testen

     }

    #print(NutzenWerte)

  return( data.table(dtNutzen[,.(Titel,Rahmenszenario)], NutzenWerte))
  #dtNutzen= data.table(dtNutzen[,.(Titel,Rahmenszenario)], NutzenWerte) #zum testen

  })

  rv_dtErgebnis <- reactive({
    rv_dtSzenarioergebnis()[,.(Gesamtergebnis=mean(Szenarioergebnis) ),by=Titel]
  #dt_Ergebnis = dtNutzen[,.(Gesamtergebnis=mean(Szenarioergebnis) ),by=Titel] #zum testen

  })

  rv_BestesErgebnis <- reactive({
    rv_dtErgebnis()[rv_dtErgebnis()[, .I[Gesamtergebnis==max(Gesamtergebnis)],], unique(Titel)]
  })


  formData <- reactive({

    #print(names(input))

    #### ALL inputs
    # #only first row, since bug in bsCollapse returns multiple rows...
    # # Replace Null-Values in bsCollapse, if no Panel opened
    # test <-  sapply(names(input),
    #                 function(x) ifelse(is.null(input[[x]]), NA, first(input[[x]]) )
    #                 )

    c( #test,

      ##Hintergrunddaten
      Zeitpunkt=date(),
      Sessionstart=session_start,
      session_id=session_id,
      gruppe=ifelse(is.null(parseQueryString(session$clientData$url_search)[["gruppe"]]),
                    NA,
                    parseQueryString(session$clientData$url_search)[["gruppe"]]),
      url_search=session$clientData$url_search ,
      speichernBtn=input$speichernBtn,
      addBtn=input$addBtn,

      ##Umfragedaten
      PlaceSlct=input$PlaceSlct,
      FirsttimeSlct=input$FirsttimeSlct,
      GenderSlct=input$GenderSlct,
      AgeSl=input$AgeSl,
      ChoiceSlct=input$ChoiceSlct,
      ChoiceSlctCount=rv$ChoiceSlctCount,
      ChoiceFinalSlct=input$ChoiceFinalSlct,
      ChoiceFinalSlctCount=rv$ChoiceFinalSlctCount,
      ## Ergebnis
      # Es können auch mehrere beste Ergebnisse sein
      BestesErgebnis= paste(levels(rv_BestesErgebnis())[rv_BestesErgebnis()], collapse=", " ),

      ##Gewichtungen
      setNames(rv_dtGewichtungen()$originalweights,
               paste0(rv_dtGewichtungen()$slname, ".originalweights"  )),
      setNames(rv_dtGewichtungen()$finalweight_in_level,
               paste0(rv_dtGewichtungen()$slname, ".finalweight_in_level"  )),
      setNames(rv_dtGewichtungen()$finalweight_in_level_corrected,
               paste0(rv_dtGewichtungen()$slname, ".finalweight_in_level_corrected"  )),
      ## Status CollapsePanels
      #Add timesClicked of slGui1 and slGui2
      setNames(slGui1$collapsePanelValues()$timesClicked + slGui2$collapsePanelValues()$timesClicked ,
               paste0(slGui2$collapsePanelValues()$bscName, ".timesClicked"  )),
      setNames(slGui2$collapsePanelValues()$visible ,
               paste0(slGui2$collapsePanelValues()$bscName, ".visible"  ))


      )
  })


  rv_dtformData<- reactive({
    data.table(t(formData() )) #TODO: DAS muss besser gehen.
  })

  rv_dtformData_long<- reactive({
    data.table(variable=names(formData()),
               values= formData() ) #TODO: DAS muss besser gehen.
  })

  # Reactive Values & Aktionen durchführen ----------------------------------------------------


  rv<- reactiveValues(data=data.table(),
                      page = 1,
                      ChoiceSlctCount=0,
                      ChoiceFinalSlctCount=0
                      )


  observeEvent(input$addBtn,{

    daten<- rv_dtformData()

    rv$data=rbind(rv$data,daten )

    future( {
      saveData(daten,speichersettings$method, speichersettings$place )
     # message("saving after input$addBtn DONE")
      })
    updateSelectInput(session,"ChoiceFinalSlct", selected = input$ChoiceSlct) #TODO BUG doesn't work
    rv$ChoiceFinalSlctCount<-rv$ChoiceFinalSlctCount-1 #account for manual change.

    hide(id="abstimmungsDiv")
    show(id="dankeDiv")


  })

  observeEvent(input$ChoiceSlct,
               rv$ChoiceSlctCount<-rv$ChoiceSlctCount+1,
               ignoreInit = TRUE
  )

  observeEvent(input$ChoiceFinalSlct,
               rv$ChoiceFinalSlctCount<-rv$ChoiceFinalSlctCount+1,
               ignoreInit = TRUE
               )





# GUI Updaten -------------------------------------------------------------

  ####GUI Updaten ---PageChange ####
  observeEvent(rv$page,{
    #NUM_Pages including resultpage
    NUM_PAGES <- input$NUM_PAGES

    if (rv$page  > 0 & rv$page  <= NUM_PAGES){
      hide(selector = ".page") #To hide other pages.
      show(paste0("page", rv$page))
      shinyjs::runjs("window.scrollTo(0, 0)")

      ##Next nur bis vorletzte Seite
      toggleState(id = "nextBtn", condition = rv$page <= NUM_PAGES -2)
      ##Next ab vorletzter Seite unsichtbar
      toggle(id = "nextBtn", condition = rv$page <= NUM_PAGES -2)
      ##SaveBtn nur auf letzter Seite
      toggle(id = "saveBtn", condition = rv$page == NUM_PAGES -1)
      ##PrevBtn nicht am Anfang
      toggleState(id = "prevBtn", condition = rv$page > 1 )
      ## PRevBtn am Ende nicht mehr sichtbar  Am Ende geht es nicht mehr zurück
      toggle(id = "prevBtn", condition = rv$page < NUM_PAGES)

      ##am Ende scroll to Result
      if (rv$page == NUM_PAGES)shinyjs::runjs("document.getElementById('MainTabset').scrollIntoView();")
    }

  })

  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }

  output$pageNrText=renderText(paste0("Seite ",rv$page," von ", input$NUM_PAGES))

  observeEvent(input$prevBtn, navPage(-1))
  observeEvent(input$nextBtn, navPage(1))

  observeEvent(input$saveBtn, {
    #TODO: SlGui2 updaten - inclduing collapsePanels!.

    syncSliderGuiInputs(slGui2, slGui1)

    navPage(1)

    daten<- rv_dtformData()

    rv$data=rbind(rv$data,daten )

    future({
      saveData(daten,speichersettings$method, speichersettings$place )
      #message("saving after input$saveBtn DONE")
    })

    })

  ####GUI Updaten ---Entscheidungen ####

  #Entscheidungen als Text

  output$ErgebnisText<- renderText({
    paste(rv_BestesErgebnis(),collapse=", ")
  })

  output$ChoiceText <- renderText({
    input$ChoiceSlct
  })

  output$ErgebnisPlot<- renderPlot({
    ggplot(rv_dtErgebnis(),aes(x=Titel,y=Gesamtergebnis, fill=Titel))+
      geom_col()+
      ylab("Punktzahl")
  })


  output$ErgebnisTable <- renderTable(rv_dtErgebnis() )

  output$SzenarioPlot<- renderPlot({
    ggplot(rv_dtSzenarioergebnis(), aes(y=Szenarioergebnis,fill=Titel,x=Titel,  shape=Rahmenszenario))+
      geom_col(position="dodge" )+
      scale_shape_manual(values=21:24)+
      geom_point(colour="Black", position=position_dodge(width=1))+
      ylab("Punktzahl")


  }  )





  #Entscheidungen visualisieren

  output$EntscheidungenPlot<-renderPlot({

    dtErgebnislong <- melt(rv_dtSzenarioergebnis(), id.vars=c("Titel", "Rahmenszenario"))


    ggplot(dtErgebnislong, aes(y=value,fill=Titel,x=Titel,  shape=Rahmenszenario))+
      facet_wrap(~variable)+
      geom_col(position="dodge" )+
      scale_shape_manual(values=21:24)+
      geom_point(colour="Black", position=position_dodge(width=1))+
      ylab("Punktzahl")

  })





  #Entscheidungen als Tabelle ausgeben
  output$EntscheidungenTable<- renderTable({rv_dtSzenarioergebnis()})

  ####GUI Updaten ---Rest ####

  #Dummy Call Um Observer im Modul "sliderCheckbox" zu initialisieren

  output$Aux_to_initialise<- renderText({

    ##Fehler, falls direkt in ui.R definiert: Error in if: argument is of length zero
    ##-Verschwindet auf dritter Seite. Da scheinen dann alle Slider initialisiert worden sein,
    ## selbst wenn einige noch in CollapsePanel sind.
    rv_dtGewichtungen()[,sum(originalweights)]

    return("")
  })

  #Direkte Gewichtungen berechnen
  output$DirGewichtungenTable<-renderTable( rv_dtGewichtungen())

  #Alternativen anzeigen
  output$AlternativenTable<- renderTable({dtAlternativen})


  #Nutzenfunktionen anzeigen
  output$NutzenPlot <- renderPlot({

    # add width to position dodge, different for each facet.
    #See: https://stackoverflow.com/questions/48946222/ggplot-with-facets-provide-different-width-to-dodge-with-each-facet
    ggplot(dtAlternativen_long,aes(x=value_dodgedx, y=nutzen, shape=Rahmenszenario, fill=Titel, alpha=as.numeric(negative) ))+
      #geom_col(aes(fill=Titel),position = "dodge")+
      geom_rect(aes(xmin=value_dodgedx-width_dodge/2,
                    xmax=value_dodgedx+width_dodge/2,
                    ymax=nutzen , fill=Titel,
                    linetype=negative),
                colour="black", ymin=0 )+
      scale_shape_manual(values=21:24)+
      scale_alpha_continuous(range=c(1,0.8),guide = 'none')+
      scale_linetype_discrete( name="Bewertungsbereich",labels=c("positiv", "negativ"))+
      scale_fill_discrete(name="Ausbaupfad")+
      geom_point(colour="Black")+
      labs(x="Wert",y="Punktzahl")+
      facet_wrap(~variable, scales = "free_x")+ # facet_wrap nach Slidernamen wäre "name", funktioniert nicht
      geom_path(data =dtNutzenFuncsList , mapping=aes(x=x,y=y, linetype=negative), inherit.aes = FALSE )
   })

  #Indikatorensettings
  output$Indikatorensettings<- renderTable({dtIndikatorensettings})



  #GUI - Bisheriges Abstimmungsverhalten anzeigen ---------
  output$BisherigeTable<-renderTable(value(dtBisherige) )
  output$BisherigeDecsPlot<- renderPlot({
    #message("outside promise . plotting BisherigeDecsPlot")
    dtBisherige %...>% {
      #message("inside promise . plotting BisherigeDecsPlot")
      ggplot(., mapping = aes(x=ChoiceSlct, fill=ChoiceSlct))+
        geom_bar()
    }
  })

  output$BisherigeHistsPlot<- renderPlot({
    dtBisherige %...>% {
      ggplot(data=melt( . ,
                        id.vars=c("Sessionstart", "session_id"),
                        measure.vars=
                          grep("^sl.*originalweights$", names(.), fixed=FALSE, value=TRUE)
      ),
      mapping = aes(x=value))+
        geom_density()+
        facet_grid(variable~.)
    }
  })

  ####GUI Updaten ---R Helferfunktionen ####

  # ##R Helferfunktionen; um anzuschauen was abgeht.

  # output$RoutputPrint<- renderPrint({
  #
  #   str(rv_dtformData())
  # })
  #
  # output$RoutputTable1<- renderTable({
  #
  #   slGui2$collapsePanelValues()
  # })
  # output$RoutputTable2<- renderTable({
  #
  #   slGui1$collapsePanelValues()
  # })
  # output$RoutputTable <- renderTable({
  #   rv$data
  #   #rv_dtformData_long()
  #   #rv$bscValues
  # })

  # output$RoutputText2<- renderPrint({
  #   #rv$data #NULL data.table
  #   slGui2$collapsePanelValues()
  #   str(slGui2$collapsePanelValues())
  # })

})
