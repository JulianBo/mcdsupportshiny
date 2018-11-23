#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Vorbereiten  ------------------------------------------------------------


library(shiny)
library(shinyjs)

library (data.table)
library(ggplot2)


library(mcdsupportshiny)

# Initialisieren#########

source("Setup.R", encoding="UTF-8") #local=FALSE, auch in ui.R sichtbar
#source("Setup_INOLA.R", encoding="UTF-8") #local=FALSE, auch in ui.R sichtbar


validateConfig(configList,dtAlternativen)


# Connection to Database --------------------------------------------------


# pool <- dbPool(
#   drv = RMySQL::MySQL(),
#   dbname = "inola_test",
#   host = "db4free.net",
#   username = "inola_test_admin",
#   password = "test2035test"
# )


initialize_datastorage( speicher_template, speichersettings$method, speichersettings$place)


# Globale Variablen berechnen ---------------------------------------------


dtIndikatorensettings<-getIndikatorensetting(configList)
dtIndikatorensettings[,slname:=paste0("sl",name)]
dtIndikatorensettings[,colors:=rColorVector(configList, color="blue")]

setkey(dtIndikatorensettings,name)

# columns: bscName bscName.parent - Get each parent-child-combination.
dtBscCombinations <- unique(merge(dtIndikatorensettings ,
                                 dtIndikatorensettings[,.(name, bscName )],
                                 by.x="parent", by.y = "name", suffixes = c("", ".parent"))
                           [, .(bscName,bscName.parent) ])
dtBscCombinations[,timesClicked:=0]
dtBscCombinations[,opened:=FALSE]
dtBscCombinations[,lastState:=""]
#visible:=FALSE, Visible lässt sich nur hinterher über opened berechnen, aufgrund BUG


dtAlternativen_long <- merge(melt(dtAlternativen, id.vars=c("titel", "rahmenszenario")),
                             dtIndikatorensettings, by.x="variable", by.y="Attribname" )

#Berechne Mittelwert
#Default: Mittelwert der Variablen, über alle Rahmenszenarien
dtAlternativen_long[,
                    centervar:=calculatecenterfunc(first(util_mean),value, first(util_offset)
                                                   ),
                    by=variable]

#Berechne Nutzen. Gruppiert, weil utilityfunc  einen single character vector für type erwartet (liegt am switch)
#Ausgehend von Attributen, nicht von Indikatoren.
dtAlternativen_long[,nutzen:=utilityfunc(x=value,
                                        type=first(util_func),
                                        offset = util_offset,
                                        centervalue = centervar,
                                        scale=util_scale),
                    by=variable]

#Füge Minimum und MAximum hinzu
#nötig um Nutzenfunktionen zu plotten; inkl. 5% außerhalb
dtAlternativen_long[,`:=`(   value_min=min(value)*0.95,
                             value_max=max(value)*1.05,
                             nutzen_min=min(nutzen)*0.95,
                             nutzen_max=max(nutzen)*1.05
                             ),
                    by=.(variable,util_func, util_offset,util_offset, util_scale, centervar)]

#FÜge width hinzu, um es beim Plotten benutzen zu können (position_dodge)
#Siehe: https://stackoverflow.com/questions/48946222/ggplot-with-facets-provide-different-width-to-dodge-with-each-facet
dtAlternativen_long[,width_dodge:= getwidth(value_min, value_max)]

#Dodging
dtAlternativen_long[,`:=`( I_group=1:.N,
                 N_group=.N,
                 group=.GRP,
                 #Here the actual dodging is done
                 value_dodgedx = value - ( (1:.N-0.5) - .N/2) *width_dodge ),
          , by=.(variable, value, nutzen)]

## Nutzenfunktionen, zum Plotten
dtNutzenFuncs <-  copy(dtAlternativen_long)[,.N,
                                            by=.(variable,util_func, util_offset,util_offset, util_scale, centervar,value_min,value_max)]
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

), by=.(variable,util_func, util_offset, centervar)]



# Reactive data.tables vorbereiten ----------------------------------------


dtGewichtungen <- copy(dtIndikatorensettings)
setkey(dtGewichtungen, name)


##Nutzenwerte, um sie später reactive füllen zu können

# benutze "name" anstatt "variable", um auf Indikatoren(Mappings)
# anstatt auf Attribute(Alternativen) zu kommen
dtNutzen <- dcast(dtAlternativen_long,titel +rahmenszenario~name,  value.var = "nutzen")

##Füge weitere Spalten hinzu, um sie später zu füllen
# Alle Spalten von Gruppierungen
if (any(!dtIndikatorensettings$is_mapping) )dtNutzen[,dtIndikatorensettings[!(is_mapping),name]  :=NA]
# Alle Spalten von Gruppierungen von nicht zugeordneten Attributen
#NA= Nicht gewusst; 0 = Ausschließen
dtNutzen[,dtIndikatorensettings[is_mapping& is.na(Attribname),name]  :=0]
#Spalte für Szenarioergebnis
dtNutzen[,dtIndikatorensettings[level==0,first(parent)]  :=NA]


important_columns =c( "titel", "rahmenszenario","Szenarioergebnis")
setcolorder(dtNutzen,  c(important_columns,
                           names(dtNutzen)[!(names(dtNutzen) %in% important_columns)]) )




# Define server logic ##########
shinyServer(function(input, output, session) {

# Session-Data -----------------------------

  session_start= date()
  session_id = as.integer(runif(n=1, max=1000000) )

  dtBisherige <- loadData(speichersettings$method, speichersettings$place )

  ##Bisherige Daten laden.


# Load Modules -----------------------------
  sliderCheckboxModules <-sapply(dtGewichtungen$name,
                                 #TODO: standardweight when creating sliderCheckboxInput
                                 function(x) callModule(sliderCheckbox,x,
                                                        default =dtGewichtungen[name==x,]$standardweight
                                                        #,name=x
                                                        )
                                 )

# Reactives berechnen -----------------------------------------------------

  rv_dtGewichtungen <- reactive({

    #Zum Testen:
   # dtGewichtungen[,originalweights:= c(0:10*10, 10,10)]

   #print(sapply( dtGewichtungen$slname, function(x) input[[x]]))

    dtGewichtungen[,originalweights:=sapply(name,
                                            function(x) sliderCheckboxModules[[x]] ()*1.

                                            )]
                   #originalweights:=sapply(slname, function(x) input[[x]])] ##alt

   # print(dtGewichtungen)


    ##HIER EIGENTLICHE LOGIK
    #getrennt nach allen leveln aufaggregieren
    #Summe aller Einstellungen pro Level
    dtGewichtungen[ ,sum_in_level:=sum(originalweights, na.rm = TRUE) ,
                    by=.(parent, level)]

    dtGewichtungen[,finalweight_in_level :=
                     #alle 0 ausschließen
                     ifelse(sum_in_level==0, 0, originalweights/sum_in_level) ]

    ## Korrigierte Gewichtungen, wo nicht zugeordnete Variablen und Äste,
    ## in denen alle Gewichtungen auf 0 gesetzt werden, nicht berücksichtigt werden
    ## Muss absteigend geschehen, weil sich 0-Werte von den Blättern propagieren könnten,
    ## falls dort auch nicht zugeordnete Variablen wären.

    dtGewichtungen[,finalweight_in_level_corrected:=0]

    for( i in max(  dtIndikatorensettings$level):0){

      ##TODO: Childssumcorrected anpassen!!!

      dtGewichtungen[!(is_mapping &is.na(Attribname) )&level==i,
                     sum_in_level_corrected := sum(originalweights, na.rm = TRUE) ,
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
                              originalweights/sum_in_level_corrected) ]

    }



    # print(
    #   dtGewichtungen[,
    #                  .(name,slname, is_mapping,Attribname,level,
    #                    parent,
    #                    originalweights,
    #                    sum_in_level, finalweight_in_level, sum_in_level_corrected,
    #                    finalweight_in_level_corrected)]
    # )
    #Nur relevanten Spalten zurückliefern
    dtGewichtungen[,
                   .(name,slname, is_mapping,Attribname,level,
                     parent,
                     originalweights,
                     sum_in_level, finalweight_in_level, sum_in_level_corrected,
                     finalweight_in_level_corrected)]


    }) #end of rv_dtGewichtungen

  rv_dtSzenarioergebnis <- reactive({

    NutzenWerte<- as.matrix(dtNutzen[,.SD,
                                     .SDcols = names(dtNutzen)
                                     [!(names(dtNutzen) %in% c( "titel", "rahmenszenario"))]
                                     ]
                            )

    #Eigentliche Logik.
   for( i in max(  dtIndikatorensettings$level):0)
     for (x in dtIndikatorensettings[level==i,.N, by=parent]$parent){
      # print(paste0("---- i=",i,";x= ",x, " ---"))
      # print(NutzenWerte)
      # print(rv_dtGewichtungen())
        NutzenWerte[,x]<-
          NutzenWerte[,dtIndikatorensettings[parent==x &level==i,name] ] %*%
          as.matrix(rv_dtGewichtungen()[parent==x &level==i, finalweight_in_level])
        # Added as.matrix for 1x1 matrices, else treated as scalar (with error)

           #dtGewichtungen[parent==x &level==i,finalweight_in_level]## Zum Testen

     }

    #print(NutzenWerte)

  return( data.table(dtNutzen[,.(titel,rahmenszenario)], NutzenWerte))
  #dtNutzen= data.table(dtNutzen[,.(titel,rahmenszenario)], NutzenWerte) #zum testen

  })

  rv_dtErgebnis <- reactive({
    rv_dtSzenarioergebnis()[,.(Gesamtergebnis=mean(Szenarioergebnis) ),by=titel]
  #dt_Ergebnis = dtNutzen[,.(Gesamtergebnis=mean(Szenarioergebnis) ),by=titel] #zum testen

  })

  rv_BestesErgebnis <- reactive({
    rv_dtErgebnis()[rv_dtErgebnis()[, .I[Gesamtergebnis==max(Gesamtergebnis)],], unique(titel)]
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
      setNames(rv_dtBscStates()$timesClicked ,
               paste0(rv_dtBscStates()$bscName, ".timesClicked"  )),
      setNames(rv_dtBscStates()$visible ,
               paste0(rv_dtBscStates()$bscName, ".visible"  ))


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


  rv<- reactiveValues(bscValues=dtBscCombinations,
                      #https://stackoverflow.com/questions/32536940/shiny-reactivity-fails-with-data-tables
                      #because no reactivity inside data.tables, extra value to trigger update
                      bscValues_update=0,

                      data=data.table()
                      )


  observeEvent(input$speichernBtn, once=TRUE, {
    message(input$speichernBtn)
    if(input$speichernBtn>0){



      daten<- rv_dtformData()

      rv$data=rbind(rv$data,daten )

      saveData(daten,speichersettings$method, speichersettings$place )

      updateTabsetPanel(session,"MainTabset", "Entscheidungen")
      #print(rv$data)

      #TODO
      #<li class="active">
      # <a href="#tab-4365-1" data-toggle="tab" data-value="Informationen">Informationen</a>
      #   </li>
      #<div class="tab-pane active" data-value="Informationen" id="tab-4365-1">

      #removeUI()
    }
  })

  observeEvent(input$addBtn,{

    daten<- rv_dtformData()

    rv$data=rbind(rv$data,daten )

    saveData(daten,speichersettings$method, speichersettings$place )

  })


  ## Ausklapp Gruppen, zum zählen wie häufig geöffnet.
  # Siehe https://stackoverflow.com/questions/38950886/generate-observers-for-dynamic-number-of-inputs
  observers <- lapply(unique(dtIndikatorensettings[!is.na(bscName), bscName]), function(x){
   # print(x)

    #Observe Closing as well, Ignore first time.
    observeEvent(input[[x]], ignoreNULL = FALSE, ignoreInit = TRUE,{

      #print( rv$bscValues)
      rv$bscValues[bscName==x,':='(timesClicked=timesClicked+1,
                                   opened=!opened,
                                   #Aufgrund BUG in bsCollapse;
                                   lastState=paste0(input[[x]],collapse=";")
                                   )
                                ]
      #Falls vorhanden, Eltern-Knoten anpassen, um die folgende -fehlerhafte-
      # Aktivierung der Observer der übergeordneten collapsePanels auszugleichen
      rv$bscValues[
                   #Elternknoten über dtIndikatorensettings herausfinden
                   bscName==dtIndikatorensettings[name==dtIndikatorensettings[x==bscName,unique(parent)], bscName],
                   ':='(timesClicked=timesClicked-1,
                        opened=!opened,
                        #Aufgrund BUG in bsCollapse;
                        lastState=paste0(input[[x]],collapse=";")
      )
      ]

      #Um Reaktivität trotz data.table zu triggern.
      #https://stackoverflow.com/questions/32536940/shiny-reactivity-fails-with-data-tables
      rv$bscValues_update<-rv$bscValues_update+1

    })

  })

  rv_dtBscStates <- reactive({
    #Visible bei Kindknoten updaten, rekursiv
    #Sichtbarkeit der jeweiligen Level hinzufügen - geht nur außerhalb und nach Observer

    rv$bscValues_update

    ret <- copy(rv$bscValues)

    ret[,visible:=recursiveTrue(bscName, opened,bscName.parent )
        ]
    # print("visible updated")
    # print(ret)
    return(ret)
  })


# GUI Updaten -------------------------------------------------------------

  ####GUI Updaten ---Entscheidungen ####

  output$ErgebnisPlot<- renderPlot({
    ggplot(rv_dtErgebnis(),aes(x=titel,y=Gesamtergebnis, fill=titel))+
      geom_col()
  })

  #Entscheidungen als Text

  output$ErgebnisText<- renderText({
        paste(rv_BestesErgebnis(),collapse=", ")
  })

  output$ChoiceText <- renderText({
    input$ChoiceSlct
  })

    # print(rv_dtErgebnis()[rv_dtErgebnis()[, .I[Gesamtergebnis==max(Gesamtergebnis)],], unique(titel)],
    #       max.levels=0, row.names=FALSE)





  output$ErgebnisTable <- renderTable(rv_dtErgebnis() )

  #Entscheidungen visualisieren

  output$EntscheidungenPlot<-renderPlot({

    dtErgebnislong <- melt(rv_dtSzenarioergebnis(), id.vars=c("titel", "rahmenszenario"))


    ggplot(dtErgebnislong, aes(y=value,fill=titel,x=titel,  shape=rahmenszenario))+
      facet_wrap(~variable)+
      geom_col(position="dodge" )+
      scale_shape_manual(values=21:24)+
      geom_point(colour="Black", position=position_dodge(width=1))

  })

  #Entscheidungen als Tabelle ausgeben
  output$EntscheidungenTable<- renderTable({rv_dtSzenarioergebnis()})

  ####GUI Updaten ---Rest ####

  #Dummy Call Um Observer im Modul "sliderCheckbox" zu initialisieren
  output$Aux_to_initialise_rv_dtGewichtungen <- renderTable({
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
    ggplot(dtAlternativen_long,aes(x=value_dodgedx, y=nutzen, shape=rahmenszenario, fill=titel))+
      #geom_col(aes(fill=titel),position = "dodge")+
      geom_rect(aes(xmin=value_dodgedx-width_dodge/2,
                    xmax=value_dodgedx+width_dodge/2,
                    ymax=nutzen , fill=titel),
                ymin=0 )+
      scale_shape_manual(values=21:24)+
      geom_point(colour="Black")+
      facet_wrap(~variable, scales = "free_x")+ # facet_wrap nach Slidernamen wäre "name", funktioniert nicht
      geom_path(data =dtNutzenFuncsList , mapping=aes(x=x,y=y), inherit.aes = FALSE )
  })

  #Indikatorensettings
  output$Indikatorensettings<- renderTable({dtIndikatorensettings})



  #GUI - Bisheriges Abstimmungsverhalten anzeigen ---------
  # output$BisherigeTable<-renderTable(dtBisherige)
  output$BisherigeDecsPlot<- renderPlot({
    ggplot(data=dtBisherige, mapping = aes(x=ChoiceSlct, fill=ChoiceSlct))+
      geom_bar()


  })
  output$BisherigeHistsPlot<- renderPlot({
    ggplot(data=melt(dtBisherige,
                     id.vars=c("Sessionstart", "session_id"),
                     measure.vars=
                       grep("^sl.*originalweights$", names(dtBisherige), fixed=FALSE, value=TRUE)
                     ),
           mapping = aes(x=value))+
      geom_density()+
      facet_grid(variable~.)
  })

  ####GUI Updaten ---R Helferfunktionen ####

  # ##R Helferfunktionen; um anzuschauen was abgeht.
  output$RoutputPrint<- renderPrint({

    str(sliderCheckboxModules)

  })
  #
  # output$RoutputTable1<- renderTable({
  #
  #   print("updating RoutputTable1")
  #   #rv$bscValues
  #   rv_dtBscStates()
  # })
  #
  # output$RoutputTable <- renderTable({
  #   rv$data
  #   #rv_dtformData_long()
  #   #rv$bscValues
  # })


})
