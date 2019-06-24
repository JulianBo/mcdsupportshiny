
# Module for analysis of previous results ------------------------------------------

###UI----------

#' Title
#'
#' @param id
#' @param all_members
#'
#' @return
#' @export
#'
#' @examples
AnalysisPreviousUI<- function(id,dtIndikatorensettings,all_members=FALSE){
  ns <- NS(id)

  tagList(
    tabsetPanel(
      # tabPanel("Daten",
      #          tableOutput(ns("Bisherige") )
      # ),

      tabPanel("Favorisierte Ergebnisse" ,

               plotOutput(ns("BisherigeDecsPlot") )
      ),
      tabPanel("Abstimmungsverhalten",
               selectInput(ns("BisherigeHistsPlotSelect"),
                           "Bitte Bereich auswählen",
                           choices=unique(dtIndikatorensettings[,parent])),
               ##parent==input$BisherigeHistsPlotSelect
               plotOutput(ns("BisherigeHistsPlot"))
      ),
      tabPanel("Gruppenanalyse",

               numericInput(ns("ClusterNumberNumeric"),
                            "Bitte Anzahl der Gruppen auswählen",
                            value=3, min=1, max=6, step=1),

               h3("Verteilung der Abstimmungsergebnisse pro Gruppe"),
               selectInput(ns("BisherigeHistsClusterPlotSelect"),
                           "Bitte Bereich auswählen",
                           choices=unique(dtIndikatorensettings[,parent])),
               plotOutput(ns("BisherigeHistsClusterPlot")),

               h3("Zusammenhänge der Abstimmungsergebnisse pro Gruppe"),
               fluidRow(
                 column(
                   selectInput(ns("ClusterXSelect"),
                               "Bitte Indikator für X-Achse auswählen",
                               choices=setNames(dtIndikatorensettings$name_new, dtIndikatorensettings$name)),
                   width = 6),
                 column(
                   selectInput(ns("ClusterYSelect"),
                               "Bitte Indikator für Y-Achse auswählen",
                               choices=setNames(dtIndikatorensettings$name_new, dtIndikatorensettings$name)),
                   width = 6)
               ),
               plotOutput(ns("ClusterPlot")),
               h3("Tabelle der Gruppen"),
               DT::dataTableOutput(ns("ClusterTable"))
      )





    ),
    textOutput(ns("infotext") )
  )
}


###Server----------

#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param data_reactivepromise
#' @param dtIndikatorensettings
#' @param check_group one of TRUE FALSE group
#' @param group
#'
#' @return
#' @export
#'
#' @examples
AnalysisPrevious<- function(input, output, session,
                            data_reactivepromise,
                            dtIndikatorensettings,
                            check_group=FALSE,
                            group=NULL){



  # output$Bisherige<-renderTable({
  #   value(data_reactivepromise())
  #
  # }
  #   )


  output$infotext <- renderText({
    if(isTRUE(check_group) ) {
      paste0("Benutzte Umfragegruppe: `", group,"`")
    } else if(check_group=="reactive") {
      paste0("Benutzte Umfragegruppe: ", paste0("`", group,"`", collapse = ","))
    } else ""
  })

  # 0 Vorbereitende Reactives -----------------------------------------------

data_checkgroup<-function(data,check_group,group){
  if(isTRUE(check_group) ) {
    if (is.na(group)|group==""|group=="NA" ) {
      data[gruppe==group|gruppe=="NA" | is.na(gruppe)]
    }else {
      data[gruppe==group]
    }

  } else if(check_group=="reactive") {
    if(isTRUE(group()) ){
      data
    } else data[gruppe %in% group()]
  } else data
}


  dtBisherigeDecsMelted<-reactive(
    data_reactivepromise() %...>% {
      # message("####start melting for results####")
      data<-data_checkgroup(data=., check_group = check_group, group=group)


      melted<- melt(data,
                    id.vars=c("Zeitpunkt","Sessionstart", "session_id","gruppe", "url_search","addBtn"),
                    measure.vars=c("ChoiceSlct", "BestesErgebnis" ))

      melted[,value_new:=ifelse(grepl(",",value),"mehrere",value)]
      melted[,position:=ifelse(addBtn==0,"1) ursprünglich", "2) nach Ansicht")]
      melted[,modus:=ifelse(variable=="BestesErgebnis", "berechnet", "ausgewählt")]



      # message("####melted####")
      # message(melted)
      melted
    }
  )

  dtBisherigeMelted <- reactive(
    data_reactivepromise() %...>%{
      # print("####print bisherige ####")
      # print(.)
      # message("####start melting for originalweights####")

      data<-data_checkgroup(data=., check_group = check_group, group=group)

      #print(data)


      melt( data,
            id.vars=c("Zeitpunkt","Sessionstart", "session_id","gruppe", "url_search","addBtn"),
            measure.vars=grep("*.originalweights$", names(data), fixed=FALSE, value=TRUE) )


    }
  )

  dtBisherigeJoined <- reactive(
    dtBisherigeMelted() %...>%{
      # print("dtBisherigeMelted() ind dtbisherigejoined")
      # print(.)
      # print("dtIndikatorensettings ind dtbisherigejoined")
      # print(dtIndikatorensettings)
      # print(str(dtIndikatorensettings))
      dt<-.
      dtIndikatorensettings[negative==FALSE][dt, on=.(name_new==variable) ]
    }


  )

 ## Not needed any more - cluster changed
  # dtBisherigeWide <- reactive(
  #   dtBisherigeJoined()%...>%{
  #     print("dtBisherigeJoined() ind dtBisherigeWide ")
  #     print(.)
  #     dt<-.
  #     dcast(dt[!is.na(name)],
  #           Zeitpunkt+Sessionstart+session_id+gruppe+url_search+addBtn~ name
  #           , value.var  ="value")
  #   }
  #
  # )

  ###1 Ergebnisse ----------

  output$BisherigeDecsPlot<- renderPlot({
    # message("outside promise . plotting BisherigeDecsPlot")

    dtBisherigeDecsMelted() %...>% {

      ggplot(.,
             mapping = aes(x=value_new, fill=value_new))+
        geom_bar()+
        facet_grid(position~modus,scales = "free_y")+
        labs(fill = "Pfad", x="Pfad", y="Anzahl")
    }
  })

  ###2 Abstimmungsergebnisse ----------
  output$BisherigeHistsPlot <- renderPlot({
    dtBisherigeJoined() %...>% {
      #print(.)

      joined_dt<-.[parent==input$BisherigeHistsPlotSelect]

      # print("###input$BisherigeHistsPlotSelect###")
      # print(input$BisherigeHistsPlotSelect)
      #
      # print("###joined_dt<-.[parent==input$BisherigeHistsPlotSelect]###")
      # print(joined_dt)
      joined_summary<-joined_dt[,
                                .(m=mean(value),
                                  s=sd(value)),
                                by=.(name,parent)]

      ggplot(data= joined_dt,
             mapping = aes(x=value)
      )+
        #geom_density()+
        geom_histogram(binwidth = 1)+
        geom_vline(aes(xintercept=m), linetype=1, data=joined_summary)+
        geom_errorbarh(aes(xmin=m-s, xmax=m+s, x=NULL, y=2.5), linetype=1,
                       data=joined_summary)+
        geom_text(aes(x=m,y=3,label=paste0(" ",format(m, digits=3)," (",format(s, digits=3),")")),
                  data=joined_summary, vjust=0, hjust=0, size=3)+
        facet_grid(name~.)+ylab("Anzahl")+xlab("Wert")
    }
  })

  ###3 Gruppenanalyse ----------

  dtBisherigeCluster <- reactive(
    data_reactivepromise()%...>%{
      wide<-data_checkgroup(data=., check_group = check_group, group=group)
      kmeans_cluster<-kmeans(wide[,dtIndikatorensettings[, "name_new"][[1]], with=FALSE],
                             input$ClusterNumberNumeric)

      wide[,cluster:=as.factor(kmeans_cluster$cluster)]


    }
  )


  backtick<-function(x) paste0("`",x,"`")

  output$ClusterPlot<- renderPlot(
    dtBisherigeCluster()%...>%{
      ggplot(.,aes_string(x=backtick(input$ClusterXSelect),
                          y= backtick(input$ClusterYSelect),
                          color="cluster"))+
        geom_point()+
        xlab(dtIndikatorensettings[name_new==input$ClusterXSelect]$name)+
        ylab(dtIndikatorensettings[name_new==input$ClusterYSelect]$name)

    }
  )

  output$ClusterTable <- DT::renderDataTable(
    promise_all(joined_df = dtBisherigeJoined(),cluster_df = dtBisherigeCluster()) %...>% with({
      joined_cluster<-joined_df[cluster_df[,.(Zeitpunkt,Sessionstart,session_id,gruppe,url_search,addBtn, cluster)],
                                on=.(Zeitpunkt,Sessionstart,session_id,gruppe,url_search,addBtn)]

      joined_cluster_summary<-joined_cluster[,.(mean=mean(value), sd=sd(value)), by=.(name,cluster)]

      joined_cluster_summary[,diff_mean:=max(mean,na.rm=FALSE)-min(mean, na.rm=FALSE), by=.(name) ]
    })
  )

  output$BisherigeHistsClusterPlot<- renderPlot(
    promise_all(joined_df = dtBisherigeJoined(),cluster_df = dtBisherigeCluster()) %...>% with({
      joined_cluster<-joined_df[cluster_df[,.(Zeitpunkt,Sessionstart,session_id,gruppe,url_search,addBtn, cluster)],
                                on=.(Zeitpunkt,Sessionstart,session_id,gruppe,url_search,addBtn)]

      joined_dt<- joined_cluster[parent==input$BisherigeHistsClusterPlotSelect]

      joined_summary<-joined_dt[,
                                .(m=mean(value),
                                  s=sd(value)),
                                by=.(name,parent, cluster)][,cluster_number:= as.numeric(cluster)]

      ggplot(data= joined_dt,
             mapping = aes(x=value,color=cluster, fill=cluster)
      )+
        #geom_density()+
        geom_histogram(binwidth = 1)+
        geom_vline(aes(xintercept=m), linetype=1, data=joined_summary)+
        geom_errorbarh(aes(xmin=m-s, xmax=m+s, x=NULL, y=2.5), linetype=1,
                       data=joined_summary)+
        geom_text(aes(x=m,y=input$ClusterNumberNumeric+1 -cluster_number,color=NULL,
                      label=paste0(cluster, ": ",format(m, digits=3)," (",format(s, digits=3),")")),
                  data=joined_summary,
                  vjust=0, hjust=0, size=3, color="black")+
        facet_grid(name~.)+ylab("Anzahl")+xlab("Wert")

    })
  )
}
