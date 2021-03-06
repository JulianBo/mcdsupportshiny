
# GUI-Functions -----------------------------------------------------------


# Module SliderGui -----------------------------------------------------------

#' Creates List of Sliders.
#'
#' @param x Configuration list, see \code{\link{validateConfig}}.
#' @param breaking Should SliderGui be split up into a list of several pages, or deliver
#'                 one single list of sliders?
#'                 \describe{
#'                    \item{NULL or FALSE}{No pagination.}
#'                    \item{TRUE}{Put all subordinate elements of first order
#'                                on extra pages, add main page containing only first order elements.}
#'                    \item{Integer}{Put all subordinate elements of nth order on extra pages, add
#'                                   main page containing elements up to nth order.
#'                                   1 corresponds to TRUE.}
#'                 }
#'                 Maybe in future versions possibilities for putting special Elements on
#'                 extra pages by name will be added, right now this is not possible.
#' @param title_text Which paragraph should be added as first element of each page?TRUE for automatic generation,  FALSE for none.
#' @param mainpageposition One of "first", "last", "none".
#'                         Should main page be first or last or be omitted?
#' @param parents_name Label used to name root when creating fold-out-panel for root.
#' @param minweight Standard minimum slider weight.
#' @param maxweight Standard maximum slider weight.
#' @param standardweight Standard slider weight.
#' @param open.maxdepth From which depth on to create fold-out-panels, if not declared otherwise in x.
#'                      Maximum:12 (see \code{\link{shiny:column}}).Must be larger than breaking (if provided).
#' @param cb_title Default description label for checkbox.
#' @param sliderlabel Default class of slidercheckboxes, to use with \href{https://stackoverflow.com/a/42887905}{custom
#' prettify-function to have individual labels}.
#' @param id
#' @param beschreibungs_text
#'
#' @return Slidergui: List of Sliders (breaking== NULL or breaking==FALSE)
#'                    or list of SliderGui-pages (breaking==TRUE or Breaking>0).
#' @export
#'
#' @examples
rSliderGuiInput<-function(id, x,
                     breaking=NULL,
                     beschreibungs_text=NULL,
                     title_text="Please adjust the sliders according to your preferences.",
                     mainpageposition=c("first","last", "none"),
                     parents_name="Hauptkategorien",
                     minweight=0,maxweight=100, standardweight=30,
                     open.maxdepth=Inf,
                     cb_title= "I don't know",
                     sliderlabel=""
                     ){

  ##Tests
  stopifnot(is.list(x))
  stopifnot(is.character(id))

  ns<-NS(id)

  if (is.null(breaking)){
    breaking <-0
  } else  if (is.logical(breaking)){breaking<-as.integer(breaking)
  } else  stopifnot(breaking>=0)



  ##Generate Gui
  slGui <- recSliderGuiInput(id=id, x,depth=0,
                        breaking=breaking,
                        parents_name = parents_name, minweight = minweight,
                        maxweight = maxweight, standardweight = standardweight,
                        open.maxdepth = open.maxdepth, cb_title=cb_title,
                        sliderlabel = sliderlabel)$result


  slGui_attribs<-data.table(element_name =sapply(slGui, function (x) attr(x,"element_name")),
                            depth= sapply(slGui, function(x)attr(x,"depth")),
                            parent_name =sapply(slGui, function (x) attr(x,"parent_name"))
                            )
  slGui_attribs[,position:=1:.N]




  ####Reorder Gui according to breaking
  ##Make vector of paging - using that slGui is ordered
  slGui_attribs[,new_page:=depth>=breaking&shift(depth, fill=0)<breaking]
  slGui_attribs[depth>=breaking,page_nr:=cumsum(new_page)]
  slGui_attribs[depth<breaking, page_nr:=switch(first(mainpageposition), #only first argument used
                                                first=0,
                                                last=max(slGui_attribs$page_nr, na.rm=TRUE)+1,
                                                none=NA )]

  ####Generate title_text
  if(isTRUE(title_text)){
    title_text=slGui_attribs[, .(title_text= ifelse(first(depth)<first(breaking),
                                                    ##First: Mainpagetitle
                                                    "Auf welche Bereiche legen Sie wie viel Wert?",
                                                    ##Other Titles
                                                    sprintf("Worauf legen sie im Bereich '%s' wie viel Wert?",
                                                            first(parent_name))
                                                    )
                                 ),
                             by=page_nr]$title_text
  }


  ##For every page-number return extra list element. Remove NA to remove mainpage if asked for
  return(
    lapply( unique(slGui_attribs$page_nr[!is.na(slGui_attribs$page_n)]),
          function(x){
            c(list(
              tags$p(beschreibungs_text),
              tags$h2(title_text[x+1]),
              tags$div(class="sliders",
               slGui[slGui_attribs[page_nr==x,position]]
              )
            )
            )
            })
  )


}

#' Internal Version of rSliderGui
#'
#' @param x
#' @param depth actual depth - root is 0
#' @param breaking
#' @param parents_name
#' @param minweight
#' @param maxweight
#' @param standardweight
#' @param open.maxdepth
#' @param cb_title
#' @param sliderlabel
#'
#' @param id
#' @param sliderstate Default class of slider.
#'
#' @return a list of rows as UIoutput - Sliders and collapsebars
#'         with attributes "depth" and "element_name". See code{\link{setNameDepth}}
#'
#' @examples
#' test<-recSliderGuiInput("slg1", configList)
#' sapply(test, function(x){setNames(attr(x,"depth"),attr(x,"element_name"))})

recSliderGuiInput<-function(id, x, depth=0,
                       breaking=0,
                       parents_name="genauer",
                     minweight=0,maxweight=100, standardweight=30,
                     open.maxdepth=Inf, cb_title= "I don't know",
                     sliderlabel="", sliderstate=""
){
  # Ziel: Elemente für innerhalb sidebarPanel bzw. für ganze Seite zurückliefern.

  #Erste Ebene: Offen
  #Zweite Ebene: Eingerückt. Offen. (ggf Korrigiert um breaking)
  # Danach: CollapsePanel.

  #Alle Sliders und Collapsepanels für sich als tagList (das meint: alle fluidrows, und collapsePanelGroups)
  #Dann  jeweils als Geschachtelte Liste.

  ns=NS(id)


  #Ebene, ab wo mit AusklappPanels gearbeitet wird. Kann nur kleiner werden, nie größer
  open.maxdepth<- getOpen.Maxdepth(x,open.maxdepth)

  ##weitere TESTS sind bereits in rCollapsePanel
  stopifnot(depth>=0)
  stopifnot(open.maxdepth>=breaking)

  if("sliderlabel" %in% names(x) ) sliderlabel <- x$sliderlabel
  #TODO: Element werden bei Root nicht geparsed..

  if("explanation_for_childs" %in% names(x) ) explanation <- x$explanation else explanation <- NULL

  ret <- list()
  children_qualitative<-vector(mode = "logical")

  ##Durch Elemente des aktuellen Knotens iterieren
  for(i in 1:length(x) ){
    list.elem <- x[[i]]
    elem.name <- names(x)[i]

    ##TODO Funktioniert wahrscheinlich nicht bei root!!!
    #Attribute parsen
    this.sliderlabel <- if("sliderlabel" %in% names(list.elem) ) list.elem$sliderlabel else sliderlabel
    #message(paste0("inside recslidergui. elem.name=", elem.name, " this.sliderlabel=", this.sliderlabel))
    this.minweight <- if("minweight" %in% names(list.elem) ) list.elem$minweight else minweight
    this.maxweight <- if("maxweight" %in% names(list.elem) )  list.elem$maxweight else maxweight
    this.standardweight <- if("standardweight" %in% names(list.elem))list.elem$standardweight else  standardweight
    if("description" %in% names(list.elem)){
      this.description<-list.elem$description
      }else this.description <-elem.name


    #Falls Element. GGf. Child-Elemente parsen
    if("class" %in% names(list.elem)){

      ##First let's determine the status of the children --> see if slider must be "qualitative"
      if(list.elem$class=="mapping" |list.elem$class=="submapping" |list.elem$class=="mappingelements"){
       # message(paste0("mapping ", elem.name))
        this.isQualitative <- if("Attribname" %in% names(list.elem)
        ) is.na(list.elem$Attribname) else TRUE #if missing->qualitative
      }


      ##Ggf. REKURSION
      ##Slider (Element selbst) mit evtl. Child-Knoten zusammensetzen.
      #mit c(); ohne extra list(), damit Liste der einzelnen Zeilen nicht geschachtelt wird.
      if(list.elem$class=="elements" |list.elem$class=="mappingelements"){
        #message(paste0("elements ", elem.name))
        this.subtree<- recSliderGuiInput(id,list.elem,depth+1,
                                         breaking = breaking,
                                         elem.name,
                                         minweight = this.minweight, maxweight=this.maxweight,
                                         standardweight = this.standardweight,
                                         open.maxdepth = open.maxdepth,
                                         cb_title = cb_title,
                                         sliderlabel = this.sliderlabel)

        this.isQualitative<-this.subtree$qualitative
      }

      ## Hier Slider selbst - Taglist!
      returnvalue <-tagList(sliderCheckboxInput(ns(elem.name),
                                                description = this.description,
                                                min = this.minweight,
                                                max = this.maxweight,
                                                value = this.standardweight,
                                                cb_title = cb_title,
                                                sliderlabel=this.sliderlabel,
                                                sliderstate=if(this.isQualitative)"qualitative" else "",
                                                sliderclass=list.elem$class
                                                )

      )


      ##Je nach Tiefe zusammenfügen
      # Falls noch offen. Je nach Tiefe eingerückt
      if (depth<open.maxdepth){

        ##Falls Mainpage
        #Ganz normal einrücken
        if (depth<breaking) {

          #zurückgeben
          ret<- c(ret,
                  list(
                    setSliderGuiAttribs(
                      indentedRow(indention = depth, returnvalue)
                      ,elem.name, depth,parents_name )
                  )
          )

          ##Falls keine Mainpage
          #Einrückung um Tiefe des Breaks bereinigen
        } else {
          ret<- c(ret,
                  list(
                    setSliderGuiAttribs(
                      indentedRow(indention = depth -breaking,returnvalue)
                      ,elem.name, depth,parents_name )
                  )
          )

        }

        #Falls nicht mehr offen; nicht einrücken
      } else  if (depth>= open.maxdepth){
        ret<- c(ret,list(returnvalue) )
      }#  if (depth>= open.maxdepth)


      #If subelements, now there is the time to put it together
      if(list.elem$class=="elements" |list.elem$class=="mappingelements"){
        # message(paste0("put together: ", list.elem$class, " ", elem.name ))
        ret <-c(ret,
                #list(  #If one uses list() here, there will be a nested list in the end.
                this.subtree$result
                 #)
                )

        # message("this.subtree$ret ---" )
        # message(this.subtree$ret )

      }

      children_qualitative<- c(children_qualitative, this.isQualitative)
    } # if "class" in names



  }#for

  ##Prepend Explanation, if not null
  if (!is.null(explanation)) ret <- c(list(explanation), ret)

  ##Falls nicht mehr offen, muss komplete Sliderliste in ein bsCollapsePanel gepackt werden

  if (depth>= open.maxdepth){
    result <- tagList(
      bsCollapse(id=NS(ns(gsub("[^A-Za-z0-9-]", "", parents_name)))("bsc"),
                 bsCollapsePanel(title=sprintf(">>> '%s' genauer einstellen",parents_name ), ##Hier beschreibung einstellen
                                 value=NS(gsub("[^A-Za-z0-9-]", "", parents_name))("bscPanel"),
                                 #No relation to outer namespace; none needed
                                 tagList(ret)
                 )#bsCollapsePanel
      )#bsCollapse
    )#tagList

    #If first level of collapsePanel: indent
    if (depth==open.maxdepth) {
      result<- list(
        setSliderGuiAttribs(
          indentedRow(indention = ifelse(depth<=breaking, depth, depth -breaking)-1,
                                             result)
          ,NS(ns(parents_name))("bsc"), depth,parents_name)
      )
    }

  } else result <- ret

  return(
    list(result=result,
         qualitative=all(children_qualitative)
    )
  )



}

#'
#' @describeIn rSliderGuiInput list of two: sliderCheckBoxValues and collapsePanelValues
#' @export
rSliderGui<- function(input, output, session, slCbNames,dtBscCombinations) {
  # message(id_string)
  # ns<-NS(id_string)
  # message(paste0( "ns(TEST)", ns("TEST") ))

  ##sliderCheckBoxValues
  sliderCheckboxModules<- sapply(slCbNames,
                                 function(x) callModule(sliderCheckbox,
                                                        gsub("[^A-Za-z0-9-]", "", x) #keep only letters and numbers
                                                        ),
                                 simplify = FALSE #otherwise all sliderCheckboxModules (lists!) will be simplified to one big list.
                                 )
  sliderCheckBoxValues <- reactive({sapply(slCbNames,
                                     function(x) {
                                       sliderCheckboxModules[[x]]$value() * 1.
                                     }
                                     )
    })



  ##collapsePanelValues
  rv<- reactiveValues(bscValues=dtBscCombinations,
                      #https://stackoverflow.com/questions/32536940/shiny-reactivity-fails-with-data-tables
                      #because no reactivity inside data.tables, extra value to trigger update
                      bscValues_triggerupdate=0
                      )


  ## Ausklapp Gruppen, zum zählen wie häufig geöffnet.
  # Siehe https://stackoverflow.com/questions/38950886/generate-observers-for-dynamic-number-of-inputs
  observers <- lapply(unique(dtBscCombinations[!is.na(bscName), bscName]), function(x){
    # print(x)
    # print( dtBscCombinations)

    #Observe Closing as well, Ignore first time.ignoreNULL = FALSE to catch closing
    observeEvent(input[[gsub("[^A-Za-z0-9-]", "", x)]],
                 ignoreNULL = FALSE, ignoreInit = TRUE,{

                   rv$bscValues[bscName==x,':='(timesClicked=timesClicked+1,
                                                opened= !opened,
                                                lastState=first(paste0(input[[x]],collapse=""))#Aufgrund BUG in bsCollapse;
                                                )
                                ]

                   ##TODO
                   #Falls vorhanden, Eltern-Knoten anpassen, um die folgende -fehlerhafte-
                   # Aktivierung der Observer der übergeordneten collapsePanels auszugleichen
                   rv$bscValues[
                     #Elternknoten über dtBscCombinations herausfinden
                     bscName==dtBscCombinations[x==bscName,unique(bscName.parent)],
                     ':='(timesClicked=timesClicked-1,
                          opened=!opened
                          )
                     ]

                   #Um Reaktivität trotz data.table zu triggern.
                   #https://stackoverflow.com/questions/32536940/shiny-reactivity-fails-with-data-tables
                   rv$bscValues_triggerupdate<-rv$bscValues_triggerupdate+1

                  })

  })

  collapsePanelValues <- reactive({
    #Visible bei Kindknoten updaten, rekursiv
    #Sichtbarkeit der jeweiligen Level hinzufügen - geht nur außerhalb und nach Observer

    rv$bscValues_triggerupdate #To trigger update

    ret <- copy(rv$bscValues)
    ret[,visible:=recursiveTrue(bscName, opened,bscName.parent )]
    # print("visible updated")
    # print(ret)
    return(ret)
  })

  setStates<- function(oldSliderCheckboxModules=NULL){
    # #Debug-
    #print(names(oldSliderCheckboxModules ))
    #str(oldSliderCheckboxModules )
    # #End of Debug-

    ##SliderCheckboxes
    if (!is.null(oldSliderCheckboxModules))lapply(
      names(oldSliderCheckboxModules  ),
      function(x){
        # #Debug-
        # message(paste(x,oldSliderCheckboxModules[[x]]$print() ))
        # #End of Debug-
        sliderCheckboxModules[[x]]$syncModules(oldSliderCheckboxModules[[x]])
      }
    )

    ##collapsePanels.

    ###TODO: Does not work.
    ## Maybe OK without - simply reopen.
    ##  for final counting add slGui1$oldCollapsePanelValues()$timesClicked and slGui2$oldCollapsePanelValues()$timesClicked


    # if (!is.null(oldCollapsePanelValues))lapply(
    #   oldCollapsePanelValues()$bscName,
    #   function(x){
    #     #Debug-
    #     print(paste0("updating ", x) )
    #     print(paste0("opening ",oldCollapsePanelValues()[bscName==x &opened,
    #                                                                .(open=paste0(x,"Panel")  )]
    #     ))
    #     # print(paste0("closing ",oldCollapsePanelValues()[bscName==x &!opened,
    #     #                                                  .(close=paste0(x,"Panel")  )]
    #     # ))
    #
    #     #End of Debug-
    #
    #     updateCollapse(session=session, id= x,
    #                    open=oldCollapsePanelValues()[bscName==x &opened,
    #                                                  .(open=paste0(x,"Panel")  )]
    #                    # ,
    #                    # close=oldCollapsePanelValues()[bscName==x &!opened,
    #                    #                                .(close=paste0(x,"Panel")  )
    #                    #                                ]
    #
    #     )
    #
    #     #Todo: Update bscValues! correction - inner BscPanel is clicked, if outer Panel is
    #     #  opened (or closed?).
    #   })


  }

  syncModules <- function(oldmodule){
    setStates(oldmodule$sliderCheckboxModules)
  }

  ##Return  #TODO
  return( list(sliderCheckBoxValues=sliderCheckBoxValues,
               sliderCheckboxModules=sliderCheckboxModules,
               collapsePanelValues=collapsePanelValues,
               syncModules=syncModules,
               setStates=setStates
               )
          )
}





#' Setting one SliderGuiInput to the state of another
#'
#' @param sliderGui    The sliderGui to be synched with the other.
#' @param oldSliderGui the old slider Gui (list object, without Parenthesis.)
#'
#' @return
#' @export
#'
#' @examples
syncSliderGuiInputs <- function( sliderGui, oldSliderGui){
  sliderGui$syncModules(oldSliderGui)


}


# General Gui-Functions -----------------------------------------------------------

#' Creation of indented fluidrows
#'
#' Creates an indented fluidrow
#'
#' @param ... Elements to be included (each on several row).
#' @param indention Integer between 0 and 11. Indention.
#' @param tagListWrap Should fluidrow be wrapped into \code{\link{shiny:tagList}}.
#'
#' @return \code{\link{shiny:fluidRow}}/\code{\link{shiny:column}} with indented elements.
#'
#' @examples
indentedRow<-function(...,indention=0, tagListWrap=FALSE){
  stopifnot(indention>=0 & indention<12)
  ret<-fluidRow(column(
    width= 12-indention,
    ...,
    offset=indention
  ))
  if (tagListWrap) { return(tagList(ret))
  } else return (ret)
}


#' Recursively turn Configurations list into a vector of colors
#'
#' @param x Configuration list, see \code{\link{validateConfig}}.
#' @param color Default color.
#' @param color_parent Should parent itself also be colored in new color or only the children?
#' @param collapse should all styles be collapsed into a single style-tag (default), or not (collapse=NULL)
#'
#' @return named vector of colors, corresponding to flatted list. names: element names.
#'
#' @note See \url{https://stackoverflow.com/questions/36906265/how-to-color-sliderbar-sliderinput.}
#'
#' @export
#'
#' @examples
#'
rColorVector <- function(x, color="", color_parent=TRUE){


  #Color einfügen.
  #Siehe: https://stackoverflow.com/questions/36906265/how-to-color-sliderbar-sliderinput

  stopifnot(is.list(x))

  recColorVector(x, color=color, color_parent =color_parent)
}


#' Internal Version of rColorVector
#'
#' @param x
#' @param num Position in flatted List, because recursive list is processed iteratively
#' @param color
#' @param color_parent
#'
#' @return

recColorVector<-function(x,num=0, color="", color_parent=TRUE){
  #TODO
  ret <- vector(mode="character")

  for(i in 1:length(x) ){


    list.elem <- x[[i]]
    elem.name <- names(x)[i]


    #

    if("class" %in% names(list.elem)){
      this.color <- ifelse("color" %in% names(list.elem),list.elem$color, color)
      this.color_parent <- ifelse("color_parent" %in% names(list.elem),
                                  list.elem$color_parent, color_parent)

      #message(sprintf("num = %i, color= %s", num, color) )

      ret <- c(ret,setNames( ifelse(this.color_parent, this.color,color),elem.name ) )
      num<-num+1

      ##Recursion
      if(list.elem$class=="elements"){
        recvalue <- recColorVector(list.elem,num ,this.color, color_parent = this.color_parent)
        #message(length(recvalue))

        ret<-c(ret,recvalue)
        num <- num+length(recvalue)
      }
    }

  } #for

  return(ret)
}



#' @describeIn rColorVector Vector of colors converted into a vector of CSS-Styles to use for ION.rangeSlider.
#'
#' @export

rColorSliders<-function(x, id=NULL, slidername="sl", color_parent=TRUE, collapse="\n"){
  ## TODO: Do this directly in SliderCheckbox-Module!

  colorsVector <- rColorVector(x, color_parent =color_parent)
  names<- gsub("[^A-Za-z0-9-]", "", names(colorsVector))

  ret<-lapply(1:length(colorsVector), function(x){

      sprintf("[for=\"%1$s\"]+span>.irs>.irs-single, [for=\"%1$s\"]+span>.irs-bar-edge, [for=\"%1$s\"]+span>.irs-bar {background: %2$s}"
              ,#Correcting for Namespace of module! - id-names(colorsVector)[x]-slidername
              NS(c(id,names[ x]))(slidername), colorsVector[x])
  })
  tags$style(HTML(paste(ret, collapse = collapse)))

}
