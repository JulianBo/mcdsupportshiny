#Daten aufbauen --------------

library (data.table)

dtAlternativen <- data.table(titel= as.factor(c("Auto1", "Auto1", "Auto2", "Auto2", "Auto3") ),
                             rahmenszenario=as.factor(c("normal", "Hohe Ölpreise", "normal", "Hohe Ölpreise", "normal")),
                             Preis = c(1000,1200, 500,700,500),
                             Treibstoffkosten = c(120,140,100, 120, 250),
                             Geschwindigkeit = c(100, 100, 80,80,100),
                             Innenraum = c(40,40,60,60, 60),
                             Kinoanlage= c(TRUE,TRUE,FALSE, FALSE,TRUE),
                             AntiBlockiersystem = c(TRUE,TRUE,TRUE, TRUE,FALSE),
                             Abstandshalter= c(FALSE,FALSE,TRUE, TRUE,TRUE),
                             Fensterheber= c(2,2,4, 4,2)
                             )
summary(dtAlternativen)

# Konfiguration aufbauen -----------------

configList <- list (
  class="elements",
  open.maxdepth=1,
  #Attribute, zum Weiterreichen
  minweight=0,
  maxweight=100,
  standardweight=20,
  util_func="prop",
  #Standardeinstellungen der utilityfunction, zum weiterreichen
  util_mean="mean",
  util_offset=0,
  util_scale=100,

  #Kindelemente
  Geschwindigkeit= list(class="mapping", Attribname="Geschwindigkeit"),
  Innenraum= list(class="mapping",Attribname="Innenraum"),
  Kosten= list (
    class="elements",
    standardweight=45, color="red",
    util_func="antiprop",   util_offset=10,
    include_parent=FALSE,
    Kaufpreis= list(class="mapping",Attribname="Preis"),
    Treibstoffkosten= list(class="mapping",Attribname="Treibstoffkosten")
  ),
  Ausstattung = list (
    class="elements",
    color="green",
    Bordkino= list(class="mapping",Attribname="Kinoanlage"),
    Fahrhilfen = list (
      class="elements",
      color="grey",
      color_parent="FALSE",
      AntiBlockiersystem= list(class="mapping",standardweight=80, Attribname="AntiBlockiersystem"),
      Abstandshalter1= list(
        class="elements",
        Abstandshalter= list(class="mapping",standardweight=20, Attribname="Abstandshalter")
      )
    ),
    Fensterheber = list(class="mapping",Attribname="Fensterheber")
  ),
  NichtvorhandenesAttribut= list(class="mapping", Attribname=NA)

)

# Zusammenspiel von Daten und Konfiguration testen ---------

validateConfig <- function (configList, dtAlternativen){

  #Flatten list
  # See https://stackoverflow.com/questions/16300344/how-to-flatten-a-list-of-lists-in-r
  flatlist <- unlist(configList)


  ## Existieren alle verbundenen Attribute?
  Attribnames <- flatlist[grep("Attribname",names(flatlist) )]

  colnames_in_Attribnames <- colnames(dtAlternativen) %in% Attribnames

  message(paste0("Sie benutzen die Attribute: ",
                 paste(colnames(dtAlternativen)[colnames_in_Attribnames], collapse=", "),
                 ". Sie benutzen im Moment nicht: ",
                 paste(colnames(dtAlternativen)[!colnames_in_Attribnames], collapse=", "),
                 ". Nicht zugeordnet ist: ",
                 paste(names(Attribnames)[is.na(Attribnames)], collapse=", ")
                 ))
  if(!(all(Attribnames %in% colnames(dtAlternativen)|is.na(Attribnames) )) )
    stop (paste(Attribnames[!(Attribnames %in% colnames(dtAlternativen))], collapse=", " ), " nicht in Daten enthalten oder NA")

  #TODO: weitere Tests einbauen.
  }


# Texte --------------------------------------------------------

texte <- list (
  begruessungstext="Dieses Programm ist eine Entscheidungshilfe. Gewichten sie auf der linken Seite die verschiedenen Attribute. Auf welches Attribut legen Sie wie viel Wert? Mit Klick auf die Textboxen können Sie die Attribute dann noch feiner einstellen.",
  begruessungstext2= "Stellen Sie die Gewichtungen erst einmal ein und geben Sie unten ein paar Informationen an. Nach einem Klick auf Speichern können Sie dann interaktiv die Auswirkungen verschiedener Gewichtungen testen.",
  ortstext="Wohnen Sie im bayerischen Oberland?",
  ortslist=list("Ja, Landkreis Bad Tölz-Wolfratshausen",
                "Ja, Landkreis Garmisch-Partenkirchen",
                "Ja, Landkreis Miesbach",
                "Ja, Landkreis Weilheim-Schongau",
                "Nein, aber in Oberbayern",
                "Nein, woanders")
)

#Speicherkonfiguration --------------------------------------------------------

#dput(dtBisherige[1,])

speicher_template = structure(
  list(
    Zeitpunkt = "Mon Mar 19 22:11:58 2018",
    Sessionstart = "Mon Mar 19 22:11:52 2018",
    session_id = 851539L,
    gruppe = NA,
    url_search = NA,
    speichernBtn = 1L,
    addBtn = 0L,
    PlaceSlct = "Nein",
    FirsttimeSlct = "Nein",
    GenderSlct = "Nicht angegeben/weitere",
    AgeSl = 0L,
    ChoiceSlct = "Auto1",
    BestesErgebnis = "Auto2",
    slAbstandshalter.originalweights = 20L,
    slAbstandshalter1.originalweights = 30L,
    slAntiBlockiersystem.originalweights = 80L,
    slAusstattung.originalweights = 30L,
    slBordkino.originalweights = 30L,
    slFahrhilfen.originalweights = 30L,
    slFensterheber.originalweights = 30L,
    slGeschwindigkeit.originalweights = 30L,
    slInnenraum.originalweights = 30L,
    slKaufpreis.originalweights = 45L,
    slKosten.originalweights = 45L,
    slNichtvorhandenesAttribut.originalweights = 30L,
    slTreibstoffkosten.originalweights = 45L,
    slAbstandshalter.finalweight_in_level = 1L,
    slAbstandshalter1.finalweight_in_level = 0.272727272727273,
    slAntiBlockiersystem.finalweight_in_level = 0.727272727272727,
    slAusstattung.finalweight_in_level = 0.181818181818182,
    slBordkino.finalweight_in_level = 0.333333333333333,
    slFahrhilfen.finalweight_in_level = 0.333333333333333,
    slFensterheber.finalweight_in_level = 0.333333333333333,
    slGeschwindigkeit.finalweight_in_level = 0.181818181818182,
    slInnenraum.finalweight_in_level = 0.181818181818182,
    slKaufpreis.finalweight_in_level = 0.5,
    slKosten.finalweight_in_level = 0.272727272727273,
    slNichtvorhandenesAttribut.finalweight_in_level = 0.181818181818182,
    slTreibstoffkosten.finalweight_in_level = 0.5,
    slAbstandshalter.finalweight_in_level_corrected = 1L,
    slAbstandshalter1.finalweight_in_level_corrected = 0.272727272727273,
    slAntiBlockiersystem.finalweight_in_level_corrected = 0.727272727272727,
    slAusstattung.finalweight_in_level_corrected = 0.222222222222222,
    slBordkino.finalweight_in_level_corrected = 0.333333333333333,
    slFahrhilfen.finalweight_in_level_corrected = 0.333333333333333,
    slFensterheber.finalweight_in_level_corrected = 0.333333333333333,
    slGeschwindigkeit.finalweight_in_level_corrected = 0.222222222222222,
    slInnenraum.finalweight_in_level_corrected = 0.222222222222222,
    slKaufpreis.finalweight_in_level_corrected = 0.5,
    slKosten.finalweight_in_level_corrected = 0.333333333333333,
    slNichtvorhandenesAttribut.finalweight_in_level_corrected = 0L,
    slTreibstoffkosten.finalweight_in_level_corrected = 0.5,
    bscAbstandshalter1.timesClicked = 0L,
    bscAusstattung.timesClicked = 0L,
    bscFahrhilfen.timesClicked = 0L,
    bscKosten.timesClicked = 0L,
    bscAbstandshalter1.visible = FALSE,
    bscAusstattung.visible = FALSE,
    bscFahrhilfen.visible = FALSE,
    bscKosten.visible = FALSE
  ),
  .Names = c(
    "Zeitpunkt",
    "Sessionstart",
    "session_id",
    "gruppe",
    "url_search",
    "speichernBtn",
    "addBtn",
    "PlaceSlct",
    "FirsttimeSlct",
    "GenderSlct",
    "AgeSl",
    "ChoiceSlct",
    "BestesErgebnis",
    "slAbstandshalter.originalweights",
    "slAbstandshalter1.originalweights",
    "slAntiBlockiersystem.originalweights",
    "slAusstattung.originalweights",
    "slBordkino.originalweights",
    "slFahrhilfen.originalweights",
    "slFensterheber.originalweights",
    "slGeschwindigkeit.originalweights",
    "slInnenraum.originalweights",
    "slKaufpreis.originalweights",
    "slKosten.originalweights",
    "slNichtvorhandenesAttribut.originalweights",
    "slTreibstoffkosten.originalweights",
    "slAbstandshalter.finalweight_in_level",
    "slAbstandshalter1.finalweight_in_level",
    "slAntiBlockiersystem.finalweight_in_level",
    "slAusstattung.finalweight_in_level",
    "slBordkino.finalweight_in_level",
    "slFahrhilfen.finalweight_in_level",
    "slFensterheber.finalweight_in_level",
    "slGeschwindigkeit.finalweight_in_level",
    "slInnenraum.finalweight_in_level",
    "slKaufpreis.finalweight_in_level",
    "slKosten.finalweight_in_level",
    "slNichtvorhandenesAttribut.finalweight_in_level",
    "slTreibstoffkosten.finalweight_in_level",
    "slAbstandshalter.finalweight_in_level_corrected",
    "slAbstandshalter1.finalweight_in_level_corrected",
    "slAntiBlockiersystem.finalweight_in_level_corrected",
    "slAusstattung.finalweight_in_level_corrected",
    "slBordkino.finalweight_in_level_corrected",
    "slFahrhilfen.finalweight_in_level_corrected",
    "slFensterheber.finalweight_in_level_corrected",
    "slGeschwindigkeit.finalweight_in_level_corrected",
    "slInnenraum.finalweight_in_level_corrected",
    "slKaufpreis.finalweight_in_level_corrected",
    "slKosten.finalweight_in_level_corrected",
    "slNichtvorhandenesAttribut.finalweight_in_level_corrected",
    "slTreibstoffkosten.finalweight_in_level_corrected",
    "bscAbstandshalter1.timesClicked",
    "bscAusstattung.timesClicked",
    "bscFahrhilfen.timesClicked",
    "bscKosten.timesClicked",
    "bscAbstandshalter1.visible",
    "bscAusstattung.visible",
    "bscFahrhilfen.visible",
    "bscKosten.visible"
  ),
  row.names = 1L,
  class = "data.frame"
)

speichersettings= list( #method="GoogleSheets",
                        method="CSV",
                        place="MCDA_Beispiel_Testsheet1")
