#Daten aufbauen --------------

library (data.table)

dtAlternativen <- data.table(Titel= as.factor(c("0) Jetzt-Zustand",
                                                "1)Fortschreibung","1)Fortschreibung","1)Fortschreibung",
                                                "2)Kleinere Anlagen","2)Kleinere Anlagen","2)Kleinere Anlagen",
                                                "3)Größere Anlagen","3)Größere Anlagen","3)Größere Anlagen") ),
                             Rahmenszenario=as.factor(c("Jetzt-Zustand",
                                                        "Das Wachstum geht weiter", "Nachhaltigkeit schafft Werte", "Kein Land in Sicht",
                                                        "Das Wachstum geht weiter", "Nachhaltigkeit schafft Werte", "Kein Land in Sicht",
                                                        "Nachhaltigkeit schafft Werte", "Das Wachstum geht weiter", "Kein Land in Sicht")),
                             Energieverbrauch_Strom=1,
                             Energieverbrauch_Wärme=1:10,
                             Anteil_Energie=c(1,rep_len(100,9)),
                             Anlagengröße_Strom=c(1,1:9*10),
                             Anlagengröße_Wärme=c(1,10,14,13, 10,10,10, 50, 60, 55),
                             Anlagengröße_Wärme_negative=c(1,10,14,13, 10,10,10, 50, 60, 55),
                             Energieerzeugungskosten=c (1, 3,3,5, 0.5, 1, NA, 0.5, 0.5, NA),
                             Stromimportkosten=NA
                             )
#summary(dtAlternativen)

# Konfiguration aufbauen -----------------

configList <- list (
  class = "elements",
  open.maxdepth = 2,
  #Attribute, zum Weiterreichen
  minweight = 0,
  maxweight = 100,
  standardweight = 20,
  util_func = "prop",
  sliderlabel="returnLabelsNormal",
  #Standardeinstellungen der utilityfunction, zum weiterreichen
  util_mean = "mean",
  util_offset = 0,
  util_scale = 100,

  #Kindelemente
  Energiewendeziel = list (
    class = "elements",
    description = tagList(
      "Mir ist wichtig, wie sehr sich die  ",
      em("Region durch regionale erneuerbare Energien mit Energie versorgt"),
      "."
    ),
    standardweight = 45,
    color = "darkred",
    util_func = "prop",
    include_parent = TRUE,
    Energieverbrauch = list(
      class = "elements",
      description = tagList(
        "Ein generell ", em("niedriger Energieverbrauch")," in der Region Oberland ist mir wichtig."
      ),
      'Energieverbrauch (Strom)' = list(
        class = "mapping",
        Attribname = "Energieverbrauch_Strom",
        util_func = "antiprop",
        util_offset = 10,
        include_parent = TRUE,
        description = tagList(
          "Ein generell ",em("niedriger Stromverbrauch")," in der Region Oberland ist mir wichtig."
        )
      ),
      'Energieverbrauch (Wärme)' = list(
        class = "mapping",
        Attribname = "Energieverbrauch_Wärme",
        util_func = "antiprop",
        util_offset = 10,
        include_parent = TRUE,
        description = tagList(
          "Ein generell ",em("niedriger Wärmeverbrauch")," in der Region Oberland ist mir wichtig."
        )
      )
    ),
    'Gesamtanteil erneuerbarer Energien' = list(
      class = "mapping",
      Attribname = "Anteil_Energie",
      description = tagList(
        "Mir ist wichtig, dass ",em("im Jahresmittel ein hoher Anteil des regionalen Energieverbrauchs durch regionale erneuerbare Energien")," erzeugt wird."
      )
    ),
    'Selbstversorgung(Strom)' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Es ist mir wichtig, dass sich die Region ",em("möglichst zu jedem Zeitpunkt  mit regional produziertem erneuerbarem Strom")," versorgt."
      )
    ),
    'Selbstversorgung(Wärme)' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Es ist mir wichtig, dass sich die Region ",em("möglichst zu jedem Zeitpunkt  mit regional produzierter Wärme bzw. Wärme aus regionalen regenerativen Rohstoffen")," versorgt."
      )
    )
  ),
  'Struktur des Energiesystems' = list (
    class = "elements",
    description = tagList(
      "Mir ist wichtig, wie das ",em("regionale Energiesystem technisch ausgestaltet")," ist."
    ),
    color = "orange",
    'Diversität des Energieerzeugungssystems' = list(
      class = "elements",
      description = tagList(
        "Es ist mir wichtig, dass das ",em("regionale Energieerzeugungssystem")," aus einem ",em("breiten Mix unterschiedlicher erneuerbarer Energieerzeugungsanlagen")," besteht."
      ),
      'Diversität der Stromversorgung' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Es ist mir wichtig, dass die ",em("regionale Stromversorgung")," aus einem breiten Mix unterschiedlicher erneuerbarer Energieerzeugungsanlagen besteht."
        )
      ),
      'Diversität der Wärmeversorgung' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Es ist mir wichtig, dass das ",em(" regionale Wärmesystem")," aus einem breiten Mix unterschiedlicher erneuerbarer Energieerzeugungsanlagen besteht."
        )
      )
    ),
    'Anlagengröße und Zentralisierung' = list(
      class = "elements",
      description = tagList(
        "Es ist mir wichtig, ob das ",em("Energiesystem in der Region")," vor allem aus ",em("vielen kleineren Anlage oder aus wenigen großen Anlagen")," besteht."
      ),
      'Anlagengröße Stromerzeugung' = list(
        class = "mapping",
        Attribname = "Anlagengröße_Strom",
        description = tagList(
          "Ich präferiere ",em("wenige große Anlagen")," zur ",em("Stromerzeugung"),", es ist mir ",em("egal"),", oder ich präferiere ",em("viele kleine Anlagen")," (z.B.: Mehr Verbundkraftwerke anstatt Einzelhausanlagen)."
        ),
        minweight=-100,
        sliderlabel="returnLabelsAnlagengroesse"
      ),
      'Anlagengröße Wärmeerzeugung' = list(
        class = "mapping",
        Attribname = "Anlagengröße_Wärme",
        negative_Attribname = "Anlagengröße_Wärme_negative",
        description = tagList(
          "Ich präferiere ",em("wenige große Anlagen")," zur ",em("Wärmeerzeugung"),", es ist mir ",em("egal"),", oder ich präferiere ",em("viele kleine Anlagen")," (z.B.: Nahwärmenetze mit großem BHKW vs. Wärmespeicher, Solarthermieanlagen, Wärmepumpe)."
        ),
        minweight=-100,
        sliderlabel="returnLabelsAnlagengroesse"
      )
    ),
    'Regionale Ausgleichsmechanismen' = list(
      class = "elements",
      description = tagList(
        "Mir ist wichtig, durch welche Technologien ",em("Schwankungen in der regionalen Energieerzeugung")," ausgeglichen werden."
      ),
      'Speicher' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Mir ist es wichtig, dass auch in Niedrigproduktionszeiten möglichst ",em("wenig Speicher")," benötigt werden."
        )
      ),
      'Importe/Exporte' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Mir  ist wichtig, dass Schwankungen in der Energieerzeugung durch möglichst ",em("wenig Stromimporte und -Exporte")," ausgeglichen werden."
        )
      ),
      'Abregelung' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Mir ist wichtig, dass Schwankungen in der Energieerzeugung durch möglichst ",em("wenig Abregelungen")," ausgeglichen werden."
        )
      )
    )
  ),

  'Ökonomische Effekte' = list (
    class = "elements",
    description = tagList(
      "Mir ist wichtig, wie das ",em("regionale Energiesystem sich in der Region ökonomisch")," auswirkt."
    ),
    color = "blue",
    'Regionale Wirtschaftsentwicklung durch erneuerbare Energien' =
      list(
        class = "elements",
        description = tagList(
          "Es ist mir wichtig, dass sich der ",em("Ausbau erneuerbarer Energien")," langfristig ",em("positiv auf die gesamte regionale Wirtschaft")," auswirkt."
        ),
        'Regionale Wertschöpfung durch erneuerbare Energien - langfristig und übergreifend' =
          list(
            class = "mapping",
            Attribname = NA,
            description = tagList(
              "Es ist mir wichtig, dass sich der ",em("Ausbau erneuerbarer Energien")," langfristig ",em("positiv auf die gesamte regionale Wertschöpfung")," auswirkt"
            )
          ),
        'Regionale Arbeitsplätze durch erneuerbare Energien - langfristig und übergreifend' =
          list(
            class = "mapping",
            Attribname = NA,
            description = tagList(
              "Es ist mir wichtig, dass der ",em("Ausbau erneuerbarer Energien")," langfristig für ",em("mehr Arbeitsplätze in der gesamten regionalen Wirtschaft")," sorgt."
            )
          )

      ),
    'Ökonomische Effekte im Bereich erneuerbare Energien' = list(
      class = "elements",
      description = tagList(
        "Es ist mir wichtig, dass ",em(" regionale Wirtschaftskraft in den Energiesektor verlagert ")," wird"
      ),
      'Regionale Wertschöpfung im Bereich erneuerbare Energien' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Es ist mir wichtig, dass ",em(" regionale Wertschöpfung in den Energiesektor verlagert ")," wird."
        )
      ),
      'Arbeitsplätze im Bereich erneuerbare Energien' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Es ist mir wichtig, dass es ",em("viele Arbeitsplätze im Energiesektor in der Region")," gibt."
        )
      )
    ),
      'Energieerzeugungskosten' = list(
        class = "mapping",
        Attribname = "Energieerzeugungskosten",
        description = tagList(
          "Mir ist wichtig, dass die ",em("durchschnittlichen Erzeugungskosten pro KwH (Strom und Wärme) gering ")," sind."
        )
      ),
    'Kosten für Energieimporte' = list(
      class = "elements",
      description = tagList(
        "Es ist mir wichtig, dass es ",em(" Kosten für Energieimporte in die Region möglichst gering")," sind."
      ),
      'Stromimportkosten' = list(
        class = "mapping",
        Attribname = "Stromimportkosten",
        description = tagList(
          "Mir ist wichtig, dass die ",em(" Kosten für Stromimporte bzw. für den Import von Rohstoffen zur Stromerzeugung")," möglichst niedrig sind."
        )
      ),
      'Wärmeimportkosten' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Mir ist wichtig, dass die ",em("Kosten für Wärmeimporte bzw. für den Import von Rohstoffen zur Wärmeerzeugung")," möglichst niedrig sind."
        )
      )
    ),
    'Stromexporterlöse' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Mir ist wichtig, dass die ",em("regionalen Erlöse aus Stromexporten möglichst hoch")," sind."
      )
    )
  ),
  'Umwelteffekte' = list (
    class = "elements",
    description = tagList(
      "Mir ist wichtig, wie das ",em("regionale Energiesystem sich auf die Umwelt")," auswirkt."
    ),
    color = "green",
    'Anteil fossiler Brennstoffe' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Es ist mir wichtig, dass die ",em("Energieversorgung möglichst CO2-frei")," ist."
      )
    ),
    'Ressourcenbeanspruchung' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Es ist mir wichtig, dass der ",em("Bau/Rückbau von Energieerzeugungsanlagen möglichst weniger Rohstoffe bedarf/möglichst wenig Müll")," verursacht."
      )
    ),
    'Rückbaumöglichkeit' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Es ist mir wichtig, dass die ",em("gebauten Energieerzeugungsanlagen einfach rückgebaut")," werden können."
      )
    ),
    'Flächenbeanspruchung' = list(
      class = "elements",
      description = tagList(
        "Es ist mir wichtig, dass der Bau von Energieerzeugungsanlagen möglichst ",em("wenig Fläche")," beansprucht."
      ),
      'Flächenverbrauch(Abstandsflächen Windkraft)' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Von den ",em("Abstandsflächen für Windkraftanlagen")," soll wie viel Prozent der Fläche einbezogen werden?"
        ),
        sliderlabel="returnLabelsProzent"
      ),
      'Flächenverbrauch(Fundamente und Gebäude Windkraft)' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Von den ",em("Fundamenten und Gebäuden für Windkraftanlagen")," soll wie viel Prozent der Fläche einbezogen werden?"
        ),
        sliderlabel="returnLabelsProzent"
      ),
      'Flächenverbrauch(Solardachflächenanlagen)' = list(
        class = "mapping",
        Attribname = NA,
        description =
          list(
            "Von den ",em("Solardachflächenanlagen")," soll wie viel Prozent der Fläche einbezogen werden?"
          ),
        sliderlabel="returnLabelsProzent"
      ),
      'Flächenverbrauch(Solarfreiflächenanlagen)' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Von den ",em("Solarfreiflächenanlagen")," soll wie viel Prozent der Fläche einbezogen werden?"
        ),
        sliderlabel="returnLabelsProzent"
      ),
      'Flächenverbrauch(tiefe Geothermie)' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Von den ",em("Geothermieanlagen")," soll wie viel Prozent der Fläche einbezogen werden?"
        ),
        sliderlabel="returnLabelsProzent"
      ),
      'Flächenverbrauch(Biomasseanbau)' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Vom ",em("landwirtschaftlichen Anbau von Biomasse")," soll wie viel Prozent der Fläche einbezogen werden?"
        ),
        sliderlabel="returnLabelsProzent"
      )

    ),
    'Anteil Energieproduktion an landwirtschaftlicher Produktion' = list(
      class = "elements",
      description = tagList(
        "Es ist mir wichtig, dass vom gesamten ",em("Aufkommen an Mais/Grünland/Gülle/Holz nur ein geringer Anteil "),"zur Energieproduktion verwendet wird."
      ),
      'Anteil Energieproduktion an Mais' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Es ist mir wichtig, dass vom ",em("gesamten Aufkommen an Mais")," nur ein geringer Anteil zur Energieproduktion verwendet wird."
        )
      ),
      'Anteil Energieproduktion an Grünschnitt' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Es ist mir wichtig, dass vom ",em("gesamten Grünschnitt")," nur ein geringer Anteil zur Energieproduktion verwendet wird."
        )
      ),
      'Anteil Energieproduktion an Gülle' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Es ist mir wichtig, dass vom ",em("gesamten Gülleaufkommen")," nur ein geringer Anteil zur Energieproduktion verwendet wird."
        )
      ),
      'Anteil Energieproduktion an Holzzuwachs' = list(
        class = "mapping",
        Attribname = NA,
        description = tagList(
          "Es ist mir wichtig, dass vom ",em("gesamten Holzzuwachs")," nur ein geringer Anteil zur Energieproduktion verwendet wird."
        )
      )


    )

  ) ,
  'Regionale Auswirkungen' = list (
    class = "elements",
    description = tagList(
      "Mir ist wichtig, welche ",em("sonstigen regionalen und lokalen Auswirkungen durch Energieerzeugungsanlagen")," erzeugt werden (z.B. Verkehrsentwicklung, lokale Emissionsbelastung, Landschaftsbild)."
    ),
    color = "grey",
    'Auswirkungen der Windkraft' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch ",em("Windkraftanlagen")," gibt."
      )
    ),
    'Auswirkungen der PV-Freiflächenanlagen' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch ",em("Solar-Freiflächenanlagen")," gibt."
      )
    ),
    'Auswirkungen der PV-Dachflächenanlagen' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch ",em("Solar-Dachflächenanlagen")," gibt."
      )
    ),
    'Auswirkungen der Wasserkraftanlagen' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch ",em("Wasserkraftanlagen")," gibt."
      )
    ),
    'Auswirkungen der Biogasanlagen' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch ",em("Biogasanlagen")," gibt."
      )
    ),
    'Auswirkungen größerer Biomasseanlagen' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch ",em("größere Biomasseanlagen")," gibt."
      )
    ),
    'Auswirkungen der Einzelhausbiomasseanlagen' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch ",em("Einzelhausbiomasseanlagen")," gibt."
      )
    ),
    'Auswirkungen der Geothermieanlagen' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch ",em("Geothermieanlagen")," gibt."
      )
    )
  )

)

# pfadbeschreibungen ------------------------------------------------------

pfadbeschreibungen<-tagList(
  p(strong("0) Jetzt-Zustand:"),"Der Zustand zu Beginn der Modellierung (2015)."),
  p(strong("1) Fortschreibung:"),"Fortschreibung der aktuellen Ausbauraten: Erneuerbare Energieanlagen werden gebaut, falls sie rentabel sind. Die Region Oberland setzt auf Energieeffizienzmaßnahmen und wirtschaftliches Wachstum und ist grundsätzlich technologieoffen. "),
  p(strong("2) Kleinere Anlagen:"),"Es werden „kleine“ Lösungen fokussiert, zum Beispiel bei gebäudegebundenen Anlagen und Speichern. Haushalte und Firmen sind die primären Investoren. "),
  p(strong("3) Größere Anlagen:"),"Bei erneuerbaren Energien und Energieeinsparung werden „größere“ Lösungen bevorzugt. Als Investoren treten vor allem Kommunen, Stadtwerke und Genossenschaften als Investoren z.B. für Kombienergiezentralen und Nachwärmenetze sowie (Quartier-)Speicher auf. ")
)



# Texte --------------------------------------------------------

texte <- list (

  begruessungstext="Dieses Programm ist eine Entscheidungshilfe. Auf den nächsten Seiten können sie gewichten, wie wichtig ihnen verschiedene Eigenschaften eines regionalen Energiesystems sind. Auf was legen Sie wie viel Wert? Das Programm berechnet dann, welches der verschiedenen Alternativen am meisten Ihren Präferenzen entspricht.",
  auswahlaufforderungstext="Bitte wählen sie jedoch zuerst aus, welche Alternative Ihnen spontan am besten gefällt.Die einzelnen Alternativen sind unten beschrieben",

  begruessungstext2= "Bitte stellen sie ein, wie wichtig Ihnen die einzelnen Indikatoren im Verhältnis zu den anderen Indikatoren sind.",

  choiceSlctText="Welcher Pfad gefällt ihnen spontan am Besten?",

  ortstext="Wohnen Sie im bayerischen Oberland?",
  ortslist=list("Ja, Landkreis Bad Tölz-Wolfratshausen",
                "Ja, Landkreis Garmisch-Partenkirchen",
                "Ja, Landkreis Miesbach",
                "Ja, Landkreis Weilheim-Schongau",
                "Nein, aber in Oberbayern",
                "Nein, woanders")
)

#Speicherkonfiguration --------------------------------------------------------
speichersettings= list( #method="GoogleSheets",
                        method="CSV",
                        place="MCDA_Beispiel_INOLA")
# Speichertemplate --------------------------------------------------------

# library(mcdsupportshiny)
#dtBisherige <- loadData("CSV", "inst/mcdsupportshinyApp/MCDA_Beispiel_INOLA")

#dput(dtBisherige[1,])

# speicher_template = structure(list(Zeitpunkt = "Fri Jan 18 15:54:28 2019", Sessionstart = "Fri Jan 18 15:52:47 2019",
#                                    session_id = 753361L, gruppe = NA, url_search = NA, addBtn = 0L,
#                                    PlaceSlct = "Nicht angegeben", FirsttimeSlct = "Nicht angegeben",
#                                    GenderSlct = "Nicht angegeben", AgeSl = 0L, ChoiceSlct = "0) Jetzt-Zustand",
#                                    ChoiceSlctCount = 0L, ChoiceFinalSlct = "0) Jetzt-Zustand",
#                                    ChoiceFinalSlctCount = 0L, BestesErgebnis = "1)Fortschreibung",
#                                    Abregelung.sl.originalweights = 30L, Anlagengröße.Stromerzeugung.sl.originalweights = 30L,
#                                    Anlagengröße.Wärmeerzeugung.sl.originalweights = 30L, Anlagengröße.und.Zentralisierung.sl.originalweights = 30L,
#                                    Anteil.Energieproduktion.an.Grünschnitt.sl.originalweights = 30L,
#                                    Anteil.Energieproduktion.an.Gülle.sl.originalweights = 30L,
#                                    Anteil.Energieproduktion.an.Holzzuwachs.sl.originalweights = 30L,
#                                    Anteil.Energieproduktion.an.Mais.sl.originalweights = 30L,
#                                    Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.sl.originalweights = 30L,
#                                    Anteil.fossiler.Brennstoffe.sl.originalweights = 30L, Arbeitsplätze.im.Bereich.EE.sl.originalweights = 30L,
#                                    Auswirkungen.der.Biogasanlagen.sl.originalweights = 30L,
#                                    Auswirkungen.der.Einzelhausbiomasseanlagen.sl.originalweights = 30L,
#                                    Auswirkungen.der.Geothermieanlagen.sl.originalweights = 30L,
#                                    Auswirkungen.der.PV.Dachflächenanlagen.sl.originalweights = 30L,
#                                    Auswirkungen.der.PV.Freiflächenanlagen.sl.originalweights = 30L,
#                                    Auswirkungen.der.Wasserkraftanlagen.sl.originalweights = 30L,
#                                    Auswirkungen.der.Windkraft.sl.originalweights = 30L, Auswirkungen.größerer.Biomasseanlagen.sl.originalweights = 30L,
#                                    Diversität.der.Stromversorgung.sl.originalweights = 30L,
#                                    Diversität.der.Wärmeversorgung.sl.originalweights = 30L,
#                                    Diversität.des.Energieerzeugungssystems.sl.originalweights = 30L,
#                                    Energieerzeugungskosten.sl.originalweights = 30L, Energiekosten.sl.originalweights = 30L,
#                                    Energieverbrauch.sl.originalweights = 45L, Energieverbrauch..Strom..sl.originalweights = 45L,
#                                    Energieverbrauch..Wärme..sl.originalweights = 45L, Energiewendeziel.sl.originalweights = 45L,
#                                    Flächenbeanspruchung.sl.originalweights = 30L, Flächenverbrauch.Abstandsflächen.Windkraft..sl.originalweights = 30L,
#                                    Flächenverbrauch.Biomasseanbau..sl.originalweights = 30L,
#                                    Flächenverbrauch.Fundamente.und.Gebäude.Windkraft..sl.originalweights = 30L,
#                                    Flächenverbrauch.Solardachflächenanlagen..sl.originalweights = 30L,
#                                    Flächenverbrauch.Solarfreiflächenanlagen..sl.originalweights = 30L,
#                                    Flächenverbrauch.tiefe.Geothermie..sl.originalweights = 30L,
#                                    Gesamtanteil.erneuerbarer.Energien.sl.originalweights = 45L,
#                                    Importe.Exporte.sl.originalweights = 30L, Kosten.des.Baus.von.EE.Anlagen.sl.originalweights = 30L,
#                                    Regionale.Arbeitsplätze.durch.EE...langfristig.und.übergreifend.sl.originalweights = 30L,
#                                    Regionale.Ausgleichsmechanismen.sl.originalweights = 30L,
#                                    Regionale.Auswirkungen.sl.originalweights = 30L, Regionale.Wertschöpfung.durch.EE..langfristig.und.übergreifend.sl.originalweights = 30L,
#                                    Regionale.Wertschöpfung.im.Bereich.EE.sl.originalweights = 30L,
#                                    Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.sl.originalweights = 30L,
#                                    Ressourcenbeanspruchung.sl.originalweights = 30L, Rückbaumöglichkeit.sl.originalweights = 30L,
#                                    Selbstversorgung.Strom..sl.originalweights = 45L, Selbstversorgung.Wärme..sl.originalweights = 45L,
#                                    Speicher.sl.originalweights = 30L, Struktur.des.Energiesystems.sl.originalweights = 30L,
#                                    Umwelteffekte.sl.originalweights = 30L, Ökonomische.Effekte.sl.originalweights = 30L,
#                                    Ökonomische.Effekte.im.Bereich.EE.sl.originalweights = 30L,
#                                    Abregelung.sl.finalweight_in_level = 0.333333333333333, Anlagengröße.Stromerzeugung.sl.finalweight_in_level = 0.5,
#                                    Anlagengröße.Wärmeerzeugung.sl.finalweight_in_level = 0.5,
#                                    Anlagengröße.und.Zentralisierung.sl.finalweight_in_level = 0.333333333333333,
#                                    Anteil.Energieproduktion.an.Grünschnitt.sl.finalweight_in_level = 0.25,
#                                    Anteil.Energieproduktion.an.Gülle.sl.finalweight_in_level = 0.25,
#                                    Anteil.Energieproduktion.an.Holzzuwachs.sl.finalweight_in_level = 0.25,
#                                    Anteil.Energieproduktion.an.Mais.sl.finalweight_in_level = 0.25,
#                                    Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.sl.finalweight_in_level = 0.2,
#                                    Anteil.fossiler.Brennstoffe.sl.finalweight_in_level = 0.2,
#                                    Arbeitsplätze.im.Bereich.EE.sl.finalweight_in_level = 0.5,
#                                    Auswirkungen.der.Biogasanlagen.sl.finalweight_in_level = 0.125,
#                                    Auswirkungen.der.Einzelhausbiomasseanlagen.sl.finalweight_in_level = 0.125,
#                                    Auswirkungen.der.Geothermieanlagen.sl.finalweight_in_level = 0.125,
#                                    Auswirkungen.der.PV.Dachflächenanlagen.sl.finalweight_in_level = 0.125,
#                                    Auswirkungen.der.PV.Freiflächenanlagen.sl.finalweight_in_level = 0.125,
#                                    Auswirkungen.der.Wasserkraftanlagen.sl.finalweight_in_level = 0.125,
#                                    Auswirkungen.der.Windkraft.sl.finalweight_in_level = 0.125,
#                                    Auswirkungen.größerer.Biomasseanlagen.sl.finalweight_in_level = 0.125,
#                                    Diversität.der.Stromversorgung.sl.finalweight_in_level = 0.5,
#                                    Diversität.der.Wärmeversorgung.sl.finalweight_in_level = 0.5,
#                                    Diversität.des.Energieerzeugungssystems.sl.finalweight_in_level = 0.333333333333333,
#                                    Energieerzeugungskosten.sl.finalweight_in_level = 0.5, Energiekosten.sl.finalweight_in_level = 0.333333333333333,
#                                    Energieverbrauch.sl.finalweight_in_level = 0.25, Energieverbrauch..Strom..sl.finalweight_in_level = 0.5,
#                                    Energieverbrauch..Wärme..sl.finalweight_in_level = 0.5, Energiewendeziel.sl.finalweight_in_level = 0.272727272727273,
#                                    Flächenbeanspruchung.sl.finalweight_in_level = 0.2, Flächenverbrauch.Abstandsflächen.Windkraft..sl.finalweight_in_level = 0.166666666666667,
#                                    Flächenverbrauch.Biomasseanbau..sl.finalweight_in_level = 0.166666666666667,
#                                    Flächenverbrauch.Fundamente.und.Gebäude.Windkraft..sl.finalweight_in_level = 0.166666666666667,
#                                    Flächenverbrauch.Solardachflächenanlagen..sl.finalweight_in_level = 0.166666666666667,
#                                    Flächenverbrauch.Solarfreiflächenanlagen..sl.finalweight_in_level = 0.166666666666667,
#                                    Flächenverbrauch.tiefe.Geothermie..sl.finalweight_in_level = 0.166666666666667,
#                                    Gesamtanteil.erneuerbarer.Energien.sl.finalweight_in_level = 0.25,
#                                    Importe.Exporte.sl.finalweight_in_level = 0.333333333333333,
#                                    Kosten.des.Baus.von.EE.Anlagen.sl.finalweight_in_level = 0.5,
#                                    Regionale.Arbeitsplätze.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level = 0.5,
#                                    Regionale.Ausgleichsmechanismen.sl.finalweight_in_level = 0.333333333333333,
#                                    Regionale.Auswirkungen.sl.finalweight_in_level = 0.181818181818182,
#                                    Regionale.Wertschöpfung.durch.EE..langfristig.und.übergreifend.sl.finalweight_in_level = 0.5,
#                                    Regionale.Wertschöpfung.im.Bereich.EE.sl.finalweight_in_level = 0.5,
#                                    Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level = 0.333333333333333,
#                                    Ressourcenbeanspruchung.sl.finalweight_in_level = 0.2, Rückbaumöglichkeit.sl.finalweight_in_level = 0.2,
#                                    Selbstversorgung.Strom..sl.finalweight_in_level = 0.25, Selbstversorgung.Wärme..sl.finalweight_in_level = 0.25,
#                                    Speicher.sl.finalweight_in_level = 0.333333333333333, Struktur.des.Energiesystems.sl.finalweight_in_level = 0.181818181818182,
#                                    Umwelteffekte.sl.finalweight_in_level = 0.181818181818182,
#                                    Ökonomische.Effekte.sl.finalweight_in_level = 0.181818181818182,
#                                    Ökonomische.Effekte.im.Bereich.EE.sl.finalweight_in_level = 0.333333333333333,
#                                    Abregelung.sl.finalweight_in_level_corrected = 0L, Anlagengröße.Stromerzeugung.sl.finalweight_in_level_corrected = 0L,
#                                    Anlagengröße.Wärmeerzeugung.sl.finalweight_in_level_corrected = 0L,
#                                    Anlagengröße.und.Zentralisierung.sl.finalweight_in_level_corrected = 0.333333333333333,
#                                    Anteil.Energieproduktion.an.Grünschnitt.sl.finalweight_in_level_corrected = 0L,
#                                    Anteil.Energieproduktion.an.Gülle.sl.finalweight_in_level_corrected = 0L,
#                                    Anteil.Energieproduktion.an.Holzzuwachs.sl.finalweight_in_level_corrected = 0L,
#                                    Anteil.Energieproduktion.an.Mais.sl.finalweight_in_level_corrected = 0L,
#                                    Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.sl.finalweight_in_level_corrected = 0.5,
#                                    Anteil.fossiler.Brennstoffe.sl.finalweight_in_level_corrected = 0L,
#                                    Arbeitsplätze.im.Bereich.EE.sl.finalweight_in_level_corrected = 0L,
#                                    Auswirkungen.der.Biogasanlagen.sl.finalweight_in_level_corrected = 0L,
#                                    Auswirkungen.der.Einzelhausbiomasseanlagen.sl.finalweight_in_level_corrected = 0L,
#                                    Auswirkungen.der.Geothermieanlagen.sl.finalweight_in_level_corrected = 0L,
#                                    Auswirkungen.der.PV.Dachflächenanlagen.sl.finalweight_in_level_corrected = 0L,
#                                    Auswirkungen.der.PV.Freiflächenanlagen.sl.finalweight_in_level_corrected = 0L,
#                                    Auswirkungen.der.Wasserkraftanlagen.sl.finalweight_in_level_corrected = 0L,
#                                    Auswirkungen.der.Windkraft.sl.finalweight_in_level_corrected = 0L,
#                                    Auswirkungen.größerer.Biomasseanlagen.sl.finalweight_in_level_corrected = 0L,
#                                    Diversität.der.Stromversorgung.sl.finalweight_in_level_corrected = 0L,
#                                    Diversität.der.Wärmeversorgung.sl.finalweight_in_level_corrected = 0L,
#                                    Diversität.des.Energieerzeugungssystems.sl.finalweight_in_level_corrected = 0.333333333333333,
#                                    Energieerzeugungskosten.sl.finalweight_in_level_corrected = 0L,
#                                    Energiekosten.sl.finalweight_in_level_corrected = 0.333333333333333,
#                                    Energieverbrauch.sl.finalweight_in_level_corrected = 0.5,
#                                    Energieverbrauch..Strom..sl.finalweight_in_level_corrected = 0.5,
#                                    Energieverbrauch..Wärme..sl.finalweight_in_level_corrected = 0.5,
#                                    Energiewendeziel.sl.finalweight_in_level_corrected = 0.272727272727273,
#                                    Flächenbeanspruchung.sl.finalweight_in_level_corrected = 0.5,
#                                    Flächenverbrauch.Abstandsflächen.Windkraft..sl.finalweight_in_level_corrected = 0L,
#                                    Flächenverbrauch.Biomasseanbau..sl.finalweight_in_level_corrected = 0L,
#                                    Flächenverbrauch.Fundamente.und.Gebäude.Windkraft..sl.finalweight_in_level_corrected = 0L,
#                                    Flächenverbrauch.Solardachflächenanlagen..sl.finalweight_in_level_corrected = 0L,
#                                    Flächenverbrauch.Solarfreiflächenanlagen..sl.finalweight_in_level_corrected = 0L,
#                                    Flächenverbrauch.tiefe.Geothermie..sl.finalweight_in_level_corrected = 0L,
#                                    Gesamtanteil.erneuerbarer.Energien.sl.finalweight_in_level_corrected = 0.5,
#                                    Importe.Exporte.sl.finalweight_in_level_corrected = 0L, Kosten.des.Baus.von.EE.Anlagen.sl.finalweight_in_level_corrected = 0L,
#                                    Regionale.Arbeitsplätze.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level_corrected = 0L,
#                                    Regionale.Ausgleichsmechanismen.sl.finalweight_in_level_corrected = 0.333333333333333,
#                                    Regionale.Auswirkungen.sl.finalweight_in_level_corrected = 0.181818181818182,
#                                    Regionale.Wertschöpfung.durch.EE..langfristig.und.übergreifend.sl.finalweight_in_level_corrected = 0L,
#                                    Regionale.Wertschöpfung.im.Bereich.EE.sl.finalweight_in_level_corrected = 0L,
#                                    Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level_corrected = 0.333333333333333,
#                                    Ressourcenbeanspruchung.sl.finalweight_in_level_corrected = 0L,
#                                    Rückbaumöglichkeit.sl.finalweight_in_level_corrected = 0L,
#                                    Selbstversorgung.Strom..sl.finalweight_in_level_corrected = 0L,
#                                    Selbstversorgung.Wärme..sl.finalweight_in_level_corrected = 0L,
#                                    Speicher.sl.finalweight_in_level_corrected = 0L, Struktur.des.Energiesystems.sl.finalweight_in_level_corrected = 0.181818181818182,
#                                    Umwelteffekte.sl.finalweight_in_level_corrected = 0.181818181818182,
#                                    Ökonomische.Effekte.sl.finalweight_in_level_corrected = 0.181818181818182,
#                                    Ökonomische.Effekte.im.Bereich.EE.sl.finalweight_in_level_corrected = 0.333333333333333,
#                                    Anlagengröße.und.Zentralisierung.bsc.timesClicked = 4L, Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.bsc.timesClicked = 2L,
#                                    Diversität.des.Energieerzeugungssystems.bsc.timesClicked = 0L,
#                                    Energiekosten.bsc.timesClicked = 0L, Energieverbrauch.bsc.timesClicked = 0L,
#                                    Flächenbeanspruchung.bsc.timesClicked = 2L, Regionale.Ausgleichsmechanismen.bsc.timesClicked = 2L,
#                                    Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.bsc.timesClicked = 0L,
#                                    Ökonomische.Effekte.im.Bereich.EE.bsc.timesClicked = 0L,
#                                    Anlagengröße.und.Zentralisierung.bsc.visible = FALSE, Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.bsc.visible = FALSE,
#                                    Diversität.des.Energieerzeugungssystems.bsc.visible = FALSE,
#                                    Energiekosten.bsc.visible = FALSE, Energieverbrauch.bsc.visible = FALSE,
#                                    Flächenbeanspruchung.bsc.visible = FALSE, Regionale.Ausgleichsmechanismen.bsc.visible = FALSE,
#                                    Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.bsc.visible = FALSE,
#                                    Ökonomische.Effekte.im.Bereich.EE.bsc.visible = FALSE),
#                               .Names = c("Zeitpunkt",
#                                          "Sessionstart", "session_id", "gruppe", "url_search", "addBtn",
#                                          "PlaceSlct", "FirsttimeSlct", "GenderSlct", "AgeSl", "ChoiceSlct",
#                                          "ChoiceSlctCount", "ChoiceFinalSlct", "ChoiceFinalSlctCount",
#                                          "BestesErgebnis", "Abregelung.sl.originalweights", "Anlagengröße.Stromerzeugung.sl.originalweights",
#                                          "Anlagengröße.Wärmeerzeugung.sl.originalweights", "Anlagengröße.und.Zentralisierung.sl.originalweights",
#                                          "Anteil.Energieproduktion.an.Grünschnitt.sl.originalweights",
#                                          "Anteil.Energieproduktion.an.Gülle.sl.originalweights", "Anteil.Energieproduktion.an.Holzzuwachs.sl.originalweights",
#                                          "Anteil.Energieproduktion.an.Mais.sl.originalweights", "Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.sl.originalweights",
#                                          "Anteil.fossiler.Brennstoffe.sl.originalweights", "Arbeitsplätze.im.Bereich.EE.sl.originalweights",
#                                          "Auswirkungen.der.Biogasanlagen.sl.originalweights", "Auswirkungen.der.Einzelhausbiomasseanlagen.sl.originalweights",
#                                          "Auswirkungen.der.Geothermieanlagen.sl.originalweights", "Auswirkungen.der.PV.Dachflächenanlagen.sl.originalweights",
#                                          "Auswirkungen.der.PV.Freiflächenanlagen.sl.originalweights",
#                                          "Auswirkungen.der.Wasserkraftanlagen.sl.originalweights", "Auswirkungen.der.Windkraft.sl.originalweights",
#                                          "Auswirkungen.größerer.Biomasseanlagen.sl.originalweights", "Diversität.der.Stromversorgung.sl.originalweights",
#                                          "Diversität.der.Wärmeversorgung.sl.originalweights", "Diversität.des.Energieerzeugungssystems.sl.originalweights",
#                                          "Energieerzeugungskosten.sl.originalweights", "Energiekosten.sl.originalweights",
#                                          "Energieverbrauch.sl.originalweights", "Energieverbrauch..Strom..sl.originalweights",
#                                          "Energieverbrauch..Wärme..sl.originalweights", "Energiewendeziel.sl.originalweights",
#                                          "Flächenbeanspruchung.sl.originalweights", "Flächenverbrauch.Abstandsflächen.Windkraft..sl.originalweights",
#                                          "Flächenverbrauch.Biomasseanbau..sl.originalweights", "Flächenverbrauch.Fundamente.und.Gebäude.Windkraft..sl.originalweights",
#                                          "Flächenverbrauch.Solardachflächenanlagen..sl.originalweights",
#                                          "Flächenverbrauch.Solarfreiflächenanlagen..sl.originalweights",
#                                          "Flächenverbrauch.tiefe.Geothermie..sl.originalweights", "Gesamtanteil.erneuerbarer.Energien.sl.originalweights",
#                                          "Importe.Exporte.sl.originalweights", "Kosten.des.Baus.von.EE.Anlagen.sl.originalweights",
#                                          "Regionale.Arbeitsplätze.durch.EE...langfristig.und.übergreifend.sl.originalweights",
#                                          "Regionale.Ausgleichsmechanismen.sl.originalweights", "Regionale.Auswirkungen.sl.originalweights",
#                                          "Regionale.Wertschöpfung.durch.EE..langfristig.und.übergreifend.sl.originalweights",
#                                          "Regionale.Wertschöpfung.im.Bereich.EE.sl.originalweights", "Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.sl.originalweights",
#                                          "Ressourcenbeanspruchung.sl.originalweights", "Rückbaumöglichkeit.sl.originalweights",
#                                          "Selbstversorgung.Strom..sl.originalweights", "Selbstversorgung.Wärme..sl.originalweights",
#                                          "Speicher.sl.originalweights", "Struktur.des.Energiesystems.sl.originalweights",
#                                          "Umwelteffekte.sl.originalweights", "Ökonomische.Effekte.sl.originalweights",
#                                          "Ökonomische.Effekte.im.Bereich.EE.sl.originalweights", "Abregelung.sl.finalweight_in_level",
#                                          "Anlagengröße.Stromerzeugung.sl.finalweight_in_level", "Anlagengröße.Wärmeerzeugung.sl.finalweight_in_level",
#                                          "Anlagengröße.und.Zentralisierung.sl.finalweight_in_level", "Anteil.Energieproduktion.an.Grünschnitt.sl.finalweight_in_level",
#                                          "Anteil.Energieproduktion.an.Gülle.sl.finalweight_in_level",
#                                          "Anteil.Energieproduktion.an.Holzzuwachs.sl.finalweight_in_level",
#                                          "Anteil.Energieproduktion.an.Mais.sl.finalweight_in_level", "Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.sl.finalweight_in_level",
#                                          "Anteil.fossiler.Brennstoffe.sl.finalweight_in_level", "Arbeitsplätze.im.Bereich.EE.sl.finalweight_in_level",
#                                          "Auswirkungen.der.Biogasanlagen.sl.finalweight_in_level", "Auswirkungen.der.Einzelhausbiomasseanlagen.sl.finalweight_in_level",
#                                          "Auswirkungen.der.Geothermieanlagen.sl.finalweight_in_level",
#                                          "Auswirkungen.der.PV.Dachflächenanlagen.sl.finalweight_in_level",
#                                          "Auswirkungen.der.PV.Freiflächenanlagen.sl.finalweight_in_level",
#                                          "Auswirkungen.der.Wasserkraftanlagen.sl.finalweight_in_level",
#                                          "Auswirkungen.der.Windkraft.sl.finalweight_in_level", "Auswirkungen.größerer.Biomasseanlagen.sl.finalweight_in_level",
#                                          "Diversität.der.Stromversorgung.sl.finalweight_in_level", "Diversität.der.Wärmeversorgung.sl.finalweight_in_level",
#                                          "Diversität.des.Energieerzeugungssystems.sl.finalweight_in_level",
#                                          "Energieerzeugungskosten.sl.finalweight_in_level", "Energiekosten.sl.finalweight_in_level",
#                                          "Energieverbrauch.sl.finalweight_in_level", "Energieverbrauch..Strom..sl.finalweight_in_level",
#                                          "Energieverbrauch..Wärme..sl.finalweight_in_level", "Energiewendeziel.sl.finalweight_in_level",
#                                          "Flächenbeanspruchung.sl.finalweight_in_level", "Flächenverbrauch.Abstandsflächen.Windkraft..sl.finalweight_in_level",
#                                          "Flächenverbrauch.Biomasseanbau..sl.finalweight_in_level", "Flächenverbrauch.Fundamente.und.Gebäude.Windkraft..sl.finalweight_in_level",
#                                          "Flächenverbrauch.Solardachflächenanlagen..sl.finalweight_in_level",
#                                          "Flächenverbrauch.Solarfreiflächenanlagen..sl.finalweight_in_level",
#                                          "Flächenverbrauch.tiefe.Geothermie..sl.finalweight_in_level",
#                                          "Gesamtanteil.erneuerbarer.Energien.sl.finalweight_in_level",
#                                          "Importe.Exporte.sl.finalweight_in_level", "Kosten.des.Baus.von.EE.Anlagen.sl.finalweight_in_level",
#                                          "Regionale.Arbeitsplätze.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level",
#                                          "Regionale.Ausgleichsmechanismen.sl.finalweight_in_level", "Regionale.Auswirkungen.sl.finalweight_in_level",
#                                          "Regionale.Wertschöpfung.durch.EE..langfristig.und.übergreifend.sl.finalweight_in_level",
#                                          "Regionale.Wertschöpfung.im.Bereich.EE.sl.finalweight_in_level",
#                                          "Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level",
#                                          "Ressourcenbeanspruchung.sl.finalweight_in_level", "Rückbaumöglichkeit.sl.finalweight_in_level",
#                                          "Selbstversorgung.Strom..sl.finalweight_in_level", "Selbstversorgung.Wärme..sl.finalweight_in_level",
#                                          "Speicher.sl.finalweight_in_level", "Struktur.des.Energiesystems.sl.finalweight_in_level",
#                                          "Umwelteffekte.sl.finalweight_in_level", "Ökonomische.Effekte.sl.finalweight_in_level",
#                                          "Ökonomische.Effekte.im.Bereich.EE.sl.finalweight_in_level",
#                                          "Abregelung.sl.finalweight_in_level_corrected", "Anlagengröße.Stromerzeugung.sl.finalweight_in_level_corrected",
#                                          "Anlagengröße.Wärmeerzeugung.sl.finalweight_in_level_corrected",
#                                          "Anlagengröße.und.Zentralisierung.sl.finalweight_in_level_corrected",
#                                          "Anteil.Energieproduktion.an.Grünschnitt.sl.finalweight_in_level_corrected",
#                                          "Anteil.Energieproduktion.an.Gülle.sl.finalweight_in_level_corrected",
#                                          "Anteil.Energieproduktion.an.Holzzuwachs.sl.finalweight_in_level_corrected",
#                                          "Anteil.Energieproduktion.an.Mais.sl.finalweight_in_level_corrected",
#                                          "Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.sl.finalweight_in_level_corrected",
#                                          "Anteil.fossiler.Brennstoffe.sl.finalweight_in_level_corrected",
#                                          "Arbeitsplätze.im.Bereich.EE.sl.finalweight_in_level_corrected",
#                                          "Auswirkungen.der.Biogasanlagen.sl.finalweight_in_level_corrected",
#                                          "Auswirkungen.der.Einzelhausbiomasseanlagen.sl.finalweight_in_level_corrected",
#                                          "Auswirkungen.der.Geothermieanlagen.sl.finalweight_in_level_corrected",
#                                          "Auswirkungen.der.PV.Dachflächenanlagen.sl.finalweight_in_level_corrected",
#                                          "Auswirkungen.der.PV.Freiflächenanlagen.sl.finalweight_in_level_corrected",
#                                          "Auswirkungen.der.Wasserkraftanlagen.sl.finalweight_in_level_corrected",
#                                          "Auswirkungen.der.Windkraft.sl.finalweight_in_level_corrected",
#                                          "Auswirkungen.größerer.Biomasseanlagen.sl.finalweight_in_level_corrected",
#                                          "Diversität.der.Stromversorgung.sl.finalweight_in_level_corrected",
#                                          "Diversität.der.Wärmeversorgung.sl.finalweight_in_level_corrected",
#                                          "Diversität.des.Energieerzeugungssystems.sl.finalweight_in_level_corrected",
#                                          "Energieerzeugungskosten.sl.finalweight_in_level_corrected",
#                                          "Energiekosten.sl.finalweight_in_level_corrected", "Energieverbrauch.sl.finalweight_in_level_corrected",
#                                          "Energieverbrauch..Strom..sl.finalweight_in_level_corrected",
#                                          "Energieverbrauch..Wärme..sl.finalweight_in_level_corrected",
#                                          "Energiewendeziel.sl.finalweight_in_level_corrected", "Flächenbeanspruchung.sl.finalweight_in_level_corrected",
#                                          "Flächenverbrauch.Abstandsflächen.Windkraft..sl.finalweight_in_level_corrected",
#                                          "Flächenverbrauch.Biomasseanbau..sl.finalweight_in_level_corrected",
#                                          "Flächenverbrauch.Fundamente.und.Gebäude.Windkraft..sl.finalweight_in_level_corrected",
#                                          "Flächenverbrauch.Solardachflächenanlagen..sl.finalweight_in_level_corrected",
#                                          "Flächenverbrauch.Solarfreiflächenanlagen..sl.finalweight_in_level_corrected",
#                                          "Flächenverbrauch.tiefe.Geothermie..sl.finalweight_in_level_corrected",
#                                          "Gesamtanteil.erneuerbarer.Energien.sl.finalweight_in_level_corrected",
#                                          "Importe.Exporte.sl.finalweight_in_level_corrected", "Kosten.des.Baus.von.EE.Anlagen.sl.finalweight_in_level_corrected",
#                                          "Regionale.Arbeitsplätze.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level_corrected",
#                                          "Regionale.Ausgleichsmechanismen.sl.finalweight_in_level_corrected",
#                                          "Regionale.Auswirkungen.sl.finalweight_in_level_corrected", "Regionale.Wertschöpfung.durch.EE..langfristig.und.übergreifend.sl.finalweight_in_level_corrected",
#                                          "Regionale.Wertschöpfung.im.Bereich.EE.sl.finalweight_in_level_corrected",
#                                          "Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level_corrected",
#                                          "Ressourcenbeanspruchung.sl.finalweight_in_level_corrected",
#                                          "Rückbaumöglichkeit.sl.finalweight_in_level_corrected", "Selbstversorgung.Strom..sl.finalweight_in_level_corrected",
#                                          "Selbstversorgung.Wärme..sl.finalweight_in_level_corrected",
#                                          "Speicher.sl.finalweight_in_level_corrected", "Struktur.des.Energiesystems.sl.finalweight_in_level_corrected",
#                                          "Umwelteffekte.sl.finalweight_in_level_corrected", "Ökonomische.Effekte.sl.finalweight_in_level_corrected",
#                                          "Ökonomische.Effekte.im.Bereich.EE.sl.finalweight_in_level_corrected",
#                                          "Anlagengröße.und.Zentralisierung.bsc.timesClicked", "Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.bsc.timesClicked",
#                                          "Diversität.des.Energieerzeugungssystems.bsc.timesClicked", "Energiekosten.bsc.timesClicked",
#                                          "Energieverbrauch.bsc.timesClicked", "Flächenbeanspruchung.bsc.timesClicked",
#                                          "Regionale.Ausgleichsmechanismen.bsc.timesClicked", "Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.bsc.timesClicked",
#                                          "Ökonomische.Effekte.im.Bereich.EE.bsc.timesClicked", "Anlagengröße.und.Zentralisierung.bsc.visible",
#                                          "Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.bsc.visible",
#                                          "Diversität.des.Energieerzeugungssystems.bsc.visible", "Energiekosten.bsc.visible",
#                                          "Energieverbrauch.bsc.visible", "Flächenbeanspruchung.bsc.visible",
#                                          "Regionale.Ausgleichsmechanismen.bsc.visible", "Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.bsc.visible",
#                                          "Ökonomische.Effekte.im.Bereich.EE.bsc.visible"),
#                               row.names = 1L, class = "data.frame")
