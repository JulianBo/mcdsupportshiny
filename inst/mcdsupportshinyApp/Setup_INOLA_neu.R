#Daten aufbauen --------------

library (data.table)
#library(bit64)

dtAlternativen <- fread(file="alternativen.csv",
                        na.strings="",
                        dec=",",
                        stringsAsFactors=TRUE,
                        integer64="double",
                        encoding="Latin-1")
#summary(dtAlternativen)

# pfadcolors=c("Fortschreibung"=,
#              "Kleinere Anlagen"="c",
#              "größere Anlagen"=,
#              "Referenzmodellierung (2019)"=
#              "mehrere"=,
#              "Bitte Auswählen")

# Konfiguration aufbauen -----------------

configList <- list (
  class = "elements",
  open.maxdepth = 2,
  #Attribute, zum Weiterreichen
  minweight = 0,
  maxweight = 100,
  standardweight = 30,
  util_func = "prop",
  sliderlabel="returnLabelsNormal",
  #Standardeinstellungen der utilityfunction, zum weiterreichen
  util_fit_x1 = "min", #"mean", #mean, min_max, interquartil
  y1=150  ,
  negative_util_fit_x1="min",
  negative_y1=0,
  util_fit_x2 = "max", #mean, min_max, interquartil
  y2 = 0 ,
  negative_util_fit_x2="max",
  negative_y2=150,




  # util_fit_x1 = 0, #"mean", #mean, min_max, interquartil
  # y1=0,
  # util_fit_x2 = "mean", #mean, min_max, interquartil
  # y2 = 100,


  #Kindelemente
  Energiewendeziel = list (
    class = "elements",
    description = tagList("Energiewendeziel: Wie wichtig ist es Ihnen, wie sehr sich die Region durch ",em("regionale erneuerbare Energien versorgt?"),""),
    color = "darkred",
    util_func = "prop",
    include_parent = TRUE,
    Energieverbrauch = list(
      class = "elements",
      description = tagList(
        "Wie wichtig ist Ihnen ein ein generell ", em("niedriger Energieverbrauch")," in der Region Oberland?"
      ),
      'Energieverbrauch (Strom)' = list(
        class = "mapping",
        Attribname = "Energieverbrauch_Strom",
        util_func = "antiprop",
        include_parent = TRUE,
        description = tagList(
          "Wie wichtig ist Ihnen ein ",em("niedriger Stromverbrauch"),"  in der Region Oberland?"
        )
      ),
      'Energieverbrauch (Wärme)' = list(
        class = "mapping",
        Attribname = "Energieverbrauch_Wärme",
        util_func = "antiprop",
        include_parent = TRUE,
        description = tagList(
          "Wie wichtig ist Ihnen ein ",em("niedriger Wärmeverbrauch")," in der Region Oberland?"
        )
      )
      # ,
      # 'Energieverbrauch (Gas)' = list(
      #   class = "mapping",
      #   Attribname = "Energieverbrauch_Gas",
      #   util_func = "antiprop",
      #   include_parent = TRUE,
      #   description = tagList(
      #     "Wie wichtig ist Ihnen ein ",em("niedriger Gasverbrauch")," in der Region Oberland?"
      #   )
      # )
    ),
    'Gesamtanteil erneuerbarer Energien' = list(
      class = "mapping",
      Attribname = "Anteil_Erneuerbar",
      description = tagList(
        "Wie wichtig ist es Ihnen, dass  ",em("bilanziell ein hoher Anteil des regionalen Energieverbrauchs durch regionale erneuerbare Energien")," erzeugt wird?"
      )
    ),
    'Exportmöglichkeit' = list(
      class = "mapping",
      Attribname = "Export",
      description = tagList(
        "Wie wichtig ist es Ihnen, dass in der Region so viele ",em(" Stromerzeugungsanlagen gebaut werden, dass potentiell auch Strom exportiert werden könnte?")," (z.B. um Städte wie München mitzuversorgen)"
      )
    )
  ),

  'Struktur des Energiesystems' = list (
    class = "elements",
    description = tagList(
      "Struktur des Energiesystems: Wie wichtig ist Ihnen, wie das ",em("regionale Energiesystem technisch ausgestaltet "),"ist?(z.B. Technologien, Anlagengröße, Speicher)"
    ),
    color = "orange",
    'Diversität des Energieerzeugungssystems' = list(
      class = "mapping",
      Attribname="Diversität",
      description = tagList(
        "Wie wichtig ist es Ihnen, dass das ",em("regionale Energieerzeugungssystem")," aus einem ",em("breiten Mix unterschiedlicher erneuerbarer Energieerzeugungsanlagen")," besteht? (d.h. eine Mischung aus PV, Windkraft, Biomasse, Wasserkraft, Geothermie)"
        )

      ),
    'Anlagengröße und Zentralisierung' = list(
      class = "elements",
      description = tagList(
        "Es ist mir wichtig, ob das ",em("Energiesystem in der Region")," vor allem aus ",em("vielen kleineren Anlage oder aus wenigen großen Anlagen")," besteht."
      )
      #,explanation_for_childs="Dies ist eine Erläuterung, die nach 'Anlagengröße und Zentralisierung' kommen sollte"
      ,

      'Anlagengröße Stromerzeugung' = list(
        class = "mapping",
        Attribname = "Anlagengröße_Strom",
        description = tagList(
          "Ich präferiere ",em("wenige große Anlagen")," zur ",em("Stromerzeugung,"),
          " es ist mir ",em("egal,")," oder ich präferiere ",em("viele kleine Anlagen")," (z.B.: Mehr Verbundkraftwerke anstatt Einzelhausanlagen)."
        ),
        minweight=-100,
        standardweight = 0,
        sliderlabel="returnLabelsAnlagengroesse"
      ),
      'Anlagengröße Wärmeerzeugung' = list(
        class = "mapping",
        Attribname = "Anlagengröße_Wärme",
        #negative_Attribname = "Anlagengröße_Wärme_negative",
        description = tagList(
          "Ich präferiere ",em("wenige große Anlagen")," zur ",em("Wärmeerzeugung,")," es ist mir ",
          em("egal,")," oder ich präferiere ",em("viele kleine Anlagen")," (z.B.: Nahwärmenetze mit großem BHKW vs. Wärmespeicher, Solarthermieanlagen, Wärmepumpe)."
        ),
        minweight=-100,
        standardweight = 0,
        sliderlabel="returnLabelsAnlagengroesse"
      )
      )
    ,
  'Regionale Ausgleichsmechanismen' = list(
    class = "mapping",
    Attribname = "Stromdelta_Summe",
    util_func = "antiprop",
    include_parent = TRUE,
    description = tagList(
      "Die Erzeugung von Strom mit Erneuerbare-Energien-Anlagen unterliegt zeitlichen Schwankungen, die von den Witterungsverhältnissen und vom Technologiemix abhängen.",
      tags$br(),"Wie wichtig ist es Ihnen, dass  ",
      em("innerhalb der Region Schwankungen zwischen Stromproduktion und Stromverbrauch ausgeglichen ")," werden?"
      )
    )
  ),

  'Ökonomische Effekte' = list (
    class = "elements",
    description = tagList(
      "Wirtschaftliche Effekte: Wie wichtig ist Ihnen, wie sich das ",em("regionale Energiesystem sich in der Region ökonomisch "),"auswirkt? (z.B. Wirkungen auf Arbeitsplätze und Wertschöpfung, Kosten)"
    ),
    color = "blue",
    'Wirtschaftsentwicklung' =
      list(
        class = "elements",

        description = tagList(
          "Es ist mir wichtig, dass sich der ",em("Ausbau erneuerbarer Energien")," langfristig ",em("positiv auf die gesamte regionale Wirtschaft")," auswirkt."
        ),
        'Wertschöpfung' =
          list(
            class = "mapping",
            Attribname="Wertschöpfung",
            description = tagList(
              "Wie wichtig ist es Ihnen, dass sich der ",em(" Ausbau erneuerbarer Energien")," langfristig ",
              em(" positiv auf die gesamte regionale Wertschöpfung")," auswirkt?")
          ),
        'Arbeitsplätze' =
          list(
            class = "mapping",
            Attribname = "Arbeitsplätze",
            escription =tagList(
              "Wie wichtig ist es Ihnen, dass der ",em(" Ausbau erneuerbarer Energien")," langfristig für ",
              em("mehr Arbeitsplätze in der gesamten regionalen Wirtschaft "),"sorgt?")
            )
        ),
      'Energieerzeugungskosten' = list(
        class = "mapping",
        Attribname = "Energieerzeugungskosten",
        description = tagList(
          "Wie wichtig ist es Ihnen, dass die  ",em("durchschnittlichen Erzeugungskosten pro KwH (Strom und Wärme) gering ")," sind?"
        )
      ),
    'Förderkosten' = list(
      class = "mapping",
      Attribname = "Förderkosten",
      description = tagList(
        "Wie wichtig ist es Ihnen, dass nur ",em("wenig regionale Fördermittel")," benötigt werden?"
      )
    ),
    'Gewinnbeteiligung' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Wie wichtig ist es Ihnen, dass ",em("BürgerInnen finanziell an den Gewinnen beteiligt"),
        " werden? (z.B. über Genossenschaften oder andere Beteiligungsmodelle)"
      )

    ),
    'Planungsbeteiligung' = list(
      class = "mapping",
      Attribname = NA,
      description = tagList(
        "Wie wichtig ist es Ihnen, dass ",em("BürgerInnen an planerischen Entscheidungen"),
        "  beim Bau von Erneuerbare-Energien-Anlagen beteiligt werden?"
      )
    )
  ),
  'Umwelteffekte' = list (
    class = "elements",
    description = tagList(
      "Umweltauswirkungen: Wie wichtig ist Ihnen, wie sich das",em("regionale Energiesystem auf die Umwelt")," auswirkt? (z.B. Flächenbeanspruchung; Nutzung von Biomasse)"
    ),
    color = "green",
    'Flächenbeanspruchung' = list(
      class = "mapping",
      Attribname = "Flächenbeanspruchung",
      description = tagList(
        "Wie wichtig ist es Ihnen, dass bei der Energieerzeugung möglichst ",em("wenig Freiflächen versiegelt oder überbaut"),
        " werden? (Es werden Windkraft, PV-Freiflächenanlagen, Biogasanlagen und Kraftwerke berücksichtigt)"
      )
    ),
    'Biomassenverwendung' = list(
      class = "elements",
      description = tagList(
        "Wie wichtig ist es Ihnen, dass von ",em("Nahrungs- und Futtermitteln (v. a. Mais) und Holz nur ein geringer Anteil zur Energieproduktion"),
        " verwendet wird? (im Modell wird von der aktuellen Flächennutzung ausgegangen)"
      ),
      'Maisverwendung' = list(
        class = "mapping",
        Attribname = "Maisanteil",
        description = tagList(
          "Wie wichtig ist es Ihnen, dass vom ",em("gesamten Aufkommen an Mais")," nur ein geringer Anteil zur Energieproduktion verwendet wird?"
        )
      ),
      'Holzverwendung' = list(
        class = "mapping",
        Attribname = "Holzanteil",
        description = tagList(
          "Wie wichtig ist es Ihnen, dass vom ",em("gesamten Holzzuwachs")," nur ein geringer Anteil zur Energieproduktion verwendet wird?"
        )
      )

    ),
    'Regionale Auswirkungen' = list (
      class = "elements",
      util_func = "negprop",
      description = tagList(
        "Wie wichtig ist es Ihnen, dass ",em("sonstige regionalen und lokalen Auswirkungen durch Energieerzeugungsanlagen"),
        " möglichst gering sind?")
      ,explanation_for_childs= tagList("Die negativen Auswirkungen welcher Anlagen bewerten  Sie im Verhältnis zueinander als wie wichtig? ",
                                       em("von 0 = keine wichtigen Auswirkungen, bis 100 = stark beeinträchtigende Auswirkungen"))
      ,
      'PV-Dachflächenanlagen' = list(
        class = "mapping",
        Attribname = "PV_Dach_Kapazität",
        description = tagList("Auf der Skala von 0 bis 100 bewerte ich ",em("Auswirkungen von Dach- und Fassadenanlagen für PV und Solarthermie")," mit:"
        )
      ),
      'PV-Freiflächenanlagen' = list(
        class = "mapping",
        Attribname = "PV_Frei_Kapazität",
        description = tagList(
          "Auf der Skala von 0 bis 100 bewerte ich ",em("Auswirkungen von Freiflächenanlagen für PV und Solarthermie")," mit:"
        )
      ),
      'Windkraft' = list(
        class = "mapping",
        Attribname = "Windkraft_Anzahl",
        description = tagList(
          "Auf der Skala von 0 bis 100 bewerte ich ",em("Auswirkungen von Windkraftanlagen")," mit:"
        )
      ),

      'Wasserkraftanlagen' = list(
        class = "mapping",
        Attribname = "Wasserkraft_Anzahl",
        description = tagList(
          "Auf der Skala von 0 bis 100 bewerte ich ",em("Auswirkungen von Wasserkraftanlagen")," mit:"
        )
      ),
      'Biogasanlagen' = list(
        class = "mapping",
        Attribname = "Biogas_Anzahl",
        description = tagList(
          "Auf der Skala von 0 bis 100 bewerte ich ",em("Auswirkungen von Biogasanlagen")," mit:"
        )
      ),
      'Biomasseheiz(kraft)werke' = list(
        class = "mapping",
        Attribname = "BiomasseWerke_Anzahl",
        description = tagList(
          "Auf der Skala von 0 bis 100 bewerte ich ",em("Auswirkungen von größeren Biomasseheiz(kraft)werken")," mit:"
        )
      ),
      'Einzelhausholzheizungen' = list(
        class = "mapping",
        Attribname ="Einzelhausbiomasse_Anzahl",
        description = tagList(
          "Auf der Skala von 0 bis 100 bewerte ich ",em("Auswirkungen von holzbefeuerten Heizungen (Einzelhäuser)")," mit:"
        )
      ),
      'Tiefengeothermieanlagen' = list(
        class = "mapping",
        Attribname = "Tiefengeothermie_Anzahl",
        description = tagList(
          "Auf der Skala von 0 bis 100 bewerte ich ",em("Auswirkungen von Tiefengeothermieanlagen")," mit:"
        )
      )
      ,
      'Wärmepumpen' = list(
        class = "mapping",
        Attribname = "Wärmepumpen_Oberflächengeothermie_Anzahl",
        description = tagList(
          "Auf der Skala von 0 bis 100 bewerte ich ",em("Auswirkungen von oberflächennahen Geothermieanlagen und Wärmepumpen")," mit:"
        )
      )
    )

  )

)

# pfadbeschreibungen ------------------------------------------------------

pfadbeschreibungen<-tagList(
  p(strong("0) Referenzmodellierung:"),"Der modellierte Zustand zu Beginn der Modellierung (2019)."),
  p(strong("1) Fortschreibung:"),"Fortschreibung der aktuellen Ausbauraten: Erneuerbare Energieanlagen werden gebaut, falls sie rentabel sind. Die Region Oberland setzt auf Energieeffizienzmaßnahmen und wirtschaftliches Wachstum und ist grundsätzlich technologieoffen. "),
  p(strong("2) Kleinere Anlagen:"),"Es werden „kleine“ Lösungen fokussiert, zum Beispiel bei gebäudegebundenen Anlagen und Speichern. Haushalte und Firmen sind die primären Investoren. "),
  p(strong("3) Größere Anlagen:"),"Es werden „größere“ Anlagen wie Solar-Freiflächenanlagen, Windkraft oder auch Biomasseheizwerke mit Nahwärmenetzen sowie (Quartier-)Speicher verstärkt gefördert. Als Investoren treten neben größeren Investoren auch Kommunen, Stadtwerke und Genossenschaften auf. ")
)



# Texte --------------------------------------------------------

texte <- list (

  begruessungstext="Dieses Programm ist eine Entscheidungshilfe. Die simulierten Ausbaupfade erneuerbarer Energien setzen sich aus vielen Faktoren zusammen, die teils nicht auf den ersten Blick ersichtlich werden. Deshalb können Sie auf den nächsten Seiten angeben, wie wichtig Ihnen verschiedene Aspekte des regionalen Energiesystems im Oberland sind. Auf was legen Sie wie viel Wert? Das Programm ermittelt dann, welcher Ausbaupfad am meisten Ihren Präferenzen entspricht.",
  auswahlaufforderungstext="Bitte wählen Sie zuerst aus, welcher Ausbaupfad Ihnen spontan am besten gefällt. Die einzelnen Ausbaupfade sind nachfolgend kurz beschrieben.",

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
speichersettings= list( method="GoogleSheets",
                        #method="CSV",
                        place="MCDA_Beispiel_INOLA_neu")
# Speichertemplate --------------------------------------------------------

# library(mcdsupportshiny)
#dtBisherige <- loadData("CSV", "MCDA_Beispiel_INOLA_neu")

#dput(dtBisherige[1,])

speicher_template = structure(list(Zeitpunkt = "Tue Jun 04 14:38:10 2019", Sessionstart = "Tue Jun 04 14:38:02 2019",
                                   session_id = 172186L, gruppe = NA, url_search = NA, addBtn = 0L,
                                   PlaceSlct = "Nicht angegeben", FirsttimeSlct = "Nicht angegeben",
                                   GenderSlct = "Nicht angegeben", AgeSl = 0L, ChoiceSlct = "Fortschreibung",
                                   ChoiceSlctCount = 0L, ChoiceFinalSlct = "Fortschreibung",
                                   ChoiceFinalSlctCount = 0L, BestesErgebnis = "Kleinere Anlagen",
                                   slGui2.Energiewendeziel.sl.originalweights = 30L, slGui2.Energieverbrauch.sl.originalweights = 30L,
                                   slGui2.EnergieverbrauchStrom.sl.originalweights = 30L, slGui2.EnergieverbrauchWrme.sl.originalweights = 30L,
                                   slGui2.GesamtanteilerneuerbarerEnergien.sl.originalweights = 30L,
                                   slGui2.Exportmglichkeit.sl.originalweights = 30L, slGui2.StrukturdesEnergiesystems.sl.originalweights = 30L,
                                   slGui2.DiversittdesEnergieerzeugungssystems.sl.originalweights = 30L,
                                   slGui2.AnlagengreundZentralisierung.sl.originalweights = 30L,
                                   slGui2.AnlagengreStromerzeugung.sl.originalweights = 30L,
                                   slGui2.AnlagengreWrmeerzeugung.sl.originalweights = 30L,
                                   slGui2.RegionaleAusgleichsmechanismen.sl.originalweights = 30L,
                                   slGui2.konomischeEffekte.sl.originalweights = 30L, slGui2.Wirtschaftsentwicklung.sl.originalweights = 30L,
                                   slGui2.Wertschpfung.sl.originalweights = 30L, slGui2.Arbeitspltze.sl.originalweights = 30L,
                                   slGui2.Energieerzeugungskosten.sl.originalweights = 30L,
                                   slGui2.Frderkosten.sl.originalweights = 30L, slGui2.Gewinnbeteiligung.sl.originalweights = 30L,
                                   slGui2.Planungsbeteiligung.sl.originalweights = 30L, slGui2.Umwelteffekte.sl.originalweights = 30L,
                                   slGui2.Flchenbeanspruchung.sl.originalweights = 30L, slGui2.Biomassenverwendung.sl.originalweights = 30L,
                                   slGui2.Maisverwendung.sl.originalweights = 30L, slGui2.Holzverwendung.sl.originalweights = 30L,
                                   slGui2.RegionaleAuswirkungen.sl.originalweights = 30L, slGui2.PV.Dachflchenanlagen.sl.originalweights = 30L,
                                   slGui2.PV.Freiflchenanlagen.sl.originalweights = 30L, slGui2.Windkraft.sl.originalweights = 30L,
                                   slGui2.Wasserkraftanlagen.sl.originalweights = 30L, slGui2.Biogasanlagen.sl.originalweights = 30L,
                                   slGui2.Biomasseheizkraftwerke.sl.originalweights = 30L, slGui2.Einzelhausholzheizungen.sl.originalweights = 30L,
                                   slGui2.Tiefengeothermieanlagen.sl.originalweights = 30L,
                                   slGui2.Wrmepumpen.sl.originalweights = 30L, slGui2.Energiewendeziel.sl.finalweight_in_level = 0.25,
                                   slGui2.Energieverbrauch.sl.finalweight_in_level = 0.333333333333333,
                                   slGui2.EnergieverbrauchStrom.sl.finalweight_in_level = 0.5,
                                   slGui2.EnergieverbrauchWrme.sl.finalweight_in_level = 0.5,
                                   slGui2.GesamtanteilerneuerbarerEnergien.sl.finalweight_in_level = 0.333333333333333,
                                   slGui2.Exportmglichkeit.sl.finalweight_in_level = 0.333333333333333,
                                   slGui2.StrukturdesEnergiesystems.sl.finalweight_in_level = 0.25,
                                   slGui2.DiversittdesEnergieerzeugungssystems.sl.finalweight_in_level = 0.333333333333333,
                                   slGui2.AnlagengreundZentralisierung.sl.finalweight_in_level = 0.333333333333333,
                                   slGui2.AnlagengreStromerzeugung.sl.finalweight_in_level = 0.5,
                                   slGui2.AnlagengreWrmeerzeugung.sl.finalweight_in_level = 0.5,
                                   slGui2.RegionaleAusgleichsmechanismen.sl.finalweight_in_level = 0.333333333333333,
                                   slGui2.konomischeEffekte.sl.finalweight_in_level = 0.25,
                                   slGui2.Wirtschaftsentwicklung.sl.finalweight_in_level = 0.2,
                                   slGui2.Wertschpfung.sl.finalweight_in_level = 0.5, slGui2.Arbeitspltze.sl.finalweight_in_level = 0.5,
                                   slGui2.Energieerzeugungskosten.sl.finalweight_in_level = 0.2,
                                   slGui2.Frderkosten.sl.finalweight_in_level = 0.2, slGui2.Gewinnbeteiligung.sl.finalweight_in_level = 0.2,
                                   slGui2.Planungsbeteiligung.sl.finalweight_in_level = 0.2,
                                   slGui2.Umwelteffekte.sl.finalweight_in_level = 0.25, slGui2.Flchenbeanspruchung.sl.finalweight_in_level = 0.333333333333333,
                                   slGui2.Biomassenverwendung.sl.finalweight_in_level = 0.333333333333333,
                                   slGui2.Maisverwendung.sl.finalweight_in_level = 0.5, slGui2.Holzverwendung.sl.finalweight_in_level = 0.5,
                                   slGui2.RegionaleAuswirkungen.sl.finalweight_in_level = 0.333333333333333,
                                   slGui2.PV.Dachflchenanlagen.sl.finalweight_in_level = 0.111111111111111,
                                   slGui2.PV.Freiflchenanlagen.sl.finalweight_in_level = 0.111111111111111,
                                   slGui2.Windkraft.sl.finalweight_in_level = 0.111111111111111,
                                   slGui2.Wasserkraftanlagen.sl.finalweight_in_level = 0.111111111111111,
                                   slGui2.Biogasanlagen.sl.finalweight_in_level = 0.111111111111111,
                                   slGui2.Biomasseheizkraftwerke.sl.finalweight_in_level = 0.111111111111111,
                                   slGui2.Einzelhausholzheizungen.sl.finalweight_in_level = 0.111111111111111,
                                   slGui2.Tiefengeothermieanlagen.sl.finalweight_in_level = 0.111111111111111,
                                   slGui2.Wrmepumpen.sl.finalweight_in_level = 0.111111111111111,
                                   slGui2.Energiewendeziel.sl.finalweight_in_level_corrected = 0.333333333333333,
                                   slGui2.Energieverbrauch.sl.finalweight_in_level_corrected = 0.333333333333333,
                                   slGui2.EnergieverbrauchStrom.sl.finalweight_in_level_corrected = 0.5,
                                   slGui2.EnergieverbrauchWrme.sl.finalweight_in_level_corrected = 0.5,
                                   slGui2.GesamtanteilerneuerbarerEnergien.sl.finalweight_in_level_corrected = 0.333333333333333,
                                   slGui2.Exportmglichkeit.sl.finalweight_in_level_corrected = 0.333333333333333,
                                   slGui2.StrukturdesEnergiesystems.sl.finalweight_in_level_corrected = 0.333333333333333,
                                   slGui2.DiversittdesEnergieerzeugungssystems.sl.finalweight_in_level_corrected = 0.333333333333333,
                                   slGui2.AnlagengreundZentralisierung.sl.finalweight_in_level_corrected = 0.333333333333333,
                                   slGui2.AnlagengreStromerzeugung.sl.finalweight_in_level_corrected = 0.5,
                                   slGui2.AnlagengreWrmeerzeugung.sl.finalweight_in_level_corrected = 0.5,
                                   slGui2.RegionaleAusgleichsmechanismen.sl.finalweight_in_level_corrected = 0.333333333333333,
                                   slGui2.konomischeEffekte.sl.finalweight_in_level_corrected = 0L,
                                   slGui2.Wirtschaftsentwicklung.sl.finalweight_in_level_corrected = 0L,
                                   slGui2.Wertschpfung.sl.finalweight_in_level_corrected = 0L,
                                   slGui2.Arbeitspltze.sl.finalweight_in_level_corrected = 0L,
                                   slGui2.Energieerzeugungskosten.sl.finalweight_in_level_corrected = 0L,
                                   slGui2.Frderkosten.sl.finalweight_in_level_corrected = 0L,
                                   slGui2.Gewinnbeteiligung.sl.finalweight_in_level_corrected = 0L,
                                   slGui2.Planungsbeteiligung.sl.finalweight_in_level_corrected = 0L,
                                   slGui2.Umwelteffekte.sl.finalweight_in_level_corrected = 0.333333333333333,
                                   slGui2.Flchenbeanspruchung.sl.finalweight_in_level_corrected = 0L,
                                   slGui2.Biomassenverwendung.sl.finalweight_in_level_corrected = 0.5,
                                   slGui2.Maisverwendung.sl.finalweight_in_level_corrected = 0.5,
                                   slGui2.Holzverwendung.sl.finalweight_in_level_corrected = 0.5,
                                   slGui2.RegionaleAuswirkungen.sl.finalweight_in_level_corrected = 0.5,
                                   slGui2.PV.Dachflchenanlagen.sl.finalweight_in_level_corrected = 0.111111111111111,
                                   slGui2.PV.Freiflchenanlagen.sl.finalweight_in_level_corrected = 0.111111111111111,
                                   slGui2.Windkraft.sl.finalweight_in_level_corrected = 0.111111111111111,
                                   slGui2.Wasserkraftanlagen.sl.finalweight_in_level_corrected = 0.111111111111111,
                                   slGui2.Biogasanlagen.sl.finalweight_in_level_corrected = 0.111111111111111,
                                   slGui2.Biomasseheizkraftwerke.sl.finalweight_in_level_corrected = 0.111111111111111,
                                   slGui2.Einzelhausholzheizungen.sl.finalweight_in_level_corrected = 0.111111111111111,
                                   slGui2.Tiefengeothermieanlagen.sl.finalweight_in_level_corrected = 0.111111111111111,
                                   slGui2.Wrmepumpen.sl.finalweight_in_level_corrected = 0.111111111111111,
                                   Anlagengröße.und.Zentralisierung.bsc.timesClicked = 0L, Biomassenverwendung.bsc.timesClicked = 0L,
                                   Energieverbrauch.bsc.timesClicked = 0L, Regionale.Auswirkungen.bsc.timesClicked = 0L,
                                   Wirtschaftsentwicklung.bsc.timesClicked = 0L, Anlagengröße.und.Zentralisierung.bsc.visible = FALSE,
                                   Biomassenverwendung.bsc.visible = FALSE, Energieverbrauch.bsc.visible = FALSE,
                                   Regionale.Auswirkungen.bsc.visible = FALSE, Wirtschaftsentwicklung.bsc.visible = FALSE),
                              .Names = c("Zeitpunkt",
                                         "Sessionstart", "session_id", "gruppe", "url_search", "addBtn",
                                         "PlaceSlct", "FirsttimeSlct", "GenderSlct", "AgeSl", "ChoiceSlct",
                                         "ChoiceSlctCount", "ChoiceFinalSlct", "ChoiceFinalSlctCount",
                                         "BestesErgebnis", "slGui2.Energiewendeziel.sl.originalweights",
                                         "slGui2.Energieverbrauch.sl.originalweights", "slGui2.EnergieverbrauchStrom.sl.originalweights",
                                         "slGui2.EnergieverbrauchWrme.sl.originalweights", "slGui2.GesamtanteilerneuerbarerEnergien.sl.originalweights",
                                         "slGui2.Exportmglichkeit.sl.originalweights", "slGui2.StrukturdesEnergiesystems.sl.originalweights",
                                         "slGui2.DiversittdesEnergieerzeugungssystems.sl.originalweights",
                                         "slGui2.AnlagengreundZentralisierung.sl.originalweights", "slGui2.AnlagengreStromerzeugung.sl.originalweights",
                                         "slGui2.AnlagengreWrmeerzeugung.sl.originalweights", "slGui2.RegionaleAusgleichsmechanismen.sl.originalweights",
                                         "slGui2.konomischeEffekte.sl.originalweights", "slGui2.Wirtschaftsentwicklung.sl.originalweights",
                                         "slGui2.Wertschpfung.sl.originalweights", "slGui2.Arbeitspltze.sl.originalweights",
                                         "slGui2.Energieerzeugungskosten.sl.originalweights", "slGui2.Frderkosten.sl.originalweights",
                                         "slGui2.Gewinnbeteiligung.sl.originalweights", "slGui2.Planungsbeteiligung.sl.originalweights",
                                         "slGui2.Umwelteffekte.sl.originalweights", "slGui2.Flchenbeanspruchung.sl.originalweights",
                                         "slGui2.Biomassenverwendung.sl.originalweights", "slGui2.Maisverwendung.sl.originalweights",
                                         "slGui2.Holzverwendung.sl.originalweights", "slGui2.RegionaleAuswirkungen.sl.originalweights",
                                         "slGui2.PV.Dachflchenanlagen.sl.originalweights", "slGui2.PV.Freiflchenanlagen.sl.originalweights",
                                         "slGui2.Windkraft.sl.originalweights", "slGui2.Wasserkraftanlagen.sl.originalweights",
                                         "slGui2.Biogasanlagen.sl.originalweights", "slGui2.Biomasseheizkraftwerke.sl.originalweights",
                                         "slGui2.Einzelhausholzheizungen.sl.originalweights", "slGui2.Tiefengeothermieanlagen.sl.originalweights",
                                         "slGui2.Wrmepumpen.sl.originalweights", "slGui2.Energiewendeziel.sl.finalweight_in_level",
                                         "slGui2.Energieverbrauch.sl.finalweight_in_level", "slGui2.EnergieverbrauchStrom.sl.finalweight_in_level",
                                         "slGui2.EnergieverbrauchWrme.sl.finalweight_in_level", "slGui2.GesamtanteilerneuerbarerEnergien.sl.finalweight_in_level",
                                         "slGui2.Exportmglichkeit.sl.finalweight_in_level", "slGui2.StrukturdesEnergiesystems.sl.finalweight_in_level",
                                         "slGui2.DiversittdesEnergieerzeugungssystems.sl.finalweight_in_level",
                                         "slGui2.AnlagengreundZentralisierung.sl.finalweight_in_level",
                                         "slGui2.AnlagengreStromerzeugung.sl.finalweight_in_level", "slGui2.AnlagengreWrmeerzeugung.sl.finalweight_in_level",
                                         "slGui2.RegionaleAusgleichsmechanismen.sl.finalweight_in_level",
                                         "slGui2.konomischeEffekte.sl.finalweight_in_level", "slGui2.Wirtschaftsentwicklung.sl.finalweight_in_level",
                                         "slGui2.Wertschpfung.sl.finalweight_in_level", "slGui2.Arbeitspltze.sl.finalweight_in_level",
                                         "slGui2.Energieerzeugungskosten.sl.finalweight_in_level", "slGui2.Frderkosten.sl.finalweight_in_level",
                                         "slGui2.Gewinnbeteiligung.sl.finalweight_in_level", "slGui2.Planungsbeteiligung.sl.finalweight_in_level",
                                         "slGui2.Umwelteffekte.sl.finalweight_in_level", "slGui2.Flchenbeanspruchung.sl.finalweight_in_level",
                                         "slGui2.Biomassenverwendung.sl.finalweight_in_level", "slGui2.Maisverwendung.sl.finalweight_in_level",
                                         "slGui2.Holzverwendung.sl.finalweight_in_level", "slGui2.RegionaleAuswirkungen.sl.finalweight_in_level",
                                         "slGui2.PV.Dachflchenanlagen.sl.finalweight_in_level", "slGui2.PV.Freiflchenanlagen.sl.finalweight_in_level",
                                         "slGui2.Windkraft.sl.finalweight_in_level", "slGui2.Wasserkraftanlagen.sl.finalweight_in_level",
                                         "slGui2.Biogasanlagen.sl.finalweight_in_level", "slGui2.Biomasseheizkraftwerke.sl.finalweight_in_level",
                                         "slGui2.Einzelhausholzheizungen.sl.finalweight_in_level", "slGui2.Tiefengeothermieanlagen.sl.finalweight_in_level",
                                         "slGui2.Wrmepumpen.sl.finalweight_in_level", "slGui2.Energiewendeziel.sl.finalweight_in_level_corrected",
                                         "slGui2.Energieverbrauch.sl.finalweight_in_level_corrected",
                                         "slGui2.EnergieverbrauchStrom.sl.finalweight_in_level_corrected",
                                         "slGui2.EnergieverbrauchWrme.sl.finalweight_in_level_corrected",
                                         "slGui2.GesamtanteilerneuerbarerEnergien.sl.finalweight_in_level_corrected",
                                         "slGui2.Exportmglichkeit.sl.finalweight_in_level_corrected",
                                         "slGui2.StrukturdesEnergiesystems.sl.finalweight_in_level_corrected",
                                         "slGui2.DiversittdesEnergieerzeugungssystems.sl.finalweight_in_level_corrected",
                                         "slGui2.AnlagengreundZentralisierung.sl.finalweight_in_level_corrected",
                                         "slGui2.AnlagengreStromerzeugung.sl.finalweight_in_level_corrected",
                                         "slGui2.AnlagengreWrmeerzeugung.sl.finalweight_in_level_corrected",
                                         "slGui2.RegionaleAusgleichsmechanismen.sl.finalweight_in_level_corrected",
                                         "slGui2.konomischeEffekte.sl.finalweight_in_level_corrected",
                                         "slGui2.Wirtschaftsentwicklung.sl.finalweight_in_level_corrected",
                                         "slGui2.Wertschpfung.sl.finalweight_in_level_corrected", "slGui2.Arbeitspltze.sl.finalweight_in_level_corrected",
                                         "slGui2.Energieerzeugungskosten.sl.finalweight_in_level_corrected",
                                         "slGui2.Frderkosten.sl.finalweight_in_level_corrected", "slGui2.Gewinnbeteiligung.sl.finalweight_in_level_corrected",
                                         "slGui2.Planungsbeteiligung.sl.finalweight_in_level_corrected",
                                         "slGui2.Umwelteffekte.sl.finalweight_in_level_corrected", "slGui2.Flchenbeanspruchung.sl.finalweight_in_level_corrected",
                                         "slGui2.Biomassenverwendung.sl.finalweight_in_level_corrected",
                                         "slGui2.Maisverwendung.sl.finalweight_in_level_corrected", "slGui2.Holzverwendung.sl.finalweight_in_level_corrected",
                                         "slGui2.RegionaleAuswirkungen.sl.finalweight_in_level_corrected",
                                         "slGui2.PV.Dachflchenanlagen.sl.finalweight_in_level_corrected",
                                         "slGui2.PV.Freiflchenanlagen.sl.finalweight_in_level_corrected",
                                         "slGui2.Windkraft.sl.finalweight_in_level_corrected", "slGui2.Wasserkraftanlagen.sl.finalweight_in_level_corrected",
                                         "slGui2.Biogasanlagen.sl.finalweight_in_level_corrected", "slGui2.Biomasseheizkraftwerke.sl.finalweight_in_level_corrected",
                                         "slGui2.Einzelhausholzheizungen.sl.finalweight_in_level_corrected",
                                         "slGui2.Tiefengeothermieanlagen.sl.finalweight_in_level_corrected",
                                         "slGui2.Wrmepumpen.sl.finalweight_in_level_corrected", "Anlagengröße.und.Zentralisierung.bsc.timesClicked",
                                         "Biomassenverwendung.bsc.timesClicked", "Energieverbrauch.bsc.timesClicked",
                                         "Regionale.Auswirkungen.bsc.timesClicked", "Wirtschaftsentwicklung.bsc.timesClicked",
                                         "Anlagengröße.und.Zentralisierung.bsc.visible", "Biomassenverwendung.bsc.visible",
                                         "Energieverbrauch.bsc.visible", "Regionale.Auswirkungen.bsc.visible",
                                         "Wirtschaftsentwicklung.bsc.visible"), row.names = 1L, class = "data.frame")
