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
                             Anteil_Energie=c(1,rep_len(100,9))

                             )
summary(dtAlternativen)

# Konfiguration aufbauen -----------------

configList <- list (
  class="elements",
  open.maxdepth = 2,
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
  Energiewendeziel= list (class="elements",
    description="Mir ist wichtig, dass die Region sich durch regionale erneuerbare Energien mit Energie versorgt.",
    standardweight=45, color="red",
    util_func="prop",
    include_parent=TRUE,
    Energieverbrauch= list(
      class="elements",
      description="Ein generell niedriger Energieverbrauch in der Region Oberland ist mir wichtig.",
      'Energieverbrauch (Strom)'= list(class="mapping",Attribname="Energieverbrauch_Strom",
                                       util_func="antiprop",   util_offset=10,include_parent=TRUE,
                                       description="Ein generell niedriger Stromverbrauch in der Region Oberland ist mir wichtig"),
      'Energieverbrauch (Wärme)'= list(class="mapping",Attribname="Energieverbrauch_Wärme",
                                       util_func="antiprop",   util_offset=10,include_parent=TRUE,
                                       description="Ein generell niedriger Wärmeverbrauch in der Region Oberland ist mir wichtig")
      ),
    'Gesamtanteil erneuerbarer Energien'= list(class="mapping",Attribname="Anteil_Energie",
                                               description="Mir ist wichtig, dass im Jahresmittel ein hoher Anteil des regionalen Energieverbrauchst durch regionale erneuerbare Energien erzeugt wird."),
    'Energieautarkie (Strom)'= list(class="mapping",Attribname=NA,
                                               description="„Es ist mir wichtig, dass sich die Region möglichst  zu jedem Zeitpunkt  mit lokal produziertem erneuerbarem Strom versorgt“ "),
    'Energieautarkie (Wärme)'= list(class="mapping",Attribname=NA,
                                   description="Es ist mir wichtig, dass sich die Region möglichst zu jedem Zeitpunkt  mit lokal produzierter Wärme bzw. Wärme aus einheimischen regenerativen Rohstoffen versorgt.")
  ),
  'Struktur des Energiesystems' = list (class="elements",
    description="Mir ist wichtig, wie das regionale Energiesystem technisch und sozial ausgestaltet ist.",
    color="orange",
    'Diversität des Energiesystems'= list(
      class="elements",
      description="Es ist mir wichtig, dass das regionale Energiesystem aus einem breiten Mix unterschiedlicher erneuerbare Energien besteht.",
      'Diversität der Stromversorgung'=list(class="mapping",Attribname=NA,
                                            description="Es ist mir wichtig, dass das regionale STROMsystem aus einem breiten Mix unterschiedlicher erneuerbare Energien besteht."),
      'Diversität der Wärmeversorgung'= list(class="mapping",Attribname=NA,
                                      description="Es ist mir wichtig, dass das regionale WÄRMEsystem aus einem breiten Mix unterschiedlicher erneuerbare Energien besteht.")
      ),
    'Anlagengröße und Zentralisierung'= list(
      class="elements",
      description="Es ist mir wichtig, ob das Energiesystem in der Region vor allem aus vielen kleineren Anlage oder aus wenigen großen Anlagen besteht.",
      'Anlagengröße Stromerzeugung'=list(class="mapping",Attribname=NA,
                                            description="Ich präferiere wenige große Anlagen zur Stromerzeugung, es ist mir egal, oder ich präferiere viele kleine Anlagen zur Stromerzeugung(z.B.: Mehr Verbundkraftwerke anstatt Einzelhausanlagen)."),
      'Anlagengröße Wärmeerzeugung'=list(class="mapping",Attribname=NA,
                                         description="Ich präferiere wenige große Anlagen zur Wärmeerzeugung, es ist mir egal, oder ich präferiere viele kleine Anlagen zur Stromerzeugung(z.B.: Nahwärmenetze mit großem BHKW vs. Nahwärmenetze mit Kombinationen Wärmespeicher, Solarthermieanlagen, Wärmepumpe)."),
      'Infrastruktur der Wärmeerzeugung'=list(class="mapping",Attribname=NA,
                                         description="Ich präferiere eigenständige Wärmeerzeugung, es ist mir egal, oder ich präferiere Gemeinschaftsanlagen zur Wärmeerzeugung(z.B.: Mehr Nahwärmenetze anstatt Einzelhausheizungen).")
    ),
    'Ausgleichsmechanismen'= list(
      class="elements",
      description="Mir ist wichtig, durch welche Technologien Schwankungen in der Energieerzeugung ausgeglichen werden.",
      'Speicher'=list(class="mapping",Attribname=NA,
                      description="Mir ist es wichtig, dass auch in Niedrigproduktionszeiten möglichst wenig Speicher benötigt werden."),
      'Importe/Exporte'=list(class="mapping",Attribname=NA,
                             description="Mir ist wichtig, dass es möglichst wenig Austausch (Stromimporte/Exporte) über die Region hinaus gibt."),
      'Abregelung'=list(class="mapping",Attribname=NA,
                        description="Mir ist wichtig, dass es möglichst wenig Abregelung gibt.")
    )
  ),

  'Ökonomische Effekte' = list (
    class="elements",
    description="Mir ist wichtig, wie das regionale Energiesystem sich in der Region ökonomisch auswirkt.",
    color="blue",
    'Ökonomische Effekte im Bereich EE' =list(
      class="elements",
      description="Es ist mir wichtig, dass im Bereich EE-Anlagen und im Energiesystem Wirtschaftskraft in der Region geschaffen wird",
      'Regionale Wertschöpfung im Bereich EE'=list(class="mapping",Attribname=NA,
                      description="Es ist mir wichtig, dass im Bereich EE-Anlagen und im Energiesystem Wertschöpfung in der Region geschaffen wird."),
      'Arbeitsplätze im Bereich EE'=list(class="mapping",Attribname=NA,
                             description="Es ist mir wichtig, dass im Bereich EE-Anlagen und im Energiesystem in der Region neue Arbeitsplätze geschaffen werden.")
      ),
    'Regionale Wirtschaftsentwicklung durch EE - langfristig und übergreifend' =list(
      class="elements",
      description="Es ist mir wichtig, dass sich der Bau von EE-Anlagen und das regionale Energiesystem langfristig positiv auf die gesamte regionale Wirtschaft auswirkt.",
      'Regionale Wertschöpfung durch EE- langfristig und übergreifend'=list(class="mapping",Attribname=NA,
                                                    description="Es ist mir wichtig, dass sich der Bau von EE-Anlagen und das regionale Energiesystem langfristig positiv auf die gesamte regionale Wertschöpfung auswirkt"),
      'Regionale Arbeitsplätze durch EE - langfristig und übergreifend'=list(class="mapping",Attribname=NA,
                                         description="Es ist mir wichtig, dass der Bau von EE-Anlagen und das regionale Energiesystem langfristig für mehr Arbeitsplätze in der gesamten regionalen Wirtschaft sorgt.")

      ),
    'Energiekosten' =list(
      class="elements",
      description="Es ist mir wichtig, dass die Kosten des regionalen Energiesystems gering sind.",
      'Energieerzeugungskosten'=list(class="mapping",Attribname=NA,
                                     description="Mir ist wichtig, dass die durchschnittlichen Erzeugungskosten pro KwH (Strom und Wärme) gering sind."),
      'Kosten des Baus von EE-Anlagen'=list(class="mapping",Attribname=NA,
                                            description="Es ist mir wichtig, dass die Kosten für den Bau und die Errichtung  von EE-Anlagen in der Region niedrig sind.")

    )
  ),
  'Umwelteffekte' = list (
    class="elements",
    description="Mir ist wichtig, wie das regionale Energiesystem sich auf die Umwelt auswirkt.",
    color="green",
    'Anteil fossiler Brennstoffe' =list(
      class="mapping",Attribname=NA,
      description="Es ist mir wichtig, dass die Energieversorgung möglichst CO2 frei ist."
      ),
    'Ressourcenbeanspruchung' =list(
      class="mapping",Attribname=NA,
      description="Es ist mir wichtig, dass der Bau/Rückbau von EE-Anlagen möglichst weniger Rohstoffe bedarf/möglichst wenig Müll verursacht."
    ),
    'Flächenbeanspruchung' =list(
      class="mapping",Attribname=NA,
      description="Es ist mir wichtig, dass der Bau von EE-Anlagen möglichst wenig Fläche beansprucht."
    ),
    'Anteil Energieproduktion an landwirtschaftlicher Produktion' =list(
      class="elements",
      description="Es ist mir wichtig, dass vom gesamten Aufkommen an Mais/Grünland/Gülle/Holz nur ein geringer Anteil zur Energieproduktion verwendet wird.",
      'Anteil Energieproduktion an Mais'=list(class="mapping",Attribname=NA,
                                     description="Es ist mir wichtig, dass vom gesamten Aufkommen an Mais nur ein geringer Anteil zur Energieproduktion verwendet wird."),
      'Anteil Energieproduktion an Holzzuwachs'=list(class="mapping",Attribname=NA,
                                            description="Es ist mir wichtig, dass vom gesamten Holzzuwachs nur ein geringer Anteil zur Energieproduktion verwendet wird.")

    )

  ) ,
  'Regionale Auswirkungen' = list (
    class="elements",
    description="Mir ist wichtig, dass es möglichst geringe regionale Auswirkungen durch Energieerzeugungsanlagen gibt.",
    color="grey",
    'Auswirkungen der Windkraft' =list(
      class="mapping",Attribname=NA,
      description="Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch Windkraftanlagen gibt."
      ),
    'Auswirkungen der PV-Freiflächenanlagen' =list(
      class="mapping",Attribname=NA,
      description="Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch PV-Freiflächenanlagen gibt."
    ),
    'Auswirkungen der PV-Dachflächenanlagen' =list(
      class="mapping",Attribname=NA,
      description="Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch PV-Dachflächenanlagen gibt."
    ),
    'Auswirkungen der Wasserkraftanlagen' =list(
      class="mapping",Attribname=NA,
      description="Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch Wasserkraftanlagen gibt."
    ),
    'Auswirkungen der Biogasanlagen' =list(
      class="mapping",Attribname=NA,
      description="Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch Biogasanlagen gibt."
    ),
    'Auswirkungen der Biomasseanlagen' =list(
      class="mapping",Attribname=NA,
      description="Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch Biomasseanlagen gibt."
    ),
    'Auswirkungen der Geothermieanlagen' =list(
      class="mapping",Attribname=NA,
      description="Es ist mir wichtig, dass es möglichst geringe regionale Auswirkungen durch Geothermieanlagen gibt."
    )
  )

)



# Texte --------------------------------------------------------

texte <- list (

  begruessungstext="Dieses Programm ist eine Entscheidungshilfe. Auf den nächsten Seiten können sie gewichten, wie wichtig ihnen verschiedene Eigenschaften sind. Auf welches Attribut legen Sie wie viel Wert? Das Programm berechnet dann, welches der verschiedenen Alternativen am meisten Ihren Präferenzen entspricht.",
  auswahlaufforderungstext="Bitte wählen sie jedoch zuerst aus, welche Alternative Ihnen spontan am besten gefällt.Die einzelnen Alternativen sind unten beschrieben",

  begruessungstext2= "Bitte stellen sie ein, wie wichtig Ihnen die einzelnen Indikatoren im Verhältnis zu den anderen Indikatoren sind.",

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
# dtBisherige <- loadData("CSV", "inst/mcdsupportshinyApp/MCDA_Beispiel_INOLA")
#
# dput(dtBisherige[1,])

speicher_template = structure(list(
               Zeitpunkt = "Tue Dec 18 16:00:50 2018", Sessionstart = "Tue Dec 18 15:58:13 2018",
               session_id = 182377L, gruppe = NA, url_search = NA, addBtn = 0L,
               PlaceSlct = "Nein, aber in Oberbayern", FirsttimeSlct = "Nein",
               GenderSlct = "Weitere/Divers", AgeSl = 30L, ChoiceSlct = "Größere Anlagen",
               ChoiceSlctCount = 1L, ChoiceFinalSlct = "Fortschreibung",
               ChoiceFinalSlctCount = 0L, BestesErgebnis = "Fortschreibung, Größere Anlagen, Jetzt-Zustand, Kleinere Anlagen",
               Abregelung.sl.originalweights = 30L, Anlagengröße.Stromerzeugung.sl.originalweights = 30L,
               Anlagengröße.Wärmeerzeugung.sl.originalweights = 30L, Anlagengröße.und.Zentralisierung.sl.originalweights = 30L,
               Anteil.Energieproduktion.an.Holzzuwachs.sl.originalweights = 30L,
               Anteil.Energieproduktion.an.Mais.sl.originalweights = 30L,
               Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.sl.originalweights = 30L,
               Anteil.fossiler.Brennstoffe.sl.originalweights = 30L, Arbeitsplätze.im.Bereich.EE.sl.originalweights = 30L,
               Ausgleichsmechanismen.sl.originalweights = 30L, Auswirkungen.der.Biogasanlagen.sl.originalweights = 30L,
               Auswirkungen.der.Biomasseanlagen.sl.originalweights = 30L,
               Auswirkungen.der.Geothermieanlagen.sl.originalweights = 30L,
               Auswirkungen.der.PV.Dachflächenanlagen.sl.originalweights = 30L,
               Auswirkungen.der.PV.Freiflächenanlagen.sl.originalweights = 30L,
               Auswirkungen.der.Wasserkraftanlagen.sl.originalweights = 30L,
               Auswirkungen.der.Windkraft.sl.originalweights = 30L, Diversität.der.Stromversorgung.sl.originalweights = 30L,
               Diversität.der.Wärmeversorgung.sl.originalweights = 30L,
               Diversität.des.Energiesystems.sl.originalweights = 30L, Energieautarkie..Strom..sl.originalweights = 45L,
               Energieautarkie..Wärme..sl.originalweights = 45L, Energieerzeugungskosten.sl.originalweights = 30L,
               Energiekosten.sl.originalweights = 30L, Energieverbrauch.sl.originalweights = 45L,
               Energieverbrauch..Strom..sl.originalweights = 45L, Energieverbrauch..Wärme..sl.originalweights = 45L,
               Energiewendeziel.sl.originalweights = 45L, Flächenbeanspruchung.sl.originalweights = 30L,
               Gesamtanteil.erneuerbarer.Energien.sl.originalweights = 45L,
               Importe.Exporte.sl.originalweights = 30L, Infrastruktur.der.Wärmeerzeugung.sl.originalweights = 30L,
               Kosten.des.Baus.von.EE.Anlagen.sl.originalweights = 30L,
               Regionale.Arbeitsplätze.durch.EE...langfristig.und.übergreifend.sl.originalweights = 30L,
               Regionale.Auswirkungen.sl.originalweights = 30L, Regionale.Wertschöpfung.durch.EE..langfristig.und.übergreifend.sl.originalweights = 30L,
               Regionale.Wertschöpfung.im.Bereich.EE.sl.originalweights = 30L,
               Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.sl.originalweights = 30L,
               Ressourcenbeanspruchung.sl.originalweights = 30L, Speicher.sl.originalweights = 30L,
               Struktur.des.Energiesystems.sl.originalweights = 30L, Umwelteffekte.sl.originalweights = 30L,
               Ökonomische.Effekte.sl.originalweights = 30L, Ökonomische.Effekte.im.Bereich.EE.sl.originalweights = 30L,
               Abregelung.sl.finalweight_in_level = 0.333333333333333, Anlagengröße.Stromerzeugung.sl.finalweight_in_level = 0.333333333333333,
               Anlagengröße.Wärmeerzeugung.sl.finalweight_in_level = 0.333333333333333,
               Anlagengröße.und.Zentralisierung.sl.finalweight_in_level = 0.333333333333333,
               Anteil.Energieproduktion.an.Holzzuwachs.sl.finalweight_in_level = 0.5,
               Anteil.Energieproduktion.an.Mais.sl.finalweight_in_level = 0.5,
               Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.sl.finalweight_in_level = 0.25,
               Anteil.fossiler.Brennstoffe.sl.finalweight_in_level = 0.25,
               Arbeitsplätze.im.Bereich.EE.sl.finalweight_in_level = 0.5,
               Ausgleichsmechanismen.sl.finalweight_in_level = 0.333333333333333,
               Auswirkungen.der.Biogasanlagen.sl.finalweight_in_level = 0.142857142857143,
               Auswirkungen.der.Biomasseanlagen.sl.finalweight_in_level = 0.142857142857143,
               Auswirkungen.der.Geothermieanlagen.sl.finalweight_in_level = 0.142857142857143,
               Auswirkungen.der.PV.Dachflächenanlagen.sl.finalweight_in_level = 0.142857142857143,
               Auswirkungen.der.PV.Freiflächenanlagen.sl.finalweight_in_level = 0.142857142857143,
               Auswirkungen.der.Wasserkraftanlagen.sl.finalweight_in_level = 0.142857142857143,
               Auswirkungen.der.Windkraft.sl.finalweight_in_level = 0.142857142857143,
               Diversität.der.Stromversorgung.sl.finalweight_in_level = 0.5,
               Diversität.der.Wärmeversorgung.sl.finalweight_in_level = 0.5,
               Diversität.des.Energiesystems.sl.finalweight_in_level = 0.333333333333333,
               Energieautarkie..Strom..sl.finalweight_in_level = 0.25, Energieautarkie..Wärme..sl.finalweight_in_level = 0.25,
               Energieerzeugungskosten.sl.finalweight_in_level = 0.5, Energiekosten.sl.finalweight_in_level = 0.333333333333333,
               Energieverbrauch.sl.finalweight_in_level = 0.25, Energieverbrauch..Strom..sl.finalweight_in_level = 0.5,
               Energieverbrauch..Wärme..sl.finalweight_in_level = 0.5, Energiewendeziel.sl.finalweight_in_level = 0.272727272727273,
               Flächenbeanspruchung.sl.finalweight_in_level = 0.25, Gesamtanteil.erneuerbarer.Energien.sl.finalweight_in_level = 0.25,
               Importe.Exporte.sl.finalweight_in_level = 0.333333333333333,
               Infrastruktur.der.Wärmeerzeugung.sl.finalweight_in_level = 0.333333333333333,
               Kosten.des.Baus.von.EE.Anlagen.sl.finalweight_in_level = 0.5,
               Regionale.Arbeitsplätze.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level = 0.5,
               Regionale.Auswirkungen.sl.finalweight_in_level = 0.181818181818182,
               Regionale.Wertschöpfung.durch.EE..langfristig.und.übergreifend.sl.finalweight_in_level = 0.5,
               Regionale.Wertschöpfung.im.Bereich.EE.sl.finalweight_in_level = 0.5,
               Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level = 0.333333333333333,
               Ressourcenbeanspruchung.sl.finalweight_in_level = 0.25, Speicher.sl.finalweight_in_level = 0.333333333333333,
               Struktur.des.Energiesystems.sl.finalweight_in_level = 0.181818181818182,
               Umwelteffekte.sl.finalweight_in_level = 0.181818181818182,
               Ökonomische.Effekte.sl.finalweight_in_level = 0.181818181818182,
               Ökonomische.Effekte.im.Bereich.EE.sl.finalweight_in_level = 0.333333333333333,
               Abregelung.sl.finalweight_in_level_corrected = 0L, Anlagengröße.Stromerzeugung.sl.finalweight_in_level_corrected = 0L,
               Anlagengröße.Wärmeerzeugung.sl.finalweight_in_level_corrected = 0L,
               Anlagengröße.und.Zentralisierung.sl.finalweight_in_level_corrected = 0.333333333333333,
               Anteil.Energieproduktion.an.Holzzuwachs.sl.finalweight_in_level_corrected = 0L,
               Anteil.Energieproduktion.an.Mais.sl.finalweight_in_level_corrected = 0L,
               Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.sl.finalweight_in_level_corrected = 1L,
               Anteil.fossiler.Brennstoffe.sl.finalweight_in_level_corrected = 0L,
               Arbeitsplätze.im.Bereich.EE.sl.finalweight_in_level_corrected = 0L,
               Ausgleichsmechanismen.sl.finalweight_in_level_corrected = 0.333333333333333,
               Auswirkungen.der.Biogasanlagen.sl.finalweight_in_level_corrected = 0L,
               Auswirkungen.der.Biomasseanlagen.sl.finalweight_in_level_corrected = 0L,
               Auswirkungen.der.Geothermieanlagen.sl.finalweight_in_level_corrected = 0L,
               Auswirkungen.der.PV.Dachflächenanlagen.sl.finalweight_in_level_corrected = 0L,
               Auswirkungen.der.PV.Freiflächenanlagen.sl.finalweight_in_level_corrected = 0L,
               Auswirkungen.der.Wasserkraftanlagen.sl.finalweight_in_level_corrected = 0L,
               Auswirkungen.der.Windkraft.sl.finalweight_in_level_corrected = 0L,
               Diversität.der.Stromversorgung.sl.finalweight_in_level_corrected = 0L,
               Diversität.der.Wärmeversorgung.sl.finalweight_in_level_corrected = 0L,
               Diversität.des.Energiesystems.sl.finalweight_in_level_corrected = 0.333333333333333,
               Energieautarkie..Strom..sl.finalweight_in_level_corrected = 0L,
               Energieautarkie..Wärme..sl.finalweight_in_level_corrected = 0L,
               Energieerzeugungskosten.sl.finalweight_in_level_corrected = 0L,
               Energiekosten.sl.finalweight_in_level_corrected = 0.333333333333333,
               Energieverbrauch.sl.finalweight_in_level_corrected = 1, Energieverbrauch..Strom..sl.finalweight_in_level_corrected = 1,
               Energieverbrauch..Wärme..sl.finalweight_in_level_corrected = 0,
               Energiewendeziel.sl.finalweight_in_level_corrected = 0.272727272727273,
               Flächenbeanspruchung.sl.finalweight_in_level_corrected = 0L,
               Gesamtanteil.erneuerbarer.Energien.sl.finalweight_in_level_corrected = 0,
               Importe.Exporte.sl.finalweight_in_level_corrected = 0L, Infrastruktur.der.Wärmeerzeugung.sl.finalweight_in_level_corrected = 0L,
               Kosten.des.Baus.von.EE.Anlagen.sl.finalweight_in_level_corrected = 0L,
               Regionale.Arbeitsplätze.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level_corrected = 0L,
               Regionale.Auswirkungen.sl.finalweight_in_level_corrected = 0.181818181818182,
               Regionale.Wertschöpfung.durch.EE..langfristig.und.übergreifend.sl.finalweight_in_level_corrected = 0L,
               Regionale.Wertschöpfung.im.Bereich.EE.sl.finalweight_in_level_corrected = 0L,
               Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level_corrected = 0.333333333333333,
               Ressourcenbeanspruchung.sl.finalweight_in_level_corrected = 0L,
               Speicher.sl.finalweight_in_level_corrected = 0L, Struktur.des.Energiesystems.sl.finalweight_in_level_corrected = 0.181818181818182,
               Umwelteffekte.sl.finalweight_in_level_corrected = 0.181818181818182,
               Ökonomische.Effekte.sl.finalweight_in_level_corrected = 0.181818181818182,
               Ökonomische.Effekte.im.Bereich.EE.sl.finalweight_in_level_corrected = 0.333333333333333,
               Anlagengröße.und.Zentralisierung.bsc.timesClicked = 0L, Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.bsc.timesClicked = 1L,
               Ausgleichsmechanismen.bsc.timesClicked = 0L, Diversität.des.Energiesystems.bsc.timesClicked = 1L,
               Energiekosten.bsc.timesClicked = 0L, Energieverbrauch.bsc.timesClicked = 1L,
               Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.bsc.timesClicked = 1L,
               Ökonomische.Effekte.im.Bereich.EE.bsc.timesClicked = 1L,
               Anlagengröße.und.Zentralisierung.bsc.visible = FALSE, Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.bsc.visible = FALSE,
               Ausgleichsmechanismen.bsc.visible = FALSE, Diversität.des.Energiesystems.bsc.visible = FALSE,
               Energiekosten.bsc.visible = FALSE, Energieverbrauch.bsc.visible = FALSE,
               Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.bsc.visible = FALSE,
               Ökonomische.Effekte.im.Bereich.EE.bsc.visible = FALSE),
               .Names = c("Zeitpunkt",
                          "Sessionstart", "session_id", "gruppe", "url_search", "addBtn",
                          "PlaceSlct", "FirsttimeSlct", "GenderSlct", "AgeSl", "ChoiceSlct",
                          "ChoiceSlctCount", "ChoiceFinalSlct", "ChoiceFinalSlctCount",
                          "BestesErgebnis", "Abregelung.sl.originalweights", "Anlagengröße.Stromerzeugung.sl.originalweights",
                          "Anlagengröße.Wärmeerzeugung.sl.originalweights", "Anlagengröße.und.Zentralisierung.sl.originalweights",
                          "Anteil.Energieproduktion.an.Holzzuwachs.sl.originalweights",
                          "Anteil.Energieproduktion.an.Mais.sl.originalweights", "Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.sl.originalweights",
                          "Anteil.fossiler.Brennstoffe.sl.originalweights", "Arbeitsplätze.im.Bereich.EE.sl.originalweights",
                          "Ausgleichsmechanismen.sl.originalweights", "Auswirkungen.der.Biogasanlagen.sl.originalweights",
                          "Auswirkungen.der.Biomasseanlagen.sl.originalweights", "Auswirkungen.der.Geothermieanlagen.sl.originalweights",
                          "Auswirkungen.der.PV.Dachflächenanlagen.sl.originalweights",
                          "Auswirkungen.der.PV.Freiflächenanlagen.sl.originalweights",
                          "Auswirkungen.der.Wasserkraftanlagen.sl.originalweights", "Auswirkungen.der.Windkraft.sl.originalweights",
                          "Diversität.der.Stromversorgung.sl.originalweights", "Diversität.der.Wärmeversorgung.sl.originalweights",
                          "Diversität.des.Energiesystems.sl.originalweights", "Energieautarkie..Strom..sl.originalweights",
                          "Energieautarkie..Wärme..sl.originalweights", "Energieerzeugungskosten.sl.originalweights",
                          "Energiekosten.sl.originalweights", "Energieverbrauch.sl.originalweights",
                          "Energieverbrauch..Strom..sl.originalweights", "Energieverbrauch..Wärme..sl.originalweights",
                          "Energiewendeziel.sl.originalweights", "Flächenbeanspruchung.sl.originalweights",
                          "Gesamtanteil.erneuerbarer.Energien.sl.originalweights", "Importe.Exporte.sl.originalweights",
                          "Infrastruktur.der.Wärmeerzeugung.sl.originalweights", "Kosten.des.Baus.von.EE.Anlagen.sl.originalweights",
                          "Regionale.Arbeitsplätze.durch.EE...langfristig.und.übergreifend.sl.originalweights",
                          "Regionale.Auswirkungen.sl.originalweights", "Regionale.Wertschöpfung.durch.EE..langfristig.und.übergreifend.sl.originalweights",
                          "Regionale.Wertschöpfung.im.Bereich.EE.sl.originalweights", "Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.sl.originalweights",
                          "Ressourcenbeanspruchung.sl.originalweights", "Speicher.sl.originalweights",
                          "Struktur.des.Energiesystems.sl.originalweights", "Umwelteffekte.sl.originalweights",
                          "Ökonomische.Effekte.sl.originalweights", "Ökonomische.Effekte.im.Bereich.EE.sl.originalweights",
                          "Abregelung.sl.finalweight_in_level", "Anlagengröße.Stromerzeugung.sl.finalweight_in_level",
                          "Anlagengröße.Wärmeerzeugung.sl.finalweight_in_level", "Anlagengröße.und.Zentralisierung.sl.finalweight_in_level",
                          "Anteil.Energieproduktion.an.Holzzuwachs.sl.finalweight_in_level",
                          "Anteil.Energieproduktion.an.Mais.sl.finalweight_in_level", "Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.sl.finalweight_in_level",
                          "Anteil.fossiler.Brennstoffe.sl.finalweight_in_level", "Arbeitsplätze.im.Bereich.EE.sl.finalweight_in_level",
                          "Ausgleichsmechanismen.sl.finalweight_in_level", "Auswirkungen.der.Biogasanlagen.sl.finalweight_in_level",
                          "Auswirkungen.der.Biomasseanlagen.sl.finalweight_in_level", "Auswirkungen.der.Geothermieanlagen.sl.finalweight_in_level",
                          "Auswirkungen.der.PV.Dachflächenanlagen.sl.finalweight_in_level",
                          "Auswirkungen.der.PV.Freiflächenanlagen.sl.finalweight_in_level",
                          "Auswirkungen.der.Wasserkraftanlagen.sl.finalweight_in_level",
                          "Auswirkungen.der.Windkraft.sl.finalweight_in_level", "Diversität.der.Stromversorgung.sl.finalweight_in_level",
                          "Diversität.der.Wärmeversorgung.sl.finalweight_in_level", "Diversität.des.Energiesystems.sl.finalweight_in_level",
                          "Energieautarkie..Strom..sl.finalweight_in_level", "Energieautarkie..Wärme..sl.finalweight_in_level",
                          "Energieerzeugungskosten.sl.finalweight_in_level", "Energiekosten.sl.finalweight_in_level",
                          "Energieverbrauch.sl.finalweight_in_level", "Energieverbrauch..Strom..sl.finalweight_in_level",
                          "Energieverbrauch..Wärme..sl.finalweight_in_level", "Energiewendeziel.sl.finalweight_in_level",
                          "Flächenbeanspruchung.sl.finalweight_in_level", "Gesamtanteil.erneuerbarer.Energien.sl.finalweight_in_level",
                          "Importe.Exporte.sl.finalweight_in_level", "Infrastruktur.der.Wärmeerzeugung.sl.finalweight_in_level",
                          "Kosten.des.Baus.von.EE.Anlagen.sl.finalweight_in_level", "Regionale.Arbeitsplätze.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level",
                          "Regionale.Auswirkungen.sl.finalweight_in_level", "Regionale.Wertschöpfung.durch.EE..langfristig.und.übergreifend.sl.finalweight_in_level",
                          "Regionale.Wertschöpfung.im.Bereich.EE.sl.finalweight_in_level",
                          "Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level",
                          "Ressourcenbeanspruchung.sl.finalweight_in_level", "Speicher.sl.finalweight_in_level",
                          "Struktur.des.Energiesystems.sl.finalweight_in_level", "Umwelteffekte.sl.finalweight_in_level",
                          "Ökonomische.Effekte.sl.finalweight_in_level", "Ökonomische.Effekte.im.Bereich.EE.sl.finalweight_in_level",
                          "Abregelung.sl.finalweight_in_level_corrected", "Anlagengröße.Stromerzeugung.sl.finalweight_in_level_corrected",
                          "Anlagengröße.Wärmeerzeugung.sl.finalweight_in_level_corrected",
                          "Anlagengröße.und.Zentralisierung.sl.finalweight_in_level_corrected",
                          "Anteil.Energieproduktion.an.Holzzuwachs.sl.finalweight_in_level_corrected",
                          "Anteil.Energieproduktion.an.Mais.sl.finalweight_in_level_corrected",
                          "Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.sl.finalweight_in_level_corrected",
                          "Anteil.fossiler.Brennstoffe.sl.finalweight_in_level_corrected",
                          "Arbeitsplätze.im.Bereich.EE.sl.finalweight_in_level_corrected",
                          "Ausgleichsmechanismen.sl.finalweight_in_level_corrected", "Auswirkungen.der.Biogasanlagen.sl.finalweight_in_level_corrected",
                          "Auswirkungen.der.Biomasseanlagen.sl.finalweight_in_level_corrected",
                          "Auswirkungen.der.Geothermieanlagen.sl.finalweight_in_level_corrected",
                          "Auswirkungen.der.PV.Dachflächenanlagen.sl.finalweight_in_level_corrected",
                          "Auswirkungen.der.PV.Freiflächenanlagen.sl.finalweight_in_level_corrected",
                          "Auswirkungen.der.Wasserkraftanlagen.sl.finalweight_in_level_corrected",
                          "Auswirkungen.der.Windkraft.sl.finalweight_in_level_corrected",
                          "Diversität.der.Stromversorgung.sl.finalweight_in_level_corrected",
                          "Diversität.der.Wärmeversorgung.sl.finalweight_in_level_corrected",
                          "Diversität.des.Energiesystems.sl.finalweight_in_level_corrected",
                          "Energieautarkie..Strom..sl.finalweight_in_level_corrected",
                          "Energieautarkie..Wärme..sl.finalweight_in_level_corrected",
                          "Energieerzeugungskosten.sl.finalweight_in_level_corrected",
                          "Energiekosten.sl.finalweight_in_level_corrected", "Energieverbrauch.sl.finalweight_in_level_corrected",
                          "Energieverbrauch..Strom..sl.finalweight_in_level_corrected",
                          "Energieverbrauch..Wärme..sl.finalweight_in_level_corrected",
                          "Energiewendeziel.sl.finalweight_in_level_corrected", "Flächenbeanspruchung.sl.finalweight_in_level_corrected",
                          "Gesamtanteil.erneuerbarer.Energien.sl.finalweight_in_level_corrected",
                          "Importe.Exporte.sl.finalweight_in_level_corrected", "Infrastruktur.der.Wärmeerzeugung.sl.finalweight_in_level_corrected",
                          "Kosten.des.Baus.von.EE.Anlagen.sl.finalweight_in_level_corrected",
                          "Regionale.Arbeitsplätze.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level_corrected",
                          "Regionale.Auswirkungen.sl.finalweight_in_level_corrected", "Regionale.Wertschöpfung.durch.EE..langfristig.und.übergreifend.sl.finalweight_in_level_corrected",
                          "Regionale.Wertschöpfung.im.Bereich.EE.sl.finalweight_in_level_corrected",
                          "Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.sl.finalweight_in_level_corrected",
                          "Ressourcenbeanspruchung.sl.finalweight_in_level_corrected",
                          "Speicher.sl.finalweight_in_level_corrected", "Struktur.des.Energiesystems.sl.finalweight_in_level_corrected",
                          "Umwelteffekte.sl.finalweight_in_level_corrected", "Ökonomische.Effekte.sl.finalweight_in_level_corrected",
                          "Ökonomische.Effekte.im.Bereich.EE.sl.finalweight_in_level_corrected",
                          "Anlagengröße.und.Zentralisierung.bsc.timesClicked", "Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.bsc.timesClicked",
                          "Ausgleichsmechanismen.bsc.timesClicked", "Diversität.des.Energiesystems.bsc.timesClicked",
                          "Energiekosten.bsc.timesClicked", "Energieverbrauch.bsc.timesClicked",
                          "Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.bsc.timesClicked",
                          "Ökonomische.Effekte.im.Bereich.EE.bsc.timesClicked", "Anlagengröße.und.Zentralisierung.bsc.visible",
                          "Anteil.Energieproduktion.an.landwirtschaftlicher.Produktion.bsc.visible",
                          "Ausgleichsmechanismen.bsc.visible", "Diversität.des.Energiesystems.bsc.visible",
                          "Energiekosten.bsc.visible", "Energieverbrauch.bsc.visible",
                          "Regionale.Wirtschaftsentwicklung.durch.EE...langfristig.und.übergreifend.bsc.visible",
                          "Ökonomische.Effekte.im.Bereich.EE.bsc.visible"),
               row.names = 1L, class = "data.frame")
