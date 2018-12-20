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

#dput(dtBisherige[1,])

speicher_template = NULL

speichersettings= list( #method="GoogleSheets",
                        method="CSV",
                        place="MCDA_Beispiel_INOLA")
