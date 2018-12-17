#Daten aufbauen --------------

library (data.table)

dtAlternativen <- data.table(titel= as.factor(c("Jetzt-Zustand",
                                                "Abgestimmtes Szenario","Abgestimmtes Szenario",
                                                "Windkraft","Windkraft","Windkraft") ),
                             rahmenszenario=as.factor(c("Jetzt-Zustand",
                                                        "Das Wachstum geht weiter", "Nachhaltigkeit schafft Werte",
                                                        "Nachhaltigkeit schafft Werte", "Das Wachstum geht weiter", "Kein Land in Sicht")),
                             Energieverbrauch_Strom=0
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
  Energiewendeziel= list (class="elements",
    description="Mir ist wichtig, dass die Region sich durch regionale erneuerbare Energien mit Energie versorgt.",
    standardweight=45, color="red",
    util_func="antiprop",   util_offset=10,
    include_parent=FALSE,
    Energieverbrauch= list(
      class="elements",
      description="Ein generell niedriger Energieverbrauch in der Region Oberland ist mir wichtig.",
      'Energieverbrauch (Strom)'= list(class="mapping",Attribname="Energieverbrauch_Strom",
                                       description="Ein generell niedriger Stromverbrauch in der Region Oberland ist mir wichtig"),
      'Energieverbrauch (Wärme)'= list(class="mapping",Attribname=NA,
                                       description="Ein generell niedriger Wärmeverbrauch in der Region Oberland ist mir wichtig")
      ),
    'Gesamtanteil erneuerbarer Energien'= list(class="mapping",Attribname=NA,
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
    color="blue"
  ),
  'Umwelteffekte' = list (
    class="elements",
    description="Mir ist wichtig, wie das regionale Energiesystem sich auf die Umwelt auswirkt.",
    color="green"
  ) ,
  'Regionale Auswirkungen' = list (
    class="elements",
    description="Mir ist wichtig, dass es möglichst geringe regionale Auswirkungen durch Energieerzeugungsanlagen gibt.",
    color="grey"
  )

)



# Texte --------------------------------------------------------

texte <- list (
  auswahlaufforderungstext="Bitte wählen sie zuerst aus, welche ALternative Ihnen spontan am besten gefällt.",
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

speicher_template = NULL

speichersettings= list( #method="GoogleSheets",
                        method="CSV",
                        place="MCDA_Beispiel_INOLA")
