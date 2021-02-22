############################## (START) Datentypen setzen bei "df_kaderRaw"
# character => numeric
df_kaderRaw$verein_id = as.numeric(df_kaderRaw$verein_id)
df_kaderRaw$spieler_id = as.numeric(df_kaderRaw$spieler_id)
df_kaderRaw$saison = as.numeric(df_kaderRaw$saison)
df_kaderRaw$rueckennummer = as.numeric(df_kaderRaw$rueckennummer)

# character => date
df_kaderRaw$im_team_seit <- strptime(df_kaderRaw$im_team_seit,format="%d.%m.%Y")
df_kaderRaw$vertrag_bis <- strptime(df_kaderRaw$vertrag_bis,format="%d.%m.%Y")
#df_kaderRaw$gespielt_von <- strptime(df_kaderRaw$gespielt_von,format="%d.%m.%Y")
#df_kaderRaw$gespielt_bis <- strptime(df_kaderRaw$gespielt_bis,format="%d.%m.%Y")

# Marktwert
df_kaderRaw$marktwert = ifelse(grepl("Tsd.", df_kaderRaw$marktwert, fixed = TRUE), as.numeric(sub(" .*", "", df_kaderRaw$marktwert)) * 1000,
                         ifelse(is.na(df_kaderRaw$marktwert) == FALSE & grepl("Mio.", df_kaderRaw$marktwert, fixed = TRUE),
                                as.numeric(sub(" .*", "", gsub(",", ".", df_kaderRaw$marktwert))) * 1000000, NA))
df_kaderRaw$marktwert <- as.numeric(df_kaderRaw$marktwert / 1000000)
############################## (ENDE) Datentypen setzen bei "df_kaderRaw"


############################## (START) Datentypen setzen bei "df_verletzungenRaw"
# character => numeric
df_verletzungenRaw$spieler_id = as.numeric(df_verletzungenRaw$spieler_id)
df_verletzungenRaw$tage = as.numeric(df_verletzungenRaw$tage)
df_verletzungenRaw$saison = as.numeric(df_verletzungenRaw$saison)
df_verletzungenRaw$verpasste_spiele =
  as.numeric(df_verletzungenRaw$verpasste_spiele)

# character => date
df_verletzungenRaw$von <- strptime(df_verletzungenRaw$von,format="%d.%m.%Y")
df_verletzungenRaw$bis <- strptime(df_verletzungenRaw$bis,format="%d.%m.%Y")
############################## (ENDE) Datentypen setzen bei "df_verletzungenRaw"


############################## (START) Datentypen setzen bei "df_spieler_Raw"
# character => date
df_spielerRaw$geburtsdatum <- strptime(df_spielerRaw$geburtsdatum,
                                       format="%d.%m.%Y")
############################## (ENDE) Datentypen setzen bei "df_spieler_Raw"


############################## (START) Datentypen setzen bei "df_tabelleRaw"
# character => numeric
df_tabelleRaw$verein_id = as.numeric(df_tabelleRaw$verein_id)
df_tabelleRaw$saison = as.numeric(df_tabelleRaw$saison)
############################## (ENDE) Datentypen setzen bei "df_tabelleRaw"