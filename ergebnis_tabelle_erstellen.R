# Join Verletzungen und Kader ####
# Inner Join um nur die Beobachtungen aus beiden Dataframes zu behalten
df_verletzungenResult <- df_verletzungenResult %>%
  inner_join(df_kaderRaw, by = c('spieler_id', 'saison'))

# Differenz prüfen
#df_temp <- anti_join(df_verletzungen,
#df_verletzungenFinal, by = c('spieler_id', 'saison'))

## Zuordnung Verein zu Verletzung
df_verletzungenResult <- df_verletzungenResult %>%
  # Es können nur Beobachtungen mit im_team_seit berücksichtigt werden
  filter(!is.na(im_team_seit)) %>%
  # Berechnung der zeitlichen Differenz
  mutate(differenz = bis - im_team_seit) %>% 
  group_by(verletzung_index) %>%
  # Auswahl: kleinsten Differenz und zeitlich vor der Verletzung
  filter(differenz == min(differenz[differenz > 0])) %>%
  select(-differenz) %>%
  ungroup()

# Nur die relevanten Spalten anzeigen
df_verletzungenResult <- select(df_verletzungenResult, verletzung_index,
                                verletzung, spieler_id, saison, von, bis,
                                verein_id, im_team_seit, tage, tage_berechnet, 
                                verpasste_spiele, marktwert)

# Geburtsdatum anfügen
df_geburtsdatum <- select(df_spielerRaw, spieler_id, geburtsdatum)
df_verletzungenResult <- left_join(df_verletzungenResult,
                                   df_geburtsdatum,
                                   by = "spieler_id")
remove(df_geburtsdatum)

# Alter zum Zeitpunkt der Verletzung berechnen
df_verletzungenResult <- df_verletzungenResult %>%
  mutate(alter = age(df_verletzungenResult$geburtsdatum,
                     df_verletzungenResult$von))

# Superstar hinzufügen
df_verletzungenResult <- df_verletzungenResult %>%
  mutate(superstar = ifelse(marktwert >= 40, 1, 0))
############################## (ENDE) Join Verletzungen und Kader


# Join Verletzungen und Liga ####
# Nur die notwendigen Spalten behalten und doppelte Einträge löschen
df_ligaUndVereinNeu <- df_tabelleRaw %>%
  select(liga, verein_id, saison) %>%
  distinct()

# Join
df_verletzungenResult <- left_join(df_verletzungenResult,
                                   df_ligaUndVereinNeu,
                                   by = c("verein_id", "saison"))
############################## (ENDE) Join Verletzungen und Liga


# Tabellenplatz und Punkte pro Saison ####
# Nur Rückrundentabelle und Definition Abstigeskampf und Meisterkampf
df_tabelleResult <- df_tabelleRaw %>%
  select(liga, verein_id, saison, spieltag, platz, punkte) %>%
  filter(spieltag > 20) %>%
  select(-spieltag) %>%
  mutate(abstiegskampf = ifelse(liga == "1-bundesliga" & 
                                platz >= 16, 1, 0)) %>%
  mutate(abstiegskampf = ifelse((liga == "1-bundesliga" |
                                 liga == "premier-league" |
                                 liga == "primera-division" |
                                 liga == "serie-a" |
                                 liga == "ligue-1") &
                                 platz >= 18, 1, 0)) %>%
  mutate(meisterkampf = ifelse(platz <= 2, 1, 0))
############################## (ENDE) Tabellenplatz und Punkte pro Saison


# Ergebnis Tabelle erzeugen ####
# Anzahl Verletzungen je Verein je Saison
df_sumInjurysByTeamBySeason <- df_verletzungenResult %>%
  group_by(verein_id, saison) %>%
  count(verein_id) %>%
  ungroup() %>%
  rename(anzahl_verletzungen = n)

# Anzahl verletzter Spieler je Verein je Saison
df_sumInjuredPlayersByTeamBySeason <- df_verletzungenResult %>%
  group_by(verein_id, saison) %>%
  select(-verletzung, -verletzung_index, -von, -bis, -im_team_seit, -tage,
         -tage_berechnet, -verpasste_spiele, -liga) %>%
  summarise(anzahl_verletzter_spieler = n_distinct(spieler_id))

# Durchschnittsalter des Kaders je Verein je Saison
df_meanAgeByTeamBySeason <- df_verletzungenResult %>%
  group_by(verein_id, saison) %>%
  summarise_at(vars(alter), funs(mean(., na.rm=TRUE))) %>%
  mutate(alter = round(alter, digits = 2)) %>%
  rename(durchschnittsalter_verletzte = alter)

# Durchschnittliche Verletzungstage je Verein je Saison
df_meanInjuryDaysByTeamBySeason <- df_verletzungenResult %>%
  group_by(verein_id, saison) %>%
  summarise_at(vars(tage_berechnet), funs(mean(., na.rm=TRUE))) %>%
  rename(durchschnittliche_verletzungstage = tage_berechnet)

# Durchschnittliche verpasste Spiele je Verein je Saison
df_meanMissedGamesByTeamBySeason <- df_verletzungenResult %>%
  group_by(verein_id, saison) %>%
  summarise_at(vars(verpasste_spiele), funs(mean(., na.rm=TRUE))) %>%
  rename(durchschnittliche_verpasste_spiele = verpasste_spiele)

# Nur die relevanten Spalten anzeigen
df_wettbewerbeResult <- select(df_wettbewerbeRaw, verein_id, saison, 
                               spiele_gesamt, spiele_pokal,
                               spiele_international)

# Kadergräße je Verein und Saison
df_kaderResult <- df_kaderRaw %>%
  group_by(verein_id, saison) %>%
  count(verein_id) %>%
  ungroup() %>%
  rename(kader_groesse = n)

# Marktwerte je Verein und Saison
df_kaderResultTemp <- df_kaderRaw %>%
  group_by(verein_id, saison) %>%
  summarise_at(vars(marktwert), sum, na.rm = TRUE)

# Anzahl Superstars je Verein und Saison
df_kaderResultSuperstars <- df_kaderRaw %>%
  mutate(superstar = ifelse(marktwert >= 40, 1, 0)) %>%
  group_by(verein_id, saison) %>%
  summarise_at(vars(superstar), sum, na.rm = TRUE) %>%
  rename(anzahl_superstars_im_kader = superstar)

# Anzahl verletzte Superstars je Verein und Saison
df_sumInjuredSuperstarsByTeamBySeason <- df_verletzungenResult %>%
  group_by(verein_id, saison) %>%
  select(-verletzung, -verletzung_index, -von, -bis, -im_team_seit, -tage,
         -tage_berechnet, -verpasste_spiele, -liga) %>%
  filter(superstar == 1) %>%
  summarise(anzahl_superstars_verletzt = n_distinct(spieler_id))

# Anzaghl der Verletzungstage je Verein und Saison
df_sumInjuryDaysByTeamBySeason <- df_verletzungenResult %>%
  group_by(verein_id, saison) %>%
  summarise_at(vars(tage_berechnet), sum, na.rm = TRUE) %>%
  rename(anzahl_verletzungstage = tage_berechnet)

# Kadergröße und Marktwerkt zusammenführen
df_kaderResult <- left_join(df_kaderResult,
                            df_kaderResultTemp, 
                            by = c("verein_id", "saison"))

# Ergebnis zusammenfügen
df_result <- left_join(df_wettbewerbeResult,
                       df_kaderResult,
                       by = c("verein_id", "saison"))
df_result <- left_join(df_result,
                       df_meanInjuryDaysByTeamBySeason,
                       by = c("verein_id", "saison"))
df_result <- left_join(df_result,
                       df_sumInjuryDaysByTeamBySeason,
                       by = c("verein_id", "saison"))
df_result <- left_join(df_result,
                       df_meanMissedGamesByTeamBySeason,
                       by = c("verein_id", "saison"))
df_result <- left_join(df_result,
                       df_sumInjurysByTeamBySeason,
                       by = c("verein_id", "saison"))
df_result <- left_join(df_result,
                       df_sumInjuredPlayersByTeamBySeason,
                       by = c("verein_id", "saison"))
df_result <- left_join(df_result,
                       df_meanAgeByTeamBySeason,
                       by = c("verein_id", "saison"))
df_result <- left_join(df_result,
                       df_kaderResultSuperstars,
                       by = c("verein_id", "saison"))
df_result <- left_join(df_result,
                       df_sumInjuredSuperstarsByTeamBySeason,
                       by = c("verein_id", "saison"))


# Spaltentyp korrigieren
df_result$durchschnittliche_verletzungstage =
  as.numeric(df_result$durchschnittliche_verletzungstage)
df_result$durchschnittliche_verpasste_spiele=
  as.numeric(df_result$durchschnittliche_verpasste_spiele)

# Hinzufügen von "Liga"
#df_result <- left_join(df_result,
#                       df_ligaUndVereinNeu, 
#                       by = c("verein_id", "saison"))

# Hinzufügen von Tabellenplatz und Punkte
df_result <- left_join(df_result,
                       df_tabelleResult, 
                       by = c("verein_id", "saison"))


# Dummyvariable "International aktiv"
df_result <- df_result %>%
  mutate(international_aktiv = ifelse(spiele_international > 0, 1, 0))

# Druckvariable" "international_ko_runde"
df_result <- df_result %>%
  mutate(international_ko_runde = ifelse(spiele_international > 6, 1, 0))

# "Druckvariable" "pokal_runde2_erreicht"
df_result <- df_result %>%
  mutate(pokal_runde2_erreicht = ifelse(spiele_pokal > 1, 1, 0))

# Verhältnis spiele_gesamt zu kadergroesse
df_result <- df_result %>%
  mutate(verhaeltnis_spiele_kader = spiele_gesamt/kader_groesse)

# NA Werte bei Anzahl Superstars durch 0 ersetzen
df_result <- df_result %>%
  mutate(anzahl_superstars_verletzt = ifelse(is.na(anzahl_superstars_verletzt),
                                             0, anzahl_superstars_verletzt))

# Temp und unwichtige DF löschen
remove(df_ligaUndVereinNeu)
remove(df_kaderResultTemp)
remove(df_meanInjuryDaysByTeamBySeason)
remove(df_meanMissedGamesByTeamBySeason)
remove(df_sumInjuredPlayersByTeamBySeason)
remove(df_sumInjurysByTeamBySeason)
remove(df_sumInjuryDaysByTeamBySeason)
remove(df_meanAgeByTeamBySeason)
remove(df_kaderResultSuperstars)
remove(df_sumInjuredSuperstarsByTeamBySeason)
############################## (ENDE) Ergebnis Tabelle erzeugen


# Dummy Variablen erzeugen ####
# Liga
df_result <- df_result %>%
  mutate(bundesliga = ifelse(liga == "1-bundesliga", 1, 0)) %>%
  mutate(premier = ifelse(liga == "premier-league", 1, 0)) %>%
  mutate(primera = ifelse(liga == "primera-division", 1, 0)) %>%
  mutate(seriea = ifelse(liga == "serie-a", 1, 0)) %>%
  mutate(ligue1 = ifelse(liga == "ligue-1", 1, 0))

mutate(superstar = ifelse(marktwert >= 40, 1, 0))