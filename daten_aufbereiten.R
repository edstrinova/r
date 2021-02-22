# Dataframe clonen
df_verletzungenResult <- df_verletzungenRaw

# Verletzungen vor 2010 entfernen (Thesis)
df_verletzungenResult <- subset(df_verletzungenResult,
                                df_verletzungenResult$saison >= 2010)

# Verletzungen ab 2020 entfernen (Thesis)
df_verletzungenResult <- subset(df_verletzungenResult, 
                                df_verletzungenResult$saison < 2020)

# Beobachtungen ohne "von" Datum oder ohne "bis" Datum entfernen (Thesis)
df_verletzungenResult <- df_verletzungenResult %>%
  filter(!is.na(von)) %>%
  filter(!is.na(bis)) 

# Eigene Berechnung der Verletzungstage (Thesis)
df_verletzungenResult <- mutate(df_verletzungenResult,
                                tage_berechnet = 
                                as.numeric(abs
                                      (as.Date(df_verletzungenResult$bis)
                                      - as.Date(df_verletzungenResult$von)))
                                + 1)
