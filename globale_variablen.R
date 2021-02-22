# Vektor mit allen Saisons
saisons <- c("2010", "2011", "2012", "2013", "2014",
             "2015", "2016", "2017", "2018", "2019")

# Verktor mit allen Spielern (ohne Duplikate)
alleSpieler <- df_kaderRaw$spieler_id [!duplicated(df_kaderRaw$spieler_id)]