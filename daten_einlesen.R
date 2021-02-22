############################## (START) Daten einlesen
df_wettbewerbeRaw <- read_excel("../../Daten/10.xlsx", 
                          sheet = "Wettbewerbe")

df_verletzungenRaw <- read_excel("../../Daten/10.xlsx", 
                           sheet = "Verletzungen")

df_kaderRaw <- read_excel("../../Daten/10.xlsx",
                    sheet = "Kader")

df_tabelleRaw <- read_excel("../../Daten/10.xlsx",
                      sheet = "Tabelle")

df_spielerRaw <- read_excel("../../Daten/10.xlsx",
                            sheet = "Profil")

df_kategorienRaw <- read_excel("../../Daten/10.xlsx",
                            sheet = "Kategorien")
############################## (ENDE) Daten einlesen