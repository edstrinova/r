# Initialisierung ####
# Environment leeren
rm(list=ls())

# Working Directory setzen
setwd("/Users/eds/Dropbox/FernUni Hagen/Semester 17 (WS 2020 2021)/Master/Analyse/R")

# Daten einlesen
source ("bibliotheken.R")
# Daten einlesen
source ("daten_einlesen.R")
# Datentypen setzen
source ("datentypen.R")
# Daten aufbereiten
source ("daten_aufbereiten.R")
# Globale Variablen 
source ("globale_variablen.R")
# Funktionen
source ("funktionen.R")
############################## (ENDE) Initialisierung


# Ergebnis Tabelle erstellen (df_result) ####
#source ("ergebnis_tabelle_erstellen_kat.R")
source ("ergebnis_tabelle_erstellen.R")
############################## (ENDE) Ergebnis Tabelle erstellen (df_result)


# Nachträgliche Bereinigungen ####
# Kadergröße in den Saisons 2013 und 2018 in Italien entfernen (Thesis)
df_result  <- df_result %>%
  mutate(kader_groesse = ifelse(
    (liga == "serie-a" & (saison == 2013 | saison == 2018)),
    NA,
    kader_groesse))
############################## (ENDE) Ergebnis Tabelle erstellen


# Eingrenzungen durchführen (df_result_Model) ####
# (0) Keine Eingrenzung
df_resultModel <- df_result
df_verletzungenResultModel <- df_verletzungenResult

# (1) Filter setzen
liga_filter <- "1-bundesliga"
liga_filter <- "primera-division"
liga_filter <- "ligue-1"
liga_filter <- "premier-league"
liga_filter <- "serie-a"

# (2a) Eingrenzung
df_resultModel <- df_result %>%
  filter(liga == liga_filter)
df_verletzungenResultModel <- df_verletzungenResult %>%
  filter(liga == liga_filter)

# (2b) Eingrenzung auf nur Liga 1 aus allen Ländern
df_resultModel <- df_result %>%
  filter(liga == "1-bundesliga"
         | liga == "serie-a"
         | liga == "premier-league"
         | liga == "ligue-1"
         | liga == "primera-division")
df_verletzungenResultModel <- df_verletzungenResult %>%
  filter(liga == "1-bundesliga"
         | liga == "serie-a"
         | liga == "premier-league"
         | liga == "ligue-1"
         | liga == "primera-division")
############################## (ENDE) Eingrenzungen durchführen


# Daten kennenlernen ####
# Verteilung durchschnittliche_verletzungstage (mit log näherungsweise NV)
hist(df_resultModel$durchschnittliche_verletzungstage)
hist(log(df_resultModel$durchschnittliche_verletzungstage))
boxplot(df_resultModel$durchschnittliche_verletzungstage)
boxplot(log(df_resultModel$durchschnittliche_verletzungstage))

# Verteilung spiele_gesamt (Näherungsweise NV)
hist(df_resultModel$spiele_gesamt)

# Verteilung verhaeltnis_spiele_kader
hist(df_resultModel$verhaeltnis_spiele_kader)

# Verteilung anzahl_superstars_im_kader
hist(df_resultModel$anzahl_superstars_im_kader)
hist(log(df_resultModel$anzahl_superstars_im_kader))

# Verteilung der Verletzungstage
plot(df_verletzungenResultModel$tage_berechnet)
hist(df_verletzungenResultModel$tage)
hist(log(df_verletzungenResultModel$tage))

# Verteilung kader_groesse mit und ohne log
hist(df_resultModel$kader_groesse)
hist(log(df_resultModel$kader_groesse))

# Verteilung tage_berechnet mit und ohne log
hist(df_verletzungenResult$tage_berechnet)
hist(log(df_verletzungenResult$tage_berechnet))

# Verteilung verpasste_spiele mit und ohne log
hist(df_verletzungenResult$verpasste_spiele)
hist(log(df_verletzungenResult$verpasste_spiele))

# Verteilung punkte (annähernd normalverteilt)
hist(df_resultModel$punkte)
hist(log(df_resultModel$punkte))

# Verteilung platz
hist(df_resultModel$platz)

# Zusammenhänge mit der abhängigen Variablen
plot(df_resultModel$durchschnittliche_verletzungstage,
     df_resultModel$kader_groesse)
plot(log(df_resultModel$durchschnittliche_verletzungstage),
     df_resultModel$kader_groesse)
plot(df_resultModel$durchschnittliche_verletzungstage,
     log(df_resultModel$kader_groesse))
plot(log(df_resultModel$durchschnittliche_verletzungstage),
     log(df_resultModel$kader_groesse))
plot(df_resultModel$durchschnittliche_verletzungstage,
     df_resultModel$spiele_gesamt)
plot(df_resultModel$durchschnittliche_verletzungstage,
     df_resultModel$spiele_international)
plot(log(df_resultModel$durchschnittliche_verletzungstage),
     df_resultModel$spiele_international)
plot(df_resultModel$durchschnittliche_verletzungstage,
     df_resultModel$international_aktiv)
plot(df_resultModel$durchschnittliche_verletzungstage,
     df_resultModel$durchschnittsalter_verletzte)
plot(log(df_resultModel$durchschnittliche_verletzungstage),
     df_resultModel$durchschnittsalter_verletzte)
plot(df_resultModel$durchschnittliche_verletzungstage,
     df_resultModel$durchschnittliche_verpasste_spiele)
plot(log(df_resultModel$durchschnittliche_verletzungstage),
     df_resultModel$durchschnittliche_verpasste_spiele)
plot(log(df_resultModel$durchschnittliche_verletzungstage),
     log(df_resultModel$durchschnittliche_verpasste_spiele))
plot(df_resultModel$durchschnittliche_verletzungstage,
     df_resultModel$anzahl_verletzungen)
plot(log(df_resultModel$durchschnittliche_verletzungstage),
     df_resultModel$anzahl_verletzungen)
plot(df_resultModel$durchschnittliche_verletzungstage,
     df_resultModel$international_aktiv)
plot(df_resultModel$durchschnittliche_verletzungstage,
     df_resultModel$marktwert)
plot(log(df_resultModel$durchschnittliche_verletzungstage),
     df_resultModel$verhaeltnis_spiele_kader)
plot(df_resultModel$durchschnittliche_verletzungstage,
     df_resultModel$durchschnittliche_verpasste_spiele)

# Zusammenhänge zwischen den unabhängigen Variablen
plot(df_resultModel$spiele_gesamt,
     df_resultModel$kader_groesse)
############################## (ENDE) Daten kennenlernen


# Plots ####
# Übersicht der Verteilungen
plot(df_result)

# Kreisdieagramm mit der Verteilung der Verletzungen je Liga
prozent <- round((table(df_verletzungenResultModel$liga)
                  /sum(table(df_verletzungenResultModel$liga)))
                 * 100)
beschriftung <- c("1. Bundesliga - ",
                  "Ligue 1 - ",
                  "Premiere League - ",
                  "Primera Division - ",
                  "Serie A - "
                  )
beschriftung <- paste(beschriftung, prozent, "%", sep = " ")
pie(table(df_verletzungenResultModel$liga),
    labels = beschriftung,
    main = "Anteil der beobachteten Verletzungen je Liga")
############################## (ENDE) Plots


# Analyse: Verteilung der Verletzungen ####
# Anzahl Verletzungen je Verletzungsart
df_verletzungenResultAnzahl <- df_verletzungenResult %>%
  group_by(verletzung) %>%
  count(verletzung) %>%
  ungroup() %>%
  rename(anzahl_verletzungen = n)


hist(df_test$tage_berechnet)
plot(df_test$tage_berechnet)
boxplot(df_test$tage_berechnet)

summary(df_test$tage_berechnet)
hist(df_test$tage_berechnet, xlim = c(0, 10), breaks = 3)
plot(df_test$tage_berechnet)

# Boxplot Horizontal mit Beschriftung
boxplot(df_test$tage_berechnet,
        outline = FALSE,
        staplewex = 1,
        horizontal = TRUE,
        main = "Kreuzbandriss",
        xlab = "Verletzungstage")
text(x = quantile(df_test$tage_berechnet),
     labels = quantile(df_test$tage_berechnet), y = 1.25)

# Eingrenzung auf nur Liga 1 aus allen Ländern
df_verletzungenResult <- df_verletzungenResult  %>%
  filter(liga == "1-bundesliga"
         | liga == "serie-a"
         | liga == "premier-league"
         | liga == "ligue-1"
         | liga == "primera-division")

# Verteilung bei "unbekannte Verletzung"
df_test <- subset(df_verletzungenResult,
                  df_verletzungenResult$verletzung == "unbekannte Verletzung")

# Verteilung bei "muskuläre Probleme"
df_test <- subset(df_verletzungenResult,
                  df_verletzungenResult$verletzung == "muskuläre Probleme")

# Verteilung bei "Muskelfaserriss"
df_test <- subset(df_verletzungenResult,
                  df_verletzungenResult$verletzung == "Muskelfaserriss")

# Verteilung bei "Oberschenkelverletzung"
df_test <- subset(df_verletzungenResult,
                  df_verletzungenResult$verletzung == "Oberschenkelverletzung")

# Verteilung bei "Muskelverletzung"
df_test <- subset(df_verletzungenResult,
                  df_verletzungenResult$verletzung == "Muskelverletzung")

# Verteilung bei "Knieverletzung"
df_test <- subset(df_verletzungenResult,
                  df_verletzungenResult$verletzung == "Knieverletzung")

# Verteilung bei "Knieprobleme"
df_test <- subset(df_verletzungenResult,
                  df_verletzungenResult$verletzung == "Knieprobleme")

# Verteilung bei "Oberschenkelprobleme"
df_test <- subset(df_verletzungenResult,
                  df_verletzungenResult$verletzung == "Oberschenkelprobleme")

# Verteilung bei "Knöchelverletzung"
df_test <- subset(df_verletzungenResult,
                  df_verletzungenResult$verletzung == "Knöchelverletzung")

# Verteilung bei "Kreuzbandriss"
df_test <- subset(df_verletzungenResult,
                  df_verletzungenResult$verletzung == "Kreuzbandriss")
############################## (ENDE) Verteilung der Verletzungen


# Analyse: Verteilung der Marktwerte ####

plot(df_kaderRaw)
hist(df_kaderRaw$marktwert)
plot(df_kaderRaw$marktwert)
boxplot(df_kaderRaw$marktwert)
mean(df_kaderRaw$marktwert, na.rm = TRUE)
median(df_kaderRaw$marktwert, na.rm = TRUE)

df_kaderMarktwert <- df_kaderResult %>%
  group_by(verletzung) %>%
  count(verletzung) %>%
  ungroup() %>%
  rename(anzahl_verletzungen = n)
############################## (ENDE) Verteilung der Marktwerte


# Analyse: Ausreißer bei der Kadergröße ####
# Durchschnittliche Kadergröße je Liga je Saison
df_kadergroessePrüfung <- df_resultModel %>%
  group_by(liga, saison) %>%
#  filter(liga == "ligue-1") %>%
  filter(liga == "serie-a") %>%
  summarise_at(vars(kader_groesse), funs(mean(., na.rm=TRUE)))

# Boxplot
boxplot(df_kadergroessePrüfung$kader_groesse,
 #       outline = FALSE,
        staplewex = 1,
        horizontal = FALSE,
        main = "Boxplot Kadergröße Serie A",
        xlab = "Kadergröße")
text(x = quantile(df_kadergroessePrüfung$kader_groesse),
     labels = quantile(df_kadergroessePrüfung$kader_groesse), y = 1.25)

# Barplot 
barplot(df_kadergroessePrüfung$kader_groesse,
        df_kadergroessePrüfung$saison,
        main = "Barplot Kadergröße Serie A
        ",
        xlab = "Saison",
        ylab = "Kadergröße")

# Plot
plot(df_kadergroessePrüfung$saison,
     df_kadergroessePrüfung$kader_groesse,
     main = "Kadergröße Serie-A",
     xlab = "Saison",
     ylab = "Kadergröße")

hist(df_kadergroessePrüfung$saison)
mean(df_kadergroessePrüfung$kader_groesse)
median(df_kadergroessePrüfung$kader_groesse)
sd(df_kadergroessePrüfung$kader_groesse)
############################## (ENDE) Ausreißer bei der Kadergröße


# Analyse: Verletzungstage pro Liga ####
# Boxplot Horizontal mit Beschriftung
boxplot(df_verletzungenResultModel$tage_berechnet,
        outline = FALSE,
        staplewex = 1,
        horizontal = TRUE,
        main = "ligue-1",
        xlab = "Verletzungstage")
text(x = quantile(df_verletzungenResultModel$tage_berechnet),
     labels = quantile(df_verletzungenResultModel$tage_berechnet), y = 1.25)

# Durchschnitt
mean(df_verletzungenResultModel$tage_berechnet)

# Verteilung Verletzungsdauer mit und ohne log
hist(df_verletzungenResultModel$tage_berechnet)
hist(log(df_verletzungenResultModel$tage_berechnet))
############################## (ENDE) Verletzungstage pro Liga 


# Analyse: POLS Vs. FE ####
# F test
pFtest(fixed, ols)
# siehe auch https://rstudio-pubs-static.s3.amazonaws.com/
# 372492_3e05f38dd3f248e89cdedd317d603b9a.html#451_time-fixed_effects_testing
############################## (ENDE) POLS Vs. FE


# Analyse: FE Vs. Time-FE ####
# FE 1
timefixed <- plm(durchschnittliche_verletzungstage
             ~ kader_groesse
             + anzahl_verletzter_spieler
             + durchschnittsalter_verletzte
             + international_aktiv
             + factor(saison),
             data = df_resultModel,
             index = c("verein_id", "saison"),
             model = "within")
summary(timefixed)

# FE 1
timefixed <- plm(durchschnittliche_verletzungstage
                 ~ anzahl_verletzter_spieler
                 + factor(saison),
                 data = df_resultModel,
                 index = c("verein_id", "saison"),
                 model = "within")
summary(timefixed)

# F test
pFtest(timefixed, fixed)

plmtest(timefixed, "time", type="bp")


############################## (ENDE) FE Vs. Time-FE


# Analyse: Weitere Tests ####
# Breusch-Pagan LM
pcdtest(fixed, test = c("lm"))

# Pasaran CD
pcdtest(fixed, test = c("cd"))

# Heteroskedasticity testing Breusch-Pagan
bptest(ols,
       data = df_resultModel,
       studentize=F)

# Heteroskedasticity consistent coefficients
coeftest(fixed, vcovHC)

# Heteroskedasticity consistent coefficients
coeftest(ols, vcovHC)

# POLS 1
pols <- plm(durchschnittliche_verletzungstage
             ~ kader_groesse
             + anzahl_verletzter_spieler
             + durchschnittsalter_verletzte
             + international_aktiv,
             data = df_resultModel,
             index = c("verein_id"),
             model = "pooling")
summary(pols)

pooltest(pols, fixed)

pwtest(pols)
############################## (ENDE) Weitere Tests


# Analyse: Durbin Watson Test ####
fixed <- plm(durchschnittliche_verletzungstage
             ~ kader_groesse
             + anzahl_verletzter_spieler
             + durchschnittsalter_verletzte
             + international_aktiv,
             data = df_resultModel_random,
             index = c("verein_id"),
             model = "within")
summary(fixed)

pdwtest(fixed)
pbgtest(fixed)
pwartest(fixed)

set.seed(1234)
df_resultModel_random <- df_resultModel[order(runif(nrow(df_resultModel))), ]
# OLS 1
ols <- lm(durchschnittliche_verletzungstage
          ~ kader_groesse
          + anzahl_verletzter_spieler
          + durchschnittsalter_verletzte
          + international_aktiv,
          data = df_resultModel_random)
summary(ols)
plot(ols)
durbinWatsonTest(ols)
############################## (ENDE) Durbin Watson Test


# Regression ####
## Aktuell beste Modellkombinationen (Basis: ALLE DATEN / ERSTE LIGA)
# FE 1
fixed <- plm(durchschnittliche_verletzungstage
             ~ kader_groesse
             + anzahl_verletzter_spieler
             + durchschnittsalter_verletzte
             + international_aktiv,
             data = df_resultModel,
             index = c("verein_id"),
             model = "within")
summary(fixed)
plot(fixed)

# FE 2
fixed <- plm(durchschnittliche_verletzungstage
             ~ anzahl_verletzter_spieler
             + durchschnittsalter_verletzte
             + punkte,
             data = df_resultModel,
             index = c("verein_id"),
             model = "within")
summary(fixed)

# FE 3 (Nur DE Bundesliga)
fixed <- plm(durchschnittliche_verletzungstage
             ~ kader_groesse
             + anzahl_verletzter_spieler,
             data = df_resultModel,
             index = c("verein_id"),
             model = "within")
summary(fixed)

# OLS 1
ols <- lm(durchschnittliche_verletzungstage
          ~ kader_groesse
          + anzahl_verletzter_spieler
          + durchschnittsalter_verletzte
          + international_aktiv,
          data = df_resultModel)
summary(ols)
plot(ols)

# OLS 2
ols <- lm(durchschnittliche_verletzungstage
          ~ anzahl_verletzter_spieler
          + durchschnittsalter_verletzte
          + punkte,
          data = df_resultModel)
summary(ols)

# OLS 3 (Nur DE Bundesliga)
ols <- lm(durchschnittliche_verletzungstage
          ~ kader_groesse
          + anzahl_verletzter_spieler,
          data = df_resultModel)
summary(ols)

# Random
random <- plm(durchschnittliche_verletzungstage
             ~ kader_groesse
             + anzahl_verletzter_spieler
             + durchschnittsalter_verletzte
             + international_aktiv,
             data = df_resultModel,
             index = c("verein_id"),
             model = "random")
summary(random)

# Prüfung auf Heteroskedastizität
plot(fitted.values(ols), residuals(ols))
plot(fitted.values(ols), rstandard(ols))
plot(ols, 1)

coeftest(ols, vcov=vcovHC(ols, type=c("HC3")))

# Hausmann test
phtest(fixed, random)
# link: https://rpubs.com/yl3413/210208
############################## (ENDE) Regression


# Subset Selection ####
# Nur bestimmte Regressoren zur Verfügung stellen
df_bestSubsetModel <- df_resultModel %>%
  select(-durchschnittliche_verpasste_spiele,
         -verhaeltnis_spiele_kader,
         -anzahl_verletzungstage,
         -liga,
         -anzahl_verletzungen,
         -premier,
         -primera,
         -seriea,
         -ligue1,
         -saison,
         -verein_id)
  
bestSubset <- regsubsets(durchschnittliche_verletzungstage ~ .,
                         data = df_bestSubsetModel,
                         #nbest = 4,
                         nvmax = 8,
                         method = "exhaustive")
summary(bestSubset)

res.sum <- summary(bestSubset)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min((res.sum$bic))
)

## statistics (rsq, cp, adjr2, bic, rss, ylim = c(0, 20))
# Alle ersten Ligen
subsets(bestSubset, statistic = "rsq", ylim = c(0.5, 0.55))
subsets(bestSubset, statistic = "rsq", ylim = c(0.82, 0.85))
subsets(bestSubset, statistic = "adjr2", ylim = c(0.5265895, 0.5265897))

subsets(bestSubset, statistic = "rsq")
subsets(bestSubset, statistic = "adjr2")


## OLS best_subset 
ols <- lm(durchschnittliche_verletzungstage
          ~ kader_groesse
          + anzahl_verletzter_spieler
          + abstiegskampf
          + punkte,
          data = df_resultModel)
summary(ols)
plot(ols)


## OLS best_subset mit 8 Variablen
ols <- lm(durchschnittliche_verletzungstage
          ~ kader_groesse
          + spiele_international
          + marktwert
          + spiele_gesamt
          + anzahl_superstars_im_kader
          + anzahl_verletzter_spieler
          + abstiegskampf
          + punkte,
          data = df_resultModel)
summary(ols)
plot(ols)


## FE best_subset 
fixed <- plm(durchschnittliche_verletzungstage
             ~ kader_groesse
             + anzahl_verletzter_spieler
             + abstiegskampf
             + punkte,
             data = df_resultModel,
             index = c("verein_id"),
             model = "within")
summary(fixed)
############################## (ENDE) Subset Selection


# Deskriptive Statistik ####
# Anzahl unterschiedliche Vereine
df_test1 <- df_resultModel %>%
  group_by(verein_id) %>%
  count(verein_id)

# Durchschnittliche Verletzungstage von Hoffenheim und Bayern München
GetMeanInjuryDaysByTeam(533, df_verletzungenResult)
GetMeanInjuryDaysByTeam(27, df_verletzungenResult)
GetMeanInjuryDaysByTeam(72, df_verletzungenResult)
GetMeanInjuryDaysByTeam(28, df_verletzungenResult)

# Durchschnittliche Verletzungstage je Verein
df_meanInjuryDaysByTeam <- df_verletzungenResultModel %>%
  group_by(verein_id) %>%
  summarise_at(vars(tage_berechnet), funs(mean(., na.rm=TRUE)))
hist(as.numeric(df_meanInjuryDaysByTeam$tage_berechnet))

# Anzahl an Beobachtungen von Hoffenheim
nrow(subset(df_verletzungenResult, df_verletzungenResult$verein_id == 533))

# Anzah Beobachtungen je Verein
df_test1 <- df_verletzungenResult %>%
  count(verein_id)
df_test2 <- left_join(df_test1, df_meanInjuryDaysByTeam, by="verein_id")

# Anzahl Beobachtungen ja Verein je Saison
df_test1 <- df_verletzungenResultModel %>%
  group_by(saison) %>%
  count(verein_id)

# Anzahl Beobachtungen je Saison
df_test1 <- df_verletzungenResultModel %>%
  group_by(saison) %>%
  count(saison)
plot(df_test1, type="o", xaxt="n")
axis(side=1, at=x, labels=x)
hist(df_test1$n)
x <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

# Durchschnittliche Anzahl an Verletzungstagen je Liga
GetMeanInjuryDaysByLeague <- function(league) {
  df_temp <- subset(df_verletzungenResult,
                    df_verletzungenResult$liga == league)
  return(mean(df_temp$tage_berechnet, na.rm = TRUE))
}
GetMeanInjuryDaysByLeague("1-bundesliga")
GetMeanInjuryDaysByLeague("2-bundesliga")
GetMeanInjuryDaysByLeague("3-liga")
GetMeanInjuryDaysByLeague("primera-division")
GetMeanInjuryDaysByLeague("premier-league")
GetMeanInjuryDaysByLeague("ligue-1")
GetMeanInjuryDaysByLeague("serie-a")

# Median Verletzungstage von Hoffenheim und Bayern München
GetMedianByTeam(533, df_verletzungenResult)
GetMedianByTeam(27, df_verletzungenResult)

# Durchschnittliche Anzahl an verpassten Spielen je Verein
GetMeanMissendGamesByTeam <- function(team_id) {
  df_temp <- subset(df_verletzungenResult, df_verletzungenResult$verein_id == team_id)
  return(mean(df_temp$verpasste_spiele, na.rm = TRUE))
}
GetMeanMissendGamesByTeam(533)
GetMeanMissendGamesByTeam(16)
GetMeanMissendGamesByTeam(27)
############################## (ENDE) Deskriptive Statistik


# Ausprobieren ####
# FE 1
fixed <- plm(durchschnittliche_verletzungstage
             ~ kader_groesse,
             data = df_resultModel,
             index = c("verein_id"),
             model = "within")
summary(fixed)


qqnorm(residuals(fixed), ylab = 'Residuals')
qqline(residuals(fixed))

hist(residuals(ols), xlab = 'Residuals')

durbinWatsonTest(ols)
pdwtest(fixed)
pbgtest(fixed)
pwartest(fixed)

library(car)
vif(ols)

plot(preds, residuals(fixed))

df_result <- df_result %>%
  filter(saison == 2018 | saison == 2019)

cor.test(df_resultModel$durchschnittliche_verletzungstage, df_resultModel$kader_groesse, method="pearson")

df_verletzungenFeatures <- select(df_verletzungenRaw, tage, verpasste_spiele)
df_verletzungenFeatures <- df_verletzungenFeatures %>%
  filter(!is.na(verpasste_spiele))

results <- kmeans(df_verletzungenFeatures, 3)

qqnorm(df_result$spiele_gesamt)
qqline(df_result$spiele_gesamt, col ='red')
qqnorm(log(df_result$durchschnittliche_verletzungstage))
qqline(log(df_result$durchschnittliche_verletzungstage), col ='red')
qqnorm(df_resultLeague1$spiele_gesamt)
qqline(df_resultLeague1$spiele_gesamt, col ='red')

# Verteilung bei "Bänderriss"
df_test <- subset(df_verletzungenResult,
                  df_verletzungenResult$verletzung == "Bänderriss")
hist(df_test$tage_berechnet, breaks = 30)


# Verteilung bei "Grippe"
df_test <- subset(df_verletzungenResult,
                  df_verletzungenResult$verletzung == "Grippe")
hist(df_test$tage_berechnet, breaks = 30)


hist(df_test$tage_berechnet[df_test$tage_berechnet <= 20],breaks = 20)





# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(df_test$tage_berechnet , horizontal=TRUE , ylim=c(0,180), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(df_test$tage_berechnet , breaks=10 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="value of the variable", xlim=c(0,180))

# Create the layout
nf <- layout( matrix(c(1,1,2,3), nrow=2, byrow=TRUE) )

# Fill with plots
hist(df_test$tage_berechnet , breaks=30 , border=F , col=rgb(0.1,0.8,0.3,0.5) , xlab="distribution of a" , main="")
boxplot(df_test$tage_berechnet , xlab="a" , col=rgb(0.8,0.8,0.3,0.5) , las=2)
boxplot(df_test$tage_berechnet , xlab="b" , col=rgb(0.4,0.2,0.3,0.5) , las=2)


# A really basic boxplot.
ggplot(df_test$tage_berechnet)


## FE
fixed <- plm(durchschnittliche_verletzungstage
             ~
               kader_groesse
             #              +spiele_gesamt
             +anzahl_verletzter_spieler
             #             + durchschnittsalter_verletzte
             #         + international_aktiv
             #            + anzahl_superstars_verletzt
             #             + anzahl_superstars_im_kader
             #            + punkte
             #             + platz
             #             + meisterkampf
             #           + abstiegskampf
             #             + international_ko_runde
             #           + verhaeltnis_spiele_kader
             #             + pokal_runde2_erreicht
             ,
             data = df_resultModel,
             index = c("verein_id"),
             model = "within")
summary(fixed)


## OLS
ols <- lm(durchschnittliche_verletzungstage
          #ols <- lm(log(durchschnittliche_verletzungstage)          
          ~ 
            #spiele_gesamt
            kader_groesse
          + international_aktiv
          #            + durchschnittsalter_verletzte
          #              + platz
          + punkte
          #+ meisterkampf
          + abstiegskampf
          #             + anzahl_superstars_verletzt
          #+ anzahl_superstars_im_kader
          + anzahl_verletzter_spieler
          # international_ko_runde
          # + verhaeltnis_spiele_kader
          #+ pokal_runde2_erreicht
          ,
          data = df_resultModel)
summary(ols)
############################## (ENDE) Ausprobieren