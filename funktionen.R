# Durchschnittliche Anzahl an Verletzungstagen je Verein
GetMeanInjuryDaysByTeam <- function(team_id, df) {
  df_temp <- subset(df, df$verein_id == team_id)
  return(mean(df_temp$tage_berechnet, na.rm = TRUE))
}

# Durchschnittliche Anzahl an Verletzungstagen je Verein je Saison
GetMeanInjuryDaysByTeamBySeason <- function(team_id, saison_input, df) {
  df_injury <- subset(df, df$verein_id == team_id & df$saison == saison_input)
  return(mean(df_injury$tage_berechnet, na.rm = TRUE))
}

# Median Verletzungstagen je Verein
GetMedianByTeam <- function(team_id, df) {
  df_temp <- subset(df, df$verein_id == team_id)
  return(median(df_temp$tage_berechnet, na.rm = TRUE))
}

# Alter in Jahren 
age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}