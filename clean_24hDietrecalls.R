# Upload functions
source("clean_24hDietrecalls_functions.R")

#######################################################
#Upload DF
df_raw <- read.csv(
  "data-raw/tb24Hour(tb24Hour).csv",
  stringsAsFactors = FALSE
)

#Seperate out metadata
list2env(seperate_metadata(df_raw), envir = .GlobalEnv)

#Format DF Foods
individuals_dffood <- clean_dffood(df_food)
list2env(individuals_dffood, envir = .GlobalEnv)

#
