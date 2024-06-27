### ----------------------------------------------------------------------------
### MAKE A COMPARISON BETWEEN MGB AND MASSACHUSETTS GOVERNMENT REPORTED CASES
### ----------------------------------------------------------------------------

## county info
mass_postal <- readRDS(
  "data/census/mass_postal.rds"
)

## get zip code and county
zip_town <- as.data.table(mass_postal@data)

## Add zip code and country to the cohort
cohort <- merge(cohort, zip_town[, .(zip = POSTCODE, county = COUNTY)], by = "zip")

## Load massachusetts covid data
mass_covid <- fread("data/census/demographics/mass_cov_evo.csv")

## Set date as date object
mass_covid[, date := mdy(date)]

## Get the incidence of SARS-CoV-2 (+) cases in our cohort
incidence <- cohort[covid == "SARS-CoV-2 (+)",
  .(tests = .N),
  by = .(date, covid)][order(date)]

## Merge with massachusetts data
incidence <- merge(
  incidence,
  mass_covid[,
    .(
      date,
      mass_tests = total
    )
  ],
  by = "date")

## Calculate the 7-day rolling average
incidence <- incidence[,
  .(date,
    MGB = tests,
    MASS = mass_tests,
    avg7 = frollmean(tests, 7, fill = NA, align = "center"),
    avg7_mass = frollmean(mass_tests, 7, fill = NA, align = "center")),
  by = covid]

## Unfold the data
incidence_mgb_unfold <- data.table(
  date = rep(incidence$date, incidence$MGB),
  source = "MGB"
)
incidence_mass_unfold <- data.table(
  date = rep(incidence$date, incidence$MASS),
  source = "MASS"
)

## Merge unfolded data
incidence_unfold <- rbind(
  incidence_mgb_unfold,
  incidence_mass_unfold
)
