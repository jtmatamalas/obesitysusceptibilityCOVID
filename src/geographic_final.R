### ----------------------------------------------------------------------------
### Prepare geographic data and plot it
### ----------------------------------------------------------------------------

## Load Mass Data
mass_postal <- readRDS(
  "data/census/mass_postal.rds"
)

## Modify the town names to match the covid data
mass_postal@data$TOWN <- gsub(", TOWN OF", "", mass_postal@data$CITY_TOWN)
mass_postal@data$id <- rownames(mass_postal@data)

## Load the town data
layers <- st_layers(sprintf("%s/maps/towns/", data_folder))
towns <- sf::read_sf(
  sprintf(
    "%s/maps/towns/",
    data_folder),
  layer = layers$name[1])


town_covid_data <- fread(sprintf("%s/covid19_mass.csv", data_folder))
setnames(town_covid_data, "City/Town", "TOWN")
town_covid_data[, TOWN := toupper(TOWN)]
town_covid_data[, `:=`(positive_mass = as.numeric(`Total Case Counts`))]

## Limit to the data before January 30th 2021
town_covid_data <- town_covid_data[End_Date == "1/30/2021"]

## Get zip code town data
zip_town <- as.data.table(mass_postal@data)

## Get the area of the town
tochange <- zip_town[,
  .(AREA_SQMI = max(AREA_SQMI)),
  by = POSTCODE]

## Merge the data
tochange <- merge(
  tochange,
  zip_town,
  by = c("POSTCODE", "AREA_SQMI")
)


cohort_ext <- merge(
  cohort,
  tochange[,
    .(
      zip = POSTCODE,
      TOWN,
      COUNTY
    )
  ],
  by = "zip"
)

## Summarize the data by town
summary_town <- cohort_ext[
  ,
  .N,
  by = .(TOWN, covid)] %>%
  dcast(
    TOWN ~ covid,
    value.var = "N",
    fill = 0)

## Set names of the town summary
setnames(summary_town, c("TOWN", "positive", "exposed"))
town_covid_data <- dplyr::left_join(town_covid_data, summary_town, by = "TOWN")

## Merge the town data with the covid data
towns_df <- dplyr::left_join(towns, town_covid_data, by = "TOWN")

## Compute ratios
towns_df <- towns_df %>%
  mutate(ratio = positive / positive_mass) %>%
  mutate(ratio = ifelse(is.na(ratio), 0, ratio)) %>%
  mutate(positive = ifelse(is.na(positive), 0, positive)) %>%
  mutate(ratio = ifelse(ratio > 1, 1, ratio))
