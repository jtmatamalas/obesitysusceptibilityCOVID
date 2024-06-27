### -----------------------------------------------------------------------------
### Load census data
### -----------------------------------------------------------------------------

## Demographics data
census_demo <- fread(
  "data/census/Demographics Clean.csv"
)

## Economics data
census_economics <- fread(
  "data/census/Economics Clean.csv"
)

## Housing data
census_housing <- fread(
  "data/census/Housing Clean.csv"
)

## Social data
census_social <- fread(
  "data/census/Social Clean.csv"
)

## Join census data
census <- inner_join(
  census_demo,
  census_economics,
  by = "ZCTA"
) %>%
  inner_join(
    census_social,
    by = "ZCTA"
  ) %>%
  inner_join(
    census_housing %>%
      select(-Rooms_Median_Rooms),
    by = "ZCTA"
  ) %>%
  as.data.table()

## Convert zip code as factor
census[, zip := as.factor(sprintf("%05d", ZCTA))]

### -----------------------------------------------------------------------------
### Extend and transform the data with Data from the cohort
### -----------------------------------------------------------------------------

## Merge data with extended demographics
c_extended <- merge(
  cohort,
  demographics_extra[,
    .(
      PATIENT_NUM = as.character(Patient_Num),
      TOWN = toupper(CityNM),
      country = CountryOfOriginDSC,
      state = StateDSC)],
  by = "PATIENT_NUM")

## Get the number of covid statuses by zip code
summary_geo_cohort <- c_extended[,
  .N,
  by = .(zip, covid)] %>%
  dcast(
    zip ~ covid,
    value.var = "N",
    fill = 0)

## Create a total exposed as the sum of the exposed and positive
summary_geo_cohort[, total_exposed := `SARS-CoV-2 (+)` + `SARS-CoV-2 (-)*`]

## Merge the census data with the summary_geo_cohort
summary_geo_cohort <- merge(
  summary_geo_cohort,
  census,
  by = "zip",
  all.y = T
)

## Calculate the exposure prevalence, prevalence and susceptibility
summary_geo_cohort[,
  `:=`(
    exposure_prevalence = total_exposed / Total_Population,
    prevalence = `SARS-CoV-2 (+)` / Total_Population,
    susceptibility = `SARS-CoV-2 (+)` / total_exposed
  )
]

## Remove the rows with invalid values and not used
summary_geo_cohort[
 total_exposed == 0 |
 total_exposed > Total_Population |
 total_exposed < 1 |
 Total_Population <= 100
 ,
  `:=`(
    exposure_prevalence = NA,
    prevalence = NA,
    total_exposed = NA,
    susceptibility = NA,
    `SARS-CoV-2 (+)` = NA,
    Income_Per_Capita = NA
  )
]

### -----------------------------------------------------------------------------
### Load the mass postal data
### -----------------------------------------------------------------------------

## Load the mass postal data
mass_postal <- readRDS(
  "data/census/mass_postal.rds"
)


mass_postal@data$TOWN <- gsub(", TOWN OF", "", mass_postal@data$CITY_TOWN)
mass_postal@data$id <- rownames(mass_postal@data)

## Merge the mass postal data with the summary_geo_cohort
mass_postal@data <- merge(
  dplyr::rename(summary_geo_cohort, POSTCODE = zip),
  mass_postal@data,
  by = "POSTCODE",
  all.y = T)

## Create a data frame with the mass postal data used to plot
mass_postal@data[is.na(mass_postal@data$count)]$count <- 0
mass_postal_df <- fortify(mass_postal)
mass_postal_df <- merge(mass_postal_df, mass_postal@data, by = "id") %>%
  as.data.table()

### -----------------------------------------------------------------------------
### Plotting the data by zip code
### -----------------------------------------------------------------------------

## Create a ggplot object with the mass postal data
map_plot <- ggplot(
  mass_postal_df,
  aes(
    x = lon,
    y = lat)
)

## Add the poligons and fill them with the total exposed
map_n <- map_plot +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = total_exposed
    ),
    size = 0.2,
    color = "black"
  ) +
  coord_fixed() +
  scale_fill_viridis_c(
    na.value = "white",
    labels = scales::label_number(big.mark = ","),
    name = "Number of individuals"
  ) +
  theme_void() +
  theme(
    legend.position = c(0.3, 0.4),
    legend.direction = "horizontal",
    legend.title = element_text(
      vjust = 0.85
    ),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  )

## Add the poligons and fill them with the exposure prevalence
map_exp <- map_plot +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = exposure_prevalence,
    ),
    size = 0.2,
    color = "black"
  ) +
  coord_fixed() +
  scale_fill_viridis_c(
    na.value = "white",
    labels = scales::label_percent(),
    name = "Prevalence SARS-CoV-2 *",
  ) +
  theme_void() +
  theme(
    legend.position = c(0.3, 0.4),
    legend.direction = "horizontal",
    legend.title = element_text(
      vjust = 0.85
    ),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  )

## Add the poligons and fill them with the prevalence
map_inf <- map_plot +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = prevalence
    ),
    size = 0.2,
    color = "black"
  ) +
  coord_fixed() +
  scale_fill_viridis_c(
    na.value = "white",
    labels = scales::label_percent(),
    name = "Prevalence SARS-CoV-2 (+)"
  ) +
  theme_void() +
  theme(
    legend.position = c(0.3, 0.4),
    legend.direction = "horizontal",
    legend.title = element_text(
      vjust = 0.85
    ),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  )

## Add the poligons and fill them with the susceptibility
map_sus <- map_plot +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = prevalence/exposure_prevalence
    ),
    size = 0.2,
    color = "black"
  ) +
  coord_fixed() +
  scale_fill_viridis_c(
    na.value = "white",
    labels = scales::label_percent(),
    name = "Susceptibility"
  ) +
  theme_void() +
  theme(
    legend.position = c(0.3, 0.4),
    legend.direction = "horizontal",
    legend.title = element_text(
      vjust = 0.85
    ),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  )


### -----------------------------------------------------------------------------
### Load the towns and counties data
### -----------------------------------------------------------------------------

## Load geographic data
layers <- st_layers(sprintf("%s/maps/towns/", data_folder))

## Extract the towns data
towns <- sf::read_sf(sprintf("%s/maps/towns/", data_folder), layer = layers$name[1])

## Read the massachusetts covid data
town_data <- fread(sprintf("%s/covid19_mass.csv", data_folder))

## Prepare the data to be used in the plots
setnames(town_data, "City/Town", "TOWN")
town_data[, TOWN := toupper(TOWN)]
town_data[, `:=`(positive_mass = as.numeric(`Total Case Counts`))]

## Filter the data to the last date (vaccine based)
town_data <- town_data[End_Date == "1/30/2021"]

## Extract the data from the RDS file
zip_town <- as.data.table(mass_postal@data)

## Summarize cases by town and dcast the data
summary_town <- c_extended[
  state == "Massachusetts",
  .N,
  by = .(TOWN, covid)] %>%
  dcast(
    TOWN ~ covid,
    value.var = "N",
    fill = 0)

## Rename the columns and merge summary_town with town_data
setnames(summary_town, c("TOWN", "positive", "exposed"))
town_data <- dplyr::left_join(town_data, summary_town, by = "TOWN")

## Add town data to the towns_df
towns_df <- dplyr::left_join(towns, town_data, by = "TOWN")

## Calculate the ratio of positive cases to positive mass
towns_df <- towns_df %>%
  mutate(ratio = positive / positive_mass) %>%
  mutate(ratio = ifelse(is.na(ratio), 0, ratio)) %>%
  mutate(positive = ifelse(is.na(positive), 0, positive)) %>%
  mutate(ratio = ifelse(ratio > 1, 1, ratio))

## Prepare the common elements of the plot
towns_plot <- ggplot(towns_df) +
  theme_void() +
  theme(
    legend.position = c(0.3, 0.4),
    legend.direction = "horizontal",
    legend.title = element_text(
      vjust = 0.85
    ),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  )

## Add the data for the prevalence of positive cases accourding to the MGB
positive_towns_plot <- towns_plot +
  geom_sf(aes(fill = positive_mass / POP2010), color = "black", size = 0.2) +
  scale_fill_viridis_c(
    na.value = "white",
    labels = scales::label_percent(),
    name = "Prevalence MGB",
    limits = c(0, 0.08)
  )

## Add the data for the prevalence of positive cases accourding to Mass
positive_mass_towns_plot <- towns_plot +
  geom_sf(
    aes(
      fill = round(positive_mass / POP2010, digits = 2)
    ),
    color = "black",
    size = 0.2
  ) +
  scale_fill_viridis_c(
    na.value = "white",
    labels = scales::label_percent(),
    name = "Prevalence Mass.",
    limits = c(0, 0.24)
  )

## Compute the ratio of positive cases between the MGB and Mass
rate_towns_plot <- towns_plot +
  geom_sf(aes(fill = ratio), color = "black", size = 0.2) +
  scale_fill_viridis_c(
    na.value = "white",
    labels = scales::label_percent(),
    name = "Ratio"
    ## limits = c(0, 2.)
  )

### -----------------------------------------------------------------------------
### Load the counties data
### -----------------------------------------------------------------------------

## Load geographic data
county_layers <- st_layers(sprintf("%s/maps/counties/", data_folder))

## Extract the county data
county_info <- sf::st_read(sprintf("%s/maps/counties/", data_folder), layer = county_layers$name[2])

## Create a county data frame containing both MASS and MGB data
county_df <- as.data.table(towns_df)[,.(
    ratio = sum(positive, na.rm = T) / sum(positive_mass, na.rm = T),
    positive = sum(positive, na.rm = T),
    positive_mass = sum(positive_mass, na.rm = T),
    population = sum(POP2010)),
    by = COUNTY
]

## Merge it with the county_info
county_df <- merge(county_df, county_info, by = "COUNTY")

## Prepare the common elements of the plot
county_plot <- ggplot(county_df) +
  theme_void() +
  theme(
    legend.position = c(0.3, 0.4),
    legend.direction = "horizontal",
    legend.title = element_text(
      vjust = 0.85
    ),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  )

## Add the data for the prevalence of positive cases accourding to the MGB
positive_county_plot <- county_plot +
  geom_sf(aes(fill = positive / population, geometry = geometry), color = "black", size = 0.2) +
  scale_fill_viridis_c(
    na.value = "white",
    labels = scales::label_percent(),
    name = "Prevalence MGB",
    limits = c(0, 0.08)
  )

## Add the data for the prevalence of positive cases accourding to Mass
positive_mass_county_plot <- county_plot +
  geom_sf(
    aes(
      fill = round(positive_mass / population, digits = 2),
      geometry = geometry
    ),
    color = "black",
    size = 0.2
  ) +
  scale_fill_viridis_c(
    na.value = "white",
    labels = scales::label_percent(),
    name = "Prevalence Mass.",
    limits = c(0, 0.12)
  )

## Compute the ratio of positive cases between the MGB and Mass
rate_county_plot <- county_plot +
  geom_sf(aes(fill = ratio, geometry = geometry), color = "black", size = 0.2) +
  scale_fill_viridis_c(
    na.value = "white",
    labels = scales::label_percent(),
    name = "Ratio"
    ## limits = c(0, 2.)
  )

## Create a grid with the plots
comparison <- plot_grid(
  positive_mass_county_plot,
  positive_mass_towns_plot,
  positive_county_plot,
  positive_towns_plot,
  rate_county_plot,
  rate_towns_plot,
  ncol = 2
)

## Save the plot
ggsave("output/mass_vs_mgb.png", width = 21, height = 21)
