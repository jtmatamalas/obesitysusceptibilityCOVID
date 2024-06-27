### ----------------------------------------------------------------------------
### Selecting variables from demographics and wellness tables
### ----------------------------------------------------------------------------

## Demographics columns
demographics_columns <- c(
  "PATIENT_NUM",
  "date",
  "BIRTH_DATE",
  "ZIP_CD",
  "RACE_CD",
  "EmploymentStatusDSC",
  "exposed",
  "test",
  "covid"
)

## Wellness columns
wellness_columns <- c(
  "PATIENT_NUM",
  "AgeNBR",
  "SexDSC",
  "LastBodyMassIndexDTS",
  "LastBodyMassIndexNBR",
  "HasTypeTwoDiabetesFLG",
  "HasHypertensionFLG"
)

### ----------------------------------------------------------------------------
### Data merge wellness and demographics tables
### ----------------------------------------------------------------------------

## Full cohort
cohort_full <- merge(
  demographics[, demographics_columns, with = F],
  wellness[, wellness_columns, with = F],
  by = "PATIENT_NUM"
  )

## Renaming
current_names <- colnames(cohort_full)
new_names <- c(
  "PATIENT_NUM",
  "date",
  "birth_date",
  "zip",
  "race",
  "employment",
  "exposed",
  "test",
  "covid",
  "age",
  "sex",
  "bmi_date",
  "bmi",
  "dm2",
  "hypertension"
)

setnames(cohort_full, current_names, new_names)

### ----------------------------------------------------------------------------
### Obesity Data
###
### Integrate obesity data from bmi_ext data
### ----------------------------------------------------------------------------

## Select patients with obesity data
cohort_substitude <- cohort_full[PATIENT_NUM %in% bmi_ext$PATIENT_NUM]

## Change names
setnames(
  cohort_substitude,
  c(
    "bmi",
    "bmi_date"
  ),
  c(
    "bmi_old",
    "bmi_date_old"
  )
)

## Merge with obesity data
cohort_substitude <- merge(
  cohort_substitude,
  bmi_ext,
  by = "PATIENT_NUM"
)

## Remove old bmi data
cohort_substitude[,
  `:=`(
    bmi_old = NULL,
    bmi_date_old = NULL
  )
]

## Set bmi date as date object
cohort_full[,
  bmi_date := ymd_hms(bmi_date)
]

## Merge the datasets with the corrected bmi data
cohort_full <- rbind(
  cohort_full[
    !(
      PATIENT_NUM %in% cohort_substitude$PATIENT_NUM
    )
  ],
  cohort_substitude
)

## Make sure that the bmi data falls within the correct time frame
cohort_full <- cohort_full[
  bmi_date > date - days(180) &
  bmi_date < date + days(60)
]


### ----------------------------------------------------------------------------
### Pediatric BMI and obesity estimation
###
### Estimate pediatric obesity based on the CDC growth charts
### ----------------------------------------------------------------------------

## Age in months
cohort_full[,
  months := as.numeric(round(
    (
      as.Date(bmi_date) - as.Date(birth_date)
    ) / 30
  ) - 0.5)
]

## Select the pediatric cohort in months
cohort_pediatric <- cohort_full[
  months <= 240.5 &
  months >= 24
]

## Merge with the CDC growth charts
cohort_pediatric <- merge(
  cohort_pediatric,
  bmi_age[, .(sex, months, P95)],
  by = c("sex", "months")
)

## Estimate obesity based on the 95th percentile
cohort_pediatric[,
  obesity := ifelse(
    bmi >= P95,
    "Y",
    "N"
  )
]

## Remove the 95th percentile
cohort_pediatric[,
  P95 := NULL
]

## Select the adult cohort
cohort_adult <- cohort_full[
  months > 240.5
]

## Estimate obesity based on the 30 kg/m^2 threshold
cohort_adult[,
  obesity := ifelse(
    bmi >= 30,
    "Y",
    "N"
  )
]

## Merge both cohorts
cohort_full <- rbind(
  cohort_adult,
  cohort_pediatric
)

### ----------------------------------------------------------------------------
### Age Segmentation
###
### Segment the age into different groups
### ----------------------------------------------------------------------------

## Establish 10 year ranges
ranges <- seq(0, 90, by = 10)

## Set the column age_range
cohort_full$age_range <- "NOT_DEFINED"
cohort_full[
  age < ranges[length(ranges)],
  age_range := sapply(.SD$age, get_age_rank, ranges)]

## Remove age greater than 90
cohort_full[
  age >= ranges[length(ranges)],
  age_range := "90>"]

## Set the column age_group
cohort_full[,
  age_group := ifelse(
    age >= 3 & age < 13,
    "3-12",
    ifelse(
      age >= 13 & age < 20,
      "13-19",
      ifelse(
        age >= 20 & age < 40,
        "20-39",
        ifelse(
          age >= 40 & age < 65,
          "40-64",
          ifelse(
            age >= 65,
            ">64",
            NA
          )
        )
      )
    )
  )
]


### ----------------------------------------------------------------------------
### Establish race categories
### ----------------------------------------------------------------------------

cohort_full[,
  race := ifelse(
    grepl(".*hispanic.*", race, ignore.case = T),
    "HISPANIC",
    race
  )
]

cohort_full[,
  race := ifelse(
    race == "BLACK OR AFRICAN AMERICAN",
   "BLACK",
   race)
]

cohort_full[,
  race := ifelse(
    race %in% c(
      "WHITE",
      "ASIAN",
      "BLACK",
      "DECLINED",
      "HISPANIC"),
    race,
    "OTHER")
]

### ----------------------------------------------------------------------------
### Factorize the variables and deal with missing values
### ----------------------------------------------------------------------------

## From characters to factors. COHORT FINAL FORM
cohort_full[,
  `:=`(
    covid = as.factor(covid),
    test = as.factor(test),
    zip = as.factor(zip),
    age_range = as.factor(age_range),
    age_group = ordered(
      age_group,
      c(
        "3-12",
        "13-19",
        "20-39",
        "40-64",
        ">64"
      )),
    obesity = as.factor(obesity),
    sex = as.factor(sex),
    race = as.factor(race),
    dm2 = as.factor(dm2),
    hypertension = as.factor(hypertension)
  )
]

## Remove not assigned
cohort_not_na <- cohort_full[
  !(
    is.na(age_group) | is.null(age_group) |
    sex == "Unknown" |
    is.na(obesity) | is.null(obesity) |
    is.na(dm2) | is.null(dm2) |
    is.na(hypertension) | is.null(hypertension)
  )
]

## Remove NA values
cohort <- cohort_not_na[covid != negative_label]
cohort <- droplevels(cohort)

## Remove patients with covid after the 25th of January of 2021 to
## avoid vaccionation bias
cohort <- cohort[date < ymd("2021-01-25")]

## Difference between COVID and BMI measurements
cohort[,
  diff := abs(date - as.Date(bmi_date))
]

## Set the cohort to the patients with BMI measurements within 60 days
cohort <- cohort[diff <= 60]

# Table total
total_patients <- dim(cohort)[1]
