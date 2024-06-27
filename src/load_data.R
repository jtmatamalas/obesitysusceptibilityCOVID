### ------------------------------------------------------------------------------
### Load demographics
###
### This table contains the demographic information of the patients, and includes
### the most common demographic descriptors
### ------------------------------------------------------------------------------

## This table contains the demographic information of the patients
demographics <- fread("data/extra_tables/demographics.csv")

## Define PATIENT_NUM
demographics[, tail(which(colnames(demographics) == "PATIENT_NUM"), 1) := NULL]

## Define date as a date object
demographics[, date := ymd_hms(date)]

## Load extra demographic parameters
demographics_extra <- fread("data/extra_tables/demographics_extended.csv")
demographics <- merge(
  demographics,
  demographics_extra[,
    .(
      PATIENT_NUM = as.character(Patient_Num),
      EmploymentStatusDSC
      )
  ],
  by = "PATIENT_NUM"
)

### ------------------------------------------------------------------------------
### Load diagnosis table
###
### This table contains all the diagnosis of the patients in the database encoded
### in ICD10 codes.
###
### It also contains the diagnosis Z20.8 which corresponds to COVID-19. Not all
### patients have this diagnosis since it was introduced later in the pandemic.
### ------------------------------------------------------------------------------

diagnosis <- fread("data/diagnosis.csv")

## Define the date as a date object
diagnosis[, START_DATE := ymd_hms(START_DATE)]

## Get the exposure icds for patients after March 1st 2020 and icd10
## code Z20.8 (corresponding to COVID-19)
exposure_icd <- diagnosis[START_DATE >= ymd("2020-03-01")]
exposure_icd <- exposure_icd[
  CONCEPT_CD %like% "ICD10:Z20.8*"
]

### ------------------------------------------------------------------------------
### Load wellness table
###
### This table contains the wellness information of the patients in the database.
### It includes information about the patients' health status, such as asthma,
### allergies, and smoking habits, bmi, etc.
### ------------------------------------------------------------------------------

wellness <- fread("data/extra_tables/wellness.csv")

## Define PATIENT_NUM
wellness[, tail(which(colnames(wellness) == "PATIENT_NUM"), 1) := NULL]

## Define date as a date object
wellness[, date := ymd_hms(date)]

## Convert the PATIENT_NUM to a character
wellness[, PATIENT_NUM := as.character(PATIENT_NUM)]

### ------------------------------------------------------------------------------
### Load infection status
###
### This table contains the COVID-19 infection status of the patients
### This table is generated from the EDW and is the most reliable source of the
### status
### ------------------------------------------------------------------------------

infection <- fread("data/extra_tables/infection.csv")

### ------------------------------------------------------------------------------
### Load the exposed status
###
### This table contains the exposed status of the patients. We have two sources
### for this information. The first source is the EDW, and the second source is
### RPDR. Here we will merge the two sources to get the most complete information.
### ------------------------------------------------------------------------------

exposed <- fread("data/extra_tables/exposed_dia.csv")

## Define the date as a date object
exposed[, date := ymd_hms(START_DATE)]

## Filter the patients that have been exposed to COVID-19 after March 1st 2020
exposed <- exposed[date >= ymd("2020-03-01")]

## Load the extra exposed information from EDW
exposed_extra <- fread("data/extra_tables/exposed_from_edw.csv")

## Bind the two tables
exposed_expanded <- rbind(
  exposed[, .(PATIENT_NUM, date = as.Date(START_DATE), CONCEPT_CD)],
  exposed_extra[, .(PATIENT_NUM, CONCEPT_CD = ConceptCD, date = as.Date(date))]
)

### ------------------------------------------------------------------------------
### Integrate the data into a single table
### ------------------------------------------------------------------------------

## Make a union of the exposed patients (PATIENT_ID) using the tables
## exposed_expanded and infection
exposed_id <- union(
    unique(exposed_expanded$PATIENT_NUM),
    unique(infection[InfectionTypeDSC == "CoV-Exposed"]$PATIENT_NUM)
)

## Create a data.table with the exposed patients
exposed_dt <- data.table(PATIENT_NUM = exposed_id, exposed = T)

## Extract the COVID-19 status from the demographics table
covid_status <- demographics[,
  .(
    PATIENT_NUM,
    test = COVIDStatusCD,
    date = date
  )
]

## Merge the exposed_dt with the covid_status table
exposed_dt <- merge(exposed_dt, covid_status, by = "PATIENT_NUM", all.y = T)

## Create a TRUE/FALSE column for the exposed patients
exposed_dt[, exposed := ifelse(is.na(exposed), FALSE, TRUE)]

## Set the date as a date object
exposed_dt[, date := as.Date(date)]

## Create a covid, if the patient is exposed but the test is negative
## then the patient is considered exposed (COVID(-)), otherwise, the patient is
## considered to have the test result
exposed_dt[,
  covid := ifelse(
    exposed & test == "Negative",
    "Exposed",
    test
  )
]

## Since there are possible sources of reporting, we will will remove them based
## on PATIENT_NUM. Here we get the PATIENT_NUM that have more than one entry.
duplicates_id <- exposed_dt[, .N, by = PATIENT_NUM][N > 1]$PATIENT_NUM

## Remove patients with more than one entry in the tables demographics, wellness
## and exposed_dt
demographics <- demographics[!(PATIENT_NUM %in% duplicates_id)]
wellness <- wellness[!(PATIENT_NUM %in% duplicates_id)]
exposed_dt <- exposed_dt[!(PATIENT_NUM %in% duplicates_id)]

### ------------------------------------------------------------------------------
### Labeling the patients with the specific COVID-19 status
### ------------------------------------------------------------------------------

## Define the labels for the test and covid status
## We consider 3 labels: SARS-CoV-2 (+), SARS-CoV-2 (-), SARS-CoV-2 (-)*
## for the positive, negative and exposed patients, respectively
positive_label <- "SARS-CoV-2 (+)"
negative_label <- "SARS-CoV-2 (-)"
exposed_label <- "SARS-CoV-2 (-)*"

## Set the labels on the exposed_dt table
exposed_dt[,
  `:=`(
    test = ifelse(
      test == "Positive",
      positive_label,
      negative_label
    ),
    covid = ifelse(
      covid == "Positive",
      positive_label,
      ifelse(
        covid == "Negative",
        negative_label,
        exposed_label
      )
    )
  )
]

## Add the exposed status to the demographics table
demographics <- merge(
  demographics[, date := as.Date(date)],
  exposed_dt,
  by = c("PATIENT_NUM", "date")
)

## Add the exposed status to the wellness table
wellness <- merge(
  wellness[, date := as.Date(date)],
  exposed_dt,
  by = c("PATIENT_NUM", "date")
)

### ------------------------------------------------------------------------------
### Load the BMI information of the patients
### ------------------------------------------------------------------------------

bmi <- fread("data/extra_tables/bmi.csv")

## Convert the PATIENT_NUM to a character
bmi$PATIENT_NUM <- as.character(bmi$PATIENT_NUM)

## Add demographic information to the BMI table
bmi_ext <- merge(
  bmi,
  demographics[,
    .(
      PATIENT_NUM,
      date
    )
  ],
  by = "PATIENT_NUM"
)

## Ensure that the reading of the BMI is within 180 days before the date of the
## infection or 60 days after the date of the infection
bmi_ext <- bmi_ext[
  START_DATE > date - days(180) &
  START_DATE < date + days(60)]

## Compute the days between the date of the BMI and the date of the infection
bmi_ext[, diff := abs(date - as.Date(START_DATE))]

## Select specfic columns
bmi_ext <- bmi_ext[,
  .(
    PATIENT_NUM,
    bmi_date = ymd_hms(START_DATE),
    bmi = NVAL_NUM
  )
]

## Get the BMI information for pediatric patients
bmi_age <- fread("data/bmiagerev.csv")

setnames(
  bmi_age,
  c("Sex", "Agemos"),
  c("sex", "months")
)

bmi_age[,
  sex := ifelse(
    sex == 1,
    'Male',
    'Female'
  )
]
