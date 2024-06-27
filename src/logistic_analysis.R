### ----------------------------------------------------------------------------
### Logistic regression of the probability of having COVID-19
### ----------------------------------------------------------------------------

## Move dates to week number
cohort[, week := week(date)]

## Logistic regression using a GLM
mod1 <- glm(
  formula = covid ~
    obesity +
    age +
    sex +
    county +
    dm2 +
    as.factor(week) +
    hypertension
 ,
  family = binomial(link = "logit"),
  data = cohort
)

## Get the odds ratio range
odds <- exp(summary(mod1)$coefficients['obesityY', 1] +
  qnorm(c(0.025,0.5,0.975)) * summary(mod1)$coefficients["obesityY", 2])

## Generate a table with the odds ratio
general_odds <- data.table(
  population = "total",
  group = "total",
  lower = odds[1],
  odds = odds[2],
  upper = odds[3],
  p_value = summary(mod1)$coefficients['obesityY', 4]
)

### ----------------------------------------------------------------------------
### Odds ratio by age group
### ----------------------------------------------------------------------------

## A function that given a dataframe compute the odds ratio
odds_age <- function(df) {
    mod1 <- glm(
    formula = covid ~
        obesity +
        sex +
        county +
        dm2 +
        as.factor(week) +
        hypertension
    ,
    family = binomial(link = "logit"),
    data = df
    )

    odds <- exp(summary(mod1)$coefficients['obesityY', 1] +
      qnorm(c(0.025,0.5,0.975)) * summary(mod1)$coefficients["obesityY", 2])

    p_value <- summary(mod1)$coefficients['obesityY', 4]

    return(list(odds = odds[2], lower = odds[1], upper = odds[3], p_value = p_value))
}

## Apply the function to each age group
age_odds <- dlply(cohort, .(age_group), .fun = odds_age)

## Bind the results
age_odds <- rbindlist(age_odds, idcol = "age_group")
setnames(age_odds, "age_group", "group")

## Bind the results to the general odds
age_odds$population <- "age"
odds_df <- rbind(general_odds, age_odds)


### ----------------------------------------------------------------------------
### Odds ratio per week
### ----------------------------------------------------------------------------

### Function that given a dataframe compute the odds ratio
odds_week <- function(df) {
    mod1 <- glm(
    formula = covid ~
        obesity +
        age +
        sex +
        county +
        dm2 +
        hypertension
    ,
    family = binomial(link = "logit"),
    data = df
    )

    odds <- exp(summary(mod1)$coefficients['obesityY', 1] +
      qnorm(c(0.025,0.5,0.975)) * summary(mod1)$coefficients["obesityY", 2])

    p_value <- summary(mod1)$coefficients['obesityY', 4]

    return(list(odds = odds[2], lower = odds[1], upper = odds[3], p_value = p_value))
}

## Apply the function to each week
weekly_odds <- dlply(cohort[week != 9], .(week), .fun = odds_week)
weekly_odds <- rbindlist(weekly_odds, idcol = "week")

## Add the adjusted p-value
weekly_odds$padjust <- p.adjust(weekly_odds$p_value)
