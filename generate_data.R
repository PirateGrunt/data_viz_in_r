library(methods)
library(imaginator)
library(dplyr)

set.seed(1234)

InflatedPremium <- function(EffectiveDate, Mean, CV, inflation = 0){
  time_lag <- as.double(difftime(EffectiveDate, as.Date('2002-01-01'), units='days') / 365.25)
  Mean <- Mean * (1 + inflation) ^ time_lag
  inflated_premium <- sapply(Mean, function(x) rgamma(1, 1 / CV^2, 1 / (x * CV^2)))
  inflated_premium
}

ApplyRateChange <- function(premium, effective_date, change, rate_change_date){
  which_premium <- effective_date >= rate_change_date
  premium[which_premium] <- premium[which_premium] * (1 + change)
  premium
}

dfGL_NY  <- SimulatePolicies(
    N = 1000, PolicyYears = 2001:2010, Retention = 0.75, Growth = .01
    , AdditionalColumns = list(Line = "GL", State = "NY")) %>%
  mutate(Premium = InflatedPremium(PolicyEffectiveDate, Mean = 30e3, CV = 0.1)
         , Premium = ApplyRateChange(Premium, PolicyEffectiveDate, .10, as.Date('2006-04-01')))

dfGL_CA <- SimulatePolicies(
  N = 1000, PolicyYears = 2001:2010, Retention = 0.9, Growth = .2
  , AdditionalColumns = list(Line = "GL", State = "CA")) %>%
  mutate(Premium = InflatedPremium(PolicyEffectiveDate, Mean = 50e3, CV = 0.1)
       , Premium = ApplyRateChange(Premium, PolicyEffectiveDate, .10, as.Date('2005-01-01')))

dfWC_NY <- SimulatePolicies(
    N = 500, PolicyYears = 2004:2010, Retention = 0.75, Growth = .05
    , AdditionalColumns = list(Line = "WC", State = "NY")) %>%
  mutate(Premium = InflatedPremium(PolicyEffectiveDate, Mean = 40e3, CV = 0.2, inflation = .02))

dfWC_CA <- SimulatePolicies(
    N = 500, PolicyYears = 2002:2010, Retention = 0.75, Growth = .2
    , AdditionalColumns = list(Line = "WC", State = "CA")) %>%
  mutate(Premium = InflatedPremium(PolicyEffectiveDate, Mean = 60e3, CV = 0.3, inflation = .04))

dfPolicies <- dplyr::bind_rows(
      dfGL_NY
    , dfGL_CA
    , dfWC_NY
    , dfWC_CA
  ) %>%
  select(-Exposure, -PolicyholderID)
