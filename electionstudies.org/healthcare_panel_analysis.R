# Healthcare Attitudes and Policy Analysis: ANES 2016-2020 Panel Study
# This R script provides code for analyzing changes in healthcare attitudes and policy preferences
# using the American National Election Studies (ANES) 2016-2020 Panel data

# Load necessary packages
library(tidyverse)    # Data manipulation and visualization
library(haven)        # For reading Stata/SPSS files 
library(plm)          # Panel data analysis
library(broom)        # Tidy model outputs
library(stargazer)    # Regression tables
library(lmtest)       # For coeftest (robust standard errors)
library(sandwich)     # For robust standard errors

#============================================================================
# PART 1: DATA PREPARATION
#============================================================================

# Assuming the ANES 2016-2020 panel data is downloaded as a .dta or .sav file
# You may need to adjust the file path based on where your data is stored
# The data can be downloaded from the ANES website: https://electionstudies.org/

# Example for loading data (adjust file name as needed)
anes_panel <- haven::read_dta("path_to_your_data/anes_panel_2016_2020.dta")
# Alternative: 
# anes_panel <- haven::read_sav("path_to_your_data/anes_panel_2016_2020.sav")

# Identify key variables for analysis
# Here we create a subset with key healthcare variables 
# Based on ANES 2016-2020 panel study codebooks

healthcare_vars <- anes_panel %>%
  select(
    # ID variables
    caseid = V200001,             # 2020 Case ID
    caseid_2016 = V160001_orig,   # 2016 Case ID (links 2016 to 2020 data)
    
    # Healthcare variables of interest from 2016
    hc_opinion_2016 = V161184,    # ACA Opinion: "Do you favor, oppose, or neither favor nor oppose the health care reform law passed in 2010? 
                                   # This law requires all Americans to buy health insurance and requires health insurance companies to 
                                   # accept everyone." (1=Favor, 2=Oppose, 3=Neither favor nor oppose)
    
    hc_govt_role_2016 = V161185,  # Government Role: "Do you think the government should provide health insurance for all Americans, 
                                   # should this be left to private health insurance companies, or is your position somewhere in between?"
                                   # (1=Government insurance plan, 7=Private insurance plan)
    
    hc_insurance_2016 = V161266,  # Insurance Status: "Are you covered by any kind of health insurance?" 
                                   # (1=Yes, 2=No)
    
    # Healthcare variables of interest from 2020
    hc_opinion_2020 = V202346,    # ACA Opinion: Similar question from 2020 on the Affordable Care Act
                                   # (1=Favor a great deal, 2=Favor moderately, 3=Favor a little, 
                                   # 4=Neither favor nor oppose, 5=Oppose a little, 6=Oppose moderately, 
                                   # 7=Oppose a great deal)
    
    hc_govt_role_2020 = V202347,  # Government Role: Similar question from 2020 on government role in healthcare
                                   # (1=Government insurance plan, 7=Private insurance plan)
    
    hc_covid_concern_2020 = V202501, # COVID-19 Concern: "How worried are you about you or someone in your family 
                                   # being infected with the coronavirus/COVID-19?"
                                   # (1=Extremely worried, 2=Very worried, 3=Moderately worried, 
                                   # 4=Slightly worried, 5=Not at all worried)
    
    hc_insurance_2020 = V201507,  # Insurance Status: Similar question from 2020 on having health insurance
                                   # (1=Yes, 2=No)
    
    # Control variables (demographics and other factors)
    age = V201507x,               # Age in years (derived/computed)
    
    gender = V201600,             # Gender: "What is your gender?"
                                   # (1=Male, 2=Female, 3=Other)
    
    race = V201549x,              # Race/ethnicity (derived): 
                                   # (1=White, non-Hispanic, 2=Black, non-Hispanic, 3=Hispanic, 
                                   # 4=Asian/Native Hawaiian/Pacific Islander, non-Hispanic, 
                                   # 5=Native American/Alaskan Native, non-Hispanic, 6=Multiple races, non-Hispanic)
    
    education = V201510x,         # Education level (derived):
                                   # (1=Less than high school, 2=High school graduate, 
                                   # 3=Some college but no degree, 4=Associate degree, 
                                   # 5=Bachelor's degree, 6=Graduate degree)
    
    income = V201617x,            # Household income (derived):
                                   # (Categorical values representing income ranges)
    
    party_id = V201228x,          # Party identification 7-point scale (derived):
                                   # (1=Strong Democrat, 2=Not very strong Democrat, 3=Independent-Democrat, 
                                   # 4=Independent, 5=Independent-Republican, 6=Not very strong Republican, 
                                   # 7=Strong Republican)
    
    ideology = V201200            # Liberal-conservative self-placement:
                                   # "Where would you place yourself on this scale, or haven't you thought much about this?"
                                   # (1=Extremely liberal, 2=Liberal, 3=Slightly liberal, 4=Moderate; middle of the road, 
                                   # 5=Slightly conservative, 6=Conservative, 7=Extremely conservative)
  )

# Clean the data
healthcare_clean <- healthcare_vars %>%
  # Remove missing values
  filter(!is.na(hc_opinion_2016) | !is.na(hc_opinion_2020)) %>%
  # Create a year variable for the panel structure
  mutate(
    # Create long format with year variable
    hc_opinion_2016_recode = case_when(
      hc_opinion_2016 == 1 ~ 5,  # Example recoding - adjust based on actual coding
      hc_opinion_2016 == 2 ~ 4,
      hc_opinion_2016 == 3 ~ 3,
      hc_opinion_2016 == 4 ~ 2,
      hc_opinion_2016 == 5 ~ 1
    ),
    
    hc_opinion_2020_recode = case_when(
      hc_opinion_2020 == 1 ~ 5,  # Example recoding - adjust based on actual coding
      hc_opinion_2020 == 2 ~ 4,
      hc_opinion_2020 == 3 ~ 3,
      hc_opinion_2020 == 4 ~ 2,
      hc_opinion_2020 == 5 ~ 1
    ),
    
    # Create change variables
    opinion_change = hc_opinion_2020_recode - hc_opinion_2016_recode
  )

# Convert to long format for panel analysis
healthcare_long <- healthcare_clean %>%
  pivot_longer(
    cols = c(
      hc_opinion_2016, hc_opinion_2020,
      hc_govt_role_2016, hc_govt_role_2020,
      hc_insurance_2016, hc_insurance_2020
    ),
    names_to = c("variable", "year"),
    names_pattern = "(.*)_(\\d{4})",
    values_to = "value"
  ) %>%
  # Make sure year is numeric
  mutate(year = as.numeric(year))

# Create a panel data object
panel_data <- pdata.frame(healthcare_long, index = c("caseid", "year"))

#============================================================================
# PART 2: DESCRIPTIVE ANALYSIS
#============================================================================

# Summary statistics of healthcare variables by year
healthcare_clean %>%
  summarize(
    mean_opinion_2016 = mean(hc_opinion_2016_recode, na.rm = TRUE),
    mean_opinion_2020 = mean(hc_opinion_2020_recode, na.rm = TRUE),
    sd_opinion_2016 = sd(hc_opinion_2016_recode, na.rm = TRUE),
    sd_opinion_2020 = sd(hc_opinion_2020_recode, na.rm = TRUE),
    n_2016 = sum(!is.na(hc_opinion_2016_recode)),
    n_2020 = sum(!is.na(hc_opinion_2020_recode))
  )

# Paired t-test to compare healthcare opinions in 2016 vs 2020
t.test(
  healthcare_clean$hc_opinion_2016_recode,
  healthcare_clean$hc_opinion_2020_recode,
  paired = TRUE
)

# Visualization of changes in healthcare attitudes
ggplot(healthcare_clean, aes(x = hc_opinion_2016_recode, y = hc_opinion_2020_recode)) +
  geom_jitter(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Change in Healthcare Attitudes from 2016 to 2020",
    x = "Healthcare Opinion 2016",
    y = "Healthcare Opinion 2020"
  ) +
  theme_minimal()

# Distribution of opinion change
ggplot(healthcare_clean, aes(x = opinion_change)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Changes in Healthcare Opinions (2016-2020)",
    x = "Opinion Change (Positive = More Favorable in 2020)",
    y = "Count"
  ) +
  theme_minimal()

#============================================================================
# PART 3: REGRESSION MODELS
#============================================================================

# Model 1: Basic panel model with fixed effects 
model1 <- plm(
  hc_opinion ~ lag(hc_opinion, 1) + party_id + ideology,
  data = panel_data,
  model = "within",
  effect = "individual"
)

# Model with robust standard errors
robust_se_model1 <- coeftest(model1, vcov = vcovHC(model1, type = "HC1"))
print(robust_se_model1)

# Model 2: Random effects model with additional controls
model2 <- plm(
  hc_opinion ~ lag(hc_opinion, 1) + party_id + ideology + age + gender + race + education + income,
  data = panel_data,
  model = "random"
)

# With robust standard errors
robust_se_model2 <- coeftest(model2, vcov = vcovHC(model2, type = "HC1"))
print(robust_se_model2)

# Model 3: First-difference model (change in healthcare opinions)
# This model directly examines change from 2016 to 2020
diff_model <- lm(
  opinion_change ~ party_id + ideology + age + gender + race + education + income,
  data = healthcare_clean
)

# With robust standard errors
robust_se_diff <- coeftest(diff_model, vcov = vcovHC(diff_model, type = "HC1"))
print(robust_se_diff)

# Create a nice regression table
stargazer(
  model1, model2, diff_model,
  type = "text",
  title = "Regression Models of Healthcare Opinions (2016-2020)",
  column.labels = c("Fixed Effects", "Random Effects", "First Difference"),
  covariate.labels = c(
    "Lagged Healthcare Opinion", "Party ID", "Ideology", 
    "Age", "Gender", "Race", "Education", "Income"
  ),
  digits = 3
)

#============================================================================
# PART 4: ADDITIONAL ANALYSES
#============================================================================

# Subgroup analysis by party identification
healthcare_clean %>%
  group_by(party_id) %>%
  summarize(
    mean_change = mean(opinion_change, na.rm = TRUE),
    sd_change = sd(opinion_change, na.rm = TRUE),
    n = n()
  )

# Interaction model to examine differential effects by party
interaction_model <- lm(
  opinion_change ~ party_id * ideology + age + gender + race + education + income,
  data = healthcare_clean
)

summary(interaction_model)

# Visualization of party differences in healthcare opinion changes
ggplot(healthcare_clean, aes(x = as.factor(party_id), y = opinion_change, fill = as.factor(party_id))) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Changes in Healthcare Opinions by Party Identification (2016-2020)",
    x = "Party Identification",
    y = "Opinion Change (Positive = More Favorable in 2020)",
    fill = "Party ID"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
