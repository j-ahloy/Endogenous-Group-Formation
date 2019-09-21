library(tidyverse)
library(haven)
library(knitr)
library(kableExtra)


setwd()


# Data--------------------------------------------------------------------------

data <- read_dta('PersonalityAndPG_Clean.dta')

# Personality traits for subjects in all conditions

personality_data <- data %>%
  select(SubjectID, condition, session, Period, 'Honesty-Humility' = honesty, 
         'Emotionality' = emotion, 
         'Extraversion' = extraver,
         'Agreeableness' = agreeable, 
         'Conscientiousness' = conscient, 
         'Openness to Experience' = openness) %>%
  distinct(SubjectID, .keep_all = TRUE)


# Contribution Info personality traits

cond1 <- personality_data %>%
  filter(condition == 1)

# Personality Info personality traits

cond2 <- personality_data %>%
  filter(condition == 2)

# Personality & Contribution Info personality traits

cond3 <- personality_data %>%
  filter(condition == 3)

# Correlation Tables------------------------------------------------------------


# All conditions
# TABLE E1: Spearman Rank Correlations for Personality Traits Across All Conditions

corr_table <- cor(personality_data[, 5:10], method = 'spearman') %>%
  round(., 2)

kable(corr_table, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("scale_down"))

# Contribution Info 
# TABLE E2: Spearman Rank Correlations for Personality Traits in Contribution Info

corr_table_base <- cor(cond1[, 5:10], method = 'spearman') %>%
  round(., 2)

kable(corr_table_base, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("scale_down"))

# Personality Info 
# TABLE E3: Spearman Rank Correlations for Personality Traits in Personality Info

corr_table_pers <- cor(cond2[, 5:10], method = 'spearman') %>%
  round(., 2)

kable(corr_table_pers, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("scale_down"))

# Personality & Contribution Info
# TABLE E4: Spearman Rank Correlations for Personality Traits in Personality & Contribution Info 

corr_table_pers_cont <- cor(cond3[, 5:10], method = 'spearman') %>%
  round(., 2)

kable(corr_table_pers_cont, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("scale_down"))







