library(haven)
library(tidyverse)
library(brms)
library(bayesplot)
library(cowplot)


setwd()


# Data--------------------------------------------------------------------------

data <- read_dta('PersonalityAndPG_Clean.dta')

reg_data <- data %>%
  select(Contribution, SubjectID, session, condition, honesty, emotion,
         extraver, agreeable, conscient, openness, time_in_group, 
         joined_one_period, joined_current_period, moves_next_period, 
         curr_grpsize, endgame, Period)


reg_data$condition <- as.factor(reg_data$condition)

reg_data$max_contribution <- 15

reg_data$honesty.z <- scale(reg_data$honesty)
reg_data$emotion.z <- scale(reg_data$emotion)
reg_data$extraver.z <- scale(reg_data$extraver)
reg_data$agreeable.z <- scale(reg_data$agreeable)
reg_data$conscient.z <- scale(reg_data$conscient)
reg_data$openness.z <- scale(reg_data$openness)


# Contribution Regression------------------------------------------------------- 

contribution_logit <- 
  brm(data = reg_data, family = binomial,
      Contribution | trials(max_contribution) ~ honesty.z*condition + 
        agreeable.z*condition + extraver.z*condition + emotion.z*condition + 
        conscient.z*condition +openness.z*condition + curr_grpsize*condition + 
        time_in_group*condition + endgame*condition + Period*condition + 
        (1|SubjectID) + (1|session),
      prior = c(prior(normal(0, 2), class = Intercept),
                prior(normal(0, 2), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 4000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.95, max_treedepth = 20),
      seed = 1111,
      file = 'contributionlogit'
  )



# Posterior Samples for Contribution Regression---------------------------------

# All posterior samples

contribution_logit_post <- posterior_samples(contribution_logit)


# Create data frames of posterior samples according to condition.

# Contribution Info posteriors

base_post <- data.frame(Honesty = (contribution_logit_post$b_honesty.z),
                          Emotionality = (contribution_logit_post$b_emotion.z),
                          Extraversion = (contribution_logit_post$b_extraver.z),
                          Agreeableness = (contribution_logit_post$b_agreeable.z),
                          Conscientiousness = (contribution_logit_post$b_conscient.z),
                          Openness = (contribution_logit_post$b_openness.z),
                          TimeInGroup = (contribution_logit_post$b_time_in_group),
                          Groupsize = (contribution_logit_post$b_curr_grpsize),
                          Endgame = (contribution_logit_post$b_endgame),
                          Period = (contribution_logit_post$b_Period)
                        )


# Personality Info posteriors

cond2_post <- data.frame(Honesty = (contribution_logit_post$b_honesty.z +
                                       contribution_logit_post$`b_honesty.z:condition2`),
                          Emotionality = (contribution_logit_post$b_emotion.z +
                                            contribution_logit_post$`b_condition2:emotion.z`),
                          Extraversion = (contribution_logit_post$b_extraver.z +
                                            contribution_logit_post$`b_condition2:extraver.z`),
                          Agreeableness = (contribution_logit_post$b_agreeable.z + 
                                             contribution_logit_post$`b_condition2:agreeable.z`),
                          Conscientiousness = (contribution_logit_post$b_conscient.z + 
                                                 contribution_logit_post$`b_condition2:conscient.z`),
                          Openness = (contribution_logit_post$b_openness.z + 
                                        contribution_logit_post$`b_condition2:openness.z`),
                          TimeInGroup = (contribution_logit_post$b_time_in_group + 
                                           contribution_logit_post$`b_condition2:time_in_group`),
                          Groupsize = (contribution_logit_post$b_curr_grpsize + 
                                         contribution_logit_post$`b_condition2:curr_grpsize`),
                          Endgame = (contribution_logit_post$b_endgame + 
                                       contribution_logit_post$`b_condition2:endgame`),
                          Period = (contribution_logit_post$b_Period + 
                                      contribution_logit_post$`b_condition2:Period`))




# Personality & Contribution Info posteriors

cond3_post <- data.frame(Honesty = (contribution_logit_post$b_honesty.z +
                                       contribution_logit_post$`b_honesty.z:condition3`),
                          Emotionality = (contribution_logit_post$b_emotion.z +
                                            contribution_logit_post$`b_condition3:emotion.z`),
                          Extraversion = (contribution_logit_post$b_extraver.z +
                                            contribution_logit_post$`b_condition3:extraver.z`),
                          Agreeableness = (contribution_logit_post$b_agreeable.z + 
                                             contribution_logit_post$`b_condition3:agreeable.z`),
                          Conscientiousness = (contribution_logit_post$b_conscient.z + 
                                                 contribution_logit_post$`b_condition3:conscient.z`),
                          Openness = (contribution_logit_post$b_openness.z + 
                                        contribution_logit_post$`b_condition2:openness.z`),
                          TimeInGroup = (contribution_logit_post$b_time_in_group + 
                                           contribution_logit_post$`b_condition3:time_in_group`),
                          Groupsize = (contribution_logit_post$b_curr_grpsize + 
                                         contribution_logit_post$`b_condition3:curr_grpsize`),
                          Endgame = (contribution_logit_post$b_endgame + 
                                       contribution_logit_post$`b_condition3:endgame`),
                          Period = (contribution_logit_post$b_Period + 
                                      contribution_logit_post$`b_condition3:Period`))


# Contribution Regression Dot Charts--------------------------------------------

color_scheme_set('darkgray')

# Contribution Info dot chart

base_intervals <- base_post %>%
  select(Honesty, Emotionality, Extraversion, Agreeableness, Conscientiousness,
         "Openness to Experience" = Openness, "Time In Group" = TimeInGroup,
         'Group Size' = Groupsize, Endgame, Period) %>%
  mcmc_intervals(., point_est = "mean", prob = 0.5, prob_outer = 0.95) +
  ggtitle('Contribution Info') +
  theme_classic() +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 13)) +
  scale_x_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, by = 0.2))

# Personality Info dot chart

cond2_intervals <- cond2_post %>%
  select(Honesty, Emotionality, Extraversion, Agreeableness, Conscientiousness,
         "Openness to Experience" = Openness, "Time In Group" = TimeInGroup,
         'Group Size' = Groupsize, Endgame, Period) %>%
  mcmc_intervals(., point_est = "mean", prob = 0.5, prob_outer = 0.95) +
  ggtitle('Personality Info') +
  theme_classic() +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 13)) +
  scale_x_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, by = 0.2))


# Personality & Contribution Info dot chart

cond3_intervals <- cond3_post %>%
  select(Honesty, Emotionality, Extraversion, Agreeableness, Conscientiousness,
         "Openness to Experience" = Openness, "Time In Group" = TimeInGroup,
         'Group Size' = Groupsize, Endgame, Period) %>%
  mcmc_intervals(., point_est = "mean", prob = 0.5, prob_outer = 0.95) +
  ggtitle('Personality and Contribution Info') +
  theme_classic() +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 13)) +
  scale_x_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, by = 0.2))


# FIGURE 3: Posterior Distributions for Parameter Estimates by Condition

plot_grid(base_intervals, cond2_intervals, cond3_intervals, nrow = 3)

