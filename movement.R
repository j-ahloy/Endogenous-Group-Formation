library(haven)
library(tidyverse)
library(brms)
library(bayesplot)
library(cowplot)

setwd()


# Data--------------------------------------------------------------------------

data <- read_dta('PersonalityAndPG_Clean.dta')

# Subset of data necessary for analyzing movement 

movement_data <- data %>%
  select(session, condition, SubjectID, Period, Contribution, Group, Oldgroup, 
         groupchange, moves_next_period, curr_grpsize, honesty, emotion, 
         extraver, agreeable, conscient, openness) %>%
  group_by(SubjectID)

movement_data$condition <- as.factor(movement_data$condition)


# Group averages of personality traits, contribution, and size

group_averages <- movement_data %>%
  group_by(session, Period, Group) %>%
  summarise(avhonesty = mean(honesty, na.rm = T),
            avagreeable = mean(agreeable, na.rm = T),
            avconscient = mean(conscient, na.rm = T),
            avopenness = mean(openness, na.rm = T),
            avextraver = mean(extraver, na.rm = T),
            avemotion = mean(emotion, na.rm = T),
            avcontribution = mean(Contribution, na.rm = T),
            group_size = mean(curr_grpsize, na.rm = T))


# All regression data

reg_data <- movement_data %>%
  left_join(., group_averages, by = c('Period', 'session', 'Group')) %>%
  mutate(honesty_diff = (avhonesty - honesty),
         agree_diff = (avagreeable - agreeable),
         conscient_diff = (avconscient - conscient),
         extraver_diff = (avextraver - extraver),
         open_diff = (avopenness - openness),
         emotion_diff = (avemotion - emotion),
         cont_diff = (avcontribution - Contribution))


# Contribution Info regression data

cond1_reg_data <- reg_data %>%
  filter(condition == 1)

# Personality Info regression data

cond2_reg_data <- reg_data %>%
  filter(condition == 2)

# Personality & Contribution Info regression data

cond3_reg_data <- reg_data %>%
  filter(condition == 3)


# Movement Decision Regressions-------------------------------------------------

cond1_move_fit <- 
  brm(data = cond1_reg_data, family = bernoulli,
      moves_next_period ~ curr_grpsize + cont_diff + Period + 
        (1|SubjectID) + (1|session),
      prior = c(prior(normal(0, 5), class = Intercept),
                prior(normal(0, 2), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 4000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.99, max_treedepth = 20),
      seed = 1111,
      file = 'pars_cond1_move_fit'
  )


cond2_move_fit <- 
  brm(data = cond2_reg_data, family = bernoulli,
      moves_next_period ~ curr_grpsize + honesty_diff + agree_diff + 
        conscient_diff + Period + (1|SubjectID) + (1|session),
      prior = c(prior(normal(0, 5), class = Intercept),
                prior(normal(0, 2), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 4000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.999, max_treedepth = 20),
      seed = 1111,
      file = 'pars_cond2_move_fit'
  )



cond3_move_fit <- 
  brm(data = cond3_reg_data, family = bernoulli,
      moves_next_period ~ curr_grpsize + honesty_diff + agree_diff + 
        conscient_diff + cont_diff + Period + (1|SubjectID) + (1|session),
      prior = c(prior(normal(0, 5), class = Intercept),
                prior(normal(0, 2), class = b),
                prior(cauchy(0, 1), class = sd)),
      iter = 4000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.995, max_treedepth = 20),
      seed = 1111,
      file = 'pars_cond3_move_fit'
  )



# Posterior Samples for Movement Regressions------------------------------------

# Contribution Info posterior samples

cond1_move_fit_post <- posterior_samples(cond1_move_fit)

# Personality Info posterior samples

cond2_move_fit_post <- posterior_samples(cond2_move_fit)

# Contribution & Personality Info posterior samples

cond3_move_fit_post <- posterior_samples(cond3_move_fit)


# Movement Regression Dot Charts------------------------------------------------

color_scheme_set('darkgray')


# Contribution Info dot chart

cond1_move_int <- cond1_move_fit_post %>%
  select('       Current Group Size' = b_curr_grpsize,
         'Contribution Dev.' = b_cont_diff,
         'Period' = b_Period) %>%
  mcmc_intervals( 
    point_est = "mean", prob = 0.5, prob_outer = 0.95) +
  ggtitle('Contribution Info') +
  theme_classic() +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 13)) +
  scale_x_continuous(limits = c(-2, 2),
                     breaks = seq(-2, 2, by = 1))

# Personality Info dot chart

cond2_move_int <- cond2_move_fit_post %>%
  select('Current Group Size' = b_curr_grpsize,
         'Honesty Dev.' = b_honesty_diff,
         'Agreeableness Dev.' = b_agree_diff,
         'Conscientiousness Dev.' = b_conscient_diff,
         'Period' = b_Period) %>%
  mcmc_intervals( 
    point_est = "mean", prob = 0.5, prob_outer = 0.95) +
  ggtitle('Personality Info') +
  theme_classic() +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 13)) +
  scale_x_continuous(limits = c(-2, 2),
                     breaks = seq(-2, 2, by = 1))


# Personality & Contribution Info dot chart

cond3_move_int <- cond3_move_fit_post %>%
  select('Current Group Size' = b_curr_grpsize,
         'Honesty Dev.' = b_honesty_diff,
         'Agreeableness Dev.' = b_agree_diff,
         'Conscientiousness Dev.' = b_conscient_diff,
         'Contribution Dev.' = b_cont_diff,
         'Period' = b_Period) %>%
  mcmc_intervals( 
    point_est = "mean", prob = 0.5, prob_outer = 0.95) +
  ggtitle('Personality and Contribution Info') +
  theme_classic() +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 13)) +
  scale_x_continuous(limits = c(-2, 2),
                     breaks = seq(-2, 2, by = 1))



# FIGURE 4: Posterior Distributions for Parameter Estimates by Condition

plot_grid(pars_cond1_move_int, pars_cond2_move_int, pars_cond3_move_int, 
          nrow = 3, rel_heights = c(.8, 1, 1))

