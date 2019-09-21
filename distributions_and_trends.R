library(tidyverse)
library(haven)
library(viridis)
library(cowplot)
library(grid)
library(gridExtra)


setwd()

# Data--------------------------------------------------------------------------

data <- read_dta('PersonalityAndPG_Clean.dta')


# Data for constructing heatmap

cont_data <- data %>%
  select(SubjectID, condition, session, Period, Group, Contribution) %>%
  group_by(condition, Period, Contribution) %>%
  summarise(Count = n()) %>%
  mutate(Share = Count/sum(Count))

# Data for trends

time_path_data <- data %>%
  select(SubjectID, condition, session, session_size, Period, Group, 
         Contribution, grpsize_prop, Profit, PercentOfOpt)


# Adjust profit data for fixed payment in final period

time_path_data$Profit <- ifelse(time_path_data$session == 11 & 
                                  time_path_data$Period == 20, 
                                time_path_data$Profit - 100,
                                time_path_data$Profit)

time_path_data$Profit <- ifelse(time_path_data$session != 11 & 
                                  time_path_data$Period == 20,
                                time_path_data$Profit - 150,
                                time_path_data$Profit)

# Summarize trend data 

time_path_data_summary <- time_path_data %>%
  group_by(condition, Period) %>%
  summarise(AvgContribution = mean(Contribution, na.rm = T),
            AvgSize = mean(grpsize_prop, na.rm = T),
            AvgProfit = mean(Profit, na.rm = T),
            AvgPercOpt = mean(PercentOfOpt, na.rm = T)) 


# Heat Map of Contributions-----------------------------------------------------

# FIGURE 1: Heatmaps of Contributions to the Group Account

cond_labels <- c('1' = 'Contribution Info',
                 '2' = 'Personality Info',
                 '3' = 'Contribution & Personality Info')


cont_map <- ggplot(cont_data, aes(Period, Contribution, fill = Share)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.spacing.x = unit(0.5, 'cm'),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        strip.text.x = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.key.height = unit(0.25, 'cm'),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = 'white')) +
  geom_tile(color = 'white', size = 0.1) +
  facet_grid(cols = vars(condition),
             labeller = labeller(condition = cond_labels)) +
  scale_fill_gradient(limits = c(0, 1), 
                      breaks = seq(0, 1, by = 0.5),
                      low = 'gray80', high = 'gray10',
                      name = 'Share of Contributions') + 
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 15, by = 1)) +
  scale_x_continuous(breaks = seq(2, 20, by = 2)) +
  ylab('Contribution') +
  xlab('\nPeriod')


cont_map


# Time Path Graphs--------------------------------------------------------------

# Contribution graph

contgraph <- ggplot(time_path_data_summary, aes(Period, AvgContribution)) + 
  geom_line(aes(linetype = as.factor(condition), size = as.factor(condition))) +
  theme_bw(base_size = 12) + 
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank()) + 
  scale_x_continuous(breaks = seq(2, 20, by = 2)) + 
  scale_linetype_manual(values = c('solid', 'twodash', 'dotted'),
                        labels = c('Contribution Info', 'Personality Info',
                                   'Contribution & Personality Info')) +
  scale_size_manual(values = c(1, 1, 1),
                    labels = c('Contribution Info', 'Personality Info',
                               'Contribution & Personality Info')) +
  ggtitle('Average Contribution')


# Add labels to contribution graph

labcont <- contgraph + labs(linetype = 'Condition', size = 'Condition')


# Group size graph

sizegraph <- ggplot(time_path_data_summary, aes(Period, AvgSize)) +
  geom_line(aes(linetype = as.factor(condition), size = as.factor(condition))) +
  theme_bw(base_size = 12) + 
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank()) + 
  scale_x_continuous(breaks = seq(2, 20, by = 2)) + 
  scale_linetype_manual(values = c('solid', 'twodash', 'dotted')) +
  scale_size_manual(values = c(1, 1, 1)) + 
  ggtitle('Average Group Size')


# Profit graph

profitgraph <- ggplot(time_path_data_summary, aes(Period, AvgProfit)) +
  geom_line(aes(linetype = as.factor(condition), size = as.factor(condition))) +
  theme_bw(base_size = 12) + 
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank()) + 
  scale_linetype_manual(values = c('solid', 'twodash', 'dotted')) +
  scale_x_continuous(breaks = seq(2, 20, by = 2)) + 
  scale_y_continuous(breaks = seq(0, 90, by = 20), limits = c(0, 95)) +
  scale_size_manual(values = c(1, 1, 1)) +
  ggtitle('Average Profit')


# Percent of optimal graph

percoptgraph <- ggplot(time_path_data_summary, aes(Period, AvgPercOpt)) +
  geom_line(aes(linetype = as.factor(condition), size = as.factor(condition))) +
  theme_bw(base_size = 12) + 
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank()) + 
  scale_x_continuous(breaks = seq(2, 20, by = 2)) + 
  scale_linetype_manual(values = c('solid', 'twodash', 'dotted')) +
  scale_size_manual(values = c(1, 1, 1)) +
  ggtitle('Average Contribution/Group Optimal')


# Create a grid of trend graphs

time_path_graphs <- plot_grid(labcont + theme(legend.position = 'none'),
                              sizegraph + theme(legend.position = 'none'),
                              profitgraph + theme(legend.position = 'none'),
                              percoptgraph + theme(legend.position = 'none'),
                              nrow = 4)

# Extract the legend from the labeled contribution graph

legend <- get_legend(labcont + theme(legend.position = 'bottom',
                                     legend.spacing.x = unit(0.10, 'cm'),
                                     legend.text = element_text(size = 9),
                                     legend.key.size = unit(1, 'cm')))

# Create a common x label for the graph grid

common_x <- textGrob("Period", 
                     gp=gpar(fontsize=10))


# FIGURE 2: Time Trends for Key Variables

grid.arrange(arrangeGrob(time_path_graphs, common_x, bottom = legend, 
                         heights = c(1, .05, .025)))