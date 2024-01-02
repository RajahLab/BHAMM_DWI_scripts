# Name: visualize_pls_singular_profile.R
# Author: Rikki Lissaman
# Last updated (dd/mm/yyyy): 02/01/2024
#
# Description: This R script contains code that imports information about the 
# singular profile of LVs identified by PLS analyses (bpls_analysis.m), and then
# generates relevant visualizations.
#
# Inputs: 
# (1) CSV files containing singular profile information per LV of interest.
#
# Outputs: 
# (1) SVG files containing figures to be combined in Inkscape.
#
# Note(s):
# (1) File names have been replaced with placeholder text.


# Load Packages -----------------------------------------------------------

# Load all required packages
library(readr) # version 2.1.2
library(tidyr) # version 1.2.0
library(dplyr) # version 1.0.8
library(stringr) # version 1.4.0
library(ggplot2) # version 3.3.6
library(svglite) # version 2.1.1


# Generate Visualizations for Sex -----------------------------------------

# Import values for LV1 obtained from the sex analysis.
sex_bPLS_corr <- read_csv("input-file.csv", col_names = F) %>%
  rename(corr = X1, ll_corr = X2, ul_corr = X3) 

# Add columns for group (female/male) and DTI metric (FA, MD)
sex_bPLS_corr <- sex_bPLS_corr %>% 
  mutate(group = rep(c("Female", "Male"), each = 2),
         dti = rep(c("FA", "MD"), times = 2))

# Amend variables so they are of the correct type
sex_bPLS_corr <- mutate(sex_bPLS_corr,
                        group = factor(group, levels = c("Male","Female")),
                        dti = factor(dti, levels = c("FA", "MD")))

# Generate figure
sex_bPLS_fig <- ggplot(sex_bPLS_corr, aes(x = dti, y = corr, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = ll_corr, ymax = ul_corr), width = .2,
                position=position_dodge(.9)) +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", colour = "black", size = 11),
        axis.text.y = element_text(family = "Arial", colour = "black", size = 11),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        strip.text.x = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        legend.position = "blank") +
  scale_y_continuous(name = "Brain-Behavior Correlations", limits = c(-1, 1),
                     breaks = seq(from = -1, to = 1, by = 0.2),
                     labels = seq(from = -1, to = 1, by = 0.2)) +
  scale_colour_manual(values = c("#0072B2", "#D55E00")) +
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  facet_wrap(~group)

# View the figure
sex_bPLS_fig

# Export figure
ggsave("output-file.svg", sex_bPLS_fig, width = 6, height = 4)


# Generate Visualizations for Menopause -----------------------------------

# Import values for LV1 obtained from the menopause analysis
meno_bPLS_corr <- read_csv("input-file.csv", col_names = F) %>%
  rename(corr = X1, ll_corr = X2, ul_corr = X3) 

# Add columns for group (pre/post) and DTI metric (FA, MD)
meno_bPLS_corr <- meno_bPLS_corr %>% 
  mutate(group = rep(c("Pre-menopause", "Post-menopause"), each = 2),
         dti = rep(c("FA", "MD"), times = 2))

# Amend variables so they are of the correct type
meno_bPLS_corr <- mutate(meno_bPLS_corr,
                         dti = factor(dti, levels = c("FA", "MD")),
                         group = factor(group, levels = c("Pre-menopause", "Post-menopause")))

# Generate figure
meno_bPLS_fig <- ggplot(meno_bPLS_corr, aes(x = dti, y = corr, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = ll_corr, ymax = ul_corr), width = .2,
                position=position_dodge(.9)) +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", colour = "black", size = 11),
        axis.text.y = element_text(family = "Arial", colour = "black", size = 11),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        strip.text.x = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        legend.position = "blank") +
  scale_y_continuous(name = "Brain-Behavior Correlations", limits = c(-1, 1),
                     breaks = seq(from = -1, to = 1, by = 0.2),
                     labels = seq(from = -1, to = 1, by = 0.2)) +
  scale_colour_manual(values = c("#EBD66B", "#698BAB")) +
  scale_fill_manual(values = c("#EBD66B", "#698BAB")) +
  facet_wrap(~group)

# View the figure
meno_bPLS_fig

# Export figure
ggsave("output-file.svg", meno_bPLS_fig, width = 6, height = 4)

