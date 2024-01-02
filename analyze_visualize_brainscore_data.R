# Name: analyze_visualize_brainscore_data.R
# Author: Rikki Lissaman
# Last updated (dd/mm/yyyy): 02/01/2024
#
# Description: This R script contains code that imports brain score data derived
# from behavior PLS analyses (generated via bpls_analysis.m), and then analyzes
# and visualizes the data. The filtered BHAMM Master database is also used.
#
# Inputs: 
# (1) SPSS file containing a filtered version of the BHAMM Master database.
# (2) CSV files containing brain scores 
#
# Outputs: 
# (1) 3 SVG files containing figures to be combined in Inkscape.
#
# Note(s):
# (1) File names have been replaced with placeholder text.
# (2) n = number of participants.


# Load Packages -----------------------------------------------------------

# Load all required packages
library(haven) # version 2.5.0
library(readr) # version 2.1.2
library(janitor) # version 2.1.0
library(tidyr) # version 1.2.0
library(dplyr) # version 1.0.8
library(ggplot2) # version 3.3.6
library(scales) # version 1.2.0
library(svglite) # version 2.1.1


# Import and Prepare Data -------------------------------------------------

# Import filtered BHAMM Master database and "clean" variable names 
bhamm <- read_sav("input-file-name.sav") %>% clean_names() # n = 96

# Convert sex and menopause status from haven-labelled variables to factors
bhamm <- mutate(bhamm, sex = as_factor(sex), s2_meno_group = as_factor(s2_meno_group))

# Set contrasts for sex to deviation/sum coding (-1, 1)
contrasts(bhamm$sex) <- contr.sum(2)

# Create a new data frame for the menopause analysis by filtering out males
meno <- filter(bhamm, sex == "Female") # n = 66

# For some reason, R continues to include unused levels in the contrasts (i.e., 
# "Indeterminate"). To get around this, convert the variable to a character vector, 
# and then re-convert to a factor
meno <- meno %>%
  mutate(s2_meno_group = as.character(s2_meno_group)) %>%
  mutate(s2_meno_group = factor(s2_meno_group, levels = c("Premenopause", 
                                                          "Postmenopause")))

# Set contrasts for menopause to deviation/sum coding (-1. 1)
contrasts(meno$s2_meno_group) <- contr.sum(2)


## Sex ---- 

# Import "brain scores" from PLS analysis of sex
brainscores_sex <- read_csv("input-file.csv", col_names = FALSE)

# Create new data frames containing brain scores for the different conditions 
# (FA/MD), maintaining their original order
fa_brainscores_sex <- brainscores_sex %>% slice(1:66, 133:162) # 1:66 = female FA, 133:162 = male FA
md_brainscores_sex <- brainscores_sex %>% slice(67:132, 163:192) # 67:132 = female MD, 163:192 = male MD

# Add the brain scores to the BHAMM Master database
bhamm$brainscore_fa <- fa_brainscores_sex$X1
bhamm$brainscore_md <- md_brainscores_sex$X1

# Create a new variable for accuracy, collapsing across easy and hard
bhamm <- mutate(bhamm, cs_rate_combo = (cs_rate_easy + cs_rate_hard) / 2)


## Menopause ----

# Import "brain scores" from PLS analysis of menopause.
brainscores_meno <- read_csv("input-file.csv", col_names = FALSE)

# Create new data frames containing brain scores for the different conditions 
# (FA/MD), maintaining their original order.
fa_brainscores_meno <- brainscores_meno %>% slice(1:32, 65:98) # 1:32 = pre FA, 65:98 = post FA
md_brainscores_meno <- brainscores_meno %>% slice(33:64, 99:132) # 33:64 = pre MD, 99-131 = post MD

# Add the brain scores to the menopause-filtered database
meno$brainscore_fa <- fa_brainscores_meno$X1
meno$brainscore_md <- md_brainscores_meno$X1

# Create a new variable for accuracy, collapsing across easy and hard
meno <- mutate(meno, cs_rate_combo = (cs_rate_easy + cs_rate_hard) / 2)


# Analyze and Visualize Brain Score Data ----------------------------------

## Sex ----

# Run analysis examining the association between brain scores (FA) and accuracy
# (combined)
summary(lm(cs_rate_combo ~ brainscore_fa, data = bhamm)) 

# Run analysis examining the association between brain scores (MD) and accuracy 
# (combined)
summary(lm(cs_rate_combo ~ brainscore_md, data = bhamm)) 

# Generate separate data frames for males and females
female <- bhamm %>% filter(sex == "Female")
male <- bhamm %>% filter(sex == "Male")

# Run analysis examining the association between brain scores (FA) and accuracy 
# (combined) in females
summary(lm(cs_rate_combo ~ brainscore_fa, data = female)) 

# Run analysis examining the association between brain scores (FA) and accuracy 
# (combined) in males
summary(lm(cs_rate_combo ~ brainscore_fa, data = male)) 

# Generate a plot of the relationship between brain scores (FA) and accuracy 
# (combined) as a function of sex
set.seed(123) # make reproducible
sex_brainFA_acc_fig <- bhamm %>% 
  ggplot(aes(x = cs_rate_combo, y = brainscore_fa, colour = sex, fill = sex)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", colour = "black", size = 11),
        axis.text.y = element_text(family = "Arial", colour = "black", size = 11),
        axis.title.x = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        axis.title.y = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        legend.text = element_text(family = "Arial", colour = "black", size = 11),
        legend.title = element_blank(), 
        legend.position = "top") +
  scale_x_continuous(name = "Retrieval Accuracy - Combined", limits = c(0, 1), 
                     oob = squish, breaks = seq(0, 1, by = .2), labels = seq(0, 1, by = .2)) +
  scale_y_continuous(name = "Brain Scores - FA") +
  scale_colour_manual(values = c("#0072B2", "#D55E00"), labels = c("Male", "Female")) +
  scale_fill_manual(values = c("#0072B2", "#D55E00"), labels = c("Male", "Female")) 

# Print figure
sex_brainFA_acc_fig

# Expor figure
ggsave("output-file.svg", sex_brainFA_acc_fig, width = 4, height = 4, dpi = 300)


## Menopause ----

# Run analysis examining the association between brain scores (FA) and accuracy 
# (combined)
summary(lm(cs_rate_combo ~ brainscore_fa, data = meno)) 

# Run analysis examining the association between brain scores (MD) and accuracy 
# (combined)
summary(lm(cs_rate_combo ~ brainscore_md, data = meno))

# Generate separate data frames for pre- and post-menopausal females
pre <- meno %>% filter(s2_meno_group == "Premenopause")
post <- meno %>% filter(s2_meno_group == "Postmenopause")

# Run analysis examining the association between brain scores (FA) and accuracy 
# (combined) in pre-menopausal females
summary(lm(cs_rate_combo ~ brainscore_fa, data = pre)) 

# Run analysis examining the association between brain scores (MD) and accuracy 
# (combined) in pre-menopausal females
summary(lm(cs_rate_combo ~ brainscore_md, data = pre))

# Run analysis examining the association between brain scores (FA) and accuracy 
# (combined) in post-menopausal females
summary(lm(cs_rate_combo ~ brainscore_fa, data = post))

# Run analysis examining the association between brain scores (MD) and accuracy 
# (combined) in post-menopausal females
summary(lm(cs_rate_combo ~ brainscore_md, data = post)) 

# Generate a plot of the relationship between brain scores (FA) and accuracy 
# (combined) as a function of sex
set.seed(123) # make reproducible
meno_brainFA_acc_fig <- meno %>% 
  ggplot(aes(x = cs_rate_combo, y = brainscore_fa, colour = s2_meno_group, fill = s2_meno_group)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", colour = "black", size = 11),
        axis.text.y = element_text(family = "Arial", colour = "black", size = 11),
        axis.title.x = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        axis.title.y = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        legend.text = element_text(family = "Arial", colour = "black", size = 11),
        legend.title = element_blank(), 
        legend.position = "top") +
  scale_x_continuous(name = "Retrieval Accuracy - Combined", limits = c(0, 1), 
                     oob = squish, breaks = seq(0, 1, by = .2), labels = seq(0, 1, by = .2)) +
  scale_y_continuous(name = "Brain Scores - FA") +
  scale_colour_manual(values = c("#EBD66B", "#698BAB"), labels = c("Pre-menopause", "Post-menopause")) +
  scale_fill_manual(values = c("#EBD66B", "#698BAB"), labels = c("Pre-menopause", "Post-menopause")) 

# Print figure
meno_brainFA_acc_fig

# Export figure
ggsave("output-file.svg", meno_brainFA_acc_fig, width = 4, height = 4, dpi = 300)

# Generate a plot of the relationship between brain scores (MD) and accuracy 
# (combined) as a function of sex
meno_brainMD_acc_fig <- meno %>% 
  ggplot(aes(x = cs_rate_combo, y = brainscore_md, colour = s2_meno_group, fill = s2_meno_group)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", colour = "black", size = 11),
        axis.text.y = element_text(family = "Arial", colour = "black", size = 11),
        axis.title.x = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        axis.title.y = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        legend.text = element_text(family = "Arial", colour = "black", size = 11),
        legend.title = element_blank(), 
        legend.position = "top") +
  scale_x_continuous(name = "Retrieval Accuracy - Combined", limits = c(0, 1), 
                     oob = squish, breaks = seq(0, 1, by = .2), labels = seq(0, 1, by = .2)) +
  scale_y_continuous(name = "Brain Scores - MD") +
  scale_colour_manual(values = c("#EBD66B", "#698BAB"), labels = c("Pre-menopause", "Post-menopause")) +
  scale_fill_manual(values = c("#EBD66B", "#698BAB"), labels = c("Pre-menopause", "Post-menopause")) 

# print
meno_brainMD_acc_fig

# Export figure
ggsave("output-file.svg", meno_brainMD_acc_fig, width = 4, height = 4, dpi = 300)
