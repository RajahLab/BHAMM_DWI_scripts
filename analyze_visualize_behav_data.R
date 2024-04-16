# Name: analyze_visualize_behav_data.R
# Author: Rikki Lissaman
# Last updated (dd/mm/yyyy): 15/04/2024
#
# Description: This R script contains code that imports behavioral data prepared
# by a previous script (prepare_behav_data.R), and then analyzes and visualizes
# the data. Also included: supplementary behavioral analyses.
#
# Inputs: 
# (1) SPSS file containing a filtered version of the BHAMM Master database.
#
# Outputs: 
# (1) 4 SVG files containing figures to be combined in Inkscape.
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
library(stringr) # version 1.4.0
library(effectsize) # version 0.8.2
library(rstatix) # version 0.7.0
library(emmeans) # version 1.7.5
library(ggplot2) # version 3.3.6
library(scales) # version 1.2.0
library(svglite) # version 2.1.1
library(MatchIt) # version 4.4.0
library(BayesFactor) # version 0.9.12-4.7

# Load Raincloud plots source code
source("R_rainclouds.R") # downloaded from https://github.com/RainCloudPlots/RainCloudPlots


# Import and Prepare Data -------------------------------------------------

# Import data and "clean" variable names (i.e., make them all lower-case with 
# spaces replaced by underscores)
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


# Analyze Demographic Data ------------------------------------------------

## Age (Years) ----

# Convert age to numeric
bhamm <- mutate(bhamm, s2_age =  as.numeric(s2_age))
meno <- mutate(meno, s2_age = as.numeric(s2_age))

### Sex ###

# For males and females, calculate age statistics
bhamm %>%
  group_by(sex) %>%
  summarise(mean = round(mean(s2_age), 2),
            sd = round(sd(s2_age), 2),
            min = round(min(s2_age), 2),
            max = round(max(s2_age), 2))

# Conduct Welch's t-test to examine differences in age by sex
t.test(s2_age ~ sex, data = bhamm)

# Calculate cohen's d for sex differences in age
cohens_d(s2_age ~ sex, data = bhamm)

### Menopause ###

# For pre- and post-menopausal females, calculate age statistics
meno %>%
  group_by(s2_meno_group) %>%
  summarise(mean = round(mean(s2_age), 2),
            sd = round(sd(s2_age), 2),
            min = round(min(s2_age), 2),
            max = round(max(s2_age), 2))

# Conduct Welch's t-test to examine differences in age by menopause status
t.test(s2_age ~ s2_meno_group, data = meno)

# Calculate cohen's d for menopause differences in age
cohens_d(s2_age ~ s2_meno_group, data = meno)


## Education (Years) ----

# Convert years of education to integer
bhamm <- mutate(bhamm, edu = as.integer(edu))
meno <- mutate(meno, edu = as.integer(edu))

### Sex ###

# For males and females, calculate education statistics
bhamm %>%
  group_by(sex) %>%
  summarise(mean = round(mean(edu), 2),
            sd = round(sd(edu), 2),
            min = min(edu),
            max = max(edu))

# Conduct Welch's t-test to examine differences in education by sex
t.test(edu ~ sex, data = bhamm)

# Calculate cohen's d for sex differences in education
cohens_d(edu ~ sex, data = bhamm)

### Menopause ###

# For pre- and post-menopausal females, calculate education statistics
meno %>%
  group_by(s2_meno_group) %>%
  summarise(mean = round(mean(edu), 2),
            sd = round(sd(edu), 2),
            min = min(edu),
            max = max(edu))

# Conduct Welch's t-test to examine differences in education by menopause status
t.test(edu ~ s2_meno_group, data = meno)

# Calculate cohen's d for menopause differences in education
cohens_d(edu ~ s2_meno_group, data = meno)


## BMI ----

# Convert BMI to numeric
bhamm <- mutate(bhamm, bmi = as.numeric(bmi))
meno <- mutate(meno, bmi = as.numeric(bmi))

### Sex ###

# For males and females, calculate BMI statistics
bhamm %>%
  group_by(sex) %>%
  summarise(mean = round(mean(bmi), 2),
            sd = round(sd(bmi), 2),
            min = round(min(bmi), 2),
            max = round(max(bmi), 2))

# Conduct Welch's t-test to examine differences in BMI by sex
t.test(bmi ~ sex, data = bhamm)

# Calculate cohen's d for sex differences in BMI
cohens_d(bmi ~ sex, data = bhamm)

### Menopause ###

# For pre- and post-menopausal females, calculate BMI statistics
meno %>%
  group_by(s2_meno_group) %>%
  summarise(mean = round(mean(bmi), 2),
            sd = round(sd(bmi), 2),
            min = round(min(bmi), 2),
            max = round(max(bmi), 2))

# Conduct Welch's t-test to examine differences in BMI by menopause status
t.test(bmi ~ s2_meno_group, data = meno)

# Calculate cohen's d for menopause differences in BMI
cohens_d(bmi ~ s2_meno_group, data = meno)


### Beck Depression Inventory (BDI) ----

# Convert BDI to integer
bhamm <- mutate(bhamm, bdi = as.integer(bdi))
meno <- mutate(meno, bdi = as.integer(bdi))

### Sex ###

# For males and females, calculate BDI statistics
bhamm %>%
  group_by(sex) %>%
  summarise(mean = round(mean(bdi), 2),
            sd = round(sd(bdi), 2),
            min = min(bdi),
            max = max(bdi))

# Conduct Welch's t-test to examine differences in BDI by sex
t.test(bdi ~ sex, data = bhamm)

# Calculate cohen's d for sex differences in BDI
cohens_d(bdi ~ sex, data = bhamm)

### Menopause ###

# For pre- and post-menopausal females, calculate BDI statistics
meno %>%
  group_by(s2_meno_group) %>%
  summarise(mean = round(mean(bdi), 2),
            sd = round(sd(bdi), 2),
            min = min(bdi),
            max = max(bdi))

# Conduct Welch's t-test to examine differences in BDI by menopause status
t.test(bdi ~ s2_meno_group, data = meno)

# Calculate cohen's d for menopause differences in BDI
cohens_d(bdi ~ s2_meno_group, data = meno)


## Handedness ----

# Convert handedness scores to integer, then create a new variable `right_hand` 
# that identifies participants who were deemed right-handed (score of 61 or above)
bhamm <- bhamm %>% 
  mutate(hand = as.integer(hand)) %>%
  mutate(right_hand = case_when(
    hand > 60 ~ "yes",
    hand <= 60 ~ "no"))

meno <- meno %>% 
  mutate(hand = as.integer(hand)) %>%
  mutate(right_hand = case_when(
    hand > 60 ~ "yes",
    hand <= 60 ~ "no"))

### Sex ###

# For males and females, calculate handedness statistics
bhamm %>%
  group_by(sex) %>%
  summarise(yes = sum(right_hand == "yes"),
            no = sum(right_hand == "no"),
            prop_yes = yes / (yes + no),
            perc_yes = round(prop_yes * 100, 2))

# Conduct chi-square test to examine differences in handedness by sex
chisq.test(table(bhamm$sex, bhamm$right_hand), correct = FALSE)

# Calculate Cramer's V for differences in handedness by sex
cramer_v(bhamm$sex, bhamm$right_hand, correct = FALSE)

### Menopause ###

# For pre- and post-menopausal females, calculate handedness statistics
meno %>%
  group_by(s2_meno_group) %>%
  summarise(yes = sum(right_hand == "yes"),
            no = sum(right_hand == "no"),
            prop_yes = yes / (yes + no),
            perc_yes = round(prop_yes * 100, 2))

# Conduct chi-square test to examine differences in handedness by menopause status
chisq.test(table(meno$s2_meno_group, meno$right_hand), correct = FALSE)

# Calculate Cramer's V for differences in handedness by menopause
cramer_v(meno$s2_meno_group, meno$right_hand, correct = FALSE)


## Antidepressants ----

### Sex ###

# For males and females, calculate antidepressant usage statistics
bhamm %>%
  group_by(sex) %>%
  summarise(yes = sum(meds_antidepressant_corrected == -1, na.rm = TRUE),
            no = sum(meds_antidepressant_corrected == 1, na.rm = TRUE),
            na = sum(is.na(meds_antidepressant_corrected)),
            freq = yes / (yes + no),
            perc = round(freq * 100, 2))

# Conduct chi-square test to examine differences in antidepressant usage by sex
chisq.test(table(bhamm$sex, bhamm$meds_antidepressant_corrected), correct = FALSE)

# Calculate Cramer's V for differences in antidepressant usage by sex
cramer_v(bhamm$sex, bhamm$meds_antidepressant_corrected, correct = FALSE)

### Menopause ###

# For pre- and post-menopausal females, calculate antidepressant usage statistics
meno %>%
  group_by(s2_meno_group) %>%
  summarise(yes = sum(meds_antidepressant_corrected == -1, na.rm = TRUE),
            no = sum(meds_antidepressant_corrected == 1, na.rm =TRUE),
            na = sum(is.na(meds_antidepressant_corrected)),
            freq = yes / (yes + no),
            perc = round(freq * 100, 2))

# Conduct chi-square test to examine differences in antidepressant usage by menopause status
chisq.test(table(meno$s2_meno_group, meno$meds_antidepressant_corrected), correct = FALSE)

# Calculate Cramer's V for differences in antidepressant usage by menopause
cramer_v(meno$s2_meno_group, meno$meds_antidepressant_corrected, correct = FALSE)


## Hormonal Birth Control ----

### Sex ###

# For females, calculate hormonal birth control usage statistics
bhamm %>%
  group_by(sex) %>%
  summarise(yes = sum(current_hbc_med_use == -1, na.rm = TRUE),
            no = sum(current_hbc_med_use == 1, na.rm = TRUE),
            na = sum(is.na(current_hbc_med_use)),
            freq = yes / (yes + no),
            perc = round(freq * 100, 2))

### Menopause ###

# For pre- and post-menopausal females, calculate hormonal birth control usage statistics
meno %>% 
  group_by(s2_meno_group) %>%
  summarise(yes = sum(current_hbc_med_use == -1, na.rm = TRUE),
            no = sum(current_hbc_med_use == 1, na.rm = TRUE),
            na = sum(is.na(current_hbc_med_use)),
            freq = yes / (yes + no),
            perc = round(freq * 100, 2))


## PCOS ----

### Sex ###

# For females, calculate PCOS prevalence statistics
bhamm %>% 
  group_by(sex) %>%
  summarise(yes = sum(repro_cond_pcos == -1, na.rm = TRUE),
            no = sum(repro_cond_pcos == 1, na.rm = TRUE),
            na = sum(is.na(repro_cond_pcos)),
            freq = yes / (yes + no),
            perc = round(freq * 100, 2))

### Menopause ###

# For pre- and post-menopausal females, calculate PCOS prevalence statistics
meno %>% 
  group_by(s2_meno_group) %>%
  summarise(yes = sum(repro_cond_pcos == -1, na.rm = TRUE),
            no = sum(repro_cond_pcos == 1, na.rm = TRUE),
            na = sum(is.na(repro_cond_pcos)),
            freq = yes / (yes + no),
            perc = round(freq * 100, 2))

# Conduct chi-square test to examine differences in PCOS by menopause status
chisq.test(table(meno$s2_meno_group, meno$repro_cond_pcos), correct = FALSE)

# Calculate Cramer's V for differences in PCOS by menopause status
cramer_v(meno$s2_meno_group, meno$repro_cond_pcos, correct = FALSE)


## Estradiol ----

# Convert estradiol to numeric
bhamm <- mutate(bhamm, e2_raw = as.numeric(e2_raw))
meno <- mutate(meno, e2_raw = as.numeric(e2_raw))

### Sex ###

# For males and females, calculate estradiol statistics
bhamm %>%
  filter(!e2_raw %in% c(NA, "NA", -1)) %>%
  group_by(sex) %>%
  summarise(n = n(),
            mean = round(mean(e2_raw, na.rm = TRUE), 2),
            sd = round(sd(e2_raw, na.rm = TRUE), 2),
            min = min(e2_raw, na.rm = TRUE),
            max = max(e2_raw, na.rm = TRUE))

# Conduct Welch's t-test to examine differences in estradiol by sex. Note: this 
# filter plus R's default features ensures that those without meaasurable values 
# are not considered
t.test(bhamm$e2_raw[bhamm$e2_raw > 0] ~ bhamm$sex[bhamm$e2_raw > 0])

# Filter out those without measurable values, calculate cohen's d for sex differences 
# in estradiol, then remove temporary data frame
tmp <- bhamm %>% filter(e2_raw > 0)
cohens_d(e2_raw ~ sex, data = tmp)
rm(tmp)

### Menopause ###

# For pre- and post-menopausal females, calculate estradiol statistics
meno %>%
  filter(!e2_raw %in% c(NA, "NA", -1)) %>%
  group_by(s2_meno_group) %>%
  summarise(n = n(),
            mean = round(mean(e2_raw, na.rm = TRUE), 2),
            sd = round(sd(e2_raw, na.rm = TRUE), 2),
            min = min(e2_raw, na.rm = TRUE),
            max = max(e2_raw, na.rm = TRUE))

# Conduct Welch's t-test to examine differences in estradiol by menopause status
t.test(meno$e2_raw[meno$e2_raw > 0] ~ meno$s2_meno_group[meno$e2_raw > 0])

# Filter out those without measurable values, calculate cohen's d for menopause 
# differences in estradiol, then remove temporary data frame
tmp <- meno %>% filter(e2_raw > 0)
cohens_d(e2_raw ~ s2_meno_group, data = tmp)
rm(tmp)


## FSH ----

# Convert FSH to numeric
bhamm <- mutate(bhamm, fsh_raw = as.numeric(fsh_raw))
meno <- mutate(meno, fsh_raw = as.numeric(fsh_raw))

### Sex ###

# For males and females, calculate FSH statistics
bhamm %>%
  filter(!fsh_raw %in% c(NA, "NA", -1)) %>%
  group_by(sex) %>%
  summarise(n = n(),
            mean = round(mean(fsh_raw, na.rm = TRUE), 2),
            sd = round(sd(fsh_raw, na.rm = TRUE), 2),
            min = min(fsh_raw, na.rm = TRUE),
            max = max(fsh_raw, na.rm = TRUE))

# Conduct Welch's t-test to examine differences in FSH by sex
t.test(bhamm$fsh_raw[bhamm$fsh_raw > 0] ~ bhamm$sex[bhamm$fsh_raw > 0])

# Filter out those without measurable values, calculate cohen's d for sex differences
# in FSH, then remove temporary data frame
tmp <- bhamm %>% filter(fsh_raw > 0)
cohens_d(fsh_raw ~ sex, data = tmp)
rm(tmp)

### Menopause ###

# For pre- and post-menopausal females, calculate FSH statistics
meno %>%
  filter(!fsh_raw %in% c(NA, "NA", -1)) %>%
  group_by(s2_meno_group) %>%
  summarise(n = n(),
            mean = round(mean(fsh_raw, na.rm = TRUE), 2),
            sd = round(sd(fsh_raw, na.rm = TRUE), 2),
            min = min(fsh_raw, na.rm = TRUE),
            max = max(fsh_raw, na.rm = TRUE))

# Conduct Welch's t-test to examine differences in FSH by menopause status
t.test(meno$fsh_raw[meno$fsh_raw > 0] ~ meno$s2_meno_group[meno$fsh_raw > 0])

# Filter out those without measurable values, calculate cohen's d for menopause 
# differences in FSH, then remove temporary data frame
tmp <- meno %>% filter(fsh_raw > 0)
cohens_d(fsh_raw ~ s2_meno_group, data = tmp)
rm(tmp)


# Analyze Behavioral Data -------------------------------------------------

# Convert all task variables to correct format
bhamm <- bhamm %>%
  mutate_at(vars(ends_with("easy")), as.numeric) %>%
  mutate_at(vars(ends_with("hard")), as.numeric) 

meno <- meno %>% 
  mutate_at(vars(ends_with("easy")), as.numeric) %>%
  mutate_at(vars(ends_with("hard")), as.numeric) 


## Accuracy ----

### Sex ###

# Calculate source correct stats for each group (sex)
bhamm %>%
  group_by(sex) %>%
  summarise(mean_easy = round(mean(cs_rate_easy), 2),
            sd_easy = round(sd(cs_rate_easy), 2),
            mean_hard = round(mean(cs_rate_hard), 2),
            sd_hard = round(sd(cs_rate_hard), 2))

# Generate a "long" dataset for sex - accuracy (correct source)
sex_acc <- bhamm %>%
  mutate(s2_age_std = scale(s2_age, center = T, scale = T),
         cs_rate_easy_std = scale(cs_rate_easy, center = T, scale = T),
         cs_rate_hard_std = scale(cs_rate_hard, center = T, scale = T)) %>%
  mutate(s2_age_std = as.numeric(s2_age_std),
         cs_rate_easy_std = as.numeric(cs_rate_easy_std),
         cs_rate_hard_std = as.numeric(cs_rate_hard_std)) %>%
  select(id, s2_age, s2_age_std, sex, cs_rate_easy_std, cs_rate_hard_std) %>%
  pivot_longer(!c(id, s2_age, s2_age_std, sex), names_to = "task", values_to = "cs_rate") %>%
  mutate(id = factor(id))

# Remove unnecessary text in task variable
sex_acc$task <- str_replace_all(sex_acc$task, "cs_rate_", "")
sex_acc$task <- str_replace_all(sex_acc$task, "_std", "")

# Convert task to factor with correct levels
sex_acc <- mutate(sex_acc, task = factor(task, levels = c("easy","hard")))

# Change coding of task and sex 
contrasts(sex_acc$task) <- contr.sum(2)
contrasts(sex_acc$sex) <- contr.sum(2)

# Run a linear mixed effects model examining the effect of task (easy, hard), 
# sex (male, female), and age (standardized) on correct source, and then generate a summary
lmm_sex_acc <- lmerTest::lmer(cs_rate ~ task * sex * s2_age_std + (1|id), data = sex_acc)
anova(lmm_sex_acc)

# Examine significance of age trends for males and females
emtrends(lmm_sex_acc, pairwise ~ "sex", var = "s2_age_std") %>% test()

# Generate predictions for the two-way interaction derived from menopause model
pred_sex_acc <- ggeffects::ggpredict(lmm_sex_acc, terms = c( "s2_age_std", "sex")) %>%
  rename(s2_age_std = x, cs_rate = predicted, sex = group)

# Plot the predictions for the sex model
set.seed(123) # make reproducible
sexAge_acc_fig <- ggplot(pred_sex_acc, aes(x = s2_age_std, y = cs_rate, colour = sex)) +
  geom_line(aes(x = s2_age_std, y = cs_rate, colour = sex)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = sex), alpha = 0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", colour = "black", size = 11),
        axis.text.y = element_text(family = "Arial", colour = "black", size = 11),
        axis.title.x = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        axis.title.y = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        legend.text = element_text(family = "Arial", colour = "black", size = 11),
        legend.title = element_blank(), 
        legend.position = "top") +
  scale_y_continuous(name = "Retrieval Accuracy (Standardized)", limits = c(-2, 2), 
                     oob = squish, breaks = seq(-2, 2, by = .5), labels = seq(-2, 2, by = .5)) +
  scale_x_continuous(name = "Age (Standardized)") +
  scale_colour_manual(values = c("#0072B2", "#D55E00")) +
  scale_fill_manual(values = c("#0072B2", "#D55E00")) 

# Print figure
sexAge_acc_fig

# Export figure
ggsave("output-file.svg", sexAge_acc_fig, width = 4, height = 4, dpi = 300)

### Menopause ###

# Calculate source correct stats for each group (menopause)
meno %>%
  group_by(s2_meno_group) %>%
  summarise(mean_easy = round(mean(cs_rate_easy), 2),
            sd_easy = round(sd(cs_rate_easy), 2),
            mean_hard = round(mean(cs_rate_hard), 2),
            sd_hard = round(sd(cs_rate_hard), 2))

# Amend the menopause status variable so it contains numbers instead of labels.
# Note: this is done for PLS regression
meno <- mutate(meno, s2_meno_num = case_when(s2_meno_group == "Premenopause" ~ -1,
                                             s2_meno_group == "Postmenopause" ~ 1))

# Create data frame, x_acc, that contains the IVs for the accuracy analysis: menopause 
# status and age
x_acc <- meno %>% select(s2_meno_num, s2_age)

# Standardize age
x_acc <- x_acc %>%
  mutate(s2_age_std = scale(s2_age, center = TRUE, scale = TRUE)) %>%
  mutate(s2_age_std = as.numeric(s2_age_std)) %>%
  select(-s2_age)

# Create data framef, y_acc, that contains the DVs for the analysis: accuracy and RTs
y_acc <- meno %>% select(cs_rate_easy, cs_rate_hard, rt_cs_easy, rt_cs_hard)

# Standardize all DVs
y_acc <- y_acc %>%
  mutate_if(is.numeric, ~scale(., center = TRUE, scale = TRUE)) %>%
  mutate_if(is.numeric, ~as.numeric(.))

# Run PLS regression analysis
set.seed(123) # make reproducible
plsreg2_acc <- plsdepot::plsreg2(predictors = x_acc, responses = y_acc, 
                                 comps = NULL, crosval = TRUE)

# Examine Q2 values
plsreg2_acc$Q2

# Examine variance explained
plsreg2_acc$expvar

# Examine correlations for dependent variables used in circle correlation plot
plsreg2_acc$cor.yt

# Examine correlations for independent variables used in circle correlation plot
plsreg2_acc$cor.xt

# Generate a data frame that contains the correlation values from the PLS regression analysis
pls_acc_corrs <- data.frame(var_name = c("Menopause", "Age", "Acc - Easy", "Acc - Hard",
                                         "RT - Easy", "RT - Hard"),
                            var_type = c("IV", "IV", "DV - Easy", "DV - Hard", 
                                         "DV - Easy", "DV - Hard"),
                            corr = c(-0.9749883, -0.9780871, 0.5198710, 0.4778551, 
                                     -0.4443186, -0.4135703))

# Amend the data frame so that variables are in their correct form
pls_acc_corrs <- pls_acc_corrs %>%
  mutate(var_name = factor(var_name, levels = c("Menopause", "Age", "Acc - Easy", 
                                                "Acc - Hard", "RT - Easy", "RT - Hard")),
         var_type = factor(var_type, levels = c("IV", "DV - Easy", "DV - Hard")))

# Generate a figure based on the correlation values
pls_acc_corrs_fig <- ggplot(pls_acc_corrs, 
                            aes(x = var_name, y = corr, fill = var_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", colour = "black", size = 11),
        axis.text.y = element_text(family = "Arial", colour = "black", size = 11),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "Arial", face = "bold", size = 11),
        strip.text.x = element_text(family = "Arial", colour = "black", size = 11),
        legend.text = element_text(family = "Arial", colour = "black", size = 11),
        legend.title = element_blank(),
        legend.position = "None") +
  scale_y_continuous(name = "Behavior-LV Correlations", limits = c(-1, 1),
                     breaks = seq(from = -1, to = 1, by = 0.2),
                     labels = c(seq(from = -1, to = 1, by = 0.2))) +
  scale_colour_manual(values = c("#698BAB", "#E0BBE4", "#957DAD")) +
  scale_fill_manual(values = c("#698BAB", "#E0BBE4", "#957DAD"))

# Print the figure
pls_acc_corrs_fig

# Export figure
ggsave("output-file.svg", pls_acc_corrs_fig, width = 8, height = 4, dpi = 300)

# Generate a "long" dataset for menopause.
meno_acc <- meno %>%
  filter(sex == "Female") %>%
  mutate(s2_age_std = scale(s2_age, center = TRUE, scale = TRUE),
         cs_rate_easy_std = scale(cs_rate_easy, center = TRUE, scale = TRUE),
         cs_rate_hard_std = scale(cs_rate_hard, center = TRUE, scale = TRUE)) %>%
  mutate(s2_age_std = as.numeric(s2_age_std),
         cs_rate_easy_std = as.numeric(cs_rate_easy_std),
         cs_rate_hard_std = as.numeric(cs_rate_hard_std)) %>%
  select(id, sex, s2_age, s2_age_std, s2_meno_group, cs_rate_easy_std, cs_rate_hard_std) %>%
  pivot_longer(!c(id, sex, s2_age, s2_age_std, s2_meno_group), names_to = "task", values_to = "cs_rate") %>%
  mutate(id = factor(id))

# Remove unnecessary text from task variable
meno_acc$task <- str_replace_all(meno_acc$task, "cs_rate_", "")
meno_acc$task <- str_replace_all(meno_acc$task, "_std", "")

# Convert task variable to factor
meno_acc <- mutate(meno_acc, task = factor(task, levels = c("easy","hard")))

# Split the long dataset so there's one for pre-menopausal females and one for 
# post-menopausal females
pre_acc <- filter(meno_acc, s2_meno_group == "Premenopause")
post_acc <- filter(meno_acc, s2_meno_group == "Postmenopause")

# Amend the coding scheme used for task
contrasts(pre_acc$task) <- contr.sum(2)
contrasts(post_acc$task) <- contr.sum(2)

# Run a linear mixed effects model examining the effect of task (easy, hard) and 
# age (standardized) on correct source in pre-menopausal females, and then generate a summary
lmm_pre_acc <- lmerTest::lmer(cs_rate ~ task * s2_age_std + (1|id), data = pre_acc)
anova(lmm_pre_acc)

# Generate predictions for age and task
pred_pre_acc <- ggeffects::ggpredict(lmm_pre_acc, terms = c( "s2_age_std", "task")) %>% 
  rename(s2_age_std = x, cs_rate = predicted, task = group)

# Plot the predictions for the pre-menopause model
set.seed(123) # make reproducible
preAge_acc_fig <- pred_pre_acc %>%
  ggplot(aes(x = s2_age_std, y = cs_rate, colour = task)) +
  geom_line(aes(x = s2_age_std, y = cs_rate, colour = task)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = task), alpha = 0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", colour = "black", size = 11),
        axis.text.y = element_text(family = "Arial", colour = "black", size = 11),
        axis.title.x = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        axis.title.y = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        plot.title = element_text(family = "Arial", face = "bold", colour = "black", size = 14, hjust = 0.5),
        legend.text = element_text(family = "Arial", colour = "black", size = 11),
        legend.title = element_blank(), 
        legend.position = "top") +
  scale_y_continuous(name = "Retrieval Accuracy (Standardized)", limits = c(-2, 2), 
                     oob = squish, breaks = seq(-2, 2, by = .5), labels = seq(-2, 2, by = .5)) +
  scale_x_continuous(name = "Age (Standardized)") +
  ggtitle("Pre-menopause") +
  scale_colour_manual(values = c("#E0BBE4", "#957DAD"), labels = c("Easy", "Hard")) +
  scale_fill_manual(values = c("#E0BBE4", "#957DAD"), labels = c("Easy", "Hard"))

# Print figure
preAge_acc_fig

# Export figure
ggsave("output-file.svg", preAge_acc_fig, width = 4, height = 4, dpi = 300)

# Run a linear mixed effects model examining the effect of task (easy, hard) and 
# age (standardized) on correct source in post-menopausal females, and then generate a summary
lmm_post_acc <- lmerTest::lmer(cs_rate ~ task * s2_age_std + (1|id), data = post_acc)
anova(lmm_post_acc)

# Examine age trend
emtrends(lmm_post_acc, ~ 1, var = "s2_age_std") %>% test()

# Generate predictions for age and task
pred_post_acc <- ggeffects::ggpredict(lmm_post_acc, terms = c( "s2_age_std", "task")) %>% 
  rename(s2_age_std = x, cs_rate = predicted, task = group)

# Plot the predictions for the post-menopause model
set.seed(123) # make reproducible
postAge_acc_fig <- pred_post_acc %>%
  ggplot(aes(x = s2_age_std, y = cs_rate, colour = task)) +
  geom_line(aes(x = s2_age_std, y = cs_rate, colour = task)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = task), alpha = 0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Arial", colour = "black", size = 11),
        axis.text.y = element_text(family = "Arial", colour = "black", size = 11),
        axis.title.x = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        axis.title.y = element_text(family = "Arial", face = "bold", colour = "black", size = 11),
        plot.title = element_text(family = "Arial", face = "bold", colour = "black", size = 14, hjust = 0.5),
        legend.text = element_text(family = "Arial", colour = "black", size = 11),
        legend.title = element_blank(), 
        legend.position = "top") +
  scale_y_continuous(name = "Retrieval Accuracy (Standardized)", limits = c(-2, 2), 
                     oob = squish, breaks = seq(-2, 2, by = .5), labels = seq(-2, 2, by = .5)) +
  scale_x_continuous(name = "Age (Standardized)") +
  ggtitle("Post-menopause") +
  scale_colour_manual(values = c("#E0BBE4", "#957DAD"), labels = c("Easy", "Hard")) +
  scale_fill_manual(values = c("#E0BBE4", "#957DAD"), labels = c("Easy", "Hard"))

# Print figure 
postAge_acc_fig

# Export figure
ggsave("output-file.svg", postAge_acc_fig, width = 4, height = 4, dpi = 300)


## RTs ----

### Sex ###

# Calculate source correct RT stats for each group (sex)
bhamm %>%
  group_by(sex) %>%
  summarise(mean_easy = round(mean(rt_cs_easy), 2),
            sd_easy = round(sd(rt_cs_easy), 2),
            mean_hard = round(mean(rt_cs_hard), 2),
            sd_hard = round(sd(rt_cs_hard), 2))

# Generate a "long" dataset for sex
sex_rt <- bhamm %>%
  mutate(s2_age_std = scale(s2_age, center = T, scale = T),
         rt_cs_easy_std = scale(rt_cs_easy, center = T, scale = T),
         rt_cs_hard_std = scale(rt_cs_hard, center = T, scale = T)) %>%
  mutate(s2_age_std = as.numeric(s2_age_std),
         rt_cs_easy_std = as.numeric(rt_cs_easy_std),
         rt_cs_hard_std = as.numeric(rt_cs_hard_std)) %>%
  select(id, s2_age, s2_age_std, sex, rt_cs_easy_std, rt_cs_hard_std) %>%
  pivot_longer(!c(id, s2_age, s2_age_std, sex), names_to = "task", values_to = "rt_cs") %>%
  mutate(id = factor(id))

# Remove unnecessary text from task variable
sex_rt$task <- str_replace_all(sex_rt$task, "rt_cs_", "")
sex_rt$task <- str_replace_all(sex_rt$task, "_std", "")

# Convert task variable to factor
sex_rt <- mutate(sex_rt, task = factor(task, levels = c("easy","hard")))

# Change coding to sum
contrasts(sex_rt$task) <- contr.sum(2)
contrasts(sex_rt$sex) <- contr.sum(2)

# Run a linear mixed effects model examining the effect of task (easy, hard), 
# sex (male, femmale), and age (standardized) on correct source RTs, and then generate a summary
lmm_sex_rt <- lmerTest::lmer(rt_cs ~ task * sex * s2_age_std + (1|id), data = sex_rt)
anova(lmm_sex_rt)

# Examine significcane of age trends for sex
emtrends(lmm_sex_rt, pairwise ~ "sex", var = "s2_age_std") %>% test()

### Menopause ###

# Calculate source correct RT stats for each group (menopause)
meno %>%
  group_by(s2_meno_group) %>%
  summarise(mean_easy = round(mean(rt_cs_easy), 2),
            sd_easy = round(sd(rt_cs_easy), 2),
            mean_hard = round(mean(rt_cs_hard), 2),
            sd_hard = round(sd(rt_cs_hard), 2))

# Generate a "long" dataset for menopause
meno_rt <- meno %>%
  mutate(s2_age_std = scale(s2_age, center = T, scale = T),
         rt_cs_easy_std = scale(rt_cs_easy, center = T, scale = T),
         rt_cs_hard_std = scale(rt_cs_hard, center = T, scale = T)) %>%
  mutate(s2_age_std = as.numeric(s2_age_std),
         rt_cs_easy_std = as.numeric(rt_cs_easy_std),
         rt_cs_hard_std = as.numeric(rt_cs_hard_std)) %>%
  select(id, sex, s2_age, s2_age_std, s2_meno_group, rt_cs_easy_std, rt_cs_hard_std) %>%
  pivot_longer(!c(id, sex, s2_age, s2_age_std, s2_meno_group), names_to = "task", values_to = "rt_cs") %>%
  mutate(id = factor(id))

# Remove unnecessary text from task variable
meno_rt$task <- str_replace_all(meno_rt$task, "rt_cs_", "")
meno_rt$task <- str_replace_all(meno_rt$task, "_std", "")

# Convert task variable to factor
meno_rt <- mutate(meno_rt, task = factor(task, levels = c("easy","hard")))

# Split the long dataset so there's one for pre-menopausal females and one for 
# post-menopausal females
pre_rt <- filter(meno_rt, s2_meno_group == "Premenopause")
post_rt <- filter(meno_rt, s2_meno_group == "Postmenopause")

# Amend the coding scheme used for task
contrasts(pre_rt$task) <- contr.sum(2)
contrasts(post_rt$task) <- contr.sum(2)

# Run a linear mixed effects model examining the effect of task (easy, hard) and 
# age (standardized) on correct source RTs in pre-menopausal females, and then generate a summary
lmm_pre_rt <- lmerTest::lmer(rt_cs ~ task * s2_age_std + (1|id), data = pre_rt)
anova(lmm_pre_rt)

# Run a linear mixed effects model examining the effect of task (easy, hard) and 
# age (standardized) on correct source RTs in post-menopausal females, and then generate a summary
lmm_post_rt <- lmerTest::lmer(rt_cs ~ task * s2_age_std + (1|id), data = post_rt)
anova(lmm_post_rt)


# Supplementary Analyses --------------------------------------------------

# Create a new data frame containing age-matched males and females. Start by 
# amending the factor levels for `sex`, then perform optimal pair matching. Use 
# the outputs to generate the matched sample
supp_data <- bhamm %>%
  mutate(sex = case_when(
    sex == "Female" ~ 1,
    sex == "Male" ~ 0)) %>%
  mutate(sex = factor(sex))

set.seed(123) # make reproducible
supp_data_match <- matchit(sex ~ s2_age + edu, data = supp_data, method = "optimal")
supp_data_matched <- match.data(supp_data_match) 

# For readability, re-convert sex to named variable
supp_data_matched <- supp_data_matched %>%
  mutate(sex = case_when(
    sex == 1 ~ "Female",
    sex == 0 ~ "Male")) %>%
  mutate(sex = factor(sex))

# Count the number of males and females, separating out the latter by menopause status
supp_data_matched %>% 
  count(sex, s2_meno_group) # males (n = 30), females (n = 30; 8 pre, 22 post)

# Change contrasts for sex
contrasts(supp_data_matched$sex) <- contr.sum(2)


## Demographic Data ----

### Age (Years) ###

# Convert age to numeric
supp_data_matched <- mutate(supp_data_matched, s2_age = as.numeric(s2_age))

# For males and females, calculate age statistics
supp_data_matched %>%
  group_by(sex) %>%
  summarise(mean = round(mean(s2_age), 2),
            sd = round(sd(s2_age), 2),
            min = round(min(s2_age), 2),
            max = round(max(s2_age), 2))

# Conduct Welch's t-test to examine differences in age by sex
t.test(s2_age ~ sex, data = supp_data_matched)

# Calculate cohen's d for sex differences in age
cohens_d(s2_age ~ sex, data = supp_data_matched)

# Run a two-sides JZS Bayes Factor (BF) test for sex differences in age
ttestBF(formula = s2_age ~ sex, data = supp_data_matched)


### Education (Years) ###

# Convert years of education to integer
supp_data_matched <- mutate(supp_data_matched, edu = as.integer(edu))

# For males and females, calculate education statistics
supp_data_matched %>%
  group_by(sex) %>%
  summarise(mean = round(mean(edu), 2),
            sd = round(sd(edu), 2),
            min = min(edu),
            max = max(edu))

# Conduct Welch's t-test to examine differences in education by sex
t.test(edu ~ sex, data = supp_data_matched)

# Calculate cohen's d for sex differences in education
cohens_d(edu ~ sex, data = supp_data_matched)

# Run a two-sides JZS Bayes Factor (BF) test for sex differences in education
ttestBF(formula = edu ~ sex, data = supp_data_matched)


## Accuracy ----

# Calculate source correct stats for each group (sex)
supp_data_matched %>%
  group_by(sex) %>%
  summarise(mean_easy = round(mean(cs_rate_easy), 2),
            sd_easy = round(sd(cs_rate_easy), 2),
            mean_hard = round(mean(cs_rate_hard), 2),
            sd_hard = round(sd(cs_rate_hard), 2))

# Generate a "long" dataset for sex - accuracy (correct source)
supp_acc <- supp_data_matched %>%
  mutate(s2_age_std = scale(s2_age, center = T, scale = T),
         cs_rate_easy_std = scale(cs_rate_easy, center = T, scale = T),
         cs_rate_hard_std = scale(cs_rate_hard, center = T, scale = T)) %>%
  mutate(s2_age_std = as.numeric(s2_age_std),
         cs_rate_easy_std = as.numeric(cs_rate_easy_std),
         cs_rate_hard_std = as.numeric(cs_rate_hard_std)) %>%
  select(id, s2_age, s2_age_std, sex, cs_rate_easy_std, cs_rate_hard_std) %>%
  pivot_longer(!c(id, s2_age, s2_age_std, sex), names_to = "task", values_to = "cs_rate") %>%
  mutate(id = factor(id))

# Remove unnecessary text in task variable
supp_acc$task <- str_replace_all(supp_acc$task, "cs_rate_", "")
supp_acc$task <- str_replace_all(supp_acc$task, "_std", "")

# Convert task to factor with correct levels
supp_acc <- mutate(supp_acc, task = factor(task, levels = c("easy","hard")))

# Change coding of task and sex 
contrasts(supp_acc$task) <- contr.sum(2)
contrasts(supp_acc$sex) <- contr.sum(2)

# Run a linear mixed effects model examining the effect of task (easy, hard), sex 
# (male, female), and age (standardized) on correct source, and then generate a summary
lmm_supp_acc <- lmerTest::lmer(cs_rate ~ task * sex * s2_age_std + (1|id), data = supp_acc)
anova(lmm_supp_acc)

# Examine estimated marginal means for easy and hard task performance, independent of sex and age
emmeans(lmm_supp_acc, ~ task)

# Examine significance of age trends for males and females
emtrends(lmm_supp_acc, pairwise ~ "sex", var = "s2_age_std") %>% test()


## RTs ----

# Calculate source correct RT stats for each group (sex)
supp_data_matched %>%
  group_by(sex) %>%
  summarise(mean_easy = round(mean(rt_cs_easy), 2),
            sd_easy = round(sd(rt_cs_easy), 2),
            mean_hard = round(mean(rt_cs_hard), 2),
            sd_hard = round(sd(rt_cs_hard), 2))

# Generate a "long" dataset for sex - RTs (correct source)
supp_rt <- supp_data_matched %>%
  mutate(s2_age_std = scale(s2_age, center = T, scale = T),
         rt_cs_easy_std = scale(rt_cs_easy, center = T, scale = T),
         rt_cs_hard_std = scale(rt_cs_hard, center = T, scale = T)) %>%
  mutate(s2_age_std = as.numeric(s2_age_std),
         rt_cs_easy_std = as.numeric(rt_cs_easy_std),
         rt_cs_hard_std = as.numeric(rt_cs_hard_std)) %>%
  select(id, s2_age, s2_age_std, sex, rt_cs_easy_std, rt_cs_hard_std) %>%
  pivot_longer(!c(id, s2_age, s2_age_std, sex), names_to = "task", values_to = "rt_cs") %>%
  mutate(id = factor(id))

# Remove unnecessary text from task variable
supp_rt$task <- str_replace_all(supp_rt$task, "rt_cs_", "")
supp_rt$task <- str_replace_all(supp_rt$task, "_std", "")

# Convert task to factor
supp_rt <- mutate(supp_rt, task = factor(task, levels = c("easy","hard")))

# Change coding to sum
contrasts(supp_rt$task) <- contr.sum(2)
contrasts(supp_rt$sex) <- contr.sum(2)

# Run a linear mixed effects model examining the effect of task (easy, hard), sex 
# (male, femmale), and age (standardized) on correct source RTs, and then generate a summary
lmm_supp_rt <- lmerTest::lmer(rt_cs ~ task * sex * s2_age_std + (1|id), data = supp_rt)
anova(lmm_supp_rt)

# Examine significance of age trends for males and females.
emtrends(lmm_supp_rt, pairwise ~ "sex", var = "s2_age_std") %>% test()
