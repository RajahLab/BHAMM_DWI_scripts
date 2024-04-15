# Name: prepare_behav_data.R
# Author: Rikki Lissaman
# Last updated (dd/mm/yyyy): 15/04/2024
#
# Description: This R script contains code that imports data from the Brain 
# Health and Memory at Midlife (BHAMM) study, filters out participants that do
# not meet the inclusion/exclusion criteria used for the current project, outputs
# key demographic information, and outputs IDs/data for later analysis.
#
# Inputs: 
# (1) SPSS file of the BHAMM Master database (Last updated: January 2024).
# (2) SPSS file of the BHAMM Session 2 (i.e., neuroimaging session) database 
# (Last updated: January 2024).
# (3) Excel spreadsheet used for DWI QC (Last updated: November 2023).
#
# Outputs: 
# (1) Text files containing IDs for males and females, as well as pre- and post-
# menopausal females.
# (2) SPSS file containing a filtered version of the BHAMM Master database.
#
# Note(s):
# (1) File names have been replaced with placeholder text.
# (2) Where IDs were used, they have been replaced with generic numbers (e.g.,
# 0001) as placeholders. The repeated use of these numbers does not indicate
# that the same participants were affected by different exclusions.
# (3) n = number of participants.


# Load Packages -----------------------------------------------------------

# Load all required packages
library(haven) # version 2.5.0
library(readxl) # version 1.4.0
library(readr) # version 2.1.2
library(janitor) # version 2.1.0
library(tidyr) # version 1.2.0
library(dplyr) # version 1.0.8


# Import and Combine BHAMM Databases --------------------------------------

# Import the BHAMM study Master database and "clean" the variable names (i.e., 
# make them all lower-case with spaces replaced by underscores, etc.)
master <- read_sav("input-file-name.sav") %>% clean_names() # n = 480

# Import the BHAMM Session 2 database, "clean" the variable names, and keep only
# variables that are already in the master database (note: the master database is 
# more comprehensive)
session_two <- read_sav("input-file-name.sav") %>% 
  clean_names() %>%
  select(c("id", "dw1_scan_num")) # n = 273

# Using participant ID, join together the two databases. Only those with Session 
# 2 data will be retained
bhamm <- left_join(session_two, master, by = "id") # n = 273


# Identify Initially Eligible Participants --------------------------------

# Filter the joined database by age, keeping only middle-aged participants. We
# used a threshold that was consistent with that used in our prior work - see
# https://doi.org/10.1523/JNEUROSCI.0663-23.2023
bhamm <- filter(bhamm, s2_age >= 39.5 & s2_age < 65.5) # n = 200

# Given our focus on sex and menopause, filter out participants whose current 
# gender identity does not match their sex assigned at birth. Note: for sex, 
# labels are "-1 [Male]" and "1 [Female]"; for gender, labels are "1 [Man]" and 
# "2 [Woman]"
bhamm <- bhamm %>% filter((sex == -1 & enrol_gender == 1) | 
                          (sex == -1 & is.na(enrol_gender)) | 
                          (sex == 1 & enrol_gender == 2) | 
                          (sex == 1 & is.na(enrol_gender))) # n = 199

# Filter out participants whose menopause status is listed as peri-menopausal 
# ("2 [Perimenopause]" under `s2_meno_group`) or indeterminate ("4 [Indeterminate]" 
# under `s2_meno_group`)
bhamm <- filter(bhamm, !s2_meno_group %in% c(2, 4)) # n = 148

# Filter out participants currently using hormone replacement therapy ("2 [Yes, 
# currently]" under `hrt_use`
bhamm <- filter(bhamm, !hrt_use %in% c(2)) # n = 138

# Filter out participants with diabetes ("-1 [Yes]" under `med_con_diabetes`)
bhamm <- filter(bhamm, !med_con_diabetes %in% c(-1)) # n = 137

# Filter out participants who, for various reasons (e.g., scan dropped from 
# sequence due to time, session cancelled, etc.) are listed as "NA"/NA for the 
# variable `dw1_scan_num`. These participants do not have diffusion data
bhamm <- filter(bhamm, !dw1_scan_num %in% c("NA", NA)) # n = 126

# Filter out participants scanned with old DWI parameters (checked separately)
bhamm <- filter(bhamm, !id %in% c(0001)) # n = 125

# Filter out participants who have been recruited since the manuscript was 
# originally submitted. This enables us to re-run our data on the amended dataset 
# without adding participants who were recruited post-submission
bhamm <- filter(bhamm, !id %in% c(0001, 0002, 0003, 0004, 
                                  0005, 0006, 0007, 0008)) # n = 117


# Obtain Demographic Data Prior to Further Exclusions ---------------------

## Overall Sample ----

# Calculate age statistics (min, max, M, SD)
bhamm %>% 
  summarise(mean = round(mean(s2_age), 2),
            sd = round(sd(s2_age), 2),
            min = round(min(s2_age), 2),
            max = round(max(s2_age), 2))

## Menopause (Pre/Post) ----

# Count the number of pre- and post-menopausal females in the sample
bhamm %>% 
  filter(sex == 1) %>%
  mutate(s2_meno_group = as_factor(s2_meno_group)) %>%
  count(s2_meno_group)

# Calculate age statistics (min, max, M, SD)
bhamm %>% 
  filter(sex == 1) %>%
  mutate(s2_meno_group = as_factor(s2_meno_group)) %>%
  group_by(s2_meno_group) %>%
  summarise(mean = round(mean(s2_age), 2),
            sd = round(sd(s2_age), 2),
            min = round(min(s2_age), 2),
            max = round(max(s2_age), 2))

# Calculate mean years of education
bhamm %>%
  filter(sex == 1) %>%
  mutate(s2_meno_group = as_factor(s2_meno_group)) %>%
  group_by(s2_meno_group) %>%
  summarise(mean = round(mean(edu), 2))

# Count the different ethnicities included
bhamm %>% 
  filter(sex == 1) %>%
  mutate(s2_meno_group = as_factor(s2_meno_group),
         ethnicity = as_factor(ethnicity)) %>%
  group_by(s2_meno_group) %>%
  count(ethnicity)

## Sex (Males/Females) ----

# Count the number of males and females in the sample
bhamm %>% 
  mutate(sex = as_factor(sex)) %>%
  count(sex)

# Calculate age statistics (min, max, M, SD)
bhamm %>% 
  mutate(sex = as_factor(sex)) %>%
  group_by(sex) %>%
  summarise(mean = round(mean(s2_age), 2),
            sd = round(sd(s2_age), 2),
            min = round(min(s2_age), 2),
            max = round(max(s2_age), 2))

# Calculate mean years of education
bhamm %>%
  mutate(sex = as_factor(sex)) %>%
  group_by(sex) %>%
  summarise(mean = round(mean(edu), 2))

# For males and females, count the different ethnicities included
bhamm %>% 
  mutate(sex = as_factor(sex),
         ethnicity = as_factor(ethnicity)) %>%
  group_by(sex) %>%
  count(ethnicity)


# Apply Further Exclusions ------------------------------------------------

# Filter out participants who are post-menopausal and currently using hormonal 
# birth control ("3 [Postmenopause]" under `s2_meno_group` and "-1 [Yes]" under 
# `current_hbc_med_use`). These participants were identified separately
bhamm <- filter(bhamm, !id %in% c(0001, 0002, 0003)) # n = 114

# Filter out participants who have a BMI of 40 or above
bhamm <- filter(bhamm, bmi < 40) # n 113

# Filter out participant who previously had a hysterectomy and a unilateral oophorectomy
bhamm <- filter(bhamm, 
                straw_issues_notes != "Hysterectomy, Unilateral oophorectomy") # n = 112

# Import the DWI QC data and "clean" the variable names
dwi_qc <- read_xlsx("input-file-name.xlsx") %>% clean_names() # n = 222

# Join together the BHAMM data and the DWI QC data 
bhamm <- left_join(bhamm, dwi_qc, by = c("id" = "participant")) # n = 112

# Filter out participants listed as DWI exclusions
bhamm <- filter(bhamm, dwi_exclude == "n") # n = 103

# For each group of interest (males, pre-menopausal females, post-menopausal 
# females) calculate age statistics, including upper and lower cut-offs for 
# outliers (i.e., M +/- 2.5*SD)
bhamm %>% 
  group_by(s2_meno_group) %>%
  summarise(mean = mean(s2_age),
            sd = sd(s2_age),
            min = min(s2_age),
            max = max(s2_age),
            ll_thresh = mean - (2.5*sd),
            ul_thresh = mean + (2.5*sd))

# Filter out participants who are above or below age cut-offs (checked separately)
bhamm <- filter(bhamm, !id %in% c(0001, 0002)) # n = 101

# Filter out participants who did not provide at least one source correct response 
# (across task versions)
bhamm <- filter(bhamm, cs_rate_easy != 0 & cs_rate_hard != 0) # n = 96

# Filter out participants who performed below chance in terms of correct rejections 
# (1/6 = 0.16)
bhamm <- filter(bhamm, cr_rate_easy >= 0.16 & cr_rate_hard >= 0.16) # n = 96

# Change task variable types to numeric
bhamm <- bhamm %>%
  mutate_at(vars(ends_with("easy")), as.numeric) %>%
  mutate_at(vars(ends_with("hard")), as.numeric) %>%
  mutate(s2_age = as.numeric(s2_age))

# Run models regressing age on correct source, recognition, and misses
lm_cs_easy <- lm(cs_rate_easy ~ s2_age, data = bhamm)
lm_cs_hard <- lm(cs_rate_hard ~ s2_age, data = bhamm)
lm_recog_easy <- lm(recog_rate_easy ~ s2_age, data = bhamm)
lm_recog_hard <- lm(recog_rate_hard ~ s2_age, data = bhamm)
lm_miss_easy <- lm(misses_rate_easy ~ s2_age, data = bhamm)
lm_miss_hard <- lm(misses_rate_hard ~ s2_age, data = bhamm)

# Calculate Cook's D for each regression model and store
cooks_d <- data.frame(id = bhamm$id, 
                      cs_easy_d = cooks.distance(lm_cs_easy),
                      cs_hard_d = cooks.distance(lm_cs_hard),
                      recog_easy_d = cooks.distance(lm_recog_easy),
                      recog_hard_d = cooks.distance(lm_recog_hard),
                      miss_easy_d = cooks.distance(lm_miss_easy),
                      miss_hard_d = cooks.distance(lm_miss_hard)) # n = 96

# For each measure (correct source, recognition, misses), determine whether a 
# given participant's Cook D value is more than 3 standard deviations higher than 
# the mean. After calculating these values, sum across the outlier columns
cooks_d <- cooks_d %>%
  mutate(cs_easy_outlier = case_when(cs_easy_d >= (mean(cs_easy_d) + 3*sd(cs_easy_d)) ~ 1,
                                     cs_easy_d < (mean(cs_easy_d) + 3*sd(cs_easy_d)) ~ 0),
         cs_hard_outlier = case_when(cs_hard_d >= (mean(cs_hard_d) + 3*sd(cs_hard_d)) ~ 1,
                                     cs_hard_d < (mean(cs_hard_d) + 3*sd(cs_hard_d)) ~ 0),
         recog_easy_outlier = case_when(recog_easy_d >= (mean(recog_easy_d) + 3*sd(recog_easy_d)) ~ 1,
                                        recog_easy_d < (mean(recog_easy_d) + 3*sd(recog_easy_d)) ~ 0),
         recog_hard_outlier = case_when(recog_hard_d >= (mean(recog_hard_d) + 3*sd(recog_hard_d)) ~ 1,
                                        recog_hard_d < (mean(recog_hard_d) + 3*sd(recog_hard_d)) ~ 0),
         miss_easy_outlier = case_when(miss_easy_d >= (mean(miss_easy_d) + 3*sd(miss_easy_d)) ~ 1,
                                       miss_easy_d < (mean(miss_easy_d) + 3*sd(miss_easy_d)) ~ 0),
         miss_hard_outlier = case_when(miss_hard_d >= (mean(miss_hard_d) + 3*sd(miss_hard_d)) ~ 1,
                                       miss_hard_d < (mean(miss_hard_d) + 3*sd(miss_hard_d)) ~ 0)) %>%
  mutate(total_outliers = rowSums(across(ends_with("outlier")))) 

# Determine whether there are any participants deemed to be outliers on 3 or more 
# measures
cooks_d %>% count(total_outliers >= 3) # n = 96


# Output Files for Analysis -----------------------------------------------

# Separate and store IDs for the primary groups of interest
male_ids <- bhamm %>% filter(sex == -1) %>% select(id) # n = 30
female_ids <- bhamm %>% filter(sex == 1) %>% select(id) # n = 66
pre_ids <- bhamm %>% filter(s2_meno_group == 1) %>% select(id) # n = 32
post_ids <- bhamm %>% filter(s2_meno_group == 3) %>% select(id) # n = 34

# Write out the IDs as separate text files
write_tsv(male_ids, "output-file-name.txt", col_names = FALSE)
write_tsv(female_ids, "output-file-name.txt", col_names = FALSE)
write_tsv(pre_ids, "output-file-name.txt", col_names = FALSE)
write_tsv(post_ids, "output-file-name.txt", col_names = FALSE)

# Write out the filtered data as an SPSS file for later analysis
write_sav(bhamm, "output-file-name.sav") 
