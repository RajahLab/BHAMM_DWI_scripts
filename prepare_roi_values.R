# Name: prepare_roi_values.R
# Author: Rikki Lissaman
# Last updated (dd/mm/yyyy): 02/01/2024
#
# Description: This R script contains code that imports ROI-specific FA and MD
# values (derived from TBSS skeleton), amends/prepares the data for later analysis, 
# and then exports this data.
#
# Inputs: 
# (1) Text file containing a list of ROI "mask" names, obtained from the JHU ICBM
# DTI-81 Atlas.
# (2) Text files containing IDs of males and females, as well as pre- and post-
# menopausal females.
# (3) Text files (saved in a sub-directory) containing FA/MD values for each ROI.
#
# Outputs: 
# (1) Text files containing ROI (n = 21) x metric (n = 2; FA/MD) data for sex 
# and menopause (total files = 4).
#
# Note(s):
# (1) File names have been replaced with placeholder text.


# Load Packages -----------------------------------------------------------

# Load all required packages
library(haven) # version 2.5.0
library(readxl) # version 1.4.0
library(readr) # version 2.1.2
library(janitor) # version 2.1.0
library(tidyr) # version 1.2.0
library(dplyr) # version 1.0.8


# Import Mask List and IDs ------------------------------------------------

# Import mask list
mask_names <- read_tsv("path/input-file-name.txt", col_names = FALSE) %>% 
  rename(mask = X1)

# Store mask list as a character vector
mask_names <- mask_names$mask

# Import IDs for females
female <- read_tsv("path/input-file-name.txt", col_names = FALSE) %>% 
  rename(id = X1)

# Import IDs for males
male <- read_tsv("path/input-file-name.txt", col_names = FALSE) %>% 
  rename(id = X1)

# Combine male and female IDs into one data frame (note: female comes first as
# this is the order used in TBSS)
sex_ids <- rbind(female, male)

# Import IDs for pre-menopausal females
pre <- read_tsv("path/input-file-name.txt", col_names = FALSE) %>% 
  rename(id = X1)

# Import IDs for post-menopausal females
post <- read_tsv("path/input-file-name.txt", col_names = FALSE) %>% 
  rename(id = X1)

# Combine pre- and post-menopausal IDs into one data frame (note: pre comes first
# as this is the order used in TBSS)
meno_ids <- rbind(pre, post)


# Generate Data Frames with ROI FA/MD Values ------------------------------

# Create a function that generates a data frame for FA or MD, separated by 
# sex or menopause status. The argument `file_path` should point to a directory
# for FA or MD, which should itself be a sub-directory of sex or menopause 
# (i.e., the path should look like this: /path/to/data/sex/FA/). Within the
# directory, there should be text files, one for each ROI-metric combo. The  
# argument `group` should be "sex" or "meno" depending on which you are examining. 
# Note: the function will only run if mask_names and IDs have been set-up as outlined 
# above
generate_roi_metric_vals <- function(file_path, group) {
  # List all text files in provided path 
  metric <- list.files(file_path, pattern = "*txt", full.names = TRUE)
  # Apply function, read.table, to list
  data_metric <- lapply(metric, read.table, header = FALSE, sep = "")
  # Convert to data frame and combine
  data_metric <- do.call("cbind", lapply(data_metric, as.data.frame))
  # Replace column names with ROI names (match as based on alphabetical order)
  colnames(data_metric) <- mask_names
  # "Clean" names so they are all lower-case etc
  data_metric <-janitor::clean_names(data_metric)
  # Replace 
  if (group == "sex"){
    # Add ID column for sex
    data_metric$id <- sex_ids$id
    # Add group column 
    data_metric$group <- rep(c("female", "male"),
                             times = c(length(female$id), length(male$id)))
    # Move ID and group to the front of the data frame
    data_metric <- relocate(data_metric, id, group)
  } else if (group == "meno"){
    # Add ID column for meno
    data_metric$id <- meno_ids$id
    # Add group column 
    data_metric$group <- rep(c("premeno", "postmeno"),
                             times = c(length(pre$id), length(post$id)))
    # Move ID and group to the front of the data frame
    data_metric <- relocate(data_metric, id, group)
  } else {
    print("Error occurred - group incorrectly enterred")
  }
  # Remove unwanted ROIs, generate bilateral ROIs and drop unilateral ROIs
  data_metric <- data_metric %>%
    select(-c("mcp", "pct", "cst_l", "cst_r", "ml_l", "ml_r", 
              "icp_l", "icp_r", "scp_l", "scp_r")) %>%
    mutate(id = as.character(id)) %>%
    mutate(acr = (acr_l + acr_r) / 2,
           alic = (alic_l + alic_r) / 2,
           cgc = (cgc_l + cgc_r) / 2,
           cgh = (cgh_l + cgh_r) / 2,
           cp = (cp_l + cp_r) / 2,
           ec = (ec_l + ec_r) / 2,
           fx_st = (fx_st_l + fx_st_r) / 2,
           pcr = (pcr_l + pcr_r) / 2,
           plic = (plic_l + plic_r) / 2,
           ptr = (ptr_l + ptr_r) / 2,
           rlic = (rlic_l + rlic_r) / 2,
           scr = (scr_l + scr_r) / 2,
           sfo = (sfo_l + sfo_r) / 2,
           slf = (slf_l + slf_r) / 2,
           ss = (ss_l + ss_r) / 2,
           tap = (tap_l + tap_r) / 2,
           unc = (unc_l + unc_r) / 2) %>%
    select(-ends_with("_r")) %>%
    select(-ends_with("_l"))
  # Re-order column names alphabetically so acr = first and unc = last
  data_metric <- data_metric %>% select(order(colnames(data_metric)))
  # Move ID and group back to the first and second columns
  data_metric <- relocate(data_metric, id, group)
  # Return final data frame
  return(data_metric)
}

# Generate a data frame containing FA data for males and females
data_sex_fa <- generate_roi_metric_vals(file_path = "path/sex/FA", group = "sex")

# Generate a data frame containing MD data for males and females
data_sex_md <- generate_roi_metric_vals(file_path = "path/sex/MD", group = "sex")

# Generate a data frame containing FA data for pre- and post-menopausal females
data_meno_fa <- generate_roi_metric_vals(file_path = "path/meno/FA", group = "meno")

# Generate a data frame containing MD data for pre- and post-menopausal females
data_meno_md <- generate_roi_metric_vals(file_path = "path/meno/MD", group = "meno")


# Export ROI FA/MD Values ------------------------------------------------

# Export text file containing FA data for males and females
write_tsv(sex_fa, "path/sex/FA/bilatROIs-sex-fa.txt", col_names = TRUE)

# Export text file containing MD data for males and females
write_tsv(data_sex_md, "path/sex/MD/bilatROIs-sex-md.txt", col_names = TRUE)

# Export text file containing FA data for pre- and post-menopausal females
write_tsv(data_meno_fa, "path/meno/FA/bilatROIs-meno-fa.txt", col_names = TRUE)

# Export text file containing MD data for pre- and post-menopausal females
write_tsv(data_meno_md, "path/meno/MD/bilatROIs-meno-md.txt", col_names = TRUE)