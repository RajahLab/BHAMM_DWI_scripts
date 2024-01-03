#!/bin/bash

# Name: dwi_extract_roi_values.sh 
# Author: Rikki Lissaman
# Last updated (dd/mm/yyyy): 02/01/2024
#
# Description: This Bash script provides an overview of the steps conducted to extract ROI-specific measures of FA and MD.
#
# Inputs:
# 	(1) JHU ICBM labels atlas.
#	(2) 2 all_FA_skeletonised images generated by TBSS, one for the sex analysis and one for the menopause analysis.
#	(3) 2 all_MD_skeletonised images generated by TBSS, one for the sex analysis and one for the menopause analysis.
# Outputs:
# 	(1) 48 binarized JHU ROI masks.
#	(2) 48 text files containing mean FA values, one per ROI, for all participants in the sex analysis.
#	(3) 48 text files containing mean FA values, one per ROI, for all participants in the menopause analysis.
#	(4) 48 text files containing mean MD values, one per ROI, for all participants in the sex analysis.
#	(5) 48 text files containing mean MD values, one per ROI, for all participants in the menopause analysis.
#
# Notes:
# 	(1) Although the script is written as though all steps were performed in one loop, in reality they were run separately (one-at-a-time). The inclusion of all steps in one script is for readability.
# 	(2) Software required: FSL (version 6.0.2).
#	(3) Filenames and directories have been replaced with placeholder text.

# Set home directory
home_dir='/path/to/home'

# For loop containing commands used (note: in reality, each step was run separately)
for subj in `cat ${home_dir}/subjects.txt`; do

# Create empty directory 
mkdir ${home_dir}/jhu-rois

# Copy JHU atlas into directory
cp JHU-ICBM_labels-1mm.nii.gz ${home_dir}/jhu-rois/JHU-ICBM-labels-1mm.nii.gz

# Move into the directory
cd ${home_dir}/jhu-rois 

# Separate the mask into individual ROIs (1-48) and binarize
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 1 -uthr 1 -bin MCP_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 2 -uthr 2 -bin PCT_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 3 -uthr 3 -bin GCC_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 4 -uthr 4 -bin BCC_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 5 -uthr 5 -bin SCC_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 6 -uthr 6 -bin FX_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 7 -uthr 7 -bin CST-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 8 -uthr 8 -bin CST-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 9 -uthr 9 -bin ML-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 10 -uthr 10 -bin ML-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 11 -uthr 11 -bin ICP-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 12 -uthr 12 -bin ICP-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 13 -uthr 13 -bin SCP-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 14 -uthr 14 -bin SCP-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 15 -uthr 15 -bin CP-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 16 -uthr 16 -bin CP-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 17 -uthr 17 -bin ALIC-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 18 -uthr 18 -bin ALIC-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 19 -uthr 19 -bin PLIC-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 20 -uthr 20 -bin PLIC-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 21 -uthr 21 -bin RLIC-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 22 -uthr 22 -bin RLIC-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 23 -uthr 23 -bin ACR-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 24 -uthr 24 -bin ACR-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 25 -uthr 25 -bin SCR-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 26 -uthr 26 -bin SCR-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 27 -uthr 27 -bin PCR-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 28 -uthr 28 -bin PCR-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 29 -uthr 29 -bin PTR-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 30 -uthr 30 -bin PTR-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 31 -uthr 31 -bin SS-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 32 -uthr 32 -bin SS-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 33 -uthr 33 -bin EC-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 34 -uthr 34 -bin EC-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 35 -uthr 35 -bin CGC-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 36 -uthr 36 -bin CGC-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 37 -uthr 37 -bin CGH-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 38 -uthr 38 -bin CGH-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 39 -uthr 39 -bin FX-ST-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 40 -uthr 40 -bin FX-ST-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 41 -uthr 41 -bin SLF-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 42 -uthr 42 -bin SLF-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 43 -uthr 43 -bin SFO-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 44 -uthr 44 -bin SFO-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 45 -uthr 45 -bin UNC-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 46 -uthr 46 -bin UNC-L_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 47 -uthr 47 -bin TAP-R_binary_mask.nii.gz
fslmaths JHU-ICBM-labels-1mm.nii.gz -thr 48 -uthr 48 -bin TAP-L_binary_mask.nii.gz

# Create a directory to store values
mkdir values

# Within values, create separate directories for sex and menopause
mkdir /values/sex
mkdir /values/meno

# Copy all_FA_skeletonised from each analysis (sex and meno) to the relevant directory
cp {$home_dir}/sex_analysis/all_FA_skeletonised.nii.gz /values/sex/sex_all_FA_skeletonised.nii.gz
cp ${home_dir}/meno_analysis/all_FA_skeletonised.nii.gz /values/meno/meno_all_FA_skeletonised.nii.gz

# Copy all_MD_skeletonised from each analysis (sex and meno) to the relevant directory
cp {$home_dir}/sex_analysis/all_MD_skeletonised.nii.gz /values/sex/sex_all_MD_skeletonised.nii.gz
cp ${home_dir}/meno_analysis/all_MD_skeletonised.nii.gz /values/meno/meno_all_MD_skeletonised.nii.gz

# Within each analysis directory, create sub-directories for FA and MD
mdkir /values/sex/FA
mdkir /values/sex/MD
mdkir /values/meno/FA
mdkir /values/meno/MD

# Produce text files containing mean FA values for each ROI, per participant (sex analysis)
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k ACR-L_binary_mask.nii.gz -M > /values/sex/FA/ACR-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k ACR-R_binary_mask.nii.gz -M > /values/sex/FA/ACR-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k ALIC-L_binary_mask.nii.gz -M > /values/sex/FA/ALIC-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k ALIC-R_binary_mask.nii.gz -M > /values/sex/FA/ALIC-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k BCC_binary_mask.nii.gz -M > /values/sex/FA/BCC_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k CGC-L_binary_mask.nii.gz -M > /values/sex/FA/CGC-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k CGC-R_binary_mask.nii.gz -M > /values/sex/FA/CGC-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k CGH-L_binary_mask.nii.gz -M > /values/sex/FA/CGH-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k CGH-R_binary_mask.nii.gz -M > /values/sex/FA/CGH-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k CP-L_binary_mask.nii.gz -M > /values/sex/FA/CP-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k CP-R_binary_mask.nii.gz -M > /values/sex/FA/CP-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k CST-L_binary_mask.nii.gz -M > /values/sex/FA/CST-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k CST-R_binary_mask.nii.gz -M > /values/sex/FA/CST-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k EC-L_binary_mask.nii.gz -M > /values/sex/FA/EC-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k EC-R_binary_mask.nii.gz -M > /values/sex/FA/EC-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k FX_binary_mask.nii.gz -M > /values/sex/FA/FX_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k FX-ST_L_binary_mask.nii.gz -M > /values/sex/FA/FX-ST_L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k FX-ST_R_binary_mask.nii.gz -M > /values/sex/FA/FX-ST_R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k GCC_binary_mask.nii.gz -M > /values/sex/FA/GCC_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k ICP-L_binary_mask.nii.gz -M > /values/sex/FA/ICP-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k ICP-R_binary_mask.nii.gz -M > /values/sex/FA/ICP-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k MCP_binary_mask.nii.gz -M > /values/sex/FA/MCP_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k ML-L_binary_mask.nii.gz -M > /values/sex/FA/ML-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k ML-R_binary_mask.nii.gz -M > /values/sex/FA/ML-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k PCR-L_binary_mask.nii.gz -M > /values/sex/FA/PCR-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k PCR-R_binary_mask.nii.gz -M > /values/sex/FA/PCR-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k PCT_binary_mask.nii.gz -M > /values/sex/FA/PCT_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k PLIC-L_binary_mask.nii.gz -M > /values/sex/FA/PLIC-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k PLIC-R_binary_mask.nii.gz -M > /values/sex/FA/PLIC-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k PTR-L_binary_mask.nii.gz -M > /values/sex/FA/PTR-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k PTR-R_binary_mask.nii.gz -M > /values/sex/FA/PTR-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k RLIC-L_binary_mask.nii.gz -M > /values/sex/FA/RLIC-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k RLIC-R_binary_mask.nii.gz -M > /values/sex/FA/RLIC-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k SCC_binary_mask.nii.gz -M > /values/sex/FA/SCC_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k SCP-L_binary_mask.nii.gz -M > /values/sex/FA/SCP-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k SCP-R_binary_mask.nii.gz -M > /values/sex/FA/SCP-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k SCR-L_binary_mask.nii.gz -M > /values/sex/FA/SCR-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k SCR-R_binary_mask.nii.gz -M > /values/sex/FA/SCR-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k SFO-L_binary_mask.nii.gz -M > /values/sex/FA/SFO-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k SFO-R_binary_mask.nii.gz -M > /values/sex/FA/SFO-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k SLF-L_binary_mask.nii.gz -M > /values/sex/FA/SLF-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k SLF-R_binary_mask.nii.gz -M > /values/sex/FA/SLF-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k SS-L_binary_mask.nii.gz -M > /values/sex/FA/SS-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k SS-R_binary_mask.nii.gz -M > /values/sex/FA/SS-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k TAP-L_binary_mask.nii.gz -M > /values/sex/FA/TAP-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k TAP-R_binary_mask.nii.gz -M > /values/sex/FA/TAP-R_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k UNC-L_binary_mask.nii.gz -M > /values/sex/FA/UNC-L_meanFA.txt
fslstats -t /values/sex/sex_all_FA_skeletonised.nii.gz -k UNC-R_binary_mask.nii.gz -M > /values/sex/FA/UNC-R_meanFA.txt

# NOTE: The above steps were repeated for MD, and then repeated for the menopause analysis

done

