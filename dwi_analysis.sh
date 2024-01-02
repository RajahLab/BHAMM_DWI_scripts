#!/bin/bash

# Name: dwi_analysis.sh 
# Author: Rikki Lissaman
# Last updated (dd/mm/yyyy): 02/01/2024
#
# Description: This Bash script provides an overview of the steps run to analyze the diffusion data (diffusion tensor fitting, TBSS). 
#
# Inputs (per participant):
# 	(1) Pre-processed diffusion data in NIfTI format.
#	(2) Bvec file associated with the pre-processed data.
# 	(3) Bval file associated with the pre-processed data.
#	(4) Mask in NIfTI format.
# Outputs (per participant):
# 	(1) Multiple DTI parameter maps, including FA and MD.
#	(2) Standard TBSS outputs, including all_FA_skeletonised.nii.gz and all_MD_skeletonised.nii.gz.
#
# Notes:
# 	(1) Although the script is written as though all steps were performed in one loop, in reality they were run separately (one-at-a-time). The inclusion of all steps in one script is for readability. 
# 	(2) Software required: FSL (version 6.0.2).
#	(3) Filenames and directories have been replaced with placeholder text.

# Set home directory
home_dir='/path/to/home'

# For loop containing commands used (note: in reality, each step was run separately)
for subj in `cat ${home_dir}/subjects.txt`; do

# Fit diffusion tensors using DTIFIT
dtifit --data=${home_dir}/smoothed/${subj}_smoothed.nii.gz --mask=${home_dir}/mask/${subj}_mask.nii.gz --bvecs=${home_dir}/unbiased_fsl/${subj}_unbiased.bvec --bvals=${home_dir}/unbiased_fsl/${subj}_unbiased.bval --out=${home_dir}/dtifit/${subj}_dti

# Run TBSS

## Create an empty directory and copy each participant's FA image into it
mkdir ${home_dir}/analyis
cp ${home_dir}/dtifit/${subj}_dti_FA.nii.gz ${home_dir}/analysis/${subj}_dti_FA.nii.gz

## Move into new directory
cd ${home_dir}/analysis

## Step 1
tbss_1_preproc *nii.gz

## Step 2
tbss_2_reg -T

## Step 3
tbss_3_postreg -S

## Step 4
tbss_4_prestats 0.2

# Repeat TBSS steps for MD

## Create a new directory called MD 
mkdir MD

## Copy MD images to the directory BUT make sure file names are same as the FA files (even though they are not FA files) 
cp ${home_dir}/dtifit/${subj}_dti_MD.nii.gz ${home_dir}/analysis/MD/${subj}_dti_FA.nii.gz

## Run tbss_non_FA script
tbss_non_FA MD

done

