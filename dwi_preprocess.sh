#!/bin/bash

# Name: dwi_preprocess.sh 
# Author: Rikki Lissaman
# Last updated (dd/mm/yyyy): 02/01/2024
#
# Description: This Bash script provides an overview of the steps run during diffusion MRI pre-processing. The steps implemented follow those recommended by Maximov et al. (2019), Human Brain Mapping. 
#
# Inputs (per participant):
# 	(1) NIfTI image containing diffusion-weighted imaging data acquired posterior-to-anterior.
#	(2) bval file containing a single number per volume that shows the diffusion gradient applied.
#	(3) bvec file containing triplets of numbers per volume showing the directions in which the diffusion gradients were applied.
#	(4) NIfTI image containing non-diffusion-weighted imaging data acquired anterior-to-posterior.
# Outputs (per participant):
# 	(1) Multiple files per participant, each output from a particular step in the pre-processing pipeline. Outputs are in MRtrix (mif) format until the latter stages, where files are converted back to FSL 
#	/ NIfTI format.
#
# Notes:
# 	(1) Although the script is written as though all steps were performed in one loop, in reality they were run separately (one-at-a-time). The inclusion of all steps in one script is for readability. 
# 	(2) Software required: MRtrix (version 3.0.2), FSL (version 6.0.2), ANTs (version 2.3.1).
#	(3) Filenames and directories have been replaced with placeholder text.
#	(4) The raw data, located in the data directory, are stored in BIDS format. 

# Set data directory
data_dir='/path/to/data'

# Set home directory
home_dir='/path/to/home'

# For loop containing commands used (note: in reality, each step was run separately)
for subj in `cat ${home_dir}/subjects.txt`; do

# Convert diffusion-weighted data from NIfTI to mif format
mrconvert ${data_dir}/${subj}/dwi/${subj}_acq-b1000_dir-PA.nii.gz ${home_dir}/mrconvert/${subj}_dwi/mif -fslgrad ${data_dir}/${subj}/dwi/${subj}_acq-b1000_dir-PA.bvec ${data_dir}/${subj}/dwi/${subj}_acq-b1000_dir-PA.bval

# Perform denoising and export noise as separate file
dwidenoise ${home_dir}/mrconvert/${subj}_dwi.mif ${home_dir}/denoise/${subj}_denoise.mif -noise ${home_dir}/noise/${subj}_noise.mif

# As a quick quality check, calculate residuals
mrcalc ${home_dir}/mrconvert/${subj}_dwi.mif ${home_dir}/denoise/${subj}_denoise.mif -subtract ${home_dir}/residuals/${subj}_residual.mif

# Perform Gibbs correction
mrdegibbs ${home_dir}/denoise/${subj}_denoise.mif ${home_dir}/degibbs/${subj}_degibbs.mif

# Perform motion and distortion correction

## Extract the non-diffusion-weighted data volume acquired in the phase-encoding direction
dwiextract -bzero ${home_dir}/degibbs/${subj}_degibbs.mif ${home_dir}/b0/${subj}_b0.mif

## Convert non-diffusion-weighted data (acquired in reverse phase-encoding direction) from NIfTI to mif format
mrconvert ${data_dir}/${subj}/dwi/${subj}_acq-b0_dir-AP.nii.gz ${home_dir}/b0_AP/${subj}_b0_AP.mif

## Calculate mean of non-diffusion-weighted data acquired in reverse phase-encoding direction
mrconvert ${home_dir}/b0_AP/${subj}_b0_AP.mif - | mrmath - mean ${home_dir}/b0_AP_mean/${subj}_b0_AP_mean.mif -axis 3

## Concatenate the non-diffusion-weighted image acquired in the phase-encoding direction and the mean of the non-diffusion-weighted images acquired in the reverse phase-encoding direction
mrcat ${home_dir}/b0/${subj}_b0.mif ${home_dir}/b0_AP_mean/${subj}_b0_AP_mean.mif -axis 3 ${home_dir}/b0_pair/${subj}_b0_pair.mif

## Run the correction
dwifslpreproc ${home_dir}/degibbs/${subj}_degibbs.mif ${home_dir}/preproc/${subj}_preproc.mif -pe_dir PA -rpe_pair -se_epi ${home_dir}/b0_pair/${subj}_b0_pair.mif -eddy_options " --slm=linear --repol"

# Perform bias field correction and export bias image
dwibiascorrect ants ${home_dir}/preproc/${subj}_preproc.mif ${home_dir}/unbiased/${subj}_unbiased.mif -bias ${home_dir}/bias/${subj}_bias.mif

# Perform spatial smoothing

## Convert unbiased image from mif format to NIfTI format
mrconvert ${home_dir}/unbiased/${subj}_unbiased.mif ${home_dir}/unbiased_fsl/${subj}_unbiased.nii.gz -export_grad_fsl ${home_dir}/unbiased_fsl/${subj}_unbiased.bvec 
${home_dir}/unbiased_fsl/${subj}_unbiased.bval

## Apply 1mm3 Gaussian smoothing kernel
fslmaths ${home_dir}/unbiased_fsl/${subj}_unbiased.nii.gz -kernel gauss 1 ${home_dir}/smoothed/${subj}_smoothed.nii.gz

# Generate a mask
bet2 ${home_dir}/smoothed/${subj}_smoothed.nii.gz ${home_dir}/mask/${subj}_mask.nii.gz -m -f 0.4

done

