# BHAMM_DWI_scripts

These scripts were used in:
 
<b>Menopause status- and sex-related differences in age associations with spatial context memory and white matter microstructure at midlife</b> -
Rikki Lissaman<sup>a</sup>\*, Sricharana Rajagopal<sup>b</sup> , Julia Kearley<sup>c</sup>, Stamatoula Pasvanis<sup>b</sup>, and M. Natasha Rajah<sup>a,d</sup>
DOI: https://doi.org/10.1016/j.neurobiolaging.2024.05.017

\*First author

<sup>a</sup>Department of Psychiatry, McGill University, Montreal, QC, Canada <br>
<sup>b</sup>Douglas Research Centre, Montreal, QC, Canada <br>
<sup>c</sup>Department of Psychology, McGill University, Montreal, QC, Canada <br>
<sup>d</sup>Department of Psychology, Toronto Metropolitan University, Toronto, ON, Canada <br>


Script descriptions:
* analyze_visualize_behav_data.R = R script that analyzes and visualizes behavioral data from the BHAMM study (includes section for supplementary analysis).
* analyze_visualize_brainscore_data.R = R script that analyzes and visualizes "brain scores" dervied from bPLS analyses (see bpls_analysis.m).
* bpls_analysis.m = MATLAB script that carries out bPLS analyses examining sex- and menopause-related differences in the association between age and regional FA/MD.
* dwi_analysis.sh = Bash script that provides an overview of the steps run to analyze the diffusion data.
* dwi_extract_roi_values.sh = Bash script that provides an overview of the steps conducted to extract ROI-specific measures of FA and MD.
* dwi_preprocess.sh = Bash script that provides an overview of the steps run during diffusion MRI pre-processing.
* prepare_behav_data.R = R script that identifies relevant participants from the BHAMM study, generates key demographic information, and outputs IDs and data (for analysis).
* prepare_roi_values.R = R script that prepares ROI-specific FA and MD values for analysis.
* visualize_pls_singular_profile.R = R script that visualizes the singular profiles of LVs identified by bPLS analyses.
