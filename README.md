# BHAMM_DWI_scripts

These scripts were used in:
 
<b>The transition from pre- to post-menopause increases vulnerability to age effects on episodic memory and white matter microstructure</b> -
Rikki Lissaman<sup>a,b,c</sup>\*, Sricharana Rajagopal<sup>b,c</sup> , Julia Kearley<sup>b,d</sup>, Stamatoula Pasvanis<sup>b,c</sup>, and M. Natasha Rajah<sup>a,b,c,d</sup>

\*First author

<sup>a</sup>Department of Psychiatry, McGill University, Montreal, QC, Canada <br>
<sup>b</sup>Douglas Research Centre, Montreal, QC, Canada <br>
<sup>c</sup>Department of Psychology, Toronto Metropolitan University, Toronto, ON, Canada <br>
<sup>d</sup>Department of Psychology, McGill University, Montreal, QC, Canada <br>


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
