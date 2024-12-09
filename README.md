# A Digital Biomarker Dataset in Hematopoietic Cell Transplantation: A Longitudinal Study of Caregiver-Patient Dyads (dHCT)

**Authors:** Aditya Jalin, Nawat Swatthong, Michelle Rozwadowski, Rajnish Kumar, Deb Barton, Tom Braun, Noelle Carlozzi, David A. Hanauer, Afton Hassett, Sung Won Choi

**Summary**:  
Hematopoietic cell transplantation (HCT) is a life-saving therapy for hematologic conditions, but it imposes substantial physical and emotional demands on patients and their family caregivers. Understanding the interplay among caregiver burden, patient recovery, mental well-being, and clinical outcomes is vital for improving care.

This dataset captures the daily experience of 166 HCT caregiver-patient dyads over 120 days post-transplant, integrating data from multiple sources:

- **Wearable Device (Fitbit® Charge 3)**: Minute-level heart rate, daily sleep metrics, step counts.
- **Mobile App (Roadmap)**: Daily self-reported mood scores (1–10 scale), usage metrics, and (for the intervention arm) positive psychology activities.
- **PROMIS® Measures**: Standardized T-scores for global health, depression, anxiety, fatigue, and more, collected at baseline, day 30, and day 120.
- **Clinical Events**: Infection details, hospital readmissions, acute and chronic graft-versus-host disease (GVHD), relapse, and mortality records.

These data provide a unique, high-resolution view of the HCT recovery process, facilitating research on digital health interventions, dyadic well-being, predictive modeling, and the relationship between physiological and psychosocial factors.

## Repository Contents

- **`/Rscripts`**:  
  R scripts for data processing, cleaning, scoring, and visualization.
  
- **`/figures`**:  
  Figures shown in the paper.
  
- **`README.md`** (this file):  
  Overview of the dataset, analysis scripts, and instructions for use.

- **`LICENSE`**:  
  License details for code use and distribution.

**Note**: Due to privacy and regulatory constraints, raw datasets are not stored in this GitHub repository. Refer to the “Data Availability” section below for how to access the data.

## Data Availability

The dataset is available through the [Deep Blue Data](https://deepblue.lib.umich.edu/data) repository (link will be provided once published). Access may require a data use agreement or permissions due to sensitive health information. Please follow the instructions on the repository page or contact the corresponding authors for details on obtaining the data.

## Potential Research Applications

- **Predictive Modeling**: Develop models to forecast clinical events (e.g., GVHD) based on physiological and mood patterns.
- **Dyadic Interactions**: Investigate relationships between caregiver well-being and patient recovery trajectories.
- **mHealth Intervention Evaluation**: Examine the impact of positive psychology activities on caregiver and patient outcomes.
- **Sleep and Activity Rhythms**: Assess changes in sleep patterns, circadian rhythm disruptions, and physical activity levels over the post-transplant period.
- **PROMIS®-Based Insights**: Explore correlations between patient-reported outcomes and physiological markers to understand stress, coping, and resilience.

## Ethical Approvals

- **IRB Approval**: University of Michigan Medical School Institutional Review Board (IRBMED HUM#00186436)
- **Informed Consent**: Obtained from all participants
- **Trial Registration**: NCT04094844 (ClinicalTrials.gov)

## Funding and Acknowledgments

Supported by the National Heart, Lung, and Blood Institute (Grant Nos. K24HL156896, R01HL146354). We are grateful to all participants, research staff, and clinicians at the U-M Blood and Marrow Transplant Program.

## Citation

If you use this dataset or code in your work, please cite:

Jalin A, Swatthong N, Rozwadowski M, Kumar R, Barton D, Braun T, Carlozzi NE, Hanauer DA, Hassett A, Choi SW. *A Digital Biomarker Dataset in Hematopoietic Cell Transplantation: A Longitudinal Study of Caregiver-Patient Dyads (dHCT)*. 
