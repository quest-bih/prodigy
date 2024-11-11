# PRODIGY: An exploratory analysis of transparency and stakeholder engagement in Global Health Research
Data processing and analysis: Research project on exploratory analysis of transparency and stakeholder engagement in Global Health Research (https://osf.io/hqdns/)
Data: OSF (https://osf.io/hqdns/) and PRODIGY GitHub account ()
Publication: TBD

## Data sources
We generated and analysed a random GHR sample using three sources: 
(1) interventional trials from ClinicalTrials.gov (2008-2019), focusing on tuberculosis and maternal health conditions, with 2/3rd from low-and-middle-income countries (LMIC):
(2) trial result publications from global health journals published post-2011, identified via PubMed: 
(3) studies listed on selected global health funder websites: 

##  Overview of scripts
Clinical-trial-registry (Trials involving global health issues, specifically maternal health and tuberculosis)
- 01a-sample-clinical-trial-registry.R: Extracts a sample of clinical trials from ClinicalTrials.gov based on specific global health conditions.
- 2-data-processing-ct.R: Cleans and prepares clinical trial registry data, ensuring the dataset is ready for analysis.
- 3-data-analysis-ct.R: Analyzes the final clinical trial dataset, focusing on transparency and stakeholder engagement.

Global health journal (Publications of trial results in selected global health journals)
- analyse-trial-result-gh-journals.R: Assesses clinical trial publications from selected global health journals.

