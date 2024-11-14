# PRODIGY: An exploratory analysis of transparency and stakeholder engagement in a generated sample of global health studies
Data processing and analysis: Research project on exploratory analysis of transparency and stakeholder engagement in Global Health Research Studies (https://osf.io/hqdns/)
- Data: OSF (https://osf.io/hqdns/) and PRODIGY GitHub account (https://github.com/quest-bih/prodigy/tree/main)
- Publication: TBD

## Data sources
We explored three complementary approaches to get a sample of  global health research studies and stored in the following folder:
- Folder 1 clinical-trial-registry: Interventional trials from ClinicalTrials.gov (2008-2019), focusing on tuberculosis and maternal health conditions, with 2/3rd from low-and-middle-income countries (LMIC) 
- Folder 2 global-health-journal: Clinical trial result publications from global health journals published post-2011, identified via PubMed
- Folder 3 global- health-funder: Studies listed from a selected global health-funder website


##  Overview of scripts
### Scripts for Analyzing Clinical Trial Registry Data (Folder 1)
We analyzed a sample of 200 clinical trials (focused on maternal health and tuberculosis) using the following scripts:
- 01a-sample-clinical-trial-registry.R: Extracts a sample of clinical trials from ClinicalTrials.gov based on specific global health conditions.
- 2-data-processing-ct.R: Cleans and prepares clinical trial registry data, ensuring the dataset is ready for analysis.
- 3-data-analysis-ct.R: Analyzes the final clinical trial dataset, focusing on transparency and stakeholder engagement.

### Scripts for Analyzing Global Health Journal Data (Folder 2)
We analyzed 200 trial results from selected global health journals with the following script:
- analyse-trial-result-gh-journals.R: Assesses clinical trial publications from selected global health journals.

### Research Output from Global Health Funder (Folder 3)
We did not analyze the research output from the selected global health funder website due to insufficient data after applying the selection criteria.
