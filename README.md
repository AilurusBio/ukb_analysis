# Project Name: COPD Risk Prediction and Assessment

This project aims to analyze samples from the UK Biobank dataset related to Chronic Obstructive Pulmonary Disease (COPD). The project involves careful sample selection, integration of personal health information and blood biochemical indicators, grouping of positive and negative samples, Genome-Wide Association Study (GWAS) analysis, multiple machine learning (ML) model development based on these features, evaluation of model performance, and graphical representation of the study results.


### Workflow and Process:

1. **Sample Selection and Preparation:**
   Careful selection of COPD-related samples from the UK Biobank ensuring quality and representativeness. Integration of personal health information and blood biochemical indicators to construct feature sets.
   - Related Files/Directories: 
     - `S3_label_select.csv`: CSV file with selected labels.
     - `feature_info`: Directory containing feature information.

2. **Positive and Negative Sample Grouping and GWAS Analysis:**
   Grouping of selected samples into positive and negative categories followed by GWAS analysis to explore genetic variations associated with COPD risk.
   - Related Files/Directories: 
     - `gwas`: Directory containing GWAS-related files.
     - `GWAS_plot`: Directory containing GWAS plots.

3. **Machine Learning Model Development:**
   Utilization of various ML models like logistic regression, random forest, neural networks, etc., based on the selected features to predict COPD risk.
   - Related Files/Directories: 
     - `code`: Directory containing code files for ML model development.

4. **Model Performance Evaluation:**
   Assessment of model performance and comparative analysis to examine predictive capabilities and stability.
   - Related Files/Directories: 
     - `S2_traintest_id.RData`: RData file with training and testing identifiers.
     - `S4_lm_res.RData`: RData file with linear regression results.
     - `S5_model_res.zip`: Compressed file containing model results.

5. **Result Visualization and Validation:**
   Graphical representation of results including GWAS plots, model evaluation outcomes, etc., for demonstrating the reliability and effectiveness of the research.
   - Related Files/Directories: 
     - `paper_chart`: Directory containing charts and figures for the research paper.
     - `paper_files`: Directory containing files related to the research paper.


