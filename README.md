# Meta-analysis_AAT_PA

**Authors:** François Jabouille, Ata Farajzadeh, Matthieu P. Boisgontier

**Contacts:** francois.jabouille@uottawa.ca

## Description

Code and data for the article: "Approach-avoidance tendencies and physical activity level: A systematic review and meta-analysis"

## File Structure

- `data/`: Data used for analyses (CSV files)

  -> `PA_Approach-avoidance_r.csv` for the main analyses

  -> `PA_Approach-avoidance_rho.csv` for the secondary analysis

  -> `quality_scores_table.csv` containing the quality assessment scores for all included studies
- `manuscript/`: Preprint and submission file
- `R/`: R and Rmd scripts.
    -> `meta_approach_avoidance_PA.Rmd`: R Markdown file used to perform the full meta-analysis pipeline, including data cleaning, computation of effect sizes, statistical modeling, and generation of tables and figures.
    -> `meta_approach_avoidance_PA.pdf`: PDF file of the R Markdown.
    -> `correlation_calculation.R`: script used to compute correlation estimates when they were missing or when data were provided by the authors. Due to ethical restrictions, we are unfortunately not allowed to share the authors' data. If you request some of the data from the authors, you can run this code simply by setting up your own working directory Line 16, and your own output_directory Line 55.
    -> `exact_p_calculation.R`: script used to compute exact p-value from sample size and correlation value.
  
