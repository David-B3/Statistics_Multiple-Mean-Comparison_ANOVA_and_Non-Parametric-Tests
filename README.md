# Perform a STEP by STEP multiple mean comparison analysis on R
This code has been developed to realize the complete process of a multiple mean comparison (ANOVA and non-parametric tests) on R.

It is composed of: 

  1. Importation on the datafile
  2. Descriptive Statistics (Mean, Standard Deviation)
  3. Visualization of the data, detection of outliers
  4. Normality and homogeneity assumption 
  5. Parametric and non-parametric test for multiple means comparison
  6. Corresponding Post-Hoc tests 
  7. Visualization of the data (graph) with significance bars and stars
  8. Exportation of the results

STUDY-CASE: 
22 subjects played a football game. Before the match ("PreMatch"), 24h, 48h and 72h after, they performed force measurements (IMVC : isometric maximal voluntary contraction). After the match, subjects were divided in 2 groups experiencing 25-min sessions of either cold water immersion (CWI, n = 11) or hot water immersion (HWI, n = 11). We want to know if the immersion temperature have an impact on the force recovery. To do so, we will perform a Multiple Mean Comparison analysis. 

Notes: all data in Example-MultipleMeanComparison.CSV have been randomly created. They are plausible values for IMVC (Isometric Maximal Voluntary Contraction) but invented for the example. 
