
#  Exposure to extreme temperatures during pregnancy and birth weight: evidence from Chile (2011 – 2020) :baby:

![GitHub Repo stars](https://img.shields.io/github/stars/ClimChange-NewbornHealth/CIIIA-ClimateBirthWeightAnalysis)
![GitHub watchers](https://img.shields.io/github/watchers/ClimChange-NewbornHealth/CIIIA-ClimateBirthWeightAnalysis)
![GitHub forks](https://img.shields.io/github/forks/ClimChange-NewbornHealth/CIIIA-ClimateBirthWeightAnalysis)
![GitHub commit activity](https://img.shields.io/github/commit-activity/t/ClimChange-NewbornHealth/CIIIA-ClimateBirthWeightAnalysis)
![GitHub contributors](https://img.shields.io/github/contributors/ClimChange-NewbornHealth/CIIIA-ClimateBirthWeightAnalysis)
![GitHub last commit](https://img.shields.io/github/last-commit/ClimChange-NewbornHealth/CIIIA-ClimateBirthWeightAnalysis)
![GitHub language count](https://img.shields.io/github/languages/count/ClimChange-NewbornHealth/CIIIA-ClimateBirthWeightAnalysis)
![GitHub top language](https://img.shields.io/github/languages/top/ClimChange-NewbornHealth/CIIIA-ClimateBirthWeightAnalysis)
![GitHub License](https://img.shields.io/github/license/ClimChange-NewbornHealth/CIIIA-ClimateBirthWeightAnalysis)
![GitHub repo file or directory count](https://img.shields.io/github/directory-file-count/ClimChange-NewbornHealth/CIIIA-ClimateBirthWeightAnalysis)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/ClimChange-NewbornHealth/CIIIA-ClimateBirthWeightAnalysis)


:moneybag: Funding: Proyecto Interuniversatrio de Iniciación en Investigación Asociativa: IUP22-37. 

:mailbox_with_mail: Estela Blanco (<estela.blanco@uc.cl>) -  **Principal Investigator**

:paperclip: Paola Rubilar -  **Principal Investigator**

:paperclip: Raquel Jiménez -  **Principal Investigator**

:paperclip: María Isabel Matute -  **Research Collaborator**

:mailbox_with_mail: José Daniel Conejeros (<jdconejeros@uc.cl>) - **Research Collaborator - Repository Manager**

:pushpin: Paper: -*Submission*-

:pushpin: **Background**: Exposure to extreme temperatures during pregnancy can have adverse effects on birth weight, however, there is little evidence from Latin America.

:pushpin: **Objective**: Evaluate the association between exposure to ambient temperatures during pregnancy and birth weight in full-term infants.

:pushpin: **Methods:** We conducted a population-based cohort design using secondary information from birth records: 2011-2020. Mean, minimum, and maximum daily temperatures were obtained from monitors in 26 municipalities representing different climatic zones of Chile. Temperature percentiles were calculated for each climatic zone and assigned for the entire pregnancy, trimester, and gestational week based on residence, date of birth and gestational age. General additive models (GAM) and distributed lag nonlinear models (DLNM) were used, using the 50th percentile for comparison. All models adjusted for month and year of last menstrual cycle and maternal and paternal: age, education level, and employment status.

:pushpin: **Discussion:** Lower birth weight was consistently observed for exposure to cold temperatures, as was exposure to extreme heat. Exposure to warmer mean temperatures related to higher birthweight. Differing results from the Chilean population highlight the importance of understanding regional impacts of climate change on child health.

![code](https://skillicons.dev/icons?i=r) **Data and Code**: You can run code 7.0 to 10.0 with the data table `births_2011_2020_weeks_temp_analysis.RData` downloading the data table [here](https://www.dropbox.com/scl/fi/6ngi8nfcszc0p86ozcgs2/births_2011_2020_weeks_temp_analysis.RData?rlkey=m7qdpqsy0ffx7s6g7r3hbl440&st=rl1uwsqc&dl=0).

## Principal Findings :sunny: :snowflake:

**Figure 1.** Estimated differences in mean term birth weight (g) and 95% confidence intervals according to climate zone-specific centiles of average daily mean, minimum and maximum temperature relative to the reference category (41st–50th centile) among singleton term live births during the entire pregnancy (panel A, C, E) and by trimester (panel B, D, F) in Chile during the period 2011–2020. 

![](/Output_analysis/temp/fig/Adjusted_GAM_models_tbw_trim_full.png)

Note: Models estimated using generalized additive model (GAM) with normal distribution and identity link function for tBW. Models were adjusted for: newborn sex, maternal and paternal age, maternal and paternal education, maternal and paternal occupation, year and month of last menstrual period splines (N= 330,118). tBW-term mean birthweight (mean birthweight among births>36 gestational age weeks).

**Figure 2.** Estimated differences in mean term birth weight (g) and 95% confidence intervals associated with exposure to cold and hot daily mean, minimum and maximum temperatures (≤10th or >90th the percentile for each climate zone, respectively, relative to the 41st–50th centile) during each gestational age week among singleton live births in Chile during the period 2011–2020. 

![](/Output_analysis/dlnm/fig/DLNM_tBW.png)

Note: Estimates were derived using distributed lag non-linear models (DLNM, lag-response function modeled as a natural spline with equidistant knots and 2 degrees of freedom, exposure–response function modeled using indicator terms for each decile of the temperature distribution). Models were adjusted for: newborn sex, maternal and paternal age, maternal and paternal education, maternal and paternal occupation, year and month of last menstrual period splines (N= 330,118). Term birthweight is mean birthweight among births >36 gestational age weeks.

**Figure 5.** Two Stage estimated differences in mean term birth weight (g) and 95% confidence intervals according to climate zone-specific centiles of average daily mean (Panels A and B), minimum (Panel C), and maximum (Panel D) temperature relative to the reference category (41st–50th centile) among singleton term live births in Chile 2011-2020. Estimates were calculated for exposure in the entire pregnancy period.

![](/Output_analysis/2Stage/2Stage_tBW.png)

Note: Estimates were derived using a generalized additive model (GAM) with a normal distribution and identity link function for tBW. Models were adjusted for: newborn sex, maternal and paternal age, maternal and paternal education, maternal and paternal occupation, year and month of last menstrual period splines (N= 330,118). Term birth weight represents birth weight for infants born after 36 weeks gestation.

