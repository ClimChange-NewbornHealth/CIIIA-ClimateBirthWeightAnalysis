
#  Exposure to extreme temperatures during pregnancy and birth weight: evidence from Chile (2011 – 2020) :baby:

:moneybag: Funding: Proyecto Interuniversatrio de Iniciación en Investigación Asociativa: IUP22-37

:mailbox_with_mail: Estela Blanco (<estela.blanco@uc.cl>)

:mailbox_with_mail: José Daniel Conejeros (<jdconejeros@uc.cl>)

:pushpin: **Background**: Exposure to extreme temperatures, both high and low, during pregnancy can have adverse effects on fetal development, with birth weight being a critical indicator of neonatal health, however, there is little evidence from Latin America.

:pushpin: **Objective**: Evaluate the association between exposure to ambient temperatures during pregnancy, birth weight (grams, g) in full-term infants (gestational age ≥ 37 weeks).

:pushpin: **Methods:** We conducted a population-based cohort design using secondary information from birth records between 2011 and 2020. Mean, minimum, and maximum daily temperatures were obtained from government monitors in 26 municipalities representing different climatic zones of Chile. Percentiles were calculated for each climatic zone and assigned for the entire pregnancy, trimester, and gestational week based on residence, date of birth and gestational age. General additive models (GAM) and distributed lag nonlinear models (DLNM) were used, using the 50th percentile for comparison. All models adjusted for month and year of last menstrual cycle and maternal and paternal: age, education level, and employment status.

:floppy_disk: [![My Skills](https://skillicons.dev/icons?i=r) **Data and Code**: You can run code 6 7.0 to 10 with the data table `births_2011_2020_weeks_temp_analysis.RData` downloading the data table [here](https://www.dropbox.com/scl/fi/6ngi8nfcszc0p86ozcgs2/births_2011_2020_weeks_temp_analysis.RData?rlkey=m7qdpqsy0ffx7s6g7r3hbl440&st=rl1uwsqc&dl=0).

## Principal Findings :sunny: :snowflake:

**Figure 1.** Estimated differences in mean tBW (grams.) and 95% CI according to climate zone-specific centiles of average daily mean, minimum and maximum temperature relative to the reference category (41st–50th centile) among singleton term live births during the entire pregnancy (panel A, C, E) and by trimester (panel B, D, F).

![](/Output_analysis/temp/fig/Adjusted_GAM_models_tbw_trim_full.png)

Note: These were estimated by generalized additive model (GAM) with normal distribution and identity link function for tBW. Models were adjusted for: newborn sex, maternal age, maternal education, maternal occupation, paternal age, paternal education, paternal occupation, year and month of LMP splines; for complete case (N= 330,118). LMP (last menstrual period); tBW-term mean birthweight (mean birthweight among births>36 gestational age weeks).

**Figure 3.** Estimated differences in mean tBW (grams.) and 95% CI associated with extreme cold and hot daily mean, minimum and maximum temperatures (≤10th or >90th the percentile for each climate zone, respectively, relative to the 41st–50th centile) during each gestational age week among singleton live births in Chile during the period 2011–2020. 

![](/Output_analysis/dlnm/fig/DLNM_tBW.png)

Estimates were derived using DLNM (lag-response function modeled as a natural spline with equidistant knots and 2 degrees of freedom, exposure–response function modeled using indicator terms for each decile of the temperature distribution). Models were adjusted newborn sex, maternal age, maternal education, maternal occupation, paternal age, paternal education, paternal occupation, year and month of LMP splines; for complete case (N= 330,118). LMP (last menstrual period); tBW-term mean birthweight (mean birthweight among births>36 gestational age weeks).

**Figure 5.** Two Stage estimated differences in mean tBW (grams) and 95% confidence intervals according to climate zone-specific centiles of average daily mean (Panels A and B), minimum (Panel C), and maximum (Panel D) temperature relative to the reference category (41st–50th centile) among singleton term live births. These estimates were calculated during the entire pregnancy period.

![](/Output_analysis/2Stage/2Stage_tBW.png)

Note: The estimates were derived using a generalized additive model (GAM) with a normal distribution and identity link function for tBW. The models were adjusted for newborn sex, maternal age, maternal education, maternal occupation, paternal age, paternal education, paternal occupation, as well as year and month of LMP using splines. The results are based on a complete case dataset of N = 330,118. LMP refers to the last menstrual period, and tBW represents term birth weight, which includes births with a gestational age greater than 36 weeks.

