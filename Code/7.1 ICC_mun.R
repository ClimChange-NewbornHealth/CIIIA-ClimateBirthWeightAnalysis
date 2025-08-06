# 7.1 Null model and ICC ---

## Settings  ----

# Previous work
rm(list=(ls()))
options(scipen=999)
source("Code/0.2 Settings.R")

# Optimal use power computation 
options(future.globals.maxSize = 3000 * 1024^2)  
plan(multisession, workers = detectCores() - 4) # Parallelization

# Paths
data_path <- "Data/Output/"
data_temp <- "births_2011_2020_weeks_temp_analysis.RData"
data_out <- "Output_analysis/temp/"

load(paste0(data_path, data_temp))

glimpse(births_weeks_temp)

## Null model  ----
# Uses unique id for check models
births_weeks_temp <- births_weeks_temp |>
  dplyr::select(id, tbw, ltbw, clim_zone, zone, name_comuna, name_reg) %>%
  distinct()

# Null model 
m01 <- lmer(tbw ~ 1 + (1 | name_comuna), data=births_weeks_temp)
m02 <- glmer(ltbw ~ 1 + (1 | name_comuna), data=births_weeks_temp, family = binomial(link = "logit"))

m03 <- lmer(tbw ~ 1 + (1 | name_reg), data=births_weeks_temp)
m04 <- glmer(ltbw ~ 1 + (1 | name_reg), data=births_weeks_temp, family = binomial(link = "logit"))

m05 <- lmer(tbw ~ 1 + (1 | zone), data=births_weeks_temp)
m06 <- glmer(ltbw ~ 1 + (1 | zone), data=births_weeks_temp, family = binomial(link = "logit"))

m07 <- lmer(tbw ~ 1 + (1 | clim_zone), data=births_weeks_temp)
m08 <- glmer(ltbw ~ 1 + (1 | clim_zone), data=births_weeks_temp, family = binomial(link = "logit"))

screenreg(m01)
screenreg(m02)
screenreg(m03)
screenreg(m04)
screenreg(m05)
screenreg(m06)
screenreg(m07)
screenreg(m08)

## ICC  ----

# Extract intrasclass correlations
merTools::ICC(outcome = "tbw", group = "name_comuna", data = births_weeks_temp) * 100 # 1,458433%
merTools::ICC(outcome = "ltbw", group = "name_comuna", data = births_weeks_temp) * 100 # 5,208291%

merTools::ICC(outcome = "tbw", group = "name_reg", data = births_weeks_temp) * 100 # 0,8639294%
merTools::ICC(outcome = "ltbw", group = "name_reg", data = births_weeks_temp) * 100 # 2,109585%

merTools::ICC(outcome = "tbw", group = "zone", data = births_weeks_temp) * 100  # 0,6452742%
merTools::ICC(outcome = "ltbw", group = "zone", data = births_weeks_temp) * 100 # 1,149011%

merTools::ICC(outcome = "tbw", group = "clim_zone", data = births_weeks_temp) * 100 # 0,406596%
merTools::ICC(outcome = "ltbw", group = "clim_zone", data = births_weeks_temp) * 100 # 0,9942641%

# Tables with ICC
outcomes <- c("tbw", "ltbw")
groups <- c("name_comuna", "name_reg", "zone", "clim_zone")

# Estima teICC
icc_results <- expand.grid(outcome = outcomes, group = groups) |>
  rowwise() |>
  mutate(ICC = merTools::ICC(outcome = outcome, group = group, data = births_weeks_temp) * 100) |>
  pivot_wider(names_from = group, values_from = ICC, names_prefix = "ICC_")

# Table
icc_results <- icc_results |>
  rename(
    `Comuna (%)` = ICC_name_comuna,
    `Region (%)` = ICC_name_reg,
    `Zone (%)` = ICC_zone,
    `Climatic Zone (%)` = ICC_clim_zone
  )

icc_results

writexl::write_xlsx(icc_results, paste0(data_out, "/tab/", "ICC_null_models.xlsx"))
