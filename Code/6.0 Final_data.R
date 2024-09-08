# Code 6-Final Data ---

## Settings ----

rm(list = ls())
source("Code/0.2 Settings.R")

# Optimal use power computation 
gc()
options(future.globals.maxSize = 3000 * 1024^2)  
plan(multisession, workers = detectCores() - 4) # Parallelization

path <- "Data/Output/"

comunas <- chilemapas::codigos_territoriales |>
  mutate(codigo_comuna=as.numeric(codigo_comuna),
         codigo_provincia=as.numeric(codigo_provincia),
         codigo_region=as.numeric(codigo_region))

################################/
## Variables temp data  ----
################################/

# Load data 
data_temp <- "births_2011_2020_weeks_temp.RData"
load(paste0(path, data_temp))
glimpse(births_weeks_temp)
length(unique(births_weeks_temp$id)) # 393951 unique obs

# Summary data 
#summary(births_weeks_temp)

# Adjust variable
births_weeks_temp <- births_weeks_temp |>
  mutate(
    # Outcomes 
    tbw=if_else(peso==9999, NA_real_, peso),
    ltbw=if_else(peso<2500, 1, 0), 
    # Other variables 
    weeks=if_else(semanas==99, NA_real_, semanas),
    #sex=if_else(sexo==9, NA_real_, sexo),
    size=if_else(talla==99, NA_real_, talla),
    age_mom=if_else(edad_madre==99, NA_real_, edad_madre),
    educ_mom=if_else(nivel_madre==9, NA_real_, nivel_madre),
    job_mom=if_else(activ_madre %in% c(9), NA_real_, activ_madre+1),
    age_dad=if_else(edad_padre==99, NA_real_, edad_padre),
    educ_dad=if_else(nivel_padre==9, NA_real_, nivel_padre),
    job_dad=if_else(activ_padre %in% c(3,9), NA_real_, activ_padre+1)
    ) |>
  mutate(
    sex=factor(sexo, levels=c(1,2,9), labels=c("Boy", "Girl", "Unknown"))
  ) |>
  left_join(comunas, by=c("comuna"="codigo_comuna")) |>
  mutate(zona = 
           case_when(
             nombre_region %in% c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo") ~ "Norte",
             nombre_region %in% c("Valparaiso", "Metropolitana de Santiago", "Libertador General Bernardo O'Higgins", "Maule", "Nuble", "Biobio") ~ "Centro",
             nombre_region %in% c("La Araucanía", "Los Ríos", "Los Lagos", "Aysen del General Carlos Ibanez del Campo", "Magallanes y de la Antartica Chilena") ~ "Sur",
             TRUE ~ NA_character_)) |>
  mutate(zone=factor(zona, 
                     levels=c("Norte", "Centro", "Sur"),
                     labels=c("North", "Center", "South")
  )) |>
  mutate(clim_zone = 
           case_when(
             #nombre_region %in% c("Arica y Parinacota") ~ "Desert hot",
             nombre_region %in% c("Arica y Parinacota") ~ "Desert",
             #nombre_region %in% c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo") ~ "Desert cold",
             nombre_region %in% c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo") ~ "Desert",
             nombre_region %in% c("Metropolitana de Santiago", "Libertador General Bernardo O'Higgins", "Maule") ~ "Temperate dry, hot summer",
             nombre_region %in% c("Valparaiso", "Nuble", "Biobio", "La Araucanía") ~ "Temperate dry, warm summer",
             nombre_region %in% c("Los Ríos", "Los Lagos", "Aysen del General Carlos Ibanez del Campo") ~ "Temperate, no dry season",
             nombre_region %in% c("Magallanes y de la Antartica Chilena") ~ "Cold steppe",
             TRUE ~ NA_character_)) |>
  mutate(clim_zone=factor(clim_zone, 
                          levels=c("Desert", "Temperate dry, hot summer", "Temperate dry, warm summer", "Temperate, no dry season", "Cold steppe"),
                          labels=c("Desert", "Temperate dry, hot summer", "Temperate dry, warm summer", "Temperate, no dry season", "Cold steppe")
  )) |>
  mutate(across(starts_with("temp_"), ~replace(., is.infinite(.), NA))) |>
  mutate(trim_gest =
           case_when(week_gest_num >= 1 & week_gest_num <= 12 ~ "T1", 
                     week_gest_num >= 13 & week_gest_num <= 24 ~ "T2", 
                     week_gest_num >= 25 ~ "T3",
                     TRUE ~ NA_character_
                     )) |>
  mutate(ntbw=if_else(weeks<=37, 1, 0)) |>
  mutate(date_week1 = date_start, 
         year_week1 = year(date_week1), 
         month_week1 = month(date_week1),
         ym_week1 = as.yearmon(date_week1, "%M-%Y")) |>
  filter(!is.na(weeks)) |>
  filter(!is.na(tbw)) |>
  rename(day_nac=dia_nac,
         month_nac=mes_nac,
         year_nac=ano_nac,
         com=comuna, 
         reg=codigo_region,
         name_reg=nombre_region, 
         date_start_week_gest=date_start_week,
         date_ends_week_gest=date_end_week,
         name_reg=nombre_region, 
         name_comuna=nombre_comuna
         ) |>  
  select(I, id, tbw,ltbw, 
         # Other variables 
         date_nac, day_nac, month_nac, year_nac, 
         date_week1, year_week1, month_week1, ym_week1,
         weeks, date_start, date_end, week_gest, week_gest_num, trim_gest,
         date_start_week_gest, date_ends_week_gest, 
         sex,size,
         age_mom,educ_mom,job_mom,
         age_dad,educ_dad,job_dad,
         reg, name_reg, com, name_comuna, zone, clim_zone,
         temp_mean, temp_min, temp_max, 
         temp_mean_min, temp_mean_max,
         ) |>
  mutate(
    age_group_mom=case_when(
      age_mom <= 20 ~ 1, 
      age_mom > 20 & age_mom <= 29 ~ 2,
      age_mom >= 30 & age_mom <= 39 ~ 3,
      age_mom >= 40 & age_mom <= 49 ~ 4,
      age_mom >= 50 ~ 5, 
      TRUE ~ 6
    ),
    age_group_mom=factor(age_group_mom, 
                         levels=c(1:6), 
                         labels=c("<=20", "20-29", "30-39", "40-49", ">=50", "Unknown")),
    educ_group_mom = case_when(
      educ_mom == 1 ~ 4, # College
      educ_mom == 2 ~ 3, # Secondary
      educ_mom == 3 ~ 3, # Secondary
      educ_mom == 4 ~ 2, # Primary
      educ_mom == 5 ~ 1, # No educaction 
      TRUE ~ 5, #Unknow
    ), 
    educ_group_mom = factor(educ_group_mom, 
                            levels = c(1:5), 
                            labels = c("Non education", "Primary", "Secondary", "College", "Unknown")),
    job_group_mom = if_else(is.na(job_mom), 4, job_mom), 
    job_group_mom = factor(job_group_mom, levels = c(1,2,3,4), labels=c("Not working", "Employed", "Unemployed", "Unknown"))
  ) |>
  relocate(age_group_mom, educ_group_mom, job_group_mom, .after=job_mom) |>
  mutate(
    age_group_dad=case_when(
      age_dad <= 20 ~ 1, 
      age_dad > 20 & age_dad <= 29 ~ 2,
      age_dad >= 30 & age_dad <= 39 ~ 3,
      age_dad >= 40 & age_dad <= 49 ~ 4,
      age_dad >= 50 ~ 5, 
      TRUE ~ 6
    ),
    age_group_dad=factor(age_group_dad, 
                         levels=c(1:6), 
                         labels=c("<=20", "20-29", "30-39", "40-49", ">=50", "Unknown")),
    educ_group_dad = case_when(
      educ_dad == 1 ~ 4, # College
      educ_dad == 2 ~ 3, # Secondary
      educ_dad == 3 ~ 3, # Secondary
      educ_dad == 4 ~ 2, # Primary
      educ_dad == 5 ~ 1, # No educaction 
      TRUE ~ 5, #Unknow
    ), 
    educ_group_dad = factor(educ_group_dad, 
                            levels = c(1:5), 
                            labels = c("Non education", "Primary", "Secondary", "College", "Unknown")),
    job_group_dad = if_else(is.na(job_dad), 4, job_dad), 
    job_group_dad = factor(job_group_dad, levels = c(1,2,3,4), labels=c("Not working", "Employed", "Unemployed", "Unknown"))
  ) |>
  relocate(age_group_dad, educ_group_dad, job_group_dad, .after=job_dad) |>
    group_by(id) |>
    mutate(temp_mean_gest = mean(temp_mean, na.rm = TRUE)) |>
    group_by(id) |>
    mutate(temp_min_gest = mean(temp_min, na.rm = TRUE)) |>
    group_by(id) |>
    mutate(temp_max_gest = mean(temp_max, na.rm = TRUE)) |>
    ungroup() |>
    group_by(id, trim_gest) %>%
    mutate(temp_mean_trim_gest = mean(temp_mean, na.rm = TRUE)) |>
    ungroup() |>
    group_by(id, trim_gest) %>%
    mutate(temp_min_trim_gest = mean(temp_min, na.rm = TRUE)) |>
    ungroup() |>
    group_by(id, trim_gest) %>%
    mutate(temp_max_trim_gest = mean(temp_max, na.rm = TRUE)) |>
    ungroup() |>
    group_by(week_gest_num) |>
    mutate(temp_mean_percentile_w = ntile(temp_mean, 10)) |>
    ungroup() |>
    group_by(clim_zone) |>
    mutate(temp_mean_percentile_cz = ntile(temp_mean, 10)) |>
    #mutate(temp_mean_percentile100_cz = ntile(temp_mean, 100)) |>
    group_by(clim_zone, week_gest_num) |>
    mutate(temp_mean_percentile_wcz = ntile(temp_mean, 10)) |>
    ungroup() |>
    group_by(clim_zone, trim_gest) |>
    mutate(temp_mean_percentile_tcz = ntile(temp_mean_trim_gest, 10)) |>
    ungroup() |>
    group_by(week_gest_num) |>
    mutate(temp_min_percentile_w = ntile(temp_min, 10)) |>
    group_by(clim_zone) |>
    mutate(temp_min_percentile_cz = ntile(temp_min, 10)) |>  
    #mutate(temp_min_percentile100_cz = ntile(temp_min, 100)) |>  
    ungroup() |>
    group_by(clim_zone, week_gest_num) |>
    mutate(temp_min_percentile_wcz = ntile(temp_min, 10)) |>
    ungroup() |>
    group_by(clim_zone, trim_gest) |>
    mutate(temp_min_percentile_tcz = ntile(temp_min_trim_gest, 10)) |>
    ungroup() |>
    group_by(week_gest_num) |>
    mutate(temp_max_percentile_w = ntile(temp_max, 10)) |>
    ungroup() |>
    group_by(clim_zone) |>
    mutate(temp_max_percentile_cz = ntile(temp_max, 10)) |>  
    #mutate(temp_max_percentile100_cz = ntile(temp_max, 100)) |>  
    group_by(clim_zone, week_gest_num) |>
    mutate(temp_max_percentile_wcz = ntile(temp_max, 10)) |>
    ungroup() |>
    group_by(clim_zone, trim_gest) |>
    mutate(temp_max_percentile_tcz = ntile(temp_max_trim_gest, 10)) |>
    ungroup() |>
    mutate(dplyr::across(
      .cols = contains("_percentile_"),
      .fns = ~ factor(case_when(
        . == 1 ~ "<=10",
        . == 2 ~ "11-20",
        . == 3 ~ "21-30",
        . == 4 ~ "31-40",
        . == 5 ~ "41-50",
        . == 6 ~ "51-60",
        . == 7 ~ "61-70",
        . == 8 ~ "71-80",
        . == 9 ~ "81-90",
        . == 10 ~ "> 90",
        TRUE ~ as.character(NA)
      ), levels = c(
        "<=10", 
        "11-20",
        "21-30",
        "31-40",
        "41-50",
        "51-60",
        "61-70",
        "71-80",
        "81-90",
        "> 90"
      ))
    ))  
# |> 
#   mutate(temp_mean_percentile_tcz=paste0(trim_gest, "_", temp_mean_percentile_tcz)) |> 
#   mutate(temp_min_percentile_tcz=paste0(trim_gest, "_", temp_min_percentile_tcz)) |> 
#   mutate(temp_max_percentile_tcz=paste0(trim_gest, "_", temp_max_percentile_tcz))


## Add vulnerability info
load(paste0("Data/Input/", "sovi_datasets", ".RData"))
glimpse(sovi_subset)
births_weeks_temp <- 
  births_weeks_temp |>
  left_join(select(sovi_subset, -name_comuna), 
            by=c("com"="cod_com")) |>
  relocate(sovi, vulnerablidad, .after = name_comuna) |>
  rename(vulnerability = vulnerablidad) |>
  mutate(vulnerability = case_when(
    vulnerability=="Baja" ~ "Low",
    vulnerability=="Medio-baja" ~ "Medium",
    vulnerability=="Medio-alta" ~ "Medium",
    vulnerability=="Alta" ~ "High", 
    TRUE ~ NA_character_
  )) |>
  mutate(vulnerability=factor(vulnerability,
                              levels = c("Low", "Medium", "High")))
# |> 
#   mutate(temp_mean_percentile_vcz=paste0(vulnerability, "_", temp_mean_percentile_wcz)) |> 
#   mutate(temp_min_percentile_vcz=paste0(vulnerability, "_", temp_min_percentile_wcz)) |> 
#   mutate(temp_max_percentile_vcz=paste0(vulnerability, "_", temp_max_percentile_wcz))


# Summary data 
#summary(births_weeks_temp)
glimpse(births_weeks_temp)

# Save results
save(births_weeks_temp, file=paste0(path, "births_2011_2020_weeks_temp_final", ".RData"))

################################/
## Variables hum data  ----
################################/

rm(list = ls())

# Optimal use power computation 
gc()
options(future.globals.maxSize = 3000 * 1024^2)  
plan(multisession, workers = detectCores() - 4) # Parallelization

path <- "Data/Output/"
comunas <- chilemapas::codigos_territoriales |>
  mutate(codigo_comuna=as.numeric(codigo_comuna),
         codigo_provincia=as.numeric(codigo_provincia),
         codigo_region=as.numeric(codigo_region))
# Load data 
data_hum <- "births_2011_2020_weeks_hum.RData"
load(paste0(path, data_hum))
glimpse(births_weeks_hum)
length(unique(births_weeks_hum$id)) # 393951 unique obs

# Summary data 
#summary(births_weeks_hum)

# Adjust variable
births_weeks_hum <- births_weeks_hum |>
  mutate(
    # Outcomes 
    tbw=if_else(peso==9999, NA_real_, peso),
    ltbw=if_else(peso<2500, 1, 0), 
    # Other variables 
    weeks=if_else(semanas==99, NA_real_, semanas),
    #sex=if_else(sexo==9, NA_real_, sexo),
    size=if_else(talla==99, NA_real_, talla),
    age_mom=if_else(edad_madre==99, NA_real_, edad_madre),
    educ_mom=if_else(nivel_madre==9, NA_real_, nivel_madre),
    job_mom=if_else(activ_madre %in% c(9), NA_real_, activ_madre+1),
    age_dad=if_else(edad_padre==99, NA_real_, edad_padre),
    educ_dad=if_else(nivel_padre==9, NA_real_, nivel_padre),
    job_dad=if_else(activ_padre %in% c(3,9), NA_real_, activ_padre+1)
  ) |>
  mutate(
    sex=factor(sexo, levels=c(1,2,9), labels=c("Boy", "Girl", "Unknown"))
  ) |>
  left_join(comunas, by=c("comuna"="codigo_comuna")) |>
  mutate(zona = 
           case_when(
             nombre_region %in% c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo") ~ "Norte",
             nombre_region %in% c("Valparaiso", "Metropolitana de Santiago", "Libertador General Bernardo O'Higgins", "Maule", "Nuble", "Biobio") ~ "Centro",
             nombre_region %in% c("La Araucanía", "Los Ríos", "Los Lagos", "Aysen del General Carlos Ibanez del Campo", "Magallanes y de la Antartica Chilena") ~ "Sur",
             TRUE ~ NA_character_)) |>
  mutate(zone=factor(zona, 
                     levels=c("Norte", "Centro", "Sur"),
                     labels=c("North", "Center", "South")
  )) |>
  mutate(clim_zone = 
           case_when(
             #nombre_region %in% c("Arica y Parinacota") ~ "Desert hot",
             nombre_region %in% c("Arica y Parinacota") ~ "Desert",
             #nombre_region %in% c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo") ~ "Desert cold",
             nombre_region %in% c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo") ~ "Desert",
             nombre_region %in% c("Metropolitana de Santiago", "Libertador General Bernardo O'Higgins", "Maule") ~ "Temperate dry, hot summer",
             nombre_region %in% c("Valparaiso", "Nuble", "Biobio", "La Araucanía") ~ "Temperate dry, warm summer",
             nombre_region %in% c("Los Ríos", "Los Lagos", "Aysen del General Carlos Ibanez del Campo") ~ "Temperate, no dry season",
             nombre_region %in% c("Magallanes y de la Antartica Chilena") ~ "Cold steppe",
             TRUE ~ NA_character_)) |>
  mutate(clim_zone=factor(clim_zone, 
                          levels=c("Desert", "Temperate dry, hot summer", "Temperate dry, warm summer", "Temperate, no dry season", "Cold steppe"),
                          labels=c("Desert", "Temperate dry, hot summer", "Temperate dry, warm summer", "Temperate, no dry season", "Cold steppe")
  )) |>
  mutate(across(starts_with("hum_"), ~replace(., is.infinite(.), NA))) |>
  mutate(trim_gest =
           case_when(week_gest_num >= 1 & week_gest_num <= 12 ~ "T1", 
                     week_gest_num >= 13 & week_gest_num <= 24 ~ "T2", 
                     week_gest_num >= 25 ~ "T3",
                     TRUE ~ NA_character_
           )) |>
  mutate(ntbw=if_else(weeks<=37, 1, 0)) |>
  mutate(date_week1 = date_start, 
         year_week1 = year(date_week1), 
         month_week1 = month(date_week1),
         ym_week1 = as.yearmon(date_week1, "%M-%Y")) |>
  filter(!is.na(weeks)) |>
  filter(!is.na(tbw)) |>
  rename(day_nac=dia_nac,
         month_nac=mes_nac,
         year_nac=ano_nac,
         com=comuna, 
         reg=codigo_region,
         name_reg=nombre_region, 
         date_start_week_gest=date_start_week,
         date_ends_week_gest=date_end_week,
         name_reg=nombre_region, 
         name_comuna=nombre_comuna
  ) |>  
  select(I, id, tbw,ltbw, 
         # Other variables 
         date_nac, day_nac, month_nac, year_nac, 
         date_week1, year_week1, month_week1, ym_week1,
         weeks, date_start, date_end, week_gest, week_gest_num, trim_gest,
         date_start_week_gest, date_ends_week_gest, 
         sex,size,
         age_mom,educ_mom,job_mom,
         age_dad,educ_dad,job_dad,
         reg, name_reg, com, name_comuna, zone, clim_zone,
         hum_mean, hum_min, hum_max, 
         hum_mean_min, hum_mean_max,
  ) |>
  mutate(
    age_group_mom=case_when(
      age_mom <= 20 ~ 1, 
      age_mom > 20 & age_mom <= 29 ~ 2,
      age_mom >= 30 & age_mom <= 39 ~ 3,
      age_mom >= 40 & age_mom <= 49 ~ 4,
      age_mom >= 50 ~ 5, 
      TRUE ~ 6
    ),
    age_group_mom=factor(age_group_mom, 
                         levels=c(1:6), 
                         labels=c("<=20", "20-29", "30-39", "40-49", ">=50", "Unknown")),
    educ_group_mom = case_when(
      educ_mom == 1 ~ 4, # College
      educ_mom == 2 ~ 3, # Secondary
      educ_mom == 3 ~ 3, # Secondary
      educ_mom == 4 ~ 2, # Primary
      educ_mom == 5 ~ 1, # No educaction 
      TRUE ~ 5, #Unknow
    ), 
    educ_group_mom = factor(educ_group_mom, 
                            levels = c(1:5), 
                            labels = c("Non education", "Primary", "Secondary", "College", "Unknown")),
    job_group_mom = if_else(is.na(job_mom), 4, job_mom), 
    job_group_mom = factor(job_group_mom, levels = c(1,2,3,4), labels=c("Not working", "Employed", "Unemployed", "Unknown"))
  ) |>
  relocate(age_group_mom, educ_group_mom, job_group_mom, .after=job_mom) |>
  mutate(
    age_group_dad=case_when(
      age_dad <= 20 ~ 1, 
      age_dad > 20 & age_dad <= 29 ~ 2,
      age_dad >= 30 & age_dad <= 39 ~ 3,
      age_dad >= 40 & age_dad <= 49 ~ 4,
      age_dad >= 50 ~ 5, 
      TRUE ~ 6
    ),
    age_group_dad=factor(age_group_dad, 
                         levels=c(1:6), 
                         labels=c("<=20", "20-29", "30-39", "40-49", ">=50", "Unknown")),
    educ_group_dad = case_when(
      educ_dad == 1 ~ 4, # College
      educ_dad == 2 ~ 3, # Secondary
      educ_dad == 3 ~ 3, # Secondary
      educ_dad == 4 ~ 2, # Primary
      educ_dad == 5 ~ 1, # No educaction 
      TRUE ~ 5, #Unknow
    ), 
    educ_group_dad = factor(educ_group_dad, 
                            levels = c(1:5), 
                            labels = c("Non education", "Primary", "Secondary", "College", "Unknown")),
    job_group_dad = if_else(is.na(job_dad), 4, job_dad), 
    job_group_dad = factor(job_group_dad, levels = c(1,2,3,4), labels=c("Not working", "Employed", "Unemployed", "Unknown"))
  ) |>
  relocate(age_group_dad, educ_group_dad, job_group_dad, .after=job_dad) |>
  group_by(id) |>
  mutate(hum_mean_gest = mean(hum_mean, na.rm = TRUE)) |>
  group_by(id) |>
  mutate(hum_min_gest = mean(hum_min, na.rm = TRUE)) |>
  group_by(id) |>
  mutate(hum_max_gest = mean(hum_max, na.rm = TRUE)) |>
  ungroup() |>
  group_by(id, trim_gest) %>%
  mutate(hum_mean_trim_gest = mean(hum_mean, na.rm = TRUE)) |>
  ungroup() |>
  group_by(id, trim_gest) %>%
  mutate(hum_min_trim_gest = mean(hum_min, na.rm = TRUE)) |>
  ungroup() |>
  group_by(id, trim_gest) %>%
  mutate(hum_max_trim_gest = mean(hum_max, na.rm = TRUE)) |>
  ungroup() |>
  group_by(week_gest_num) |>
  mutate(hum_mean_percentile_w = ntile(hum_mean, 10)) |>
  ungroup() |>
  group_by(clim_zone) |>
  mutate(hum_mean_percentile_cz = ntile(hum_mean, 10)) |>
  #mutate(hum_mean_percentile100_cz = ntile(hum_mean, 100)) |>
  group_by(clim_zone, week_gest_num) |>
  mutate(hum_mean_percentile_wcz = ntile(hum_mean, 10)) |>
  ungroup() |>
  group_by(clim_zone, trim_gest) |>
  mutate(hum_mean_percentile_tcz = ntile(hum_mean_trim_gest, 10)) |>
  ungroup() |>
  group_by(week_gest_num) |>
  mutate(hum_min_percentile_w = ntile(hum_min, 10)) |>
  ungroup() |>
  group_by(clim_zone) |>
  mutate(hum_min_percentile_cz = ntile(hum_min, 10)) |>
  #mutate(hum_min_percentile100_cz = ntile(hum_min, 100)) |>
  group_by(clim_zone, week_gest_num) |>
  mutate(hum_min_percentile_wcz = ntile(hum_min, 10)) |>
  ungroup() |>
  group_by(clim_zone, trim_gest) |>
  mutate(hum_min_percentile_tcz = ntile(hum_min_trim_gest, 10)) |>
  ungroup() |>
  group_by(week_gest_num) |>
  mutate(hum_max_percentile_w = ntile(hum_max, 10)) |>
  ungroup() |>
  group_by(clim_zone) |>
  mutate(hum_max_percentile_cz = ntile(hum_max, 10)) |>
  #mutate(hum_max_percentile100_cz = ntile(hum_max, 100)) |>
  group_by(clim_zone, week_gest_num) |>
  mutate(hum_max_percentile_wcz = ntile(hum_max, 10)) |>
  ungroup() |>
  group_by(clim_zone, trim_gest) |>
  mutate(hum_max_percentile_tcz = ntile(hum_max_trim_gest, 10)) |>
  ungroup() |>
  mutate(dplyr::across(
    .cols = contains("_percentile_"),
    .fns = ~ factor(case_when(
      . == 1 ~ "<=10",
      . == 2 ~ "11-20",
      . == 3 ~ "21-30",
      . == 4 ~ "31-40",
      . == 5 ~ "41-50",
      . == 6 ~ "51-60",
      . == 7 ~ "61-70",
      . == 8 ~ "71-80",
      . == 9 ~ "81-90",
      . == 10 ~ "> 90",
      TRUE ~ as.character(NA)
    ), levels = c(
      "<=10", 
      "11-20",
      "21-30",
      "31-40",
      "41-50",
      "51-60",
      "61-70",
      "71-80",
      "81-90",
      "> 90"
    ))
  ))  

# |> 
#   mutate(hum_mean_percentile_tcz=paste0(trim_gest, "_", hum_mean_percentile_tcz)) |> 
#   mutate(hum_min_percentile_tcz=paste0(trim_gest, "_", hum_min_percentile_tcz)) |> 
#   mutate(hum_max_percentile_tcz=paste0(trim_gest, "_", hum_max_percentile_tcz))


## Add vulnerability info
load(paste0("Data/Input/", "sovi_datasets", ".RData"))
glimpse(sovi_subset)
births_weeks_hum <- 
  births_weeks_hum |>
  left_join(select(sovi_subset, -name_comuna), 
            by=c("com"="cod_com")) |>
  relocate(sovi, vulnerablidad, .after = name_comuna) |>
  rename(vulnerability = vulnerablidad) |>
  mutate(vulnerability = case_when(
    vulnerability=="Baja" ~ "Low",
    vulnerability=="Medio-baja" ~ "Medium",
    vulnerability=="Medio-alta" ~ "Medium",
    vulnerability=="Alta" ~ "High", 
    TRUE ~ NA_character_
  )) |>
  mutate(vulnerability=factor(vulnerability,
                              levels = c("Low", "Medium", "High")))

# |> 
#   mutate(temp_mean_percentile_vcz=paste0(vulnerability, "_", hum_mean_percentile_wcz)) |> 
#   mutate(temp_min_percentile_vcz=paste0(vulnerability, "_", hum_min_percentile_wcz)) |> 
#   mutate(temp_max_percentile_vcz=paste0(vulnerability, "_", hum_max_percentile_wcz))

# Summary data 
#summary(births_weeks_hum)
glimpse(births_weeks_hum)

# Save results
save(births_weeks_hum, file=paste0(path, "births_2011_2020_weeks_hum_final", ".RData"))
beepr::beep(5)
