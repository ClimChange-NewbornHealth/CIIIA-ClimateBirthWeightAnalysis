# Code 2: Temp data preparation ----

## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"


## Birth data ---- 

# ID file load
nac <- "births_2011_2020.RData"

# Open data in R
load(paste0(data_out, nac)) 

## Temp data ---- 

# ID file load
temp <- "resumen_temp2011_2020.xlsx"

# Open data in R (AJUSTAR COMUNAS)
temp <- rio::import(paste0(data_inp, temp)) %>% janitor::clean_names()
temp <- temp %>% 
  mutate(comuna_adj = case_when(
    comuna == "Curicó" ~ "Curico", 
    comuna == "Los Ángeles" ~ "Los Angeles", 
    comuna == "Chile Chico" ~ "Chile chico", 
    comuna == "Isla Juan Fernández" ~ "Isla Juán Fernández", 
    TRUE ~ comuna
  ))

# District data 
district <- import(paste0(data_inp, "comunas_sample.xlsx")) %>% select(com, code_com)
district_name <- district$com
setdiff(unique(temp$comuna_adj), district_name) 

temp <- temp %>% 
  select(-comuna) %>% 
  left_join(district, by=c("comuna_adj"="com")) %>% 
  rename(comuna = code_com)

# Adjust dates
temp <- temp %>% 
  mutate(fecha=lubridate::as_date(fecha)) %>%  # Year, Month, Date 
  drop_na(comuna)

# Save results 
save(temp, file=paste0(data_out, "temp_2011_2020", ".RData"))
