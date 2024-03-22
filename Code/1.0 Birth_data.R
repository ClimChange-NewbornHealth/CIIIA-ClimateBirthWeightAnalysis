# Code 1: Birth data preparation ---

 
source("Code/0.1 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

## Birth data ---- 

# ID file load
file <- "data_1992_2020.RData"

# Open data in R
load(paste0(data_inp, file)) 

data_1992_2020 <- data_1992_2020 %>% janitor::clean_names()

# Explorer data 
glimpse(data_1992_2020)

# Prepare data
# 1. Date 2011-2020
table(data_1992_2020$ano_nac)

# 2. 27 districts
table(data_1992_2020$comuna)

district <- import(paste0(data_inp, "comunas_sample.xlsx")) %>% select(code_com)
district <- district$code_com

births <- data_1992_2020 %>% 
  filter(ano_nac>=2011 & ano_nac<=2020) %>% 
  filter(comuna %in% district)

# Check results
table(births$ano_nac)
table(births$comuna)
table(data_1992_2020$comuna)

## Save new births data ----
save(births, file=paste0(data_out, "births_2011_2020", ".RData"))

