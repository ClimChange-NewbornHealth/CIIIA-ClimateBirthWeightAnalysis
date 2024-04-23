# Code 4: Create Births with temp/hum data ---

## Settings ----
source("Code/0.2 Settings.R")
source("Code/3.0 Functions_tem_hum.R")
# source("Code/1.0 Birth_data.R")
# source("Code/2.0 Temp_data.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

## Birth data ---- 

#nac <- "births_2011_2020.RData"
#test <- "test_births_2011_2020_weeks.RData"
#load(paste0(data_out, test))  

nac_weeks <- "births_2011_2020_weeks.RData" # ID file load
load(paste0(data_out, nac_weeks))  # Open data in R

## Temp data ---- 

temp <- "temp_2011_2020.RData" # ID file load
load(paste0(data_out, temp)) # Open data in R

## Multijoin Data ----

# Clean memory
rm(list = ls()[!ls() %in%  c("temp", 
                             "births_weeks", # "data_test", 
                             "calculate_temperature_stats",
                             "process_chunk_t", 
                             "gen_bt_data",
                             "data_out"
)])

# Small data
#setDT(data_test)
setDT(births_weeks)
#parts <- split(data_test, data_test$id) # Dividir en partes de N filas
parts <- split(births_weeks, births_weeks$id) # Dividir en partes de N filas
rm(births_weeks) # Remueve la data original que es un objeto muy pesado
#save.image(file=paste0(data_out, "births_2011_2020_weeks_list", ".RData"))

# Cores PC USE
plan(multisession, workers = detectCores() - 2) 

### Temperature ----

# Example
# gen_bt_data(input = parts, id_start = 1, id_end = 100000, data_out = "Data/Output/") time: 
# start <- min(seq_along(parts))
# end <- max(seq_along(parts))

#load(file=paste0(data_out, "births_2011_2020_weeks_list", ".RData"))
gen_bt_data(input = parts, id_start = 1, id_end = 1000, data_out = "Data/Output/")
beepr::beep(8)

