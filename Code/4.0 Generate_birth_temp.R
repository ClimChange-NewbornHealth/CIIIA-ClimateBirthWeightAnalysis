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
test <- "test_births_2011_2020_weeks.RData"
load(paste0(data_out, test))  

nac_weeks <- "births_2011_2020_weeks.RData" # ID file load
load(paste0(data_out, nac_weeks))  # Open data in R

## Temp data ---- 

temp <- "temp_2011_2020.RData" # ID file load
load(paste0(data_out, temp)) # Open data in R

## Multijoin Data ----

# Data in parts 
# Dado que la carga computacional es muy grande, optaremos por dividir los datos en 50 trozos q
# Guardaremos estos trozo y los procesaremos uno a uno. 
start <- Sys.time()
parts(data=births_weeks, 
      path="Data/Output/",
      folder="parts_data_final_births",
      num_parts = 50)
end <- Sys.time()
end-start 


## Clean memory use
rm(list = ls()[!ls() %in%  c("temp", 
                             "calculate_temperature_stats",
                             "calculate_humidity_stats",
                             "process_files"
                             
)])

gc() # Explicit clean memory

# Temp Analysis
start <- Sys.time()
process_files(input_directory = "Data/Output/parts_data_final_births", 
              output_directory = "Data/Output/parts_data_final_temp", 
              temp_data = temp,
              calc_func = calculate_temperature_stats)
end <- Sys.time()
end-start 
beepr::beep(1)

# Hum Analysis
start <- Sys.time()
process_files(input_directory = "Data/Output/parts_data_final_births", 
              output_directory = "Data/Output/parts_data_final_hum", 
              hum_data = temp,
              calc_func = calculate_humidity_stats)
end <- Sys.time()
end-start 
beepr::beep(1)

