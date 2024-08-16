# 8.0 GAM Models ---

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
data_temp <- "births_2011_2020_weeks_temp_final.RData"
data_out <- "Output_analysis/temp/

