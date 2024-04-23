# Code 5: Append final data ---

## Settings ----
source("Code/0.2 Settings.R")

## Data load ----

# Directory path 
path_files <- "Data/Output/temp_data"

# List all files .RData in the directory
files <- list.files(path = path_files, pattern = "births_temp_id\\d+\\.RData", full.names = TRUE)
files

## Iterative open ----

# List save all files 
list_dfs <- list()

# load files and add list
for (i in seq_along(files)) {
  load(files[i]) 
  list_dfs[[length(list_dfs) + 1]] <- results
}

# Append all data in a object 
df_final <- bind_rows(list_dfs)

# Clean temporal memory
rm(list = ls()[!ls() %in%  c("df_final")])

# Save results
save(df_final, file=paste0("Data/Output/", "births_2011_2020_weeks_temp", ".RData"))
