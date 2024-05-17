# Code 5: Append final data ---

## Settings ----
rm(list=ls())
source("Code/0.2 Settings.R")

# Save results
load_and_extract_df <- function(file_path) {
  e <- new.env()  # Crea un nuevo entorno para cargar los datos
  load(file_path, envir = e)  # Carga el archivo en el entorno nuevo
  # Extrae el dataframe asumiendo que es el único objeto en el entorno
  df <- e[[names(e)[1]]]  # Accede al primer objeto en el entorno, que debe ser el dataframe
  return(df)
}

path_to_files <- "Data/Output/parts_data_final_temp"  # Asegúrate de que este sea el path correcto a tu carpeta
file_list <- list.files(path = path_to_files, pattern = "*.RDS", full.names = TRUE)
births_weeks_temp <- map_df(file_list, load_and_extract_df)

save(births_weeks_temp, file=paste0("Data/Output/", "births_2011_2020_weeks_temp", ".RData"))
#save(births_weeks_hum, file=paste0("Data/Output/", "births_2011_2020_weeks_hum", ".RData"))


path_to_files <- "Data/Output/parts_data_final_hum"  # Asegúrate de que este sea el path correcto a tu carpeta
file_list <- list.files(path = path_to_files, pattern = "*.RDS", full.names = TRUE)
births_weeks_temp <- map_df(file_list, load_and_extract_df)

save(births_weeks_temp, file=paste0("Data/Output/", "births_2011_2020_weeks_hum", ".RData"))


