# Code 3: Function join births (test) and tempertature ----

########################################
## Data frame in parts -------
########################################

parts <- function(data, path, folder, num_parts = 20) {
  # Calcula el tamaño de cada parte
  part_size <- ceiling(nrow(data) / num_parts)
  
  # Procesar cada parte
  for (part_id in 1:num_parts) {
    # Divide los datos en la parte correspondiente
    part_data <- data[((part_id - 1) * part_size + 1):min(part_id * part_size, nrow(data)), ]
    
    # Guarda el resultado en un archivo CSV con un ID único para cada parte
    save(part_data, file=sprintf(paste0(path, folder, "/part_%02d_results.RData"), part_id))
  }
}

########################################
## Temperature/Humidity variables -------
########################################

### Calculate stats -----
#### Temp stats -----
calculate_temperature_stats <- function(row, temp_data) {
  # Asegurarse que tanto row como temp_data son data.table
  setDT(row)
  setDT(temp_data)
  
  # Convertir fechas solo si es necesario
  row_copy <- copy(row)
  row_copy[, date_start_week := as.Date(date_start_week)]
  row_copy[, date_end_week := as.Date(date_end_week)]
  
  # Filtrar los datos de temperatura
  week_temperatures <- temp_data[fecha >= row_copy$date_start_week[1] & fecha < row_copy$date_end_week[1] & comuna == row_copy$comuna[1]]
  
  if (nrow(week_temperatures) == 0) {
    return(data.table(row_copy, temp_mean=NA_real_, temp_min=NA_real_, 
                      temp_max=NA_real_, temp_mean_min=NA_real_, temp_mean_max=NA_real_))
  } else {
    return(data.table(row_copy, 
                      temp_mean = round(mean(as.numeric(week_temperatures$xt_diario), na.rm = TRUE), 3),
                      temp_min = min(as.numeric(week_temperatures$t_min), na.rm = TRUE),
                      temp_max = max(as.numeric(week_temperatures$t_max), na.rm = TRUE),
                      temp_mean_min = round(mean(as.numeric(week_temperatures$t_min), na.rm = TRUE), 3),
                      temp_mean_max = round(mean(as.numeric(week_temperatures$t_max), na.rm = TRUE), 3))
                      )
  }
}


#### Humidity stats -----
calculate_humidity_stats <- function(row, hum_data) {
  # Asegurarse que tanto row como temp_data son data.table
  setDT(row)
  setDT(temp_data)
  
  # Convertir fechas solo si es necesario
  row_copy <- copy(row)
  row_copy[, date_start_week := as.Date(date_start_week)]
  row_copy[, date_end_week := as.Date(date_end_week)]
  
  # Filtrar los datos de temperatura
  week_hum <- hum_data[fecha >= row_copy$date_start_week[1] & fecha < row_copy$date_end_week[1] & comuna == row_copy$comuna[1]]
  
  if (nrow(week_hum) == 0) {
    return(data.table(row_copy, hum_mean=NA_real_, hum_min=NA_real_, 
                      hum_max=NA_real_, hum_mean_min=NA_real_, hum_mean_max=NA_real_))
  } else {
    return(data.table(row_copy, 
                      hum_mean = round(mean(as.numeric(week_temperatures$xh_diario), na.rm = TRUE), 3),
                      hum_min = min(as.numeric(week_temperatures$h_min), na.rm = TRUE),
                      hum_max = max(as.numeric(week_temperatures$h_max), na.rm = TRUE),
                      hum_mean_min = round(mean(as.numeric(week_temperatures$h_min), na.rm = TRUE), 3),
                      hum_mean_max = round(mean(as.numeric(week_temperatures$h_max), na.rm = TRUE), 3))
    )
  }
}

### Process Global -----

# Function process files
process_files <- function(input_directory, output_directory, temp_data, calc_func) {
  
  # Assure temp_data is available
  setDT(temp_data)
  
  files <- list.files(path = input_directory, full.names = TRUE, pattern = "\\.RData$")
  file_count <- 0
  
  for (file_path in files) {
    
    start <- Sys.time()
    
    file_count <- file_count + 1
    load(file_path)
    setDT(part_data)
    
    # Apply calculation function to each row as a data.table slice
    results <- part_data[, calc_func(.SD, temp_data), by = .I]
    
    # Save results
    save(results, file = file.path(output_directory, sprintf("%s_processed.RDS", tools::file_path_sans_ext(basename(file_path)))))
    
    end <- Sys.time()
    cat("Time process data:", end-start, "\n") 
    
    if (file_count %% 5 == 0) {
      cat("Pause for 30 seconds to avoid overload...\n")
      Sys.sleep(30)
    }
    
    
  }
  
  cat("All files have been processed and saved in:", output_directory, "\n")
}

# Optimal use power computation 
options(future.globals.maxSize = 3000 * 1024^2)  
plan(multisession, workers = detectCores() - 4) # Parallelization























