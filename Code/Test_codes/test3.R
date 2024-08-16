process_parts <- function(data, process_function, folder, num_parts = 20) {
  part_size <- ceiling(nrow(data) / num_parts)
  
  # Procesar cada parte en grupos de 5
  for (group_start in seq(1, num_parts, by = 5)) {
    cat(sprintf("Procesando partes %d a %d\n", group_start, min(group_start + 4, num_parts)))
    
    future_lapply(seq(group_start, min(group_start + 4, num_parts)), function(part_id) {
      # Divide los datos en la parte correspondiente
      part_data <- data[((part_id - 1) * part_size + 1):min(part_id * part_size, nrow(data)), ]
      
      # Transpone y procesa la parte
      result <- part_data %>%
        purrr::transpose() %>%
        future_map_dfr(~ process_function(.x), .progress = TRUE)
      
      # Guarda el resultado en un archivo RData con un ID único para cada parte
      saveRDS(result, file=sprintf(paste0("Data/Output/", folder, "/part_%02d_results.RData"), part_id))
    })
    
    # Descansa 3 minutos después de cada grupo de 5 partes
    if (group_start + 4 < num_parts) {
      cat("Descansando 5 minutos para liberar memoria...\n")
      Sys.sleep(300)  # Descansa 5 minutos
    }
  }
}

process_parts <- function(data, process_function, folder, num_parts = 20) {
  part_size <- ceiling(nrow(data) / num_parts)
  
  # Prepara los índices para cada parte
  parts_indices <- lapply(1:num_parts, function(part_id) {
    ((part_id - 1) * part_size + 1):min(part_id * part_size, nrow(data))
  })
  
  for (group_start in seq(1, num_parts, by = 5)) {
    cat(sprintf("Procesando partes %d a %d\n", group_start, min(group_start + 4, num_parts)))
    
    future_lapply(parts_indices[group_start:min(group_start + 4, num_parts)], function(indices) {
      # Procesa solo la parte específica de datos
      part_data <- data[indices, ]
      
      result <- part_data %>%
        purrr::transpose() %>%
        future_map_dfr(~ process_function(.x), .progress = TRUE)
      
      saveRDS(result, file=sprintf(paste0("Data/Output/", folder, "/part_%02d_results.RData"), indices[1] / part_size + 1))
    })
    
    if (group_start + 4 < num_parts) {
      cat("Descansando 3 minutos para liberar memoria...\n")
      Sys.sleep(300)  # Descansa 5 minutos
    }
  }
}












######################################
## Humidity variables -------
######################################

calculate_hum_stats <- function(row, temp_data) {
  # Adjust temp_data with data.table() format
  setDT(temp_data)
  
  # Dates are dates 
  if (!inherits(row$date_start_week, "Date")) {
    row$date_start_week <- as.Date(row$date_start_week)
  }
  if (!inherits(row$date_end_week, "Date")) {
    row$date_end_week <- as.Date(row$date_end_week)
  }
  
  # Extract values for each row
  start_date <- row$date_start_week
  end_date <- row$date_end_week
  comuna_id <- row$comuna
  
  # New temporal data with filter by time and district
  week_temperatures <- temp_data[fecha >= start_date & fecha < end_date & comuna == comuna_id, 
                                 .(h_min, h_max, xh_diario)]
  
  # Blank Results 
  result <- as.list(row)
  
  # NA with missing values when haven't information in temp data 
  if (nrow(week_temperatures) == 0) {
    result <- c(result, list(hum_mean=NA, hum_min=NA, hum_max=NA,
                             hum_mean_min=NA, hum_mean_max=NA))
    return(as.data.table(result))
  }
  
  # Calculate stats and return results as data.frame()
  result <- c(result, list(
    hum_mean = mean(week_temperatures$xh_diario, na.rm = TRUE),
    hum_min = min(week_temperatures$h_min, na.rm = TRUE),
    hum_max = max(week_temperatures$h_max, na.rm = TRUE),
    hum_mean_min = mean(week_temperatures$h_min, na.rm = TRUE),
    hum_mean_max = mean(week_temperatures$h_max, na.rm = TRUE)
  ))
  
  return(as.data.table(result))
}












process_parts <- function(data, process_function, folder, num_parts = 20) {
  part_size <- ceiling(nrow(data) / num_parts)
  
  # Procesar cada parte en grupos de 5
  for (group_start in seq(1, num_parts, by = 5)) {
    cat(sprintf("Procesando partes %d a %d\n", group_start, min(group_start + 4, num_parts)))
    
    future_lapply(seq(group_start, min(group_start + 4, num_parts)), function(part_id) {
      # Divide los datos en la parte correspondiente
      part_data <- data[((part_id - 1) * part_size + 1):min(part_id * part_size, nrow(data)), ]
      
      # Transpone y procesa la parte
      result <- part_data %>%
        purrr::transpose() %>%
        future_map_dfr(~ process_function(.x), .progress = TRUE)
      
      # Guarda el resultado en un archivo RData con un ID único para cada parte
      saveRDS(result, file=sprintf(paste0("Data/Output/", folder, "/part_%02d_results.RData"), part_id))
    })
    
    # Descansa 3 minutos después de cada grupo de 5 partes
    if (group_start + 4 < num_parts) {
      cat("Descansando 5 minutos para liberar memoria...\n")
      Sys.sleep(300)  # Descansa 5 minutos
    }
  }
}







process_temp_stats <- function(data) {
  calculate_temperature_stats(data, temp)  # Suponiendo que 'temp' está definido o es parte de 'data'
}

### Process applied each parts -----

process_parts <- function(data, process_function, folder, num_parts = 10) {
  # Calcula el tamaño de cada parte
  part_size <- ceiling(nrow(data) / num_parts)
  
  # Procesar cada parte
  for (part_id in 1:num_parts) {
    # Divide los datos en la parte correspondiente
    part_data <- data[((part_id - 1) * part_size + 1):min(part_id * part_size, nrow(data)), ]
    
    # Transpone y procesa la parte
    result <- part_data %>%
      purrr::transpose() %>%
      future_map_dfr(~ process_function(.x), .progress = TRUE)
    
    # Guarda el resultado en un archivo CSV con un ID único para cada parte
    save(result, file=sprintf(paste0("Data/Output/", folder, "/part_%02d_results.RData"), part_id))
  }
}

process_parts <- function(data, process_function, folder, num_parts = 20) {
  part_size <- ceiling(nrow(data) / num_parts)
  
  # Procesar cada parte en grupos de 5
  for (group_start in seq(1, num_parts, by = 5)) {
    cat(sprintf("Procesando partes %d a %d\n", group_start, min(group_start + 4, num_parts)))
    
    future_lapply(seq(group_start, min(group_start + 4, num_parts)), function(part_id) {
      # Divide los datos en la parte correspondiente
      part_data <- data[((part_id - 1) * part_size + 1):min(part_id * part_size, nrow(data)), ]
      
      # Transpone y procesa la parte
      result <- part_data %>%
        purrr::transpose() %>%
        future_map_dfr(~ process_function(.x), .progress = TRUE)
      
      # Guarda el resultado en un archivo RData con un ID único para cada parte
      saveRDS(result, file=sprintf(paste0("Data/Output/", folder, "/part_%02d_results.RData"), part_id))
    })
    
    # Descansa 3 minutos después de cada grupo de 5 partes
    if (group_start + 4 < num_parts) {
      cat("Descansando 5 minutos para liberar memoria...\n")
      Sys.sleep(300)  # Descansa 5 minutos
    }
  }
}

process_parts <- function(data, process_function, folder, num_parts = 20) {
  part_size <- ceiling(nrow(data) / num_parts)
  
  # Prepara los índices para cada parte
  parts_indices <- lapply(1:num_parts, function(part_id) {
    ((part_id - 1) * part_size + 1):min(part_id * part_size, nrow(data))
  })
  
  for (group_start in seq(1, num_parts, by = 5)) {
    cat(sprintf("Procesando partes %d a %d\n", group_start, min(group_start + 4, num_parts)))
    
    future_lapply(parts_indices[group_start:min(group_start + 4, num_parts)], function(indices) {
      # Procesa solo la parte específica de datos
      part_data <- data[indices, ]
      
      result <- part_data %>%
        purrr::transpose() %>%
        future_map_dfr(~ process_function(.x), .progress = TRUE)
      
      saveRDS(result, file=sprintf(paste0("Data/Output/", folder, "/part_%02d_results.RData"), indices[1] / part_size + 1))
    })
    
    if (group_start + 4 < num_parts) {
      cat("Descansando 3 minutos para liberar memoria...\n")
      Sys.sleep(300)  # Descansa 5 minutos
    }
  }
}

