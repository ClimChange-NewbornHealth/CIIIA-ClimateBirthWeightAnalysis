# Code 3: Function join births (test) and tempertature ----

## Temperature variables -------
calculate_temperature_stats <- function(start_date, end_date, comuna_id, temp_data) {
  # Asegurar que temp_data es un data.table
  setDT(temp_data)
  
  # Filtrar los datos de temperatura dentro del rango de fechas y por comuna específica usando data.table
  week_temperatures <- temp_data[fecha >= start_date & fecha < end_date & comuna == comuna_id,
                                 .(t_min, t_max, xt_diario)]
  
  # Si no hay datos, retornar NA para cada estadística
  if (nrow(week_temperatures) == 0) {
    return(as.data.table(list(temp_mean=NA, temp_min=NA, temp_max=NA,
                              temp_mean_min=NA, temp_mean_max=NA,
                              temp_q05=NA, temp_q10=NA, temp_q20=NA, temp_q30=NA,
                              temp_q40=NA, temp_q50=NA, temp_q60=NA, temp_q70=NA,
                              temp_q80=NA, temp_q90=NA, temp_q95=NA)))
  }
  
  # Calcular y retornar estadísticas usando data.table
  return(list(
    temp_mean = mean(week_temperatures$xt_diario, na.rm = TRUE),
    temp_min = min(week_temperatures$t_min, na.rm = TRUE),
    temp_max = max(week_temperatures$t_max, na.rm = TRUE),
    temp_mean_min = mean(week_temperatures$t_min, na.rm = TRUE),
    temp_mean_max = mean(week_temperatures$t_max, na.rm = TRUE),
    temp_q05 = quantile(week_temperatures$xt_diario, probs = 0.05, na.rm = TRUE),
    temp_q10 = quantile(week_temperatures$xt_diario, probs = 0.1, na.rm = TRUE),
    temp_q20 = quantile(week_temperatures$xt_diario, probs = 0.2, na.rm = TRUE),
    temp_q30 = quantile(week_temperatures$xt_diario, probs = 0.3, na.rm = TRUE),
    temp_q40 = quantile(week_temperatures$xt_diario, probs = 0.4, na.rm = TRUE),
    temp_q50 = quantile(week_temperatures$xt_diario, probs = 0.5, na.rm = TRUE),
    temp_q60 = quantile(week_temperatures$xt_diario, probs = 0.6, na.rm = TRUE),
    temp_q70 = quantile(week_temperatures$xt_diario, probs = 0.7, na.rm = TRUE),
    temp_q80 = quantile(week_temperatures$xt_diario, probs = 0.8, na.rm = TRUE),
    temp_q90 = quantile(week_temperatures$xt_diario, probs = 0.9, na.rm = TRUE),
    temp_q95 = quantile(week_temperatures$xt_diario, probs = 0.95, na.rm = TRUE)
  ))
}

## Humidity variables -------

calculate_humidity_stats <- function(start_date, end_date, comuna_id, temp_data) {
  # Asegurar que temp_data es un data.table
  setDT(temp_data)
  
  # Filtrar los datos de humedad dentro del rango de fechas y por comuna específica usando data.table
  week_humidity <- temp_data[fecha >= start_date & fecha < end_date & comuna == comuna_id,
                                 .(h_min, h_max, ht_diario)]
  
  # Si no hay datos, retornar NA para cada estadística
  if (nrow(week_humidity) == 0) {
    return(as.data.table(list(hum_mean=NA, hum_min=NA, hum_max=NA,
                              hum_mean_min=NA, hum_mean_max=NA,
                              hum_q05=NA, hum_q10=NA, hum_q20=NA, hum_q30=NA,
                              hum_q40=NA, hum_q50=NA, hum_q60=NA, hum_q70=NA,
                              hum_q80=NA, hum_q90=NA, hum_q95=NA)))
  }
  
  # Calcular y retornar estadísticas usando data.table
  return(list(
    hum_mean = mean(week_humidity$xh_diario, na.rm = TRUE),
    hum_min = min(week_humidity$h_min, na.rm = TRUE),
    hum_max = max(week_humidity$h_max, na.rm = TRUE),
    hum_mean_min = mean(week_humidity$h_min, na.rm = TRUE),
    hum_mean_max = mean(week_humidity$h_max, na.rm = TRUE),
    hum_q05 = quantile(week_humidity$ht_diario, probs = 0.05, na.rm = TRUE),
    hum_q10 = quantile(week_humidity$ht_diario, probs = 0.1, na.rm = TRUE),
    hum_q20 = quantile(week_humidity$ht_diario, probs = 0.2, na.rm = TRUE),
    hum_q30 = quantile(week_humidity$ht_diario, probs = 0.3, na.rm = TRUE),
    hum_q40 = quantile(week_humidity$ht_diario, probs = 0.4, na.rm = TRUE),
    hum_q50 = quantile(week_humidity$ht_diario, probs = 0.5, na.rm = TRUE),
    hum_q60 = quantile(week_humidity$ht_diario, probs = 0.6, na.rm = TRUE),
    hum_q70 = quantile(week_humidity$ht_diario, probs = 0.7, na.rm = TRUE),
    hum_q80 = quantile(week_humidity$ht_diario, probs = 0.8, na.rm = TRUE),
    hum_q90 = quantile(week_humidity$ht_diario, probs = 0.9, na.rm = TRUE),
    hum_q95 = quantile(week_humidity$ht_diario, probs = 0.95, na.rm = TRUE)
  ))
}


## Chunk de ejecución -------

process_chunk_t <- function(chunk) {
  # Aplicar la función calculate_temperature_stats a cada fila de este chunk
  chunk[, c("temp_mean", "temp_min", "temp_max", "temp_mean_min", "temp_mean_max",
            "temp_q05", "temp_q10", "temp_q20", "temp_q30", "temp_q40", "temp_q50",
            "temp_q60", "temp_q70", "temp_q80", "temp_q90", "temp_q95") :=
          calculate_temperature_stats(date_start_week, date_end_week, comuna, temp),
        by = .(date_start_week, date_end_week, comuna)]
  return(chunk)
}

process_chunk_h <- function(chunk) {
  # Aplicar la función calculate_temperature_stats a cada fila de este chunk
  chunk[, c("hum_mean", "hum_min", "hum_max", "hum_mean_min", "hum_mean_max",
            "hum_q05", "hum_q10", "hum_q20", "hum_q30", "hum_q40", "hum_q50",
            "hum_q60", "hum_q70", "hum_q80", "hum_q90", "hum_q95") :=
          calculate_humidity_stats(date_start_week, date_end_week, comuna, temp),
        by = .(date_start_week, date_end_week, comuna)]
  return(chunk)
}

## Función de ejecución -------
gen_bt_data <- function(input, id_start, id_end, data_out){
  stime <- Sys.time()
  
  for(i in id_start:id_end){
    results <- future.apply::future_lapply(input[i], process_chunk_t)
    results <- rbindlist(results)
    
    # Save results
    save(results, file=paste0(data_out, "temp_data/","births_temp", "_id", i, ".RData"))
  }
  
  etime <- Sys.time()
  t1 <- etime - stime
  print(t1) # Time execution 
}

gen_bh_data <- function(input, id_start, id_end, data_out){
  stime <- Sys.time()
  
  for(i in id_start:id_end){
    results <- future.apply::future_lapply(input[i], process_chunk_h)
    results <- rbindlist(results)
    
    # Save results
    save(results, file=paste0(data_out, "hum_data/","births_temp", "_id", i, ".RData"))
  }
  
  etime <- Sys.time()
  t1 <- etime - stime
  print(t1) # Time execution 
}


