# Test ----
multi_join_temp <- function(df_births, df_temp_hum) {
  
  # Data table format
  setDT(df_births)
  setDT(df_temp_hum)
  
  data_births_temp <- df_births %>%
    rowwise() %>%
    mutate(
      temp_mean = mean(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                               df_temp_hum$fecha >= date_start_week & 
                                               df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      temp_min = min(df_temp_hum$t_min[df_temp_hum$comuna == comuna & 
                                         df_temp_hum$fecha >= date_start_week & 
                                         df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      temp_max = max(df_temp_hum$t_max[df_temp_hum$comuna == comuna & 
                                         df_temp_hum$fecha >= date_start_week & 
                                         df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      temp_mean_min = mean(df_temp_hum$t_min[df_temp_hum$comuna == comuna & 
                                               df_temp_hum$fecha >= date_start_week & 
                                               df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      temp_mean_max = mean(df_temp_hum$t_max[df_temp_hum$comuna == comuna & 
                                               df_temp_hum$fecha >= date_start_week & 
                                               df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      temp_q05 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.05),
      
      temp_q10 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.1),
      
      temp_q20 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.2),
      
      temp_q30 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.3),
      
      temp_q40 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.4),
      
      temp_q50 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.5),
      
      temp_q60 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.6),
      
      temp_q70 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.7),
      
      temp_q80 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.8),
      
      temp_q90 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.9),
      
      temp_q95 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.95)
    )
  return(data_births_temp)
}


# Test II ----

source("Code/0.2 Settings.R")

test <- "test_births_2011_2020_weeks.RData"
load(paste0("Data/Output/", test))  

temp <- "temp_2011_2020.RData"
load(paste0("Data/Output/", temp)) 


parts <- split(data_test, data_test$id)  # Dividir en partes de N filas

plan(multisession, workers = detectCores() - 2) 
stime <- Sys.time() 
temperature_data <- parts %>% future_map_dfr(~ multi_join_temp(df_births = .x, df_temp_hum = temp), .progress = TRUE)
etime <- Sys.time()
t1 <- etime - stime 
t1


# Opción II ----

source("Code/0.2 Settings.R")

test <- "test_births_2011_2020_weeks.RData"
load(paste0("Data/Output/", test))  

temp <- "temp_2011_2020.RData"
load(paste0("Data/Output/", temp)) 


multi_join_temp <- function(df_births, df_temp_hum) {
  # Data table format
  setDT(df_births)
  setDT(df_temp_hum)
  
  # Realizar un join no equi para vincular los registros de temperatura dentro del rango de fechas especificado
  joined_data <- df_births[df_temp_hum, on = .(comuna = comuna,
                                               date_start_week <= fecha, 
                                               date_end_week > fecha),
                           allow.cartesian = TRUE]
  
  # Calcular las estadísticas requeridas para cada grupo definido por las filas de df_births
  results <- joined_data[, .(
    temp_mean = mean(xt_diario, na.rm = TRUE),
    temp_min = min(t_min, na.rm = TRUE),
    temp_max = max(t_max, na.rm = TRUE),
    temp_mean_min = mean(t_min, na.rm = TRUE),
    temp_mean_max = mean(t_max, na.rm = TRUE),
    temp_q05= quantile(xt_diario, probs = 0.05, na.rm = TRUE),
    temp_q10 = quantile(xt_diario, probs = 0.1, na.rm = TRUE),
    temp_q20 = quantile(xt_diario, probs = 0.2, na.rm = TRUE),
    temp_q30 = quantile(xt_diario, probs = 0.3, na.rm = TRUE),
    temp_q40 = quantile(xt_diario, probs = 0.4, na.rm = TRUE),
    temp_q50 = quantile(xt_diario, probs = 0.5, na.rm = TRUE),
    temp_q60 = quantile(xt_diario, probs = 0.6, na.rm = TRUE),
    temp_q70 = quantile(xt_diario, probs = 0.7, na.rm = TRUE),
    temp_q80 = quantile(xt_diario, probs = 0.8, na.rm = TRUE),
    temp_q90 = quantile(xt_diario, probs = 0.9, na.rm = TRUE),
    temp_q95= quantile(xt_diario, probs = 0.95, na.rm = TRUE)
  ), by = .(comuna, date_start_week, date_end_week)]
  
  # Unir los resultados calculados con df_births para mantener cualquier otra columna relevante
  final_results <- merge(joined_data, results, by = c("comuna", "date_start_week", "date_end_week"), all.x = TRUE)
  
  return(final_results)

}

data_exp <- data_test[1:38,]

save(data_exp, file="Data_exp.RData")

test2 <- multi_join_temp(df_births = data_test, df_temp_hum = temp)


multi_join_temp <- function(df_births, df_temp_hum) {
  # Set data tables
  setDT(df_births)
  setDT(df_temp_hum)
  
  # Join df_births with df_temp_hum based on comuna
  joined_data <- df_births[df_temp_hum, on = .(comuna = comuna),
                           allow.cartesian = TRUE,
                           nomatch = 0]
  
  # Create date range columns for each week
  joined_data[, date_range := as.Date(date_start_week):as.Date(date_end_week)]
  
  # Calculate descriptive statistics for each week and comuna
  data_births_temp <- joined_data[, .(
    comuna = comuna,
    date_start_week = date_start_week,
    date_end_week = date_end_week,
    temp_mean = mean(xt_diario[fecha %in% date_range], na.rm = TRUE),
    temp_min = min(t_min[fecha %in% date_range], na.rm = TRUE),
    temp_max = max(t_max[fecha %in% date_range], na.rm = TRUE),
    # ... (other temperature statistics)
    temp_q05 = quantile(xt_diario[fecha %in% date_range], probs = 0.05, na.rm = TRUE),
    temp_q10 = quantile(xt_diario[fecha %in% date_range], probs = 0.1, na.rm = TRUE),
    # ... (other quantile calculations)
  ), by = .(comuna, date_start_week, date_end_week)]
  
  # Remove temporary date range column
  data_births_temp[, date_range := NULL]
  
  return(data_births_temp)
}

test2 <- multi_join_temp(df_births = data_exp, df_temp_hum = temp)


parts <- split(data_test, data_test$id)  # Dividir en partes de N filas

plan(multisession, workers = detectCores() - 2) 
stime <- Sys.time() 
temperature_data2 <- parts %>% future_map_dfr(~ multi_join_temp(df_births = .x, df_temp_hum = temp), .progress = TRUE)
etime <- Sys.time()
t2 <- etime - stime 
t2




## OLD FUNCTIONS ----

multi_join_temp <- function(df_births, df_temp_hum) {
  
  # Data table format
  setDT(df_births)
  setDT(df_temp_hum)
  
  data_births_temp <- df_births %>%
    rowwise() %>%
    mutate(
      temp_mean = mean(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                               df_temp_hum$fecha >= date_start_week & 
                                               df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      temp_min = min(df_temp_hum$t_min[df_temp_hum$comuna == comuna & 
                                         df_temp_hum$fecha >= date_start_week & 
                                         df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      temp_max = max(df_temp_hum$t_max[df_temp_hum$comuna == comuna & 
                                         df_temp_hum$fecha >= date_start_week & 
                                         df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      temp_mean_min = mean(df_temp_hum$t_min[df_temp_hum$comuna == comuna & 
                                               df_temp_hum$fecha >= date_start_week & 
                                               df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      temp_mean_max = mean(df_temp_hum$t_max[df_temp_hum$comuna == comuna & 
                                               df_temp_hum$fecha >= date_start_week & 
                                               df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      temp_q10 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.1),
      
      temp_q20 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.2),
      
      temp_q30 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.3),
      
      temp_q40 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.4),
      
      temp_q50 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.5),
      
      temp_q60 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.6),
      
      temp_q70 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.7),
      
      temp_q80 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.8),
      
      temp_q90 = quantile(df_temp_hum$xt_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.9),
    )
  return(data_births_temp)
}


multi_join_hum <- function(df_births, df_temp_hum) {
  
  # Data table format
  setDT(df_births)
  setDT(df_temp_hum)
  
  data_births_hum <- df_births %>%
    rowwise() %>%
    mutate(
      hum_promedio = mean(df_temp_hum$xh_diario[df_temp_hum$comuna == comuna & 
                                                  df_temp_hum$fecha >= date_start_week & 
                                                  df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      hum_min = min(df_temp_hum$h_min[df_temp_hum$comuna == comuna & 
                                        df_temp_hum$fecha >= date_start_week & 
                                        df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      hum_max = max(df_temp_hum$h_max[df_temp_hum$comuna == comuna & 
                                        df_temp_hum$fecha >= date_start_week & 
                                        df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      hum_mean_min = mean(df_temp_hum$h_min[df_temp_hum$comuna == comuna & 
                                              df_temp_hum$fecha >= date_start_week & 
                                              df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      hum_mean_max = mean(df_temp_hum$h_max[df_temp_hum$comuna == comuna & 
                                              df_temp_hum$fecha >= date_start_week & 
                                              df_temp_hum$fecha < date_end_week], na.rm=TRUE),
      
      hum_q10 = quantile(df_temp_hum$xh_diario[df_temp_hum$comuna == comuna & 
                                                 df_temp_hum$fecha >= date_start_week & 
                                                 df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.1),
      
      hum_q20 = quantile(df_temp_hum$xh_diario[df_temp_hum$comuna == comuna & 
                                                 df_temp_hum$fecha >= date_start_week & 
                                                 df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.2),
      
      hum_q30 = quantile(df_temp_hum$xh_diario[df_temp_hum$comuna == comuna & 
                                                 df_temp_hum$fecha >= date_start_week & 
                                                 df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.3),
      
      hum_q40 = quantile(df_temp_hum$xh_diario[df_temp_hum$comuna == comuna & 
                                                 df_temp_hum$fecha >= date_start_week & 
                                                 df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.4),
      
      hum_q50 = quantile(df_temp_hum$xh_diario[df_temp_hum$comuna == comuna & 
                                                 df_temp_hum$fecha >= date_start_week & 
                                                 df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.5),
      
      hum_q60 = quantile(df_temp_hum$xh_diario[df_temp_hum$comuna == comuna & 
                                                 df_temp_hum$fecha >= date_start_week & 
                                                 df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.6),
      
      hum_q70 = quantile(df_temp_hum$xh_diario[df_temp_hum$comuna == comuna & 
                                                 df_temp_hum$fecha >= date_start_week & 
                                                 df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.7),
      
      hum_q80 = quantile(df_temp_hum$xh_diario[df_temp_hum$comuna == comuna & 
                                                 df_temp_hum$fecha >= date_start_week & 
                                                 df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.8),
      
      hum_q90 = quantile(df_temp_hum$xh_diario[df_temp_hum$comuna == comuna & 
                                                 df_temp_hum$fecha >= date_start_week & 
                                                 df_temp_hum$fecha < date_end_week], na.rm=TRUE, probs=0.9),
    )
  return(data_births_hum)
}


calculate_temperature_stats <- function(start_date, end_date, comuna_id, temp_data) {
  # Filtrar los datos de temperatura dentro del rango de fechas y por comuna específica
  week_temperatures <- temp_data %>%
    filter(fecha >= start_date & fecha <= end_date, comuna == comuna_id) %>%
    select(t_min, t_max, xt_diario) %>%
    na.omit()  # Eliminar valores NA para realizar cálculos
  
  # Si no hay datos, retornar NA para cada estadística
  if (nrow(week_temperatures) == 0) {
    return(tibble(temp_mean=NA, temp_min=NA, temp_max=NA,
                  temp_mean_min=NA, temp_mean_max=NA
                  # temp_q05=NA, temp_q10=NA, temp_q20=NA, temp_q30=NA,
                  # temp_q40=NA, temp_q50=NA, temp_q60=NA, temp_q70=NA,
                  # temp_q80=NA, temp_q90=NA, temp_q95=NA
                  ))
  }
  
  # Calcular y retornar estadísticas
  tibble(
    temp_mean = mean(week_temperatures$xt_diario, na.rm = TRUE),
    temp_min = min(week_temperatures$t_min, na.rm = TRUE),
    temp_max = max(week_temperatures$t_max, na.rm = TRUE),
    temp_mean_min = mean(week_temperatures$t_min, na.rm = TRUE),
    temp_mean_max = mean(week_temperatures$t_max, na.rm = TRUE),
    temp_q05 = quantile(week_temperatures$xt_diario, probs = 0.05, na.rm = TRUE)
    # temp_q10 = quantile(week_temperatures$xt_diario, probs = 0.1, na.rm = TRUE),
    # temp_q20 = quantile(week_temperatures$xt_diario, probs = 0.2, na.rm = TRUE),
    # temp_q30 = quantile(week_temperatures$xt_diario, probs = 0.3, na.rm = TRUE),
    # temp_q40 = quantile(week_temperatures$xt_diario, probs = 0.4, na.rm = TRUE),
    # temp_q50 = quantile(week_temperatures$xt_diario, probs = 0.5, na.rm = TRUE),
    # temp_q60 = quantile(week_temperatures$xt_diario, probs = 0.6, na.rm = TRUE),
    # temp_q70 = quantile(week_temperatures$xt_diario, probs = 0.7, na.rm = TRUE),
    # temp_q80 = quantile(week_temperatures$xt_diario, probs = 0.8, na.rm = TRUE),
    # temp_q90 = quantile(week_temperatures$xt_diario, probs = 0.9, na.rm = TRUE),
    # temp_q95 = quantile(week_temperatures$xt_diario, probs = 0.95, na.rm = TRUE)
  )
}

# Aplicar la función a cada fila usando mutate y expandir la tibble directamente en nuevas columnas

plan(multisession, workers = detectCores() - 2) 
stime <- Sys.time() 
data_exp2 <- data_exp %>% 
  rowwise() %>%
  mutate(temp_stats = list(calculate_temperature_stats(start=date_start_week, 
                                                       end=date_end_week, 
                                                       comuna_id=comuna, 
                                                       temp_data=temp))) 
etime <- Sys.time()
t1 <- etime - stime 
t1

data_exp <- data_exp %>%
  rowwise() %>%
  mutate(temp_stats = list(calculate_temperature_stats(date_start_week, date_end_week, comuna, temp))) %>%
  unnest_wider(temp_stats)

stime <- Sys.time() 
#time_temp <- profvis::profvis({
temperature_data <- parts %>% future_map_dfr(~ multi_join_temp(df_births = .x, df_temp_hum = temp), .progress = TRUE)
#})
etime <- Sys.time()
beepr::beep(8)

etime - stime # Total time execution:  

# Save time execution results 
#htmlwidgets::saveWidget(time_temp, "Products/time_profile_temp.html")

# Save data 
save(temperature_data, file=paste0(data_out, "births_2011_2020_weeks_temperature", ".RData"))
rm(temperature_data)


