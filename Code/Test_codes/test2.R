# Cargar las librerías necesarias
library(dplyr)
library(lubridate)

# # Asegurar que las fechas están en el formato correcto
# data_exp$date_start_week <- as.Date(data_exp$date_start_week)
# data_exp$date_end_week <- as.Date(data_exp$date_end_week)
# temp_data$fecha <- as.Date(temp_data$fecha)

# Función para calcular estadísticas de temperatura por semana
calculate_temperature_stats <- function(start, end, comuna_id, temp_data) {
  
  # Data table format
  setDT(df_births)
  setDT(df_temp_hum)
  
  # Filtrar los datos de temperatura dentro del rango de fechas
  week_temperatures <- temp_data %>%
    filter(fecha >= start & fecha < end, comuna == comuna_id) %>%
    select(t_min, t_max, xt_diario) %>%
    na.omit()  # Eliminar valores NA para realizar cálculos
  
  # Si no hay datos, retornar NA para cada estadística
  if (nrow(week_temperatures) == 0) {
    return(data.frame(temp_mean=NA, temp_min=NA, temp_max=NA,
                      temp_mean_min=NA, temp_mean_max=NA,
                      temp_q05=NA, temp_q10=NA, temp_q20=NA, temp_q30=NA,
                      temp_q40=NA, temp_q50=NA, temp_q60=NA, temp_q70=NA,
                      temp_q80=NA, temp_q90=NA, temp_q95=NA))
  }
  
  # Calcular y retornar estadísticas
  tibble(
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
  )
  
}

# Aplicar la función a cada fila usando mutate

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


# %>% 
#   unnest_wider(temp_stats) 


results <- future_map_dfr(data_exp, ~calculate_temperature_stats(.x$date_start_week, .x$date_end_week, .x$comuna, temp_data), .progress = TRUE)



parts <- split(data_test, data_test$id)  # Dividir en partes de N filas

plan(multisession, workers = detectCores() - 2) 
stime <- Sys.time() 
temperature_data2 <- parts %>% future_map_dfr(~ multi_join_temp(df_births = .x, df_temp_hum = temp), .progress = TRUE)
etime <- Sys.time()
t2 <- etime - stime 
t2


results <- future_map_dfr(data_exp, ~calculate_temperature_stats(start = .x$date_start_week, 
                                                                 end = .x$date_end_week, 
                                                                 comuna_id = .x$comuna, 
                                                                 temp_data = temp), 
                          .progress = TRUE)



# Mostrar los resultados
head(data_exp)
