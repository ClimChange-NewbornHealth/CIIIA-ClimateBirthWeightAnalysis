# 9.1 Sensitivity analyses DLNM ---

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

## DLNM Data  ----

data_temp <- "births_2011_2020_weeks_temp_analysis.RData"
load(paste0(data_path, data_temp))

# Variables 
time <- c("id", "week_gest_num")
vd <- c("tbw", "ltbw")
vi <- c("temp_mean") # "temp_mean_percentile_wcz"
vi2 <- c("temp_min") # "temp_mean_percentile_wcz"
vi3 <- c("temp_max") # "temp_mean_percentile_wcz"

# vi <- c("temp_mean", "temp_min", "temp_max", 
#         "temp_mean_percentile_wcz", "temp_min_percentile_wcz", "temp_max_percentile_wcz")
vc <- c("sex", "age_group_mom", "educ_group_mom", "job_group_mom", 
        "age_group_dad", "educ_group_dad", "job_group_dad",  "clim_zone")
trend <- c("year_week1", "month_week1")
        
# Select and transform to wide data 
bw_dlnm_mean <- births_weeks_temp |>
  #filter(week_gest_num<=37) |>
  dplyr::select(all_of(c(time, vd, vi, vc, trend))) %>%
  group_by(clim_zone, week_gest_num) %>%
  mutate(temp_mean_percentile_wcz = ntile(temp_mean, 100)) %>%
  ungroup() |> 
  dplyr::select(-temp_mean) |>
  pivot_wider(names_from = "week_gest_num", 
              values_from = "temp_mean_percentile_wcz",
              names_prefix = "ptem_gw")


bw_dlnm_min <- births_weeks_temp |>
  #filter(week_gest_num<=37) |>
  dplyr::select(all_of(c(time, vd, vi2, vc, trend))) %>%
  group_by(clim_zone, week_gest_num) %>%
  mutate(temp_min_percentile_wcz = ntile(temp_min, 100)) %>%
  ungroup() |> 
  dplyr::select(-temp_min) |>
  pivot_wider(names_from = "week_gest_num", 
              values_from = "temp_min_percentile_wcz",
              names_prefix = "ptem_gw") 

bw_dlnm_max <- births_weeks_temp |>
  #filter(week_gest_num<=37) |>
  dplyr::select(all_of(c(time, vd, vi3, vc, trend))) %>%
  group_by(clim_zone, week_gest_num) %>%
  mutate(temp_max_percentile_wcz = ntile(temp_max, 100)) %>%
  group_by(clim_zone, week_gest_num) %>%
  ungroup() |> 
  dplyr::select(-temp_max) |>
  pivot_wider(names_from = "week_gest_num", 
              values_from = "temp_max_percentile_wcz",
              names_prefix = "ptem_gw") 


rm(births_weeks_temp)
glimpse(bw_dlnm_mean)
glimpse(bw_dlnm_min)
glimpse(bw_dlnm_max)

####################################################/
### Optimal knots and lags specification -------- 
####################################################/

plan(multisession, workers = parallel::detectCores() - 6)
options(future.globals.maxSize = 3 * 1024^3)  # 1.5 GB

# Grid sensitivity analysis 
knots <- c(2, 4, 6, 8, 10)
lag_fun <- c("ns", "poly")
grid_specs <- expand.grid(knots = knots, lag_fun = lag_fun, poly_degree = c(2, 3), stringsAsFactors = FALSE)
grid_specs <- grid_specs %>%
  filter(!(lag_fun == "ns" & poly_degree != 2)) |> # adjust ns without degree
  filter(!(lag_fun == "poly" & knots >2))
grid_specs

# Matrix exposition
mat_mean <- bw_dlnm_mean |>
  dplyr::select(ptem_gw1:ptem_gw37) |>
  as.matrix()

mat_min <- bw_dlnm_min |>
  dplyr::select(ptem_gw1:ptem_gw37) |>
  as.matrix()

mat_max <- bw_dlnm_max |>
  dplyr::select(ptem_gw1:ptem_gw37) |>
  as.matrix()

# Function to eval models
evaluate_model <- function(knot_val, lag_fun, poly_deg, data, mat) {
  
  lagknots <- equalknots(x = c(2, 36), nk = knot_val, fun = "ns")
  
  arglag_spec <- if (lag_fun == "ns") {
    list(fun = "ns", knots = lagknots)
  } else {
    list(fun = "poly", degree = poly_deg)
  }
  
  cb <- crossbasis(mat, 
                   lag = c(1, 37),
                   argvar = list(fun = "strata", breaks = seq(10, 90, by = 10), ref = 5),
                   arglag = arglag_spec)
  
  mod1 <- gam(tbw ~ cb + sex +
                age_group_mom + educ_group_mom + job_group_mom +
                age_group_dad + educ_group_dad + job_group_dad +
                s(year_week1) + s(month_week1),
              data = data,
              na.action = na.exclude,
              family = gaussian())
  
  mod2 <- gam(ltbw ~ cb + sex +
                age_group_mom + educ_group_mom + job_group_mom +
                age_group_dad + educ_group_dad + job_group_dad +
                s(year_week1) + s(month_week1),
              data = data,
              na.action = na.exclude,
              family = binomial(link = "logit"),
              gc.level = 0)
  
  tibble(
    Outcome = c("tbw", "ltbw"),
    Model = ifelse(lag_fun == "ns", "NSLag", paste0("PolyLag", poly_deg)),
    Knots = knot_val,
    AIC = c(AIC(mod1), AIC(mod2)),
    BIC = c(BIC(mod1), BIC(mod2))
  )
}

# Iteration models 
# Mean, time: 499,912 sec elapsed
tic()
results_mean <- future_pmap_dfr(
  .l = list(grid_specs$knots, grid_specs$lag_fun, grid_specs$poly_degree),
  .f = evaluate_model,
  data = bw_dlnm_mean,  
  mat = mat_mean             
)
toc()

# Min, time: 225,92 sec elapsed
tic()
results_min <- future_pmap_dfr(
  .l = list(grid_specs$knots, grid_specs$lag_fun, grid_specs$poly_degree),
  .f = evaluate_model,
  data = bw_dlnm_min,  
  mat = mat_min             
)
toc()

# Max, time: 256,473 sec elapsed
tic()
results_max <- future_pmap_dfr(
  .l = list(grid_specs$knots, grid_specs$lag_fun, grid_specs$poly_degree),
  .f = evaluate_model,
  data = bw_dlnm_max,  
  mat = mat_max             
)
toc()

# Export models 
writexl::write_xlsx(results_mean, "Output_analysis/temp/tab/Sensitivity_GAM_mean.xlsx")
writexl::write_xlsx(results_min, "Output_analysis/temp/tab/Sensitivity_GAM_min.xlsx")
writexl::write_xlsx(results_max, "Output_analysis/temp/tab/Sensitivity_GAM_max.xlsx")


plan(sequential)