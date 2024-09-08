# 7.0 Descriptive analysis ---

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
data_temp <- "births_2011_2020_weeks_temp_analysis.RData"
data_out <- "Output_analysis/temp/"

load(paste0(data_path, data_temp))

glimpse(births_weeks_temp)

#### Prepare data ----


# Wide data by trimester analysis
births_weeks_temp_wide <- births_weeks_temp |> 
  select(id, tbw, ltbw, weeks, trim_gest, sex,  
         age_group_mom, educ_group_mom, job_group_mom,
         age_group_dad, educ_group_dad, job_group_dad, 
         year_week1, month_week1, 
         clim_zone, 
         temp_mean_percentile_wcz,
         temp_min_percentile_wcz,
         temp_max_percentile_wcz
  ) |> 
  distinct() |> 
  pivot_wider(names_from = trim_gest,
              values_from = c(temp_mean_percentile_wcz, temp_min_percentile_wcz, temp_max_percentile_wcz),
              values_fn = list(
                temp_mean_percentile_wcz = ~ .x[length(.x)],
                temp_min_percentile_wcz = ~ .x[length(.x)],
                temp_max_percentile_wcz = ~ .x[length(.x)]
              )) 


#### Descriptives characteristics of the sample ----

births_des <-  births_weeks_temp |> 
   select(id, tbw, ltbw, weeks, sex,  
          age_group_mom, educ_group_mom, job_group_mom,
          age_group_dad, educ_group_dad, job_group_dad, 
          year_week1, month_week1, 
          clim_zone,
          name_comuna,
          temp_mean_gest,
          temp_min_gest,
          temp_max_gest
   ) |> 
   group_by(clim_zone) |> 
   mutate(temp_mean_percentile = ntile(temp_mean_gest, 10),
          temp_min_percentile = ntile(temp_min_gest, 10),
          temp_max_percentile = ntile(temp_max_gest, 10)
          ) |> 
   ungroup() |> 
   distinct(id, .keep_all = TRUE) |> 
   select(!id)
   

tab <- births_des |> 
   select(!c("temp_mean_gest", 
             "temp_min_gest", 
             "temp_max_gest", 
             "temp_mean_percentile",
             "temp_min_percentile",
             "temp_max_percentile")) |> 
   mutate(year_week1=factor(year_week1)) |> 
   mutate(month_week1=factor(month_week1)) |> 
   st(,
   digits = 1, 
   out="return", 
   add.median = TRUE,
   fixed.digits = TRUE, 
   simple.kable = FALSE,
   title="",
   numformat = NA) |> 
   data.frame() |> 
   select(1:4)

tab10 <- births_des |> 
   filter(temp_mean_percentile==1) |> 
   select(!c("temp_mean_gest", "temp_mean_percentile")) |> 
   mutate(year_week1=factor(year_week1)) |> 
   mutate(month_week1=factor(month_week1)) |> 
   st(,
      digits = 1, 
      out="return", 
      add.median = TRUE,
      fixed.digits = TRUE, 
      simple.kable = FALSE,
      title="",
      numformat = NA) |> 
   data.frame() |> 
   select(1:4)

tab90 <- births_des |> 
   filter(temp_mean_percentile==10) |> 
   select(!c("temp_mean_gest", "temp_mean_percentile")) |> 
   mutate(year_week1=factor(year_week1)) |> 
   mutate(month_week1=factor(month_week1)) |> 
   st(,
      digits = 1, 
      out="return", 
      add.median = TRUE,
      fixed.digits = TRUE, 
      simple.kable = FALSE,
      title="",
      numformat = NA) |> 
   data.frame() |> 
   select(1:4)

lista_tab <- list(
   "tab"=tab, 
   "tab10"=tab10, 
   "tab90"=tab90
)

writexl::write_xlsx(lista_tab, path =  paste0(data_out, "des/", "Descriptives", ".xlsx"))


#### Temperature percentile ----

tab2 <- births_weeks_temp |> 
   select(tbw, ltbw,
          clim_zone,
          temp_mean_gest,
          temp_min_gest,
          temp_max_gest
   ) |> 
   summarise(
      tbw=mean(tbw, na.rm=TRUE),
      ltbw=mean(ltbw, na.rm=TRUE),
      temp_mean=mean(temp_mean_gest, na.rm = TRUE),
      temp_min=mean(temp_min_gest, na.rm = TRUE),
      temp_max=mean(temp_max_gest, na.rm = TRUE),
      
      p10_mean=quantile(temp_mean_gest, probs = 0.1, na.rm = TRUE),
      p50_mean=quantile(temp_mean_gest, probs = 0.5, na.rm = TRUE),
      p90_mean=quantile(temp_mean_gest, probs = 0.9, na.rm = TRUE),
      
      p50_min=quantile(temp_min_gest, probs = 0.5, na.rm = TRUE),
      p10_min=quantile(temp_min_gest, probs = 0.1, na.rm = TRUE),
      
      p50_max=quantile(temp_max_gest, probs = 0.5, na.rm = TRUE),
      p90_max=quantile(temp_max_gest, probs = 0.9, na.rm = TRUE)
      ) |> 
   mutate(clim_zone="Overall") |> 
   relocate(clim_zone)

tab2b <- births_weeks_temp |> 
   select(tbw, ltbw,
          clim_zone,
          temp_mean_gest,
          temp_min_gest,
          temp_max_gest
   ) |> 
   group_by(clim_zone) |> 
   summarise(
      tbw=mean(tbw, na.rm=TRUE),
      ltbw=mean(ltbw, na.rm=TRUE),
      temp_mean=mean(temp_mean_gest, na.rm = TRUE),
      temp_min=mean(temp_min_gest, na.rm = TRUE),
      temp_max=mean(temp_max_gest, na.rm = TRUE),
      p10_mean=quantile(temp_mean_gest, probs = 0.1, na.rm = TRUE),
      p50_mean=quantile(temp_mean_gest, probs = 0.5, na.rm = TRUE),
      p90_mean=quantile(temp_mean_gest, probs = 0.9, na.rm = TRUE),
      
      p50_min=quantile(temp_min_gest, probs = 0.5, na.rm = TRUE),
      p10_min=quantile(temp_min_gest, probs = 0.1, na.rm = TRUE),
      
      p50_max=quantile(temp_max_gest, probs = 0.5, na.rm = TRUE),
      p90_max=quantile(temp_max_gest, probs = 0.9, na.rm = TRUE)
   )


tab2 <- tab2 |> 
   bind_rows(tab2b) |> 
   mutate_if(is.numeric, round, 2)

writexl::write_xlsx(tab2, path =  paste0(data_out, "des/", "Descriptives_clim_zone", ".xlsx"))


#### Suplemental table S2 ----

tableS2 <-  births_weeks_temp |> 
   select(id, 
          clim_zone,
          temp_mean_gest,
          temp_min_gest,
          temp_max_gest
   ) |> 
   group_by(clim_zone) |> 
   mutate(temp_mean_percentile = ntile(temp_mean_gest, 10),
          temp_min_percentile = ntile(temp_min_gest, 10),
          temp_max_percentile = ntile(temp_max_gest, 10)
   ) |> 
   ungroup() |> 
   distinct(id, .keep_all = TRUE) |> 
   select(!id) |> 
   group_by(clim_zone, name_comuna) |> 
   summarise(
      n=n(), 
      min=min(temp_mean_gest, na.rm = TRUE), 
      p10=quantile(temp_mean_gest, probs = 0.1, na.rm = TRUE), 
      p50=quantile(temp_mean_gest, probs = 0.5, na.rm = TRUE), 
      p90=quantile(temp_mean_gest, probs = 0.9, na.rm = TRUE), 
      max=max(temp_mean_gest, na.rm = TRUE)
      ) |> 
   mutate_if(is.numeric, round, 0)


tableS2a <-  births_weeks_temp |> 
   filter(trim_gest=="T1") |> 
   select(id, tbw, ltbw, weeks, sex,  
          age_group_mom, educ_group_mom, job_group_mom,
          age_group_dad, educ_group_dad, job_group_dad, 
          year_week1, month_week1, 
          clim_zone,
          name_comuna,
          temp_mean_gest,
          temp_min_gest,
          temp_max_gest,
          temp_mean_trim_gest,
          temp_min_trim_gest,
          temp_max_trim_gest
   ) |> 
   group_by(clim_zone) |> 
   mutate(temp_mean_percentile = ntile(temp_mean_gest, 10),
          temp_min_percentile = ntile(temp_min_gest, 10),
          temp_max_percentile = ntile(temp_max_gest, 10)
   ) |> 
   ungroup() |> 
   distinct(id, .keep_all = TRUE) |> 
   select(!id) |> 
   group_by(clim_zone, name_comuna) |> 
   summarise(
      p10=quantile(temp_mean_trim_gest, probs = 0.1, na.rm = TRUE), 
      p50=quantile(temp_mean_trim_gest, probs = 0.5, na.rm = TRUE), 
      p90=quantile(temp_mean_trim_gest, probs = 0.9, na.rm = TRUE)
   ) |> 
   mutate_if(is.numeric, round, 0)

tableS2b <-  births_weeks_temp |> 
   filter(trim_gest=="T2") |> 
   select(id, tbw, ltbw, weeks, sex,  
          age_group_mom, educ_group_mom, job_group_mom,
          age_group_dad, educ_group_dad, job_group_dad, 
          year_week1, month_week1, 
          clim_zone,
          name_comuna,
          temp_mean_gest,
          temp_min_gest,
          temp_max_gest,
          temp_mean_trim_gest,
          temp_min_trim_gest,
          temp_max_trim_gest
   ) |> 
   group_by(clim_zone) |> 
   mutate(temp_mean_percentile = ntile(temp_mean_gest, 10),
          temp_min_percentile = ntile(temp_min_gest, 10),
          temp_max_percentile = ntile(temp_max_gest, 10)
   ) |> 
   ungroup() |> 
   distinct(id, .keep_all = TRUE) |> 
   select(!id) |> 
   group_by(clim_zone, name_comuna) |> 
   summarise(
      p10=quantile(temp_mean_trim_gest, probs = 0.1, na.rm = TRUE), 
      p50=quantile(temp_mean_trim_gest, probs = 0.5, na.rm = TRUE), 
      p90=quantile(temp_mean_trim_gest, probs = 0.9, na.rm = TRUE)
   ) |> 
   mutate_if(is.numeric, round, 0)

tableS2c <-  births_weeks_temp |> 
   filter(trim_gest=="T3") |> 
   select(id, tbw, ltbw, weeks, sex,  
          age_group_mom, educ_group_mom, job_group_mom,
          age_group_dad, educ_group_dad, job_group_dad, 
          year_week1, month_week1, 
          clim_zone,
          name_comuna,
          temp_mean_gest,
          temp_min_gest,
          temp_max_gest,
          temp_mean_trim_gest,
          temp_min_trim_gest,
          temp_max_trim_gest
   ) |> 
   group_by(clim_zone) |> 
   mutate(temp_mean_percentile = ntile(temp_mean_gest, 10),
          temp_min_percentile = ntile(temp_min_gest, 10),
          temp_max_percentile = ntile(temp_max_gest, 10)
   ) |> 
   ungroup() |> 
   distinct(id, .keep_all = TRUE) |> 
   select(!id) |> 
   group_by(clim_zone, name_comuna) |> 
   summarise(
      p10=quantile(temp_mean_trim_gest, probs = 0.1, na.rm = TRUE), 
      p50=quantile(temp_mean_trim_gest, probs = 0.5, na.rm = TRUE), 
      p90=quantile(temp_mean_trim_gest, probs = 0.9, na.rm = TRUE)
   ) |> 
   mutate_if(is.numeric, round, 0)


lista_tab <- list(
   "tab2s"=tableS2, 
   "tab2sa"=tableS2a, 
   "tab2sb"=tableS2b,
   "tab2sc"=tableS2c
)

writexl::write_xlsx(lista_tab, path =  paste0(data_out, "des/", "Descriptives_clim_region", ".xlsx"))

#### Suplemental table S3: MEAN ----

s3 <- births_weeks_temp |> 
   select(id, 
          trim_gest,
          clim_zone,
          temp_mean,
          temp_min,
          temp_max,
          temp_mean_gest,
          temp_min_gest,
          temp_max_gest,
          temp_mean_trim_gest,
          temp_min_trim_gest,
          temp_max_trim_gest
          #temp_mean_trim_gest
   )


tab3 <- s3 |> 
   select(id, clim_zone, temp_mean) |> 
   #distinct(id, .keep_all = TRUE) |> 
   group_by(clim_zone) |> 
   summarise(
      "Mean" = mean(temp_mean, na.rm=TRUE),
      "Min." = min(temp_mean, na.rm=TRUE),
      "P.5" = quantile(temp_mean, na.rm=TRUE, probs = 0.05),
      "P.10" = quantile(temp_mean, na.rm=TRUE, probs = 0.1),
      "P.20" = quantile(temp_mean, na.rm=TRUE, probs = 0.2),
      "P.30" = quantile(temp_mean, na.rm=TRUE, probs = 0.3),
      "P.40" = quantile(temp_mean, na.rm=TRUE, probs = 0.4),
      "P.50" = quantile(temp_mean, na.rm=TRUE, probs = 0.5),
      "P.60" = quantile(temp_mean, na.rm=TRUE, probs = 0.6),
      "P.70" = quantile(temp_mean, na.rm=TRUE, probs = 0.7),
      "P.80" = quantile(temp_mean, na.rm=TRUE, probs = 0.8),
      "P.90" = quantile(temp_mean, na.rm=TRUE, probs = 0.9),
      "P.95" = quantile(temp_mean, na.rm=TRUE, probs = 0.95),
      "Max." = max(temp_mean, na.rm=TRUE),
      
   ) |> 
   ungroup() 


tab3a <- s3 |> 
   select(id, clim_zone, temp_mean_gest) |> 
   distinct(id, .keep_all = TRUE) |> 
   group_by(clim_zone) |> 
   summarise(
      "Mean" = mean(temp_mean_gest, na.rm=TRUE),
      "Min." = min(temp_mean_gest, na.rm=TRUE),
      "P.5" = quantile(temp_mean_gest, na.rm=TRUE, probs = 0.05),
      "P.10" = quantile(temp_mean_gest, na.rm=TRUE, probs = 0.1),
      "P.20" = quantile(temp_mean_gest, na.rm=TRUE, probs = 0.2),
      "P.30" = quantile(temp_mean_gest, na.rm=TRUE, probs = 0.3),
      "P.40" = quantile(temp_mean_gest, na.rm=TRUE, probs = 0.4),
      "P.50" = quantile(temp_mean_gest, na.rm=TRUE, probs = 0.5),
      "P.60" = quantile(temp_mean_gest, na.rm=TRUE, probs = 0.6),
      "P.70" = quantile(temp_mean_gest, na.rm=TRUE, probs = 0.7),
      "P.80" = quantile(temp_mean_gest, na.rm=TRUE, probs = 0.8),
      "P.90" = quantile(temp_mean_gest, na.rm=TRUE, probs = 0.9),
      "P.95" = quantile(temp_mean_gest, na.rm=TRUE, probs = 0.95),
      "Max." = max(temp_mean_gest, na.rm=TRUE),
      
   ) |> 
   ungroup() 

tab3a

tab3b <- s3 |> 
   select(id, clim_zone, trim_gest, temp_mean_trim_gest) |> 
   group_by(clim_zone, trim_gest) |> 
   summarise(
      "Mean" = mean(temp_mean_trim_gest, na.rm=TRUE),
      "Min." = min(temp_mean_trim_gest, na.rm=TRUE),
      "P.5" = quantile(temp_mean_trim_gest, na.rm=TRUE, probs = 0.05),
      "P.10" = quantile(temp_mean_trim_gest, na.rm=TRUE, probs = 0.1),
      "P.20" = quantile(temp_mean_trim_gest, na.rm=TRUE, probs = 0.2),
      "P.30" = quantile(temp_mean_trim_gest, na.rm=TRUE, probs = 0.3),
      "P.40" = quantile(temp_mean_trim_gest, na.rm=TRUE, probs = 0.4),
      "P.50" = quantile(temp_mean_trim_gest, na.rm=TRUE, probs = 0.5),
      "P.60" = quantile(temp_mean_trim_gest, na.rm=TRUE, probs = 0.6),
      "P.70" = quantile(temp_mean_trim_gest, na.rm=TRUE, probs = 0.7),
      "P.80" = quantile(temp_mean_trim_gest, na.rm=TRUE, probs = 0.8),
      "P.90" = quantile(temp_mean_trim_gest, na.rm=TRUE, probs = 0.9),
      "P.95" = quantile(temp_mean_trim_gest, na.rm=TRUE, probs = 0.95),
      "Max." = max(temp_mean_trim_gest, na.rm=TRUE),
      
   ) |> 
   ungroup() 

# FUll table
table3 <- bind_rows(tab3, tab3a, tab3b) 
table3[1:10, 16] <- c(rep("In the region", 5),
                      rep("Entire Pregnancy average", 5)
                      )


table3 <- table3 |> arrange(clim_zone) |> relocate(clim_zone, trim_gest)
table3 <- table3 |> mutate_if(is.numeric, round, 1)
table3 <- table3 |> mutate_if(is.numeric, ~ str_replace_all(., ",", "."))

writexl::write_xlsx(table3, path =  paste0(data_out, "des/", "Descriptives_clim_mean_temp", ".xlsx"))

#### Suplemental table S4: MIN ----

tab3 <- s3 |> 
   select(id, clim_zone, temp_min) |> 
   #distinct(id, .keep_all = TRUE) |> 
   group_by(clim_zone) |> 
   summarise(
      "Mean" = mean(temp_min, na.rm=TRUE),
      "Min." = min(temp_min, na.rm=TRUE),
      "P.5" = quantile(temp_min, na.rm=TRUE, probs = 0.05),
      "P.10" = quantile(temp_min, na.rm=TRUE, probs = 0.1),
      "P.20" = quantile(temp_min, na.rm=TRUE, probs = 0.2),
      "P.30" = quantile(temp_min, na.rm=TRUE, probs = 0.3),
      "P.40" = quantile(temp_min, na.rm=TRUE, probs = 0.4),
      "P.50" = quantile(temp_min, na.rm=TRUE, probs = 0.5),
      "P.60" = quantile(temp_min, na.rm=TRUE, probs = 0.6),
      "P.70" = quantile(temp_min, na.rm=TRUE, probs = 0.7),
      "P.80" = quantile(temp_min, na.rm=TRUE, probs = 0.8),
      "P.90" = quantile(temp_min, na.rm=TRUE, probs = 0.9),
      "P.95" = quantile(temp_min, na.rm=TRUE, probs = 0.95),
      "Max." = max(temp_min, na.rm=TRUE),
      
   ) |> 
   ungroup() 


tab3a <- s3 |> 
   select(id, clim_zone, temp_min_gest) |> 
   distinct(id, .keep_all = TRUE) |> 
   group_by(clim_zone) |> 
   summarise(
      "Mean" = mean(temp_min_gest, na.rm=TRUE),
      "Min." = min(temp_min_gest, na.rm=TRUE),
      "P.5" = quantile(temp_min_gest, na.rm=TRUE, probs = 0.05),
      "P.10" = quantile(temp_min_gest, na.rm=TRUE, probs = 0.1),
      "P.20" = quantile(temp_min_gest, na.rm=TRUE, probs = 0.2),
      "P.30" = quantile(temp_min_gest, na.rm=TRUE, probs = 0.3),
      "P.40" = quantile(temp_min_gest, na.rm=TRUE, probs = 0.4),
      "P.50" = quantile(temp_min_gest, na.rm=TRUE, probs = 0.5),
      "P.60" = quantile(temp_min_gest, na.rm=TRUE, probs = 0.6),
      "P.70" = quantile(temp_min_gest, na.rm=TRUE, probs = 0.7),
      "P.80" = quantile(temp_min_gest, na.rm=TRUE, probs = 0.8),
      "P.90" = quantile(temp_min_gest, na.rm=TRUE, probs = 0.9),
      "P.95" = quantile(temp_min_gest, na.rm=TRUE, probs = 0.95),
      "Max." = max(temp_min_gest, na.rm=TRUE),
      
   ) |> 
   ungroup() 

tab3a

tab3b <- s3 |> 
   select(id, clim_zone, trim_gest, temp_min_trim_gest) |> 
   group_by(clim_zone, trim_gest) |> 
   summarise(
      "Mean" = mean(temp_min_trim_gest, na.rm=TRUE),
      "Min." = min(temp_min_trim_gest, na.rm=TRUE),
      "P.5" = quantile(temp_min_trim_gest, na.rm=TRUE, probs = 0.05),
      "P.10" = quantile(temp_min_trim_gest, na.rm=TRUE, probs = 0.1),
      "P.20" = quantile(temp_min_trim_gest, na.rm=TRUE, probs = 0.2),
      "P.30" = quantile(temp_min_trim_gest, na.rm=TRUE, probs = 0.3),
      "P.40" = quantile(temp_min_trim_gest, na.rm=TRUE, probs = 0.4),
      "P.50" = quantile(temp_min_trim_gest, na.rm=TRUE, probs = 0.5),
      "P.60" = quantile(temp_min_trim_gest, na.rm=TRUE, probs = 0.6),
      "P.70" = quantile(temp_min_trim_gest, na.rm=TRUE, probs = 0.7),
      "P.80" = quantile(temp_min_trim_gest, na.rm=TRUE, probs = 0.8),
      "P.90" = quantile(temp_min_trim_gest, na.rm=TRUE, probs = 0.9),
      "P.95" = quantile(temp_min_trim_gest, na.rm=TRUE, probs = 0.95),
      "Max." = max(temp_min_trim_gest, na.rm=TRUE),
      
   ) |> 
   ungroup() 

# FUll table
table3 <- bind_rows(tab3, tab3a, tab3b) 
table3[1:10, 16] <- c(rep("In the region", 5),
                      rep("Entire Pregnancy average", 5)
)


table3 <- table3 |> arrange(clim_zone) |> relocate(clim_zone, trim_gest)
table3 <- table3 |> mutate_if(is.numeric, round, 1)
table3 <- table3 |> mutate_if(is.numeric, ~ str_replace_all(., ",", "."))

writexl::write_xlsx(table3, path =  paste0(data_out, "des/", "Descriptives_clim_min_temp", ".xlsx"))

#### Suplemental table S5: MAX ----

tab3 <- s3 |> 
   select(id, clim_zone, temp_max) |> 
   #distinct(id, .keep_all = TRUE) |> 
   group_by(clim_zone) |> 
   summarise(
      "Mean" = mean(temp_max, na.rm=TRUE),
      "Min." = min(temp_max, na.rm=TRUE),
      "P.5" = quantile(temp_max, na.rm=TRUE, probs = 0.05),
      "P.10" = quantile(temp_max, na.rm=TRUE, probs = 0.1),
      "P.20" = quantile(temp_max, na.rm=TRUE, probs = 0.2),
      "P.30" = quantile(temp_max, na.rm=TRUE, probs = 0.3),
      "P.40" = quantile(temp_max, na.rm=TRUE, probs = 0.4),
      "P.50" = quantile(temp_max, na.rm=TRUE, probs = 0.5),
      "P.60" = quantile(temp_max, na.rm=TRUE, probs = 0.6),
      "P.70" = quantile(temp_max, na.rm=TRUE, probs = 0.7),
      "P.80" = quantile(temp_max, na.rm=TRUE, probs = 0.8),
      "P.90" = quantile(temp_max, na.rm=TRUE, probs = 0.9),
      "P.95" = quantile(temp_max, na.rm=TRUE, probs = 0.95),
      "Max." = max(temp_max, na.rm=TRUE),
      
   ) |> 
   ungroup() 


tab3a <- s3 |> 
   select(id, clim_zone, temp_max_gest) |> 
   distinct(id, .keep_all = TRUE) |> 
   group_by(clim_zone) |> 
   summarise(
      "Mean" = mean(temp_max_gest, na.rm=TRUE),
      "Min." = min(temp_max_gest, na.rm=TRUE),
      "P.5" = quantile(temp_max_gest, na.rm=TRUE, probs = 0.05),
      "P.10" = quantile(temp_max_gest, na.rm=TRUE, probs = 0.1),
      "P.20" = quantile(temp_max_gest, na.rm=TRUE, probs = 0.2),
      "P.30" = quantile(temp_max_gest, na.rm=TRUE, probs = 0.3),
      "P.40" = quantile(temp_max_gest, na.rm=TRUE, probs = 0.4),
      "P.50" = quantile(temp_max_gest, na.rm=TRUE, probs = 0.5),
      "P.60" = quantile(temp_max_gest, na.rm=TRUE, probs = 0.6),
      "P.70" = quantile(temp_max_gest, na.rm=TRUE, probs = 0.7),
      "P.80" = quantile(temp_max_gest, na.rm=TRUE, probs = 0.8),
      "P.90" = quantile(temp_max_gest, na.rm=TRUE, probs = 0.9),
      "P.95" = quantile(temp_max_gest, na.rm=TRUE, probs = 0.95),
      "Max." = max(temp_max_gest, na.rm=TRUE),
      
   ) |> 
   ungroup() 

tab3a

tab3b <- s3 |> 
   select(id, clim_zone, trim_gest, temp_max_trim_gest) |> 
   group_by(clim_zone, trim_gest) |> 
   summarise(
      "Mean" = mean(temp_max_trim_gest, na.rm=TRUE),
      "Min." = min(temp_max_trim_gest, na.rm=TRUE),
      "P.5" = quantile(temp_max_trim_gest, na.rm=TRUE, probs = 0.05),
      "P.10" = quantile(temp_max_trim_gest, na.rm=TRUE, probs = 0.1),
      "P.20" = quantile(temp_max_trim_gest, na.rm=TRUE, probs = 0.2),
      "P.30" = quantile(temp_max_trim_gest, na.rm=TRUE, probs = 0.3),
      "P.40" = quantile(temp_max_trim_gest, na.rm=TRUE, probs = 0.4),
      "P.50" = quantile(temp_max_trim_gest, na.rm=TRUE, probs = 0.5),
      "P.60" = quantile(temp_max_trim_gest, na.rm=TRUE, probs = 0.6),
      "P.70" = quantile(temp_max_trim_gest, na.rm=TRUE, probs = 0.7),
      "P.80" = quantile(temp_max_trim_gest, na.rm=TRUE, probs = 0.8),
      "P.90" = quantile(temp_max_trim_gest, na.rm=TRUE, probs = 0.9),
      "P.95" = quantile(temp_max_trim_gest, na.rm=TRUE, probs = 0.95),
      "Max." = max(temp_max_trim_gest, na.rm=TRUE),
      
   ) |> 
   ungroup() 

# FUll table
table3 <- bind_rows(tab3, tab3a, tab3b) 
table3[1:10, 16] <- c(rep("In the region", 5),
                      rep("Entire Pregnancy average", 5)
)


table3 <- table3 |> arrange(clim_zone) |> relocate(clim_zone, trim_gest)
table3 <- table3 |> mutate_if(is.numeric, round, 1)
table3 <- table3 |> mutate_if(is.numeric, ~ str_replace_all(., ",", "."))

writexl::write_xlsx(table3, path =  paste0(data_out, "des/", "Descriptives_clim_max_temp", ".xlsx"))


#### Plot temperature ----

temp_data_plot  <-  births_weeks_temp |> 
   select(id, tbw, ltbw, weeks, sex,  
          clim_zone,
          temp_mean_gest,
          temp_min_gest,
          temp_max_gest
   ) |> 
   distinct(id, .keep_all = TRUE)

plot <- temp_data_plot |> 
   pivot_longer(cols = starts_with("temp_"), 
                names_to = "type", 
                values_to = "val") |> 
   mutate(type=factor(type, 
                      levels = c("temp_min_gest", "temp_mean_gest", "temp_max_gest"),
                      labels = c("Min", "Mean", "Max")))

plot_list <- list()
clim_zones <- unique(plot$clim_zone)

for (i in seq_along(clim_zones)) {
   p <- ggplot(plot |> filter(clim_zone == clim_zones[i]), aes(x = val, fill = type)) +
      geom_histogram(alpha = 0.5, bins = 150, color = "gray50") +
      scale_x_continuous(limits = c(-10, 40)) +
      scale_fill_manual(name = "Temperature:", values = c("royalblue", "khaki1", "tomato1")) +
      labs(title = paste(LETTERS[i],".", clim_zones[i]),
           x = "Temperature (°C)", y = "Frequency") +
      theme_bw() +
      theme(legend.position = "top",
            legend.background = element_rect(color = NA),
            legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
            panel.grid = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            plot.title = element_text(size = 12, hjust = 0, face = "bold"),
            strip.background = element_rect(fill = "white", color = "white"), 
            strip.text.x = element_text(size = 10, hjust = 0))
   
   plot_list[[i]] <- p
}

overall_plot <- ggplot(plot, aes(x = val, fill = type)) +
   geom_histogram(alpha = 0.5, bins = 150, color = "gray50") +
   scale_x_continuous(limits = c(-10, 40)) +
   scale_fill_manual(name = "Temperature:", values = c("royalblue", "khaki1", "tomato1")) +
   labs(title = "F. Overall",
        x = "Temperature (°C)", y = "Frequency") +
   theme_bw() +
   theme(legend.position = "top",
         legend.background = element_rect(color = NA),
         legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
         panel.grid = element_blank(),
         axis.text.y = element_text(size = 12),
         axis.text.x = element_text(size = 12),
         plot.title = element_text(size = 12, hjust = 0, face = "bold"),
         strip.background = element_rect(fill = "white", color = "white"), 
         strip.text.x = element_text(size = 10, hjust = 0))

plot_list <- c(plot_list, list(overall_plot))

do.call(ggarrange, c(plot_list, list(nrow = 3, ncol = 2, common.legend=TRUE)))

ggsave(filename = paste0(data_out, "des/", "Temperature_distribution", ".png"),
       res = 300,
       width = 27,
       height = 30,
       units = 'cm',
       scaling = 1,
       device = ragg::agg_png)

#### Plot tbW ----

plot  <-  births_weeks_temp |> 
   select(id, tbw,
          clim_zone
   ) |> 
   distinct(id, .keep_all = TRUE)

plot_list <- list()
clim_zones <- unique(plot$clim_zone)

for (i in seq_along(clim_zones)) {
   p <- ggplot(plot |> filter(clim_zone == clim_zones[i]), aes(x = tbw)) +
      geom_histogram(alpha=0.5, bins=150, fill = "deepskyblue3", color="deepskyblue3") +
      scale_x_continuous(breaks=seq(0, 10000, by=1000)) + 
      labs(title = paste(LETTERS[i],".", clim_zones[i]),
           x = "tBW in grams.", y = "Frequency") +
      theme_bw() +
      theme(legend.position = "top",
            legend.background = element_rect(color = NA),
            legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
            panel.grid = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            plot.title = element_text(size = 12, hjust = 0, face = "bold"),
            strip.background = element_rect(fill = "white", color = "white"), 
            strip.text.x = element_text(size = 10, hjust = 0))
   
   plot_list[[i]] <- p
}

overall_plot <- ggplot(plot, aes(x = tbw)) +
   geom_histogram(alpha=0.5, bins=150, fill = "deepskyblue3", color="deepskyblue3") +
   scale_x_continuous(breaks=seq(0, 10000, by=1000)) + 
   labs(title = "F. Overall",
        x = "tBW in grams.", y = "Frequency") +
   theme_bw() +
   theme(legend.position = "top",
         legend.background = element_rect(color = NA),
         legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
         panel.grid = element_blank(),
         axis.text.y = element_text(size = 12),
         axis.text.x = element_text(size = 12),
         plot.title = element_text(size = 12, hjust = 0, face = "bold"),
         strip.background = element_rect(fill = "white", color = "white"), 
         strip.text.x = element_text(size = 10, hjust = 0))

plot_list <- c(plot_list, list(overall_plot))

do.call(ggarrange, c(plot_list, list(nrow = 3, ncol = 2, common.legend=TRUE)))

ggsave(filename = paste0(data_out, "des/", "Tbw_distribution", ".png"),
       res = 300,
       width = 27,
       height = 30,
       units = 'cm',
       scaling = 1,
       device = ragg::agg_png)
