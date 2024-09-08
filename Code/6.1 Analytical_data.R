# 6.1 Analytical Data -----

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

## Generate analytical data temperature ----

data_temp <- "births_2011_2020_weeks_temp_final.RData"

load(paste0(data_path, data_temp))

glimpse(births_weeks_temp)
length(unique(births_weeks_temp$id)) # 393888

births_weeks_temp <- births_weeks_temp |> 
  filter(weeks>=37) 

length(unique(births_weeks_temp$id)) # 361913

ids_con_na <- births_weeks_temp |>
  group_by(id) |>                       
  filter(any(is.na(temp_mean))) |> 
  ungroup()

ids_na <- unique(ids_con_na$id)

(length(ids_na)/361913)*100 # 8.8%

births_weeks_temp <- births_weeks_temp |> 
  filter(!id %in% ids_na) 

length(unique(births_weeks_temp$id)) # 330118

births_weeks_temp <- births_weeks_temp |> 
  drop_na(sex,
          age_group_mom, educ_group_mom, job_group_mom,
          age_group_dad, educ_group_dad, job_group_dad 
  )

length(unique(births_weeks_temp$id)) # 330118, PÉRDIDA DE 6567


births_weeks_temp <- births_weeks_temp |> 
  select(!c(age_mom, educ_mom, job_mom,
            age_dad, educ_dad, job_dad)) 

births_weeks_temp <- births_weeks_temp |> 
  mutate(con_na = rowSums(across(everything(), is.na)) > 0)

table(births_weeks_temp$con_na, useNA="ifany")

length(unique(births_weeks_temp$id[births_weeks_temp$con_na==FALSE])) 

save(births_weeks_temp, file=paste0("Data/Output/births_2011_2020_weeks_temp_analysis", ".RData"))

## Generate analytical data humidity ----

rm(births_weeks_temp)
data_hum <- "births_2011_2020_weeks_hum_final.RData"

load(paste0(data_path, data_hum))

glimpse(births_weeks_hum)
length(unique(births_weeks_hum$id)) # 393888

births_weeks_hum <- births_weeks_hum |> 
  filter(weeks>=37) 

length(unique(births_weeks_hum$id)) # 361913

ids_con_na <- births_weeks_hum |>
  group_by(id) |>                       
  filter(any(is.na(hum_mean))) |> 
  ungroup()

ids_na <- unique(ids_con_na$id)

(length(ids_na)/361913)*100 # 8.8%

births_weeks_hum <- births_weeks_hum |> 
  filter(!id %in% ids_na) 

length(unique(births_weeks_hum$id)) # 330044

births_weeks_hum <- births_weeks_hum |> 
  drop_na(sex,
          age_group_mom, educ_group_mom, job_group_mom,
          age_group_dad, educ_group_dad, job_group_dad, 
  )

length(unique(births_weeks_hum$id)) # 330044. Not remove obs.

births_weeks_hum <- births_weeks_hum |>
  select(!c(age_mom, educ_mom, job_mom,
            age_dad, educ_dad, job_dad)) 

births_weeks_hum <- births_weeks_hum |>
  mutate(con_na = rowSums(across(everything(), is.na)) > 0)

table(births_weeks_hum$con_na, useNA="ifany")

length(unique(births_weeks_hum$id[births_weeks_hum$con_na==FALSE])) 

save(births_weeks_hum, file=paste0("Data/Output/births_2011_2020_weeks_hum_analysis", ".RData"))

beepr::beep(5)
