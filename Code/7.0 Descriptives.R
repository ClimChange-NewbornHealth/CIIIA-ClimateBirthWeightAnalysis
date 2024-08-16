# Code 7.0: Descriptives Analysis ---

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
data_temp <- "births_2011_2020_weeks_temp_final.RData"
data_out <- "Output_analysis/temp_mean/"

load(paste0(data_path, data_temp))

#########################################################/
## Filter Data for analysis ----
#########################################################/

glimpse(births_weeks_temp)
length(unique(births_weeks_temp$id)) # 393888

births_weeks_temp <- births_weeks_temp %>% 
  filter(weeks>=37) 

length(unique(births_weeks_temp$id)) # 361913

ids_con_na <- births_weeks_temp %>%
  group_by(id) %>%                       
  filter(any(is.na(temp_mean))) %>% 
  ungroup()

ids_na <- unique(ids_con_na$id)

(length(ids_na)/361913)*100 # 8.8%

births_weeks_temp <- births_weeks_temp %>% 
  filter(!id %in% ids_na) 

length(unique(births_weeks_temp$id)) # 330118 

births_weeks_temp <- births_weeks_temp %>% 
  drop_na(sex
          #age_mom, educ_mom, job_mom
          #age_dad, educ_dad, job_dad
          )

length(unique(births_weeks_temp$id)) # 323551, lost 6567 obs [no lost by recodification].

births_weeks_temp <- births_weeks_temp %>%
  mutate(con_na = rowSums(is.na(.)) > 0)

table(births_weeks_temp$con_na, useNA="ifany")
# 291861, Lost 31690 obs [no lost by recodification]..
length(unique(births_weeks_temp$id[births_weeks_temp$con_na==FALSE])) 

save(births_weeks_temp, file=paste0("Data/Output/births_2011_2020_weeks_temp_analysis", ".RData"))

#########################################################/
## Descriptives analysis ----
#########################################################/

gc() # Clean memory 
rm(list = ls()) # Clean temporal memory 
options(future.globals.maxSize = 3000 * 1024^2)  # Optimal use power computation 
plan(multisession, workers = detectCores() - 4) # Parallelization

# Open data
load(paste0("Data/Output/births_2011_2020_weeks_temp_analysis", ".RData"))

### Descriptive statistics ----
glimpse(births_weeks_temp)

outcomes <- c("tbw", "ltbw") # Outcomes paper
bc <- c("weeks", "sex", "size") # Births cacharacteristics 
mom <- c("year_week1", "month_week1", "age_mom", "educ_mom", "job_mom") # Characteristics mom 
dad <- c( "age_dad", "educ_dad", "job_dad")
zone <- c("zone", "clim_zone", "vul")

