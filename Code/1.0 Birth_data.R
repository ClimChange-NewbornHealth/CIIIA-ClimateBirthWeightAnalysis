# Code 1: Birth data preparation ----

## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

## Birth data ---- 

# ID file load
file <- "data_1992_2020.RData"

# Open data in R
load(paste0(data_inp, file)) 

data_1992_2020 <- data_1992_2020 %>% janitor::clean_names()

# Explorer data 
glimpse(data_1992_2020)

# Prepare data
# 1. Date 2011-2020
table(data_1992_2020$ano_nac)

# 2. 27 districts
table(data_1992_2020$comuna)

district <- import(paste0(data_inp, "comunas_sample.xlsx")) %>% select(code_com)
district <- district$code_com

births <- data_1992_2020 %>% 
  filter(ano_nac>=2011 & ano_nac<=2020) %>% 
  filter(comuna %in% district)

# Check results
table(births$ano_nac)
table(births$comuna)
table(data_1992_2020$comuna)

rm(data_1992_2020)

# Construction births date
births <- births %>% 
  mutate(date_nac = make_date(year = ano_nac, month = mes_nac, day = dia_nac)) # Year, Month, Date 

glimpse(births)

# Create week_gest obs
stime <- Sys.time()
births_weeks <- births %>%
  drop_na(date_nac, semanas) %>%  # 394546 - 393951 = 595 cases without geek gestation 
  mutate(id=1:n()) %>% 
  mutate(date_start = date_nac - weeks(semanas-2),
         date_start = date_start - weeks(1), 
         date_end = date_nac) %>%
  rowwise() %>%
  mutate(week_gest = list(seq.Date(date_start, date_end, by = "week"))) %>%
  unnest(week_gest) %>%
  group_by(id) %>%
  mutate(week_gest_num = paste0(abs(semanas - row_number())),  
         week_gest_num = (semanas) - as.numeric(week_gest_num), 
         date_start_week = (week_gest - (7 * abs(week_gest_num - row_number()))) - weeks(1), #(abs(week_gest_num - row_number())),
         date_end_week = week_gest - (7 * abs(week_gest_num - row_number()))
         ) %>% # ,(abs(week_gest_num - row_number())
  group_by(id) %>% 
  distinct(week_gest_num, .keep_all = TRUE) %>% 
  arrange(id, week_gest_num) %>% 
  ungroup() 
etime <- Sys.time()

etime - stime # total time execution: 1,6 min 

# Check results

t1 <- births_weeks %>%
  group_by(id) %>% 
  summarise(min=min(week_gest_num), 
            max=max(week_gest_num), 
            n=n(), 
            test=if_else(n==max, 1, 0))

table(t1$test)

#t1 %>% filter(test==0)

## Random sample test ----
set.seed(today())
id_test <- births_weeks %>% 
  select(id) %>% 
  distinct(id)  %>% 
  ungroup() %>% 
  sample_n(size=1000)
id_test <- id_test$id

data_test <- births_weeks %>% 
  filter(id %in% id_test) %>%
  ungroup() 

## Save new births data ----
save(births, file=paste0(data_out, "births_2011_2020", ".RData"))
save(births_weeks, file=paste0(data_out, "births_2011_2020_weeks", ".RData"))
save(data_test, file=paste0(data_out, "test_births_2011_2020_weeks", ".RData"))

