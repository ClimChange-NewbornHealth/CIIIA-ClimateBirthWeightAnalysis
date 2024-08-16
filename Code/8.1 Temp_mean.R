# Code 7.1: Temperature mean ---

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

## Data ----

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
  drop_na(sex,
          age_mom, educ_mom, job_mom
          #age_dad, educ_dad, job_dad
          )

length(unique(births_weeks_temp$id)) # 323551, PÉRDIDA DE 6567


births_weeks_temp <- births_weeks_temp %>%
  mutate(con_na = rowSums(is.na(.)) > 0)

table(births_weeks_temp$con_na, useNA="ifany")

# 291861, PÉRDIDA DE 31690
length(unique(births_weeks_temp$id[births_weeks_temp$con_na==FALSE])) 

save(births_weeks_temp, file=paste0("Data/Output/births_2011_2020_weeks_temp_mod", ".RData"))

## Descriptives  ----

### Observations by climate zone ----

t1_1 <- births_weeks_temp %>% 
  filter(con_na==FALSE) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  group_by(zone) %>% 
  summarise(n_nac=n()) 

aux <- births_weeks_temp %>% 
  filter(con_na==FALSE) %>% 
  distinct(com, .keep_all = TRUE) %>% 
  group_by(zone) %>% 
  summarise(n_com=n()) 

aux2 <- births_weeks_temp %>% 
  filter(con_na==FALSE) %>% 
  group_by(zone) %>% 
  distinct(name_comuna) %>% 
  summarise(name_comuna = paste(name_comuna, collapse = " / ")) 

t1_1 <- t1_1 %>% 
  left_join(aux, by="zone") %>% 
  left_join(aux2, by="zone") 


t1_2 <- births_weeks_temp %>% 
  filter(con_na==FALSE) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  group_by(clim_zone) %>% 
  summarise(n_nac=n()) 

aux <- births_weeks_temp %>% 
  filter(con_na==FALSE) %>% 
  distinct(com, .keep_all = TRUE) %>% 
  group_by(clim_zone) %>% 
  summarise(n_com=n()) 

aux2 <- births_weeks_temp %>% 
  filter(con_na==FALSE) %>% 
  group_by(clim_zone) %>% 
  distinct(name_comuna) %>% 
  summarise(name_comuna = paste(name_comuna, collapse = " / ")) 


t1_2 <- t1_2 %>% 
  left_join(aux, by="clim_zone") %>% 
  left_join(aux2, by="clim_zone") 


tables <- list("zone" = t1_1, 
               "zone_clim" = t1_2)

write.xlsx(tables, file =  paste0(data_out, "tab/", "Obs_zone", ".xlsx"))

### tbw & Ltbw ----

mean(births_weeks_temp$tbw)
mean(births_weeks_temp$ltbw)
sd(births_weeks_temp$tbw)

mean(births_weeks_temp$tbw[births_weeks_temp$con_na==FALSE])
mean(births_weeks_temp$ltbw[births_weeks_temp$con_na==FALSE])
sd(births_weeks_temp$tbw[births_weeks_temp$con_na==FALSE])

### Descriptive stats ---- 

#### Zone temp ----
t1_1 <- births_weeks_temp %>% 
  summarise(
    n=n(), 
    tbw_mean = mean(tbw, na.rm = TRUE),
    tbw_sd = sd(tbw, na.rm = TRUE),
    ltbw_prop = mean(ltbw, na.rm = TRUE),
    mean = mean(temp_mean, na.rm = TRUE),
    min = min(temp_mean, na.rm = TRUE),
    q05 = quantile(temp_mean, probs = 0.05, na.rm = TRUE), 
    q10 = quantile(temp_mean, probs = 0.10, na.rm = TRUE), 
    q20 = quantile(temp_mean, probs = 0.20, na.rm = TRUE), 
    q30 = quantile(temp_mean, probs = 0.30, na.rm = TRUE), 
    q40 = quantile(temp_mean, probs = 0.40, na.rm = TRUE), 
    q50 = quantile(temp_mean, probs = 0.50, na.rm = TRUE), 
    q60 = quantile(temp_mean, probs = 0.60, na.rm = TRUE), 
    q70 = quantile(temp_mean, probs = 0.70, na.rm = TRUE), 
    q80 = quantile(temp_mean, probs = 0.80, na.rm = TRUE), 
    q90 = quantile(temp_mean, probs = 0.90, na.rm = TRUE), 
    q95 = quantile(temp_mean, probs = 0.95, na.rm = TRUE),
    max=max(temp_mean, na.rm = TRUE)
  )

# Zone
t1_2 <- births_weeks_temp %>% 
  group_by(zone) %>% 
  summarise(
    n=n(), 
    tbw_mean = mean(tbw, na.rm = TRUE),
    tbw_sd = sd(tbw, na.rm = TRUE),
    ltbw_prop = mean(ltbw, na.rm = TRUE),
    mean=mean(temp_mean, na.rm = TRUE),
    min=min(temp_mean, na.rm = TRUE),
    q05 = quantile(temp_mean, probs = 0.05, na.rm = TRUE), 
    q10 = quantile(temp_mean, probs = 0.10, na.rm = TRUE), 
    q20 = quantile(temp_mean, probs = 0.20, na.rm = TRUE), 
    q30 = quantile(temp_mean, probs = 0.30, na.rm = TRUE), 
    q40 = quantile(temp_mean, probs = 0.40, na.rm = TRUE), 
    q50 = quantile(temp_mean, probs = 0.50, na.rm = TRUE), 
    q60 = quantile(temp_mean, probs = 0.60, na.rm = TRUE), 
    q70 = quantile(temp_mean, probs = 0.70, na.rm = TRUE), 
    q80 = quantile(temp_mean, probs = 0.80, na.rm = TRUE), 
    q90 = quantile(temp_mean, probs = 0.90, na.rm = TRUE), 
    q95 = quantile(temp_mean, probs = 0.95, na.rm = TRUE),
    max=max(temp_mean, na.rm = TRUE)
  )

# Trimester average 
t1_3 <- births_weeks_temp %>% 
  group_by(zone, trim_gest) %>% 
  summarise(
    n=n(), 
    tbw_mean = mean(tbw, na.rm = TRUE),
    tbw_sd = sd(tbw, na.rm = TRUE),
    ltbw_prop = mean(ltbw, na.rm = TRUE),
    mean=mean(temp_mean, na.rm = TRUE),
    min=min(temp_mean, na.rm = TRUE),
    q05 = quantile(temp_mean, probs = 0.05, na.rm = TRUE), 
    q10 = quantile(temp_mean, probs = 0.10, na.rm = TRUE), 
    q20 = quantile(temp_mean, probs = 0.20, na.rm = TRUE), 
    q30 = quantile(temp_mean, probs = 0.30, na.rm = TRUE), 
    q40 = quantile(temp_mean, probs = 0.40, na.rm = TRUE), 
    q50 = quantile(temp_mean, probs = 0.50, na.rm = TRUE), 
    q60 = quantile(temp_mean, probs = 0.60, na.rm = TRUE), 
    q70 = quantile(temp_mean, probs = 0.70, na.rm = TRUE), 
    q80 = quantile(temp_mean, probs = 0.80, na.rm = TRUE), 
    q90 = quantile(temp_mean, probs = 0.90, na.rm = TRUE), 
    q95 = quantile(temp_mean, probs = 0.95, na.rm = TRUE),
    max=max(temp_mean, na.rm = TRUE)
  )

t1 <- t1_1 %>% 
  bind_rows(t1_2) %>% 
  bind_rows(t1_3) %>% 
  relocate(zone, trim_gest)

#### Climate zone temp ----
t2_1 <- births_weeks_temp %>% 
  summarise(
    n=n(), 
    tbw_mean = mean(tbw, na.rm = TRUE),
    tbw_sd = sd(tbw, na.rm = TRUE),
    ltbw_prop = mean(ltbw, na.rm = TRUE),
    mean = mean(temp_mean, na.rm = TRUE),
    min = min(temp_mean, na.rm = TRUE),
    q05 = quantile(temp_mean, probs = 0.05, na.rm = TRUE), 
    q10 = quantile(temp_mean, probs = 0.10, na.rm = TRUE), 
    q20 = quantile(temp_mean, probs = 0.20, na.rm = TRUE), 
    q30 = quantile(temp_mean, probs = 0.30, na.rm = TRUE), 
    q40 = quantile(temp_mean, probs = 0.40, na.rm = TRUE), 
    q50 = quantile(temp_mean, probs = 0.50, na.rm = TRUE), 
    q60 = quantile(temp_mean, probs = 0.60, na.rm = TRUE), 
    q70 = quantile(temp_mean, probs = 0.70, na.rm = TRUE), 
    q80 = quantile(temp_mean, probs = 0.80, na.rm = TRUE), 
    q90 = quantile(temp_mean, probs = 0.90, na.rm = TRUE), 
    q95 = quantile(temp_mean, probs = 0.95, na.rm = TRUE),
    max=max(temp_mean, na.rm = TRUE)
  )

# Zone
t2_2 <- births_weeks_temp %>% 
  group_by(clim_zone) %>% 
  summarise(
    n=n(), 
    tbw_mean = mean(tbw, na.rm = TRUE),
    tbw_sd = sd(tbw, na.rm = TRUE),
    ltbw_prop = mean(ltbw, na.rm = TRUE),
    mean=mean(temp_mean, na.rm = TRUE),
    min=min(temp_mean, na.rm = TRUE),
    q05 = quantile(temp_mean, probs = 0.05, na.rm = TRUE), 
    q10 = quantile(temp_mean, probs = 0.10, na.rm = TRUE), 
    q20 = quantile(temp_mean, probs = 0.20, na.rm = TRUE), 
    q30 = quantile(temp_mean, probs = 0.30, na.rm = TRUE), 
    q40 = quantile(temp_mean, probs = 0.40, na.rm = TRUE), 
    q50 = quantile(temp_mean, probs = 0.50, na.rm = TRUE), 
    q60 = quantile(temp_mean, probs = 0.60, na.rm = TRUE), 
    q70 = quantile(temp_mean, probs = 0.70, na.rm = TRUE), 
    q80 = quantile(temp_mean, probs = 0.80, na.rm = TRUE), 
    q90 = quantile(temp_mean, probs = 0.90, na.rm = TRUE), 
    q95 = quantile(temp_mean, probs = 0.95, na.rm = TRUE),
    max=max(temp_mean, na.rm = TRUE)
  )

# Trimester average 
t2_3 <- births_weeks_temp %>% 
  group_by(clim_zone, trim_gest) %>% 
  summarise(
    n=n(), 
    tbw_mean = mean(tbw, na.rm = TRUE),
    tbw_sd = sd(tbw, na.rm = TRUE),
    ltbw_prop = mean(ltbw, na.rm = TRUE),
    mean=mean(temp_mean, na.rm = TRUE),
    min=min(temp_mean, na.rm = TRUE),
    q05 = quantile(temp_mean, probs = 0.05, na.rm = TRUE), 
    q10 = quantile(temp_mean, probs = 0.10, na.rm = TRUE), 
    q20 = quantile(temp_mean, probs = 0.20, na.rm = TRUE), 
    q30 = quantile(temp_mean, probs = 0.30, na.rm = TRUE), 
    q40 = quantile(temp_mean, probs = 0.40, na.rm = TRUE), 
    q50 = quantile(temp_mean, probs = 0.50, na.rm = TRUE), 
    q60 = quantile(temp_mean, probs = 0.60, na.rm = TRUE), 
    q70 = quantile(temp_mean, probs = 0.70, na.rm = TRUE), 
    q80 = quantile(temp_mean, probs = 0.80, na.rm = TRUE), 
    q90 = quantile(temp_mean, probs = 0.90, na.rm = TRUE), 
    q95 = quantile(temp_mean, probs = 0.95, na.rm = TRUE),
    max=max(temp_mean, na.rm = TRUE)
  )


t2 <- t2_1 %>% 
  bind_rows(t2_2) %>% 
  bind_rows(t2_3) %>% 
  relocate(clim_zone, trim_gest)


#### Save tables ----
tables <- list("zone" = t1, 
               "zone_clim" = t2)

write.xlsx(tables, file =  paste0(data_out, "tab/", "Descriptives_temp", ".xlsx"))

## Models  ----

#########################################################/
### ZONE ----
#########################################################/

### GAM Models tBW ----

#### Prepare factors ----
births_weeks_temp$temp_mean_percentile_wz <- relevel(births_weeks_temp$temp_mean_percentile_wz, ref = "41-50")
births_weeks_temp$temp_mean_percentile_wcz <- relevel(births_weeks_temp$temp_mean_percentile_wcz, ref = "41-50")

#### Estimate and save Models (tbw) ----

gm <- gam(tbw ~ temp_mean_percentile_wz + sex + 
            age_mom + factor(educ_mom) + factor(job_mom) +
            age_dad + factor(educ_dad) + factor(job_dad) + 
            s(year_week1) + s(month_week1),  
          data = births_weeks_temp, 
          family=gaussian())

gm_t1 <- gam(tbw ~ temp_mean_percentile_wz + sex + 
               age_mom + factor(educ_mom) + factor(job_mom) +
               age_dad + factor(educ_dad) + factor(job_dad) + 
               s(year_week1) + s(month_week1),  
             data = filter(births_weeks_temp, trim_gest=="T1"),
             family=gaussian())

gm_t2 <- gam(tbw ~ temp_mean_percentile_wz + sex + 
               age_mom + factor(educ_mom) + factor(job_mom) +
               age_dad + factor(educ_dad) + factor(job_dad) +
               s(year_week1) + s(month_week1),  
             data = filter(births_weeks_temp, trim_gest=="T2"),
             family=gaussian())

gm_t3 <- gam(tbw ~ temp_mean_percentile_wz + sex + 
               age_mom + factor(educ_mom) + factor(job_mom) +
               age_dad + factor(educ_dad) + factor(job_dad) +
               s(year_week1) + s(month_week1),  
             data = filter(births_weeks_temp, trim_gest=="T3"),
             family=gaussian())


m1 <-  broom::tidy(gm, parametric = TRUE)
mt1 <- broom::tidy(gm_t1, parametric = TRUE)
mt2 <- broom::tidy(gm_t2, parametric = TRUE)
mt3 <- broom::tidy(gm_t3, parametric = TRUE)

tables <- list("m1" = m1, 
               "m1_t1" = mt1,
               "m1_t2" = mt2,
               "m1_t3" = mt3)

write.xlsx(tables, file =  paste0(data_out, "models/", "GAM_models_tbw_zone", ".xlsx"))


#### Graph Models reports tbw ----

p50 <- data.frame(term="temp_mean_percentile_wz41-50",
                  estimate=0, 
                  std.error=0, 
                  statistic=0, 
                  p.value=0)

##### Full period ----

vis_data <- m1 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wz<=10" ~ "<=10",
    term == "temp_mean_percentile_wz11-20" ~ "11-20",
    term == "temp_mean_percentile_wz21-30" ~ "21-30",
    term == "temp_mean_percentile_wz31-40" ~ "31-40",
    term == "temp_mean_percentile_wz41-50" ~ "41-50",
    term == "temp_mean_percentile_wz51-60" ~ "51-60",
    term == "temp_mean_percentile_wz61-70" ~ "61-70",
    term == "temp_mean_percentile_wz71-80" ~ "71-80",
    term == "temp_mean_percentile_wz81-90" ~ "81-90",
    term == "temp_mean_percentile_wz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) 
  
g1 <- ggplot(vis_data, aes(x = term, y = estimate)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = lci, ymax = rci), width = 0.1) +
  scale_y_continuous(limits = c(-30, 30), n.breaks = 6) +
  geom_hline(yintercept = 0, linewidth = 0.5, color="black", linetype = "dashed") +
  labs(x = "Temperature centiles",
       y = "Differences in tBW and 95% CI (grams)",
       tag="A)") +
  theme_bw() +
  theme(axis.text.y = element_text(size=8, angle=0), 
        legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=10, angle=0, vjust=0.70, hjust=0.70),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=10, hjust = 0))

g1

##### Trimester period ---- 

vis_t1 <- mt1 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wz<=10" ~ "<=10",
    term == "temp_mean_percentile_wz11-20" ~ "11-20",
    term == "temp_mean_percentile_wz21-30" ~ "21-30",
    term == "temp_mean_percentile_wz31-40" ~ "31-40",
    term == "temp_mean_percentile_wz41-50" ~ "41-50",
    term == "temp_mean_percentile_wz51-60" ~ "51-60",
    term == "temp_mean_percentile_wz61-70" ~ "61-70",
    term == "temp_mean_percentile_wz71-80" ~ "71-80",
    term == "temp_mean_percentile_wz81-90" ~ "81-90",
    term == "temp_mean_percentile_wz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) %>% 
  mutate(Trimester="T1")

vis_t2 <- mt2 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wz<=10" ~ "<=10",
    term == "temp_mean_percentile_wz11-20" ~ "11-20",
    term == "temp_mean_percentile_wz21-30" ~ "21-30",
    term == "temp_mean_percentile_wz31-40" ~ "31-40",
    term == "temp_mean_percentile_wz41-50" ~ "41-50",
    term == "temp_mean_percentile_wz51-60" ~ "51-60",
    term == "temp_mean_percentile_wz61-70" ~ "61-70",
    term == "temp_mean_percentile_wz71-80" ~ "71-80",
    term == "temp_mean_percentile_wz81-90" ~ "81-90",
    term == "temp_mean_percentile_wz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) %>% 
  mutate(Trimester="T2")

vis_t3 <- mt3 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wz<=10" ~ "<=10",
    term == "temp_mean_percentile_wz11-20" ~ "11-20",
    term == "temp_mean_percentile_wz21-30" ~ "21-30",
    term == "temp_mean_percentile_wz31-40" ~ "31-40",
    term == "temp_mean_percentile_wz41-50" ~ "41-50",
    term == "temp_mean_percentile_wz51-60" ~ "51-60",
    term == "temp_mean_percentile_wz61-70" ~ "61-70",
    term == "temp_mean_percentile_wz71-80" ~ "71-80",
    term == "temp_mean_percentile_wz81-90" ~ "81-90",
    term == "temp_mean_percentile_wz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) %>% 
  mutate(Trimester="T3")

vis_data <- rbind(vis_t1, vis_t2, vis_t3)

g2 <- ggplot(vis_data, aes(x = term, y = estimate, color = Trimester, shape = Trimester, group = Trimester)) +
  geom_point(position = position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin = lci, ymax = rci), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("T1" = "black", "T2" = "grey", "T3" = "lightblue")) +
  scale_shape_manual(values = c("T1" = 16, "T2" = 17, "T3" = 15)) +
  labs(x = "Temperature centiles",
       y = "Differences in tBW and 95% CI (grams)",
       tag="B)") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8, angle = 0), 
        legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color = NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10, angle = 0, vjust = 0.70, hjust = 0.70),
        plot.title = element_text(size = 12, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white"), 
        strip.text.x = element_text(size = 10, hjust = 0))

g2


### GAM Models ltBW ----

#### Estimate and save Models (ltbw) ----

gml <- gam(ltbw ~ temp_mean_percentile_wz + sex + 
            age_mom + factor(educ_mom) + factor(job_mom) +
            age_dad + factor(educ_dad) + factor(job_dad) + 
            s(year_week1) + s(month_week1),  
          data = births_weeks_temp, 
          family = binomial(link = "logit"))

gml_t1 <- gam(ltbw ~ temp_mean_percentile_wz + sex + 
               age_mom + factor(educ_mom) + factor(job_mom) +
               age_dad + factor(educ_dad) + factor(job_dad) +
               s(year_week1) + s(month_week1),  
             data = filter(births_weeks_temp, trim_gest=="T1"),
             family = binomial(link = "logit"))

gml_t2 <- gam(ltbw ~ temp_mean_percentile_wz + sex + 
               age_mom + factor(educ_mom) + factor(job_mom) +
               age_dad + factor(educ_dad) + factor(job_dad) + 
                s(year_week1) + s(month_week1),  
             data = filter(births_weeks_temp, trim_gest=="T2"),
             family = binomial(link = "logit"))

gml_t3 <- gam(ltbw ~ temp_mean_percentile_wz + sex + 
               age_mom + factor(educ_mom) + factor(job_mom) +
               age_dad + factor(educ_dad) + factor(job_dad) +
                s(year_week1) + s(month_week1),  
             data = filter(births_weeks_temp, trim_gest=="T3"),
             family = binomial(link = "logit"))


ml1 <-  broom::tidy(gml, parametric = TRUE, exponentiate=TRUE)
mlt1 <- broom::tidy(gml_t1, parametric = TRUE, exponentiate=TRUE)
mlt2 <- broom::tidy(gml_t2, parametric = TRUE, exponentiate=TRUE)
mlt3 <- broom::tidy(gml_t3, parametric = TRUE, exponentiate=TRUE)

tables <- list("ml1" = ml1, 
               "ml1_t1" = mlt1,
               "ml1_t2" = mlt2,
               "ml1_t3" = mlt3)

write.xlsx(tables, file =  paste0(data_out, "models/", "GAM_models_ltbw_zone", ".xlsx"))


#### Graph Models reports tbw ----

p50 <- data.frame(term="temp_mean_percentile_wz41-50",
                  estimate=1, 
                  std.error=0, 
                  statistic=0, 
                  p.value=0)

##### Full period ----

vis_data <- ml1 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wz<=10" ~ "<=10",
    term == "temp_mean_percentile_wz11-20" ~ "11-20",
    term == "temp_mean_percentile_wz21-30" ~ "21-30",
    term == "temp_mean_percentile_wz31-40" ~ "31-40",
    term == "temp_mean_percentile_wz41-50" ~ "41-50",
    term == "temp_mean_percentile_wz51-60" ~ "51-60",
    term == "temp_mean_percentile_wz61-70" ~ "61-70",
    term == "temp_mean_percentile_wz71-80" ~ "71-80",
    term == "temp_mean_percentile_wz81-90" ~ "81-90",
    term == "temp_mean_percentile_wz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) 

g3 <- ggplot(vis_data, aes(x = term, y = estimate)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = lci, ymax = rci), width = 0.1) +
  scale_y_continuous(limits = c(0.8, 1.2), n.breaks = 6) +
  geom_hline(yintercept = 1, linewidth = 0.5, color="black", linetype = "dashed") +
  labs(x = "Temperature centiles",
       y = "tLBW (ORs and 95% CI)",
       tag="A)") +
  theme_bw() +
  theme(axis.text.y = element_text(size=8, angle=0), 
        legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=10, angle=0, vjust=0.70, hjust=0.70),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=10, hjust = 0))

g3

##### Trimester period ---- 

vis_t1 <- mlt1 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wz<=10" ~ "<=10",
    term == "temp_mean_percentile_wz11-20" ~ "11-20",
    term == "temp_mean_percentile_wz21-30" ~ "21-30",
    term == "temp_mean_percentile_wz31-40" ~ "31-40",
    term == "temp_mean_percentile_wz41-50" ~ "41-50",
    term == "temp_mean_percentile_wz51-60" ~ "51-60",
    term == "temp_mean_percentile_wz61-70" ~ "61-70",
    term == "temp_mean_percentile_wz71-80" ~ "71-80",
    term == "temp_mean_percentile_wz81-90" ~ "81-90",
    term == "temp_mean_percentile_wz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) %>% 
  mutate(Trimester="T1")

vis_t2 <- mlt2 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wz<=10" ~ "<=10",
    term == "temp_mean_percentile_wz11-20" ~ "11-20",
    term == "temp_mean_percentile_wz21-30" ~ "21-30",
    term == "temp_mean_percentile_wz31-40" ~ "31-40",
    term == "temp_mean_percentile_wz41-50" ~ "41-50",
    term == "temp_mean_percentile_wz51-60" ~ "51-60",
    term == "temp_mean_percentile_wz61-70" ~ "61-70",
    term == "temp_mean_percentile_wz71-80" ~ "71-80",
    term == "temp_mean_percentile_wz81-90" ~ "81-90",
    term == "temp_mean_percentile_wz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) %>% 
  mutate(Trimester="T2")

vis_t3 <- mlt3 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wz<=10" ~ "<=10",
    term == "temp_mean_percentile_wz11-20" ~ "11-20",
    term == "temp_mean_percentile_wz21-30" ~ "21-30",
    term == "temp_mean_percentile_wz31-40" ~ "31-40",
    term == "temp_mean_percentile_wz41-50" ~ "41-50",
    term == "temp_mean_percentile_wz51-60" ~ "51-60",
    term == "temp_mean_percentile_wz61-70" ~ "61-70",
    term == "temp_mean_percentile_wz71-80" ~ "71-80",
    term == "temp_mean_percentile_wz81-90" ~ "81-90",
    term == "temp_mean_percentile_wz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) %>% 
  mutate(Trimester="T3")

vis_data <- rbind(vis_t1, vis_t2, vis_t3)

g4 <- ggplot(vis_data, aes(x = term, y = estimate, color = Trimester, shape = Trimester, group = Trimester)) +
  geom_point(position = position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin = lci, ymax = rci), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linewidth = 0.5, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("T1" = "black", "T2" = "grey", "T3" = "lightblue")) +
  scale_shape_manual(values = c("T1" = 16, "T2" = 17, "T3" = 15)) +
  labs(x = "Temperature centiles",
       y = "tLBW (ORs and 95% CI)",
       tag="B)") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8, angle = 0), 
        legend.position = "right",
        legend.title = element_blank(),
        legend.background = element_rect(color = NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10, angle = 0, vjust = 0.70, hjust = 0.70),
        plot.title = element_text(size = 12, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white"), 
        strip.text.x = element_text(size = 10, hjust = 0))

g4

#### Save plots ----

ggarrange(g1, g2, common.legend = FALSE, ncol = 2, legend="right")

ggsave(filename = paste0(data_out, "fig/", "GAM_models_tbw_zone", ".png"),
       res = 300,
       width = 25,
       height = 10,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)

ggarrange(g3, g4, common.legend = FALSE, ncol = 2, legend="right")

ggsave(filename = paste0(data_out, "fig/", "GAM_models_tbw_zone_trim", ".png"),
       res = 300,
       width = 25,
       height = 10,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)

#########################################################/
### CLIMATE ZONE ----
#########################################################/

### GAM Models tBW (climate zone) ----

#### Estimate and save Models (tbw) ----

gm <- gam(tbw ~ temp_mean_percentile_wcz + sex + 
            age_mom + factor(educ_mom) + factor(job_mom) +
            age_dad + factor(educ_dad) + factor(job_dad) + 
            s(year_week1) + s(month_week1),  
          data = births_weeks_temp, 
          family=gaussian())

gm_t1 <- gam(tbw ~ temp_mean_percentile_wcz + sex + 
               age_mom + factor(educ_mom) + factor(job_mom) +
               age_dad + factor(educ_dad) + factor(job_dad) + 
               s(year_week1) + s(month_week1),  
             data = filter(births_weeks_temp, trim_gest=="T1"),
             family=gaussian())

gm_t2 <- gam(tbw ~ temp_mean_percentile_wcz + sex + 
               age_mom + factor(educ_mom) + factor(job_mom) +
               age_dad + factor(educ_dad) + factor(job_dad) + 
               s(year_week1) + s(month_week1),  
             data = filter(births_weeks_temp, trim_gest=="T2"),
             family=gaussian())

gm_t3 <- gam(tbw ~ temp_mean_percentile_wcz + sex + 
               age_mom + factor(educ_mom) + factor(job_mom) +
               age_dad + factor(educ_dad) + factor(job_dad) + 
             s(year_week1) + s(month_week1),  
             data = filter(births_weeks_temp, trim_gest=="T3"),
             family=gaussian())


m1 <-  broom::tidy(gm, parametric = TRUE)
mt1 <- broom::tidy(gm_t1, parametric = TRUE)
mt2 <- broom::tidy(gm_t2, parametric = TRUE)
mt3 <- broom::tidy(gm_t3, parametric = TRUE)

tables <- list("m1" = m1, 
               "m1_t1" = mt1,
               "m1_t2" = mt2,
               "m1_t3" = mt3)

write.xlsx(tables, file =  paste0(data_out, "models/", "GAM_models_tbw_zone_wcz", ".xlsx"))


#### Graph Models reports tbw ----

p50 <- data.frame(term="temp_mean_percentile_wcz41-50",
                  estimate=0, 
                  std.error=0, 
                  statistic=0, 
                  p.value=0)

##### Full period ----

vis_data <- m1 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wcz<=10" ~ "<=10",
    term == "temp_mean_percentile_wcz11-20" ~ "11-20",
    term == "temp_mean_percentile_wcz21-30" ~ "21-30",
    term == "temp_mean_percentile_wcz31-40" ~ "31-40",
    term == "temp_mean_percentile_wcz41-50" ~ "41-50",
    term == "temp_mean_percentile_wcz51-60" ~ "51-60",
    term == "temp_mean_percentile_wcz61-70" ~ "61-70",
    term == "temp_mean_percentile_wcz71-80" ~ "71-80",
    term == "temp_mean_percentile_wcz81-90" ~ "81-90",
    term == "temp_mean_percentile_wcz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) 

g1 <- ggplot(vis_data, aes(x = term, y = estimate)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = lci, ymax = rci), width = 0.1) +
  scale_y_continuous(limits = c(-30, 30), n.breaks = 6) +
  geom_hline(yintercept = 0, linewidth = 0.5, color="black", linetype = "dashed") +
  labs(x = "Temperature centiles",
       y = "Differences in tBW and 95% CI (grams)",
       tag="A)") +
  theme_bw() +
  theme(axis.text.y = element_text(size=8, angle=0), 
        legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=10, angle=0, vjust=0.70, hjust=0.70),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=10, hjust = 0))

g1

##### Trimester period ---- 

vis_t1 <- mt1 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wcz<=10" ~ "<=10",
    term == "temp_mean_percentile_wcz11-20" ~ "11-20",
    term == "temp_mean_percentile_wcz21-30" ~ "21-30",
    term == "temp_mean_percentile_wcz31-40" ~ "31-40",
    term == "temp_mean_percentile_wcz41-50" ~ "41-50",
    term == "temp_mean_percentile_wcz51-60" ~ "51-60",
    term == "temp_mean_percentile_wcz61-70" ~ "61-70",
    term == "temp_mean_percentile_wcz71-80" ~ "71-80",
    term == "temp_mean_percentile_wcz81-90" ~ "81-90",
    term == "temp_mean_percentile_wcz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) %>% 
  mutate(Trimester="T1")

vis_t2 <- mt2 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wcz<=10" ~ "<=10",
    term == "temp_mean_percentile_wcz11-20" ~ "11-20",
    term == "temp_mean_percentile_wcz21-30" ~ "21-30",
    term == "temp_mean_percentile_wcz31-40" ~ "31-40",
    term == "temp_mean_percentile_wcz41-50" ~ "41-50",
    term == "temp_mean_percentile_wcz51-60" ~ "51-60",
    term == "temp_mean_percentile_wcz61-70" ~ "61-70",
    term == "temp_mean_percentile_wcz71-80" ~ "71-80",
    term == "temp_mean_percentile_wcz81-90" ~ "81-90",
    term == "temp_mean_percentile_wcz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) %>% 
  mutate(Trimester="T2")

vis_t3 <- mt3 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wcz<=10" ~ "<=10",
    term == "temp_mean_percentile_wcz11-20" ~ "11-20",
    term == "temp_mean_percentile_wcz21-30" ~ "21-30",
    term == "temp_mean_percentile_wcz31-40" ~ "31-40",
    term == "temp_mean_percentile_wcz41-50" ~ "41-50",
    term == "temp_mean_percentile_wcz51-60" ~ "51-60",
    term == "temp_mean_percentile_wcz61-70" ~ "61-70",
    term == "temp_mean_percentile_wcz71-80" ~ "71-80",
    term == "temp_mean_percentile_wcz81-90" ~ "81-90",
    term == "temp_mean_percentile_wcz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) %>% 
  mutate(Trimester="T3")

vis_data <- rbind(vis_t1, vis_t2, vis_t3)

g2 <- ggplot(vis_data, aes(x = term, y = estimate, color = Trimester, shape = Trimester, group = Trimester)) +
  geom_point(position = position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin = lci, ymax = rci), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("T1" = "black", "T2" = "grey", "T3" = "lightblue")) +
  scale_shape_manual(values = c("T1" = 16, "T2" = 17, "T3" = 15)) +
  labs(x = "Temperature centiles",
       y = "Differences in tBW and 95% CI (grams)",
       tag="B)") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8, angle = 0), 
        legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color = NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10, angle = 0, vjust = 0.70, hjust = 0.70),
        plot.title = element_text(size = 12, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white"), 
        strip.text.x = element_text(size = 10, hjust = 0))

g2

### GAM Models ltBW ----

#### Estimate and save Models (ltbw) ----

gml <- gam(ltbw ~ temp_mean_percentile_wcz + sex + 
             age_mom + factor(educ_mom) + factor(job_mom) +
             age_dad + factor(educ_dad) + factor(job_dad) + 
             s(year_week1) + s(month_week1),  
           data = births_weeks_temp, 
           family = binomial(link = "logit"))

gml_t1 <- gam(ltbw ~ temp_mean_percentile_wcz + sex + 
                age_mom + factor(educ_mom) + factor(job_mom) +
                age_dad + factor(educ_dad) + factor(job_dad) + 
                s(year_week1) + s(month_week1), 
              data = filter(births_weeks_temp, trim_gest=="T1"),
              family = binomial(link = "logit"))

gml_t2 <- gam(ltbw ~ temp_mean_percentile_wcz + sex + 
                age_mom + factor(educ_mom) + factor(job_mom) +
                age_dad + factor(educ_dad) + factor(job_dad) + 
                s(year_week1) + s(month_week1),  
              data = filter(births_weeks_temp, trim_gest=="T2"),
              family = binomial(link = "logit"))

gml_t3 <- gam(ltbw ~ temp_mean_percentile_wcz + sex + 
                age_mom + factor(educ_mom) + factor(job_mom) +
                age_dad + factor(educ_dad) + factor(job_dad) + 
                s(year_week1) + s(month_week1),  
              data = filter(births_weeks_temp, trim_gest=="T3"),
              family = binomial(link = "logit"))


ml1 <-  broom::tidy(gml, parametric = TRUE, exponentiate=TRUE)
mlt1 <- broom::tidy(gml_t1, parametric = TRUE, exponentiate=TRUE)
mlt2 <- broom::tidy(gml_t2, parametric = TRUE, exponentiate=TRUE)
mlt3 <- broom::tidy(gml_t3, parametric = TRUE, exponentiate=TRUE)

tables <- list("ml1" = ml1, 
               "ml1_t1" = mlt1,
               "ml1_t2" = mlt2,
               "ml1_t3" = mlt3)

write.xlsx(tables, file =  paste0(data_out, "models/", "GAM_models_ltbw_zone_wcz", ".xlsx"))


#### Graph Models reports tbw ----

p50 <- data.frame(term="temp_mean_percentile_wcz41-50",
                  estimate=1, 
                  std.error=0, 
                  statistic=0, 
                  p.value=0)

##### Full period ----

vis_data <- ml1 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wcz<=10" ~ "<=10",
    term == "temp_mean_percentile_wcz11-20" ~ "11-20",
    term == "temp_mean_percentile_wcz21-30" ~ "21-30",
    term == "temp_mean_percentile_wcz31-40" ~ "31-40",
    term == "temp_mean_percentile_wcz41-50" ~ "41-50",
    term == "temp_mean_percentile_wcz51-60" ~ "51-60",
    term == "temp_mean_percentile_wcz61-70" ~ "61-70",
    term == "temp_mean_percentile_wcz71-80" ~ "71-80",
    term == "temp_mean_percentile_wcz81-90" ~ "81-90",
    term == "temp_mean_percentile_wcz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) 

g3 <- ggplot(vis_data, aes(x = term, y = estimate)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = lci, ymax = rci), width = 0.1) +
  scale_y_continuous(limits = c(0.8, 1.2), n.breaks = 6) +
  geom_hline(yintercept = 1, linewidth = 0.5, color="black", linetype = "dashed") +
  labs(x = "Temperature centiles",
       y = "tLBW (ORs and 95% CI)",
       tag="A)") +
  theme_bw() +
  theme(axis.text.y = element_text(size=8, angle=0), 
        legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=10, angle=0, vjust=0.70, hjust=0.70),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=10, hjust = 0))

g3

##### Trimester period ---- 

vis_t1 <- mlt1 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wcz<=10" ~ "<=10",
    term == "temp_mean_percentile_wcz11-20" ~ "11-20",
    term == "temp_mean_percentile_wcz21-30" ~ "21-30",
    term == "temp_mean_percentile_wcz31-40" ~ "31-40",
    term == "temp_mean_percentile_wcz41-50" ~ "41-50",
    term == "temp_mean_percentile_wcz51-60" ~ "51-60",
    term == "temp_mean_percentile_wcz61-70" ~ "61-70",
    term == "temp_mean_percentile_wcz71-80" ~ "71-80",
    term == "temp_mean_percentile_wcz81-90" ~ "81-90",
    term == "temp_mean_percentile_wcz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) %>% 
  mutate(Trimester="T1")

vis_t2 <- mlt2 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wcz<=10" ~ "<=10",
    term == "temp_mean_percentile_wcz11-20" ~ "11-20",
    term == "temp_mean_percentile_wcz21-30" ~ "21-30",
    term == "temp_mean_percentile_wcz31-40" ~ "31-40",
    term == "temp_mean_percentile_wcz41-50" ~ "41-50",
    term == "temp_mean_percentile_wcz51-60" ~ "51-60",
    term == "temp_mean_percentile_wcz61-70" ~ "61-70",
    term == "temp_mean_percentile_wcz71-80" ~ "71-80",
    term == "temp_mean_percentile_wcz81-90" ~ "81-90",
    term == "temp_mean_percentile_wcz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) %>% 
  mutate(Trimester="T2")

vis_t3 <- mlt3 %>% 
  slice(2:10) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error)) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_wcz<=10" ~ "<=10",
    term == "temp_mean_percentile_wcz11-20" ~ "11-20",
    term == "temp_mean_percentile_wcz21-30" ~ "21-30",
    term == "temp_mean_percentile_wcz31-40" ~ "31-40",
    term == "temp_mean_percentile_wcz41-50" ~ "41-50",
    term == "temp_mean_percentile_wcz51-60" ~ "51-60",
    term == "temp_mean_percentile_wcz61-70" ~ "61-70",
    term == "temp_mean_percentile_wcz71-80" ~ "71-80",
    term == "temp_mean_percentile_wcz81-90" ~ "81-90",
    term == "temp_mean_percentile_wcz> 90" ~ "> 90"
  ), 
  levels=c("<=10",
           "11-20",
           "21-30",
           "31-40",
           "41-50",
           "51-60",
           "61-70",
           "71-80",
           "81-90",
           "> 90"
  )
  )) %>% 
  mutate(Trimester="T3")

vis_data <- rbind(vis_t1, vis_t2, vis_t3)

g4 <- ggplot(vis_data, aes(x = term, y = estimate, color = Trimester, shape = Trimester, group = Trimester)) +
  geom_point(position = position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin = lci, ymax = rci), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linewidth = 0.5, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("T1" = "black", "T2" = "grey", "T3" = "lightblue")) +
  scale_shape_manual(values = c("T1" = 16, "T2" = 17, "T3" = 15)) +
  labs(x = "Temperature centiles",
       y = "tLBW (ORs and 95% CI)",
       tag="B)") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8, angle = 0), 
        legend.position = "right",
        legend.title = element_blank(),
        legend.background = element_rect(color = NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10, angle = 0, vjust = 0.70, hjust = 0.70),
        plot.title = element_text(size = 12, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white"), 
        strip.text.x = element_text(size = 10, hjust = 0))

g4

#### Save plots ----

ggarrange(g1, g2, common.legend = FALSE, ncol = 2, legend="right")

ggsave(filename = paste0(data_out, "fig/", "GAM_models_tbw_zone_wcz", ".png"),
       res = 300,
       width = 25,
       height = 10,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)

ggarrange(g3, g4, common.legend = FALSE, ncol = 2, legend="right")

ggsave(filename = paste0(data_out, "fig/", "GAM_models_tbw_zone_trim_wcz", ".png"),
       res = 300,
       width = 25,
       height = 10,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)

#########################################################/
#########################################################/

