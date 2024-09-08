# 9.0 DLNM ---

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
  select(all_of(c(time, vd, vi, vc, trend))) %>%
  group_by(clim_zone, week_gest_num) %>%
  mutate(temp_mean_percentile_wcz = ntile(temp_mean, 100)) %>%
  ungroup() |> 
  select(-temp_mean) |>
  pivot_wider(names_from = "week_gest_num", 
              values_from = "temp_mean_percentile_wcz",
              names_prefix = "ptem_gw")


bw_dlnm_min <- births_weeks_temp |>
  #filter(week_gest_num<=37) |>
  select(all_of(c(time, vd, vi2, vc, trend))) %>%
  group_by(clim_zone, week_gest_num) %>%
  mutate(temp_min_percentile_wcz = ntile(temp_min, 100)) %>%
  ungroup() |> 
  select(-temp_min) |>
  pivot_wider(names_from = "week_gest_num", 
              values_from = "temp_min_percentile_wcz",
              names_prefix = "ptem_gw") 

bw_dlnm_max <- births_weeks_temp |>
  #filter(week_gest_num<=37) |>
  select(all_of(c(time, vd, vi3, vc, trend))) %>%
  group_by(clim_zone, week_gest_num) %>%
  mutate(temp_max_percentile_wcz = ntile(temp_max, 100)) %>%
  group_by(clim_zone, week_gest_num) %>%
  ungroup() |> 
  select(-temp_max) |>
  pivot_wider(names_from = "week_gest_num", 
              values_from = "temp_max_percentile_wcz",
              names_prefix = "ptem_gw") 


rm(births_weeks_temp)
glimpse(bw_dlnm_mean)
glimpse(bw_dlnm_min)
glimpse(bw_dlnm_max)

####################################################/
# DLNM -------- 
####################################################/

####################################################/
### Optimal knots -------- 
####################################################/

# grid the knots
knots = c(2, 3, 4, 5, 6, 8, 10)

# Mean

AICs.bw = c()
BICs.bw = c()

AICs.lbw = c()
BICs.lbw = c()

mat <- bw_dlnm_mean |>
  dplyr::select(ptem_gw1:ptem_gw37) |>
  as.matrix()

for (i in 1:length(knots)) {
  
  lagknots <- equalknots(x = c(2, 36), # same as the one that we define earlier
                         nk = knots[i], fun = "ns")
  
  cb <- crossbasis(mat, 
                   lag = c(1, 37),
                   argvar = list(fun = "strata", breaks=seq(10,90,by=10), ref=5),
                   arglag = list(fun = "ns", knots = lagknots)) # the form of the curve across all the lags, that is, the lag constrain 
  
  mod1 <- gam(tbw ~ cb + sex +
               age_group_mom + educ_group_mom + job_group_mom +
               age_group_dad + educ_group_dad + job_group_dad +
               s(year_week1) + s(month_week1),
             data = bw_dlnm_mean,
             na.action = na.exclude,
             family=gaussian())
  
  mod2 <- gam(ltbw ~ cb + sex +
               age_group_mom + educ_group_mom + job_group_mom +
               age_group_dad + educ_group_dad + job_group_dad +
               s(year_week1) + s(month_week1),
             data = bw_dlnm_mean,
             na.action = na.exclude,
             family = binomial(link = "logit"),
             gc.level = 0)
  
  AICs.bw[i] = AIC(mod1)
  BICs.bw[i] = BIC(mod1)
  
  AICs.lbw[i] = AIC(mod2)
  BICs.lbw[i] = BIC(mod2)
  
}

# Min

AICs.bw = c()
BICs.bw = c()

AICs.lbw = c()
BICs.lbw = c()

mat <- bw_dlnm_min |>
  dplyr::select(ptem_gw1:ptem_gw37) |>
  as.matrix()

for (i in 1:length(knots)) {
  
  lagknots <- equalknots(x = c(2, 36), # same as the one that we define earlier
                         nk = knots[i], fun = "ns")
  
  cb <- crossbasis(mat, 
                   lag = c(1, 37),
                   argvar = list(fun = "strata", breaks=seq(10,90,by=10), ref=5),
                   arglag = list(fun = "ns", knots = lagknots)) # the form of the curve across all the lags, that is, the lag constrain 
  
  mod1 <- gam(tbw ~ cb + sex +
                age_group_mom + educ_group_mom + job_group_mom +
                age_group_dad + educ_group_dad + job_group_dad +
                s(year_week1) + s(month_week1),
              data = bw_dlnm_mean,
              na.action = na.exclude,
              family=gaussian())
  
  mod2 <- gam(ltbw ~ cb + sex + 
                age_group_mom + educ_group_mom + job_group_mom +
                age_group_dad + educ_group_dad + job_group_dad +
                s(year_week1) + s(month_week1),
              data = bw_dlnm_mean, 
              na.action = na.exclude,
              family = binomial(link = "logit"),
              gc.level = 0)
  
  AICs.bw[i] = AIC(mod1)
  BICs.bw[i] = BIC(mod1)
  
  AICs.lbw[i] = AIC(mod2)
  BICs.lbw[i] = BIC(mod2)
  
}

# Max

AICs.bw = c()
BICs.bw = c()

AICs.lbw = c()
BICs.lbw = c()


mat <- bw_dlnm_max |>
  dplyr::select(ptem_gw1:ptem_gw37) |>
  as.matrix()

for (i in 1:length(knots)) {
  
  lagknots <- equalknots(x = c(2, 36), # same as the one that we define earlier
                         nk = knots[i], fun = "ns")
  
  cb <- crossbasis(mat, 
                   lag = c(1, 37),
                   argvar = list(fun = "strata", breaks=seq(10,90,by=10), ref=5),
                   arglag = list(fun = "ns", knots = lagknots)) # the form of the curve across all the lags, that is, the lag constrain 
  
  mod1 <- gam(tbw ~ cb + sex +
                age_group_mom + educ_group_mom + job_group_mom +
                age_group_dad + educ_group_dad + job_group_dad +
                s(year_week1) + s(month_week1),
              data = bw_dlnm_mean,
              na.action = na.exclude,
              family=gaussian())
  
  mod2 <- gam(ltbw ~ cb + sex + 
                age_group_mom + educ_group_mom + job_group_mom +
                age_group_dad + educ_group_dad + job_group_dad +
                s(year_week1) + s(month_week1),
              data = bw_dlnm_mean, 
              na.action = na.exclude,
              family = binomial(link = "logit"),
              gc.level = 0)
  
  AICs.bw[i] = AIC(mod1)
  BICs.bw[i] = BIC(mod1)
  
  AICs.lbw[i] = AIC(mod2)
  BICs.lbw[i] = BIC(mod2)
  
}

# Show the knots 
AICs.lbw # We obtained the AIC for the different knots 
BICs.lbw # We obtained the BIC for the different knots

AICs.lbw # We obtained the AIC for the different knots 
BICs.lbw # We obtained the BIC for the different knots

# This return the minimum AIC (knots), this mean that we going to use nk = 2 in the lagknots 
knots[which.min(AICs.bw)] 
knots[which.min(BICs.bw)]

knots[which.min(AICs.lbw)] 
knots[which.min(BICs.lbw)]

############################################################/
### Crossbasis for our model -------
############################################################/

lagknots <- equalknots(x = c(1, 37), nk = 2, fun = "ns")

mat1 <- bw_dlnm_mean |>
  dplyr::select(ptem_gw1:ptem_gw37) |>
  as.matrix()

mat2 <- bw_dlnm_min |>
  dplyr::select(ptem_gw1:ptem_gw37) |>
  as.matrix()

mat3 <- bw_dlnm_max |>
  dplyr::select(ptem_gw1:ptem_gw37) |>
  as.matrix()


cb1 <- crossbasis(mat1,
                 lag = c(1, 37),
                 #argvar = list(fun = "lin"),
                 argvar = list(fun = "strata", breaks=seq(10,90,by=10),ref=5),
                 arglag = list(fun = "ns", knots = lagknots)) # the form of the curve across all the lags, that is, the lag constrain 

cb2 <- crossbasis(mat2,
                  lag = c(1, 37),
                  #argvar = list(fun = "lin"),
                  argvar = list(fun = "strata", breaks=seq(10,90,by=10),ref=5),
                  arglag = list(fun = "ns", knots = lagknots)) # the form of the curve across all the lags, that is, the lag constrain 

cb3 <- crossbasis(mat3,
                  lag = c(1, 37),
                  #argvar = list(fun = "lin"),
                  argvar = list(fun = "strata", breaks=seq(10,90,by=10),ref=5),
                  arglag = list(fun = "ns", knots = lagknots)) # the form of the curve across all the lags, that is, the lag constrain 

#############################################################/
### Model with our crossbasis ------
#############################################################/

# Mean 
m1 <- gam(tbw ~ cb1 + sex + 
           age_group_mom + educ_group_mom + job_group_mom +
           age_group_dad + educ_group_dad + job_group_dad +
          s(year_week1) + s(month_week1),
          data = bw_dlnm_mean, 
          na.action = na.exclude,
          family=gaussian())

m2 <- gam(ltbw ~ cb1 + sex + 
            age_group_mom + educ_group_mom + job_group_mom +
            age_group_dad + educ_group_dad + job_group_dad +
            s(year_week1) + s(month_week1),
          data = bw_dlnm_mean, 
          na.action = na.exclude,
          family = binomial(link = "logit"))

# Min
m3 <- gam(tbw ~ cb2 + sex + 
            age_group_mom + educ_group_mom + job_group_mom +
            age_group_dad + educ_group_dad + job_group_dad +
            s(year_week1) + s(month_week1),
          data = bw_dlnm_min, 
          na.action = na.exclude,
          family=gaussian())

m4 <- gam(ltbw ~ cb2 + sex + 
            age_group_mom + educ_group_mom + job_group_mom +
            age_group_dad + educ_group_dad + job_group_dad +
            s(year_week1) + s(month_week1),
          data = bw_dlnm_min, 
          na.action = na.exclude,
          family = binomial(link = "logit"))

# Max
m5 <- gam(tbw ~ cb3 + sex + 
            age_group_mom + educ_group_mom + job_group_mom +
            age_group_dad + educ_group_dad + job_group_dad +
            s(year_week1) + s(month_week1),
          data = bw_dlnm_max, 
          na.action = na.exclude,
          family=gaussian())

m6 <- gam(ltbw ~ cb3 + sex + 
            age_group_mom + educ_group_mom + job_group_mom +
            age_group_dad + educ_group_dad + job_group_dad +
            s(year_week1) + s(month_week1),
          data = bw_dlnm_max, 
          na.action = na.exclude,
          family = binomial(link = "logit"))
          

#############################################################/
### Predicted values -----
#############################################################/

at = seq(5,95, by=10)
at

pred1 <- crosspred(cb1, m1, at=at)
pred2 <- crosspred(cb1, m2, at=at)
pred3 <- crosspred(cb2, m3, at=at)
pred4 <- crosspred(cb2, m4, at=at)
pred5 <- crosspred(cb3, m5, at=at)
pred6 <- crosspred(cb3, m6, at=at)

#### Plot effects ------
old_par <- par(no.readonly = TRUE)
# Save plots
png(paste0("Output_analysis/", "dlnm/fig/", "DLNM_full", ".png"), width = 1200, height = 1200, res = 100) 

par(mfrow = c(3, 2),
    mar = c(3, 0, 3, 0),  # Reduce intern margins
    oma = c(2, 2, 2, 2),  # Remove extern margins
    mgp = c(2.5, -5, 0))  # Plots outside

# Plots
plot(pred1, zlab="Differences in Mean tBW", xlab="Percentile Mean Temperature", ylab="Lag Weeks", main="")
mtext("A. Adjusted Differences in mean tBW (grams)\nPercentile mean temperature")  

plot(pred2, zlab="Differences in Mean tBW", xlab="Percentile Mean Temperature", ylab="Lag Weeks", main="")
mtext("B. Adjusted ORs-tLBW\nPercentile mean temperature")  

plot(pred3, zlab="Differences in Mean tBW", xlab="Percentile Minimum Temperature", ylab="Lag Weeks", main="")
mtext("C. Adjusted Differences in Mean tBW (grams)\nPercentile minimum temperature") 

plot(pred4, zlab="Differences in Mean tBW", xlab="Percentile Minimum Temperature", ylab="Lag Weeks", main="")
mtext("D. Adjusted ORs-tLBW\nPercentile minimum temperature")  

plot(pred5, zlab="Differences in Mean tBW", xlab="Percentile Maximum Temperature", ylab="Lag Weeks", main="")
mtext("E. Adjusted Differences in Mean tBW (grams)\nPercentile maximum temperature")  

plot(pred6, zlab="Differences in Mean tBW", xlab="Percentile Maximum Temperature", ylab="Lag Weeks", main="")
mtext("F. Adjusted ORs-tLBW\nPercentile maximum temperature")  

# Close
dev.off()
par(old_par)

#### Plot effects ------

##### Mean ------

###### tBW -----

pred_df1 <- data.frame(
  Week = 1:37,
  Cold_Fit = pred1$matfit[1,],
  Cold_Lower = pred1$matlow[1,],
  Cold_Upper = pred1$mathigh[1,],
  Heat_Fit = pred1$matfit[9,],
  Heat_Lower = pred1$matlow[9,],
  Heat_Upper = pred1$mathigh[9,]
  ) 


pred_long <- pred_df1 |> 
  pivot_longer(cols = c(Cold_Fit, Cold_Lower, Cold_Upper, Heat_Fit, Heat_Lower, Heat_Upper),
               names_to = c("Condition", "Metric"),
               names_sep = "_",
               values_to = "Value") |> 
  pivot_wider(names_from = Metric, values_from = Value) |> 
  # mutate(color = ifelse((Lower < 0 & Upper < 0), "blue", 
  #                       ifelse((Lower > 0 & Upper > 0), "red", 
  #                              "black"))) |> 
  mutate(Condition=factor(Condition, 
                          levels = c("Cold", "Heat"), 
                          labels = c("Adjusted Mean, Cold (<10th) vs reference (41st - 50th)",
                                     "Adjusted Mean, Heat (>90th) vs reference (41st - 50th)")))
  
g1 <- pred_long |> 
  filter(Condition=="Adjusted Mean, Cold (<10th) vs reference (41st - 50th)") |> 
  ggplot(aes(x = Week, y = Fit)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(5,35,5)) +
  scale_y_continuous(limits = c(-15, 15)) +
  labs(title = "Adjusted Mean, Cold (<10th) vs reference (41st - 50th)",
       x = "Week of gestational age", 
       y = "Differences in tBW and 95% CI (grams)", 
       tag = "A.") +
  #facet_wrap(~ Condition, scales="free") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=12, hjust = 0), 
        strip.placement = "outside")

g1


g2 <- pred_long |> 
  filter(Condition=="Adjusted Mean, Heat (>90th) vs reference (41st - 50th)") |> 
  ggplot(aes(x = Week, y = Fit)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(5,35,5)) +
  scale_y_continuous(limits = c(-15, 15)) +
  labs(title = "Adjusted Mean, Heat (>90th) vs reference (41st - 50th)",
       x = "Week of gestational age", 
       y = "Differences in tBW and 95% CI (grams)", 
       tag = "B.") +
  #facet_wrap(~ Condition, scales="free") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=12, hjust = 0), 
        strip.placement = "outside")

g2


g1 | g2

###### tLBW -----

pred_df2 <- data.frame(
  Week = 1:37,
  Cold_Fit = pred2$matRRfit[1,],
  Cold_Lower = pred2$matRRlow[1,],
  Cold_Upper = pred2$matRRhigh[1,],
  Heat_Fit = pred2$matRRfit[9,],
  Heat_Lower = pred2$matRRlow[9,],
  Heat_Upper = pred2$matRRhigh[9,]
) 


pred_long <- pred_df2 |> 
  pivot_longer(cols = c(Cold_Fit, Cold_Lower, Cold_Upper, Heat_Fit, Heat_Lower, Heat_Upper),
               names_to = c("Condition", "Metric"),
               names_sep = "_",
               values_to = "Value") %>%
  pivot_wider(names_from = Metric, values_from = Value) |> 
  mutate(Condition=factor(Condition, 
                          levels = c("Cold", "Heat"), 
                          labels = c("Adjusted Mean, Cold (<10th) vs reference (41st - 50th)",
                                     "Adjusted Mean, Heat (>90th) vs reference (41st - 50th)")))

g1b <- pred_long |> 
  filter(Condition=="Adjusted Mean, Cold (<10th) vs reference (41st - 50th)") |> 
  ggplot(aes(x = Week, y = Fit)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(5,35,5)) +
  scale_y_continuous(limits = c(0.8, 1.2)) +
  labs(title = "Adjusted Mean, Cold (<10th) vs reference (41st - 50th)",
       x = "Week of gestational age", 
       y = "tLBW (ORs and 95% CI)", 
       tag = "A.") +
  #facet_wrap(~ Condition, scales="free") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=12, hjust = 0), 
        strip.placement = "outside")

g1b


g2b <- pred_long |> 
  filter(Condition=="Adjusted Mean, Heat (>90th) vs reference (41st - 50th)") |> 
  ggplot(aes(x = Week, y = Fit)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(5,35,5)) +
  scale_y_continuous(limits = c(0.8, 1.2)) +
  labs(title = "Adjusted Mean, Heat (>90th) vs reference (41st - 50th)",
       x = "Week of gestational age", 
       y = "tLBW (ORs and 95% CI)", 
       tag = "B.") +
  #facet_wrap(~ Condition, scales="free") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=12, hjust = 0), 
        strip.placement = "outside")

g2b


g1b | g2b


pred_df1 |> 
  mutate(Trimester=c(rep("T1", 12), rep("T2", 12), rep("T3", 13))) |> 
  relocate(Trimester) |> 
  mutate(Week=paste("Week", Week)) |> 
  mutate_if(is.numeric, round, 2) |> 
  mutate("Cold (<=10th) adjusted"=paste0(Cold_Fit, " ", "(", Cold_Lower, "; ", Cold_Upper, ")")) |> 
  mutate("Heat (>90th) adjusted"=paste0(Heat_Fit, " ", "(", Heat_Lower, "; ", Heat_Upper, ")")) |> 
  select(c(1,2,9,10)) |>
  left_join(
    pred_df2 |> 
      mutate(Trimester=c(rep("T1", 12), rep("T2", 12), rep("T3", 13))) |> 
      relocate(Trimester) |> 
      mutate(Week=paste("Week", Week)) |> 
      mutate_if(is.numeric, round, 2) |> 
      mutate("Cold (<=10th) adjusted"=paste0(Cold_Fit, " ", "(", Cold_Lower, "; ", Cold_Upper, ")")) |> 
      mutate("Heat (>90th) adjusted"=paste0(Heat_Fit, " ", "(", Heat_Lower, "; ", Heat_Upper, ")")) |> 
      select(c(1,2,9,10)),
    by=c("Trimester", "Week"),
    suffix =  c("1", "2")
  ) |> 
  mutate_if(is.character, ~str_replace_all(., ",", ".")) |> 
  write.xlsx(paste0("Output_analysis/", "dlnm/tab/", "DLNM_mean_tBW_tLBW", ".xlsx"))
  

##### Min ------

###### tBW -----


pred_df3 <- data.frame(
  Week = 1:37,
  Cold_Fit = pred3$matfit[1,],
  Cold_Lower = pred3$matlow[1,],
  Cold_Upper = pred3$mathigh[1,],
  Heat_Fit = pred3$matfit[9,],
  Heat_Lower = pred3$matlow[9,],
  Heat_Upper = pred3$mathigh[9,]
) 


pred_long <- pred_df3 |> 
  pivot_longer(cols = c(Cold_Fit, Cold_Lower, Cold_Upper, Heat_Fit, Heat_Lower, Heat_Upper),
               names_to = c("Condition", "Metric"),
               names_sep = "_",
               values_to = "Value") %>%
  pivot_wider(names_from = Metric, values_from = Value) |> 
  mutate(Condition=factor(Condition, 
                          levels = c("Cold", "Heat"), 
                          labels = c("Adjusted Minimum, Cold (<10th) vs reference (41st - 50th)",
                                     "Adjusted Minimum, Heat (>90th) vs reference (41st - 50th)")))

g3 <- pred_long |> 
  filter(Condition=="Adjusted Minimum, Cold (<10th) vs reference (41st - 50th)") |> 
  ggplot(aes(x = Week, y = Fit)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(5,35,5)) +
  scale_y_continuous(limits = c(-15, 15)) +
  labs(title = "Adjusted Minimum, Cold (<10th) vs reference (41st - 50th)",
       x = "Week of gestational age", 
       y = "Differences in tBW and 95% CI (grams)", 
       tag = "A.") +
  #facet_wrap(~ Condition, scales="free") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=12, hjust = 0), 
        strip.placement = "outside")

g3


g4 <- pred_long |> 
  filter(Condition=="Adjusted Minimum, Heat (>90th) vs reference (41st - 50th)") |> 
  ggplot(aes(x = Week, y = Fit)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(5,35,5)) +
  scale_y_continuous(limits = c(-15, 15)) +
  labs(title = "Adjusted Minimum, Heat (>90th) vs reference (41st - 50th)",
       x = "Week of gestational age", 
       y = "Differences in tBW and 95% CI (grams)", 
       tag = "B.") +
  #facet_wrap(~ Condition, scales="free") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=12, hjust = 0), 
        strip.placement = "outside")

g4


g3 | g4

###### tLBW -----

pred_df4 <- data.frame(
  Week = 1:37,
  Cold_Fit = pred4$matRRfit[1,],
  Cold_Lower = pred4$matRRlow[1,],
  Cold_Upper = pred4$matRRhigh[1,],
  Heat_Fit = pred4$matRRfit[9,],
  Heat_Lower = pred4$matRRlow[9,],
  Heat_Upper = pred4$matRRhigh[9,]
) 


pred_long <- pred_df4 |> 
  pivot_longer(cols = c(Cold_Fit, Cold_Lower, Cold_Upper, Heat_Fit, Heat_Lower, Heat_Upper),
               names_to = c("Condition", "Metric"),
               names_sep = "_",
               values_to = "Value") %>%
  pivot_wider(names_from = Metric, values_from = Value) |> 
  mutate(Condition=factor(Condition, 
                          levels = c("Cold", "Heat"), 
                          labels = c("Adjusted Minimum, Cold (<10th) vs reference (41st - 50th)",
                                     "Adjusted Minimum, Heat (>90th) vs reference (41st - 50th)")))

g3b <- pred_long |> 
  filter(Condition=="Adjusted Minimum, Cold (<10th) vs reference (41st - 50th)") |> 
  ggplot(aes(x = Week, y = Fit)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(5,35,5)) +
  scale_y_continuous(limits = c(0.8, 1.2)) +
  labs(title = "Adjusted Minimum, Cold (<10th) vs reference (41st - 50th)",
       x = "Week of gestational age", 
       y = "tLBW (ORs and 95% CI)", 
       tag = "A.") +
  #facet_wrap(~ Condition, scales="free") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=12, hjust = 0), 
        strip.placement = "outside")

g3b


g4b <- pred_long |> 
  filter(Condition=="Adjusted Minimum, Heat (>90th) vs reference (41st - 50th)") |> 
  ggplot(aes(x = Week, y = Fit)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(5,35,5)) +
  scale_y_continuous(limits = c(0.8, 1.2)) +
  labs(title = "Adjusted Minimum, Heat (>90th) vs reference (41st - 50th)",
       x = "Week of gestational age", 
       y = "tLBW (ORs and 95% CI)", 
       tag = "B.") +
  #facet_wrap(~ Condition, scales="free") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=12, hjust = 0), 
        strip.placement = "outside")

g4b


g3b | g4b


pred_df3 |> 
  mutate(Trimester=c(rep("T1", 12), rep("T2", 12), rep("T3", 13))) |> 
  relocate(Trimester) |> 
  mutate(Week=paste("Week", Week)) |> 
  mutate_if(is.numeric, round, 2) |> 
  mutate("Cold (<=10th) adjusted"=paste0(Cold_Fit, " ", "(", Cold_Lower, "; ", Cold_Upper, ")")) |> 
  mutate("Heat (>90th) adjusted"=paste0(Heat_Fit, " ", "(", Heat_Lower, "; ", Heat_Upper, ")")) |> 
  select(c(1,2,9,10)) |>
  left_join(
    pred_df4 |> 
      mutate(Trimester=c(rep("T1", 12), rep("T2", 12), rep("T3", 13))) |> 
      relocate(Trimester) |> 
      mutate(Week=paste("Week", Week)) |> 
      mutate_if(is.numeric, round, 2) |> 
      mutate("Cold (<=10th) adjusted"=paste0(Cold_Fit, " ", "(", Cold_Lower, "; ", Cold_Upper, ")")) |> 
      mutate("Heat (>90th) adjusted"=paste0(Heat_Fit, " ", "(", Heat_Lower, "; ", Heat_Upper, ")")) |> 
      select(c(1,2,9,10)),
    by=c("Trimester", "Week"),
    suffix =  c("1", "2")
  ) |> 
  mutate_if(is.character, ~str_replace_all(., ",", ".")) |> 
  write.xlsx(paste0("Output_analysis/", "dlnm/tab/", "DLNM_min_tBW_tLBW", ".xlsx"))


##### Max ------

###### tBW -----


pred_df5 <- data.frame(
  Week = 1:37,
  Cold_Fit = pred5$matfit[1,],
  Cold_Lower = pred5$matlow[1,],
  Cold_Upper = pred5$mathigh[1,],
  Heat_Fit = pred5$matfit[9,],
  Heat_Lower = pred5$matlow[9,],
  Heat_Upper = pred5$mathigh[9,]
) 


pred_long <- pred_df5 |> 
  pivot_longer(cols = c(Cold_Fit, Cold_Lower, Cold_Upper, Heat_Fit, Heat_Lower, Heat_Upper),
               names_to = c("Condition", "Metric"),
               names_sep = "_",
               values_to = "Value") %>%
  pivot_wider(names_from = Metric, values_from = Value) |> 
  mutate(Condition=factor(Condition, 
                          levels = c("Cold", "Heat"), 
                          labels = c("Adjusted Maximum, Cold (<10th) vs reference (41st - 50th)",
                                     "Adjusted Maximum, Heat (>90th) vs reference (41st - 50th)")))

g5 <- pred_long |> 
  filter(Condition=="Adjusted Maximum, Cold (<10th) vs reference (41st - 50th)") |> 
  ggplot(aes(x = Week, y = Fit)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(5,35,5)) +
  scale_y_continuous(limits = c(-15, 15)) +
  labs(title = "Adjusted Maximum, Cold (<10th) vs reference (41st - 50th)",
       x = "Week of gestational age", 
       y = "Differences in tBW and 95% CI (grams)", 
       tag = "A.") +
  #facet_wrap(~ Condition, scales="free") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=12, hjust = 0), 
        strip.placement = "outside")

g5


g6 <- pred_long |> 
  filter(Condition=="Adjusted Maximum, Heat (>90th) vs reference (41st - 50th)") |> 
  ggplot(aes(x = Week, y = Fit)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(5,35,5)) +
  scale_y_continuous(limits = c(-15, 15)) +
  labs(title = "Adjusted Maximum, Heat (>90th) vs reference (41st - 50th)",
       x = "Week of gestational age", 
       y = "Differences in tBW and 95% CI (grams)", 
       tag = "B.") +
  #facet_wrap(~ Condition, scales="free") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=12, hjust = 0), 
        strip.placement = "outside")

g6


g5 | g6

###### tLBW -----

pred_df6 <- data.frame(
  Week = 1:37,
  Cold_Fit = pred6$matRRfit[1,],
  Cold_Lower = pred6$matRRlow[1,],
  Cold_Upper = pred6$matRRhigh[1,],
  Heat_Fit = pred6$matRRfit[9,],
  Heat_Lower = pred6$matRRlow[9,],
  Heat_Upper = pred6$matRRhigh[9,]
) 


pred_long <- pred_df6 |> 
  pivot_longer(cols = c(Cold_Fit, Cold_Lower, Cold_Upper, Heat_Fit, Heat_Lower, Heat_Upper),
               names_to = c("Condition", "Metric"),
               names_sep = "_",
               values_to = "Value") %>%
  pivot_wider(names_from = Metric, values_from = Value) |> 
  mutate(Condition=factor(Condition, 
                          levels = c("Cold", "Heat"), 
                          labels = c("Adjusted Maximum, Cold (<10th) vs reference (41st - 50th)",
                                     "Adjusted Maximum, Heat (>90th) vs reference (41st - 50th)")))

g5b <- pred_long |> 
  filter(Condition=="Adjusted Maximum, Cold (<10th) vs reference (41st - 50th)") |> 
  ggplot(aes(x = Week, y = Fit)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(5,35,5)) +
  scale_y_continuous(limits = c(0.8, 1.2)) +
  labs(title = "Adjusted Maximum, Cold (<10th) vs reference (41st - 50th)",
       x = "Week of gestational age", 
       y = "tLBW (ORs and 95% CI)", 
       tag = "A.") +
  #facet_wrap(~ Condition, scales="free") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=12, hjust = 0), 
        strip.placement = "outside")

g5b


g6b <- pred_long |> 
  filter(Condition=="Adjusted Maximum, Heat (>90th) vs reference (41st - 50th)") |> 
  ggplot(aes(x = Week, y = Fit)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(5,35,5)) +
  scale_y_continuous(limits = c(0.8, 1.2)) +
  labs(title = "Adjusted Maximum, Heat (>90th) vs reference (41st - 50th)",
       x = "Week of gestational age", 
       y = "tLBW (ORs and 95% CI)", 
       tag = "B.") +
  #facet_wrap(~ Condition, scales="free") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=12, hjust = 0), 
        strip.placement = "outside")

g6b


g5b | g6b


pred_df5 |> 
  mutate(Trimester=c(rep("T1", 12), rep("T2", 12), rep("T3", 13))) |> 
  relocate(Trimester) |> 
  mutate(Week=paste("Week", Week)) |> 
  mutate_if(is.numeric, round, 2) |> 
  mutate("Cold (<=10th) adjusted"=paste0(Cold_Fit, " ", "(", Cold_Lower, "; ", Cold_Upper, ")")) |> 
  mutate("Heat (>90th) adjusted"=paste0(Heat_Fit, " ", "(", Heat_Lower, "; ", Heat_Upper, ")")) |> 
  select(c(1,2,9,10)) |>
  left_join(
    pred_df6 |> 
      mutate(Trimester=c(rep("T1", 12), rep("T2", 12), rep("T3", 13))) |> 
      relocate(Trimester) |> 
      mutate(Week=paste("Week", Week)) |> 
      mutate_if(is.numeric, round, 2) |> 
      mutate("Cold (<=10th) adjusted"=paste0(Cold_Fit, " ", "(", Cold_Lower, "; ", Cold_Upper, ")")) |> 
      mutate("Heat (>90th) adjusted"=paste0(Heat_Fit, " ", "(", Heat_Lower, "; ", Heat_Upper, ")")) |> 
      select(c(1,2,9,10)),
    by=c("Trimester", "Week"),
    suffix =  c("1", "2")
  ) |> 
  mutate_if(is.character, ~str_replace_all(., ",", ".")) |> 
  write.xlsx(paste0("Output_analysis/", "dlnm/tab/", "DLNM_max_tBW_tLBW", ".xlsx"))



(g1 | g2) / (g3 + labs(tag="C") | g6 + labs(tag="D")) 

ggsave(filename = paste0("Output_analysis/", "dlnm/fig/", "DLNM_tBW", ".png"),
       res = 300,
       width = 22,
       height = 16,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)

(g1b | g2b) / (g3b + labs(tag="C") | g6b + labs(tag="D")) 

ggsave(filename = paste0("Output_analysis/", "dlnm/fig/", "DLNM_tLBW", ".png"),
       res = 300,
       width = 22,
       height = 16,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)

