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

## Prepare data DLNM TEMP  ----

data_temp <- "births_2011_2020_weeks_temp_analysis.RData"
load(paste0(data_path, data_temp))
glimpse(births_weeks_temp)

# Variables 
time <- c("id", "week_gest_num")
vd <- "tbw"
vi <- c("temp_mean") # "temp_mean_percentile_wcz"
vi2 <- c("temp_min") # "temp_mean_percentile_wcz"
vi3 <- c("temp_max") # "temp_mean_percentile_wcz"
# vi <- c("temp_mean", "temp_min", "temp_max", 
#         "temp_mean_percentile_wcz", "temp_min_percentile_wcz", "temp_max_percentile_wcz")
vc <- c("sex", "age_group_mom", "educ_group_mom", "job_group_mom", 
        "age_group_dad", "educ_group_dad", "job_group_dad", "sovi", "clim_zone")
trend <- c("year_week1", "month_week1")
        
# Select and transform to wide data 
bw_dlnm <- births_weeks_temp %>% 
  filter(week_gest_num<=37) %>% 
  select(all_of(c(time, vd, vi, vc, trend))) %>%
  group_by(clim_zone, week_gest_num) %>%
  mutate(temp_mean_percentile_wcz = factor(ntile(temp_mean, 100))) %>%
  ungroup()

#bw_dlnm$temp_mean_percentile_wcz <- relevel(bw_dlnm$temp_mean_percentile_wcz, ref = "41-50")

glimpse(bw_dlnm)

# Wide data 
bw_dlnm_w <-  bw_dlnm %>% 
  select(-temp_mean) %>% 
  pivot_wider(names_from = "week_gest_num", 
              values_from = "temp_mean_percentile_wcz",
              names_prefix = "ptem_gw") %>% 
  drop_na()

glimpse(bw_dlnm_w)

######################################################
### --- Step 1. Find optimal number of knots --- ### 
####################################################

# grid the knots
knots = c(2, 3, 4, 5, 6, 8, 10, 12)

AICs.bw = c()
BICs.bw = c()

for (i in 1:length(knots)) {
  
  lagknots <- equalknots(x = c(2, 36), # same as the one that we define earlier
                         nk = knots[i], fun = "ns")
  
  cb <- crossbasis(mat, 
                   lag = c(1, 37),
                   argvar = list(fun = "strata", breaks=seq(10,90,by=10), ref=5),
                   arglag = list(fun = "ns", knots = lagknots)) # the form of the curve across all the lags, that is, the lag constrain 
  
  mod <- gam(tbw ~ cb + sex + 
               age_group_mom + educ_group_mom + job_group_mom +
               age_group_dad + educ_group_dad + job_group_dad +
               s(year_week1) + s(month_week1) + sovi,  
             data = bw_dlnm_w, 
             na.action = na.exclude,
             family=gaussian())
  
  AICs.bw[i] = AIC(mod)
  BICs.bw[i] = BIC(mod)
}

AICs.bw
BICs.bw


##############################################################
### --- Step 2. Defining the crossbasis for our model --- ###
############################################################


# show the knots 
AICs.bw # We obtained the AIC for the different knots 
BICs.bw # We obtained the BIC for the different knots

# This return the minimum AIC (knots), this mean that we going to use nk = 2 in the lagknots 
knots[which.min(AICs.bw)] 
knots[which.min(BICs.bw)]


mat <- bw_dlnm_w %>% 
  dplyr::select(ptem_gw1:ptem_gw37) %>% 
  as.matrix()

lagknots <- equalknots(x = c(1, 37), nk = 2, fun = "ns")
test <- crossbasis(mat, 
                   lag = c(1, 37),
                   #argvar = list(fun = "lin"),
                   argvar = list(fun = "strata", breaks=seq(10,90,by=10),ref=5),
                   arglag = list(fun = "ns", knots = lagknots)) # the form of the curve across all the lags, that is, the lag constrain 

summary(test)

###############################################################
### --- Step 3. Defining the model with our crossbasis --- ###
#############################################################

m1 <- gam(tbw ~ test + sex + 
           age_group_mom + educ_group_mom + job_group_mom +
           age_group_dad + educ_group_dad + job_group_dad +
          s(year_week1) + s(month_week1) + sovi,  
          data = bw_dlnm_w, 
          na.action = na.exclude,
          family=gaussian())
          
summary(m1)

#####################################################################
### --- Step 4. Predict with the model created in the Step 3 --- ###
###################################################################


at = seq(5,95, by=10)
at

pred <- crosspred(test, m1, at=at)

min(test) # -37
max(test) # 111

par(mfrow=c(1,1))
plot(pred, zlab="Differences in Mean tBW", xlab="Percentile Temperature", ylab="Lag Weeks")
plot(pred, "contour", xlab="Temperature", key.title=title("Dif-Mean"), 
     plot.title=title("Contour plot",xlab="Percentile Temperature",ylab="Lag"))


pred$matfit
pred$allfit

cold.bw.adj<-round(cbind(pred$matfit[1,],pred$matlow[1,],pred$mathigh[1,]),4)
heat.bw.adj<-round(cbind(pred$matfit[10,],pred$matlow[10,],pred$mathigh[10,]),4)

BWcradj_cold_heat_lags<-cbind(cold.bw.adj,heat.bw.adj)
BWcradj_cold_heat_lags

# Cold
par(mfrow=c(1,2))

plot(1:37, pred$matfit[1,], pch=16, ylim=c(min(pred$matlow[1,]), max(pred$mathigh[1,])),
     ylab="Differences in tBW(95% CI)", xlab="Gestational week", main="Adjusted, Cold (<10th) vs reference (41st - 50th)")
segments(1:37, pred$matlow[1,], 1:37, pred$mathigh[1,])
abline(h=0, lty=3, col="red")

# Heat
plot(1:37, pred$matfit[10,], pch=16, ylim=c(min(pred$matlow[10,]), max(pred$mathigh[10,])), 
     ylab="Differences in tBW(95% CI)", xlab="Gestational week", main="Adjusted, Heat (>90th) vs reference (41st - 50th)")
segments(1:37, pred$matlow[10,], 1:37, pred$mathigh[10,])
abline(h=0, lty=3, col="red")

## MIN ------

# Select and transform to wide data 
bw_dlnm <- births_weeks_temp %>% 
  filter(week_gest_num<=37) %>% 
  select(all_of(c(time, vd, vi2, vc, trend))) %>%
  group_by(clim_zone, week_gest_num) %>%
  mutate(temp_min_percentile_wcz = factor(ntile(temp_min, 100))) %>%
  ungroup()

#bw_dlnm$temp_mean_percentile_wcz <- relevel(bw_dlnm$temp_mean_percentile_wcz, ref = "41-50")

glimpse(bw_dlnm)

# Wide data 
bw_dlnm_w <-  bw_dlnm %>% 
  select(-temp_min) %>% 
  pivot_wider(names_from = "week_gest_num", 
              values_from = "temp_min_percentile_wcz",
              names_prefix = "ptem_gw") %>% 
  drop_na()

glimpse(bw_dlnm_w)


##############################################################
### --- Step 2. Defining the crossbasis for our model --- ###
############################################################

mat <- bw_dlnm_w %>% 
  dplyr::select(ptem_gw1:ptem_gw37) %>% 
  as.matrix()

lagknots <- equalknots(x = c(1, 37), nk = 2, fun = "ns")
test <- crossbasis(mat, 
                   lag = c(1, 37),
                   #argvar = list(fun = "lin"),
                   argvar = list(fun = "strata",breaks=seq(10,90,by=10),ref=5),
                   arglag = list(fun = "ns", knots = lagknots)) # the form of the curve across all the lags, that is, the lag constrain 

summary(test)

###############################################################
### --- Step 3. Defining the model with our crossbasis --- ###
#############################################################

m1 <- gam(tbw ~ test + sex + 
            age_group_mom + educ_group_mom + job_group_mom +
            age_group_dad + educ_group_dad + job_group_dad +
            s(year_week1) + s(month_week1) + sovi,  
          data = bw_dlnm_w, 
          na.action = na.exclude,
          family=gaussian())

summary(m1)

#####################################################################
### --- Step 4. Predict with the model created in the Step 3 --- ###
###################################################################

at = seq(5,95, by=10)
at

pred <- crosspred(test, m1, at=at)

min(test) 
max(test) 

par(mfrow=c(1,1))
plot(pred, zlab="Differences in Mean tBW", xlab="Percentile Temperature (Min)", ylab="Lag Weeks")
plot(pred, "contour", xlab="Temperature", key.title=title("Dif-Mean"), 
     plot.title=title("Contour plot",xlab="Percentile Temperature (Min)",ylab="Lag"))


pred$matfit
pred$allfit

cold.bw.adj<-round(cbind(pred$matfit[1,],pred$matlow[1,],pred$mathigh[1,]),4)
heat.bw.adj<-round(cbind(pred$matfit[10,],pred$matlow[10,],pred$mathigh[10,]),4)

BWcradj_cold_heat_lags<-cbind(cold.bw.adj,heat.bw.adj)
BWcradj_cold_heat_lags

# Cold
par(mfrow=c(1,2))

plot(1:37, pred$matfit[1,], pch=16, ylim=c(min(pred$matlow[1,]), max(pred$mathigh[1,])),
     ylab="Differences in tBW(95% CI)", xlab="Gestational week", main="Adjusted, Cold (<10th) vs reference (41st - 50th)")
segments(1:37, pred$matlow[1,], 1:37, pred$mathigh[1,])
abline(h=0, lty=3, col="red")

# Heat
plot(1:37, pred$matfit[10,], pch=16, ylim=c(min(pred$matlow[10,]), max(pred$mathigh[10,])), 
     ylab="Differences in tBW(95% CI)", xlab="Gestational week", main="Adjusted, Heat (>90th) vs reference (41st - 50th)")
segments(1:37, pred$matlow[10,], 1:37, pred$mathigh[10,])
abline(h=0, lty=3, col="red")

## MAX ------

# Select and transform to wide data 
bw_dlnm <- births_weeks_temp %>% 
  filter(week_gest_num<=37) %>% 
  select(all_of(c(time, vd, vi3, vc, trend))) %>%
  group_by(clim_zone, week_gest_num) %>%
  mutate(temp_max_percentile_wcz = factor(ntile(temp_max, 100))) %>%
  ungroup()

#bw_dlnm$temp_mean_percentile_wcz <- relevel(bw_dlnm$temp_mean_percentile_wcz, ref = "41-50")

glimpse(bw_dlnm)

# Wide data 
bw_dlnm_w <-  bw_dlnm %>% 
  select(-temp_max) %>% 
  pivot_wider(names_from = "week_gest_num", 
              values_from = "temp_max_percentile_wcz",
              names_prefix = "ptem_gw") %>% 
  drop_na()

glimpse(bw_dlnm_w)


##############################################################
### --- Step 2. Defining the crossbasis for our model --- ###
############################################################

mat <- bw_dlnm_w %>% 
  dplyr::select(ptem_gw1:ptem_gw37) %>% 
  as.matrix()

lagknots <- equalknots(x = c(1, 37), nk = 2, fun = "ns")
test <- crossbasis(mat, 
                   lag = c(1, 37),
                   #argvar = list(fun = "lin"),
                   argvar = list(fun = "strata",breaks=seq(10,90,by=10),ref=5),
                   arglag = list(fun = "ns", knots = lagknots)) # the form of the curve across all the lags, that is, the lag constrain 

summary(test)

###############################################################
### --- Step 3. Defining the model with our crossbasis --- ###
#############################################################

m1 <- gam(tbw ~ test + sex + 
            age_group_mom + educ_group_mom + job_group_mom +
            age_group_dad + educ_group_dad + job_group_dad +
            s(year_week1) + s(month_week1) + sovi,  
          data = bw_dlnm_w, 
          na.action = na.exclude,
          family=gaussian())

summary(m1)

#####################################################################
### --- Step 4. Predict with the model created in the Step 3 --- ###
###################################################################

at = seq(5,95, by=10)
at

pred <- crosspred(test, m1, at=at)

min(test) 
max(test) 

par(mfrow=c(1,1))
plot(pred, zlab="Differences in Mean tBW", xlab="Percentile Temperature (Max)", ylab="Lag Weeks")
plot(pred, "contour", xlab="Temperature", key.title=title("Dif-Mean"), 
     plot.title=title("Contour plot",xlab="Percentile Temperature (Max)",ylab="Lag"))


pred$matfit
pred$allfit

cold.bw.adj<-round(cbind(pred$matfit[1,],pred$matlow[1,],pred$mathigh[1,]),4)
heat.bw.adj<-round(cbind(pred$matfit[10,],pred$matlow[10,],pred$mathigh[10,]),4)

BWcradj_cold_heat_lags<-cbind(cold.bw.adj,heat.bw.adj)
BWcradj_cold_heat_lags

# Cold
par(mfrow=c(1,2))

plot(1:37, pred$matfit[1,], pch=16, ylim=c(min(pred$matlow[1,]), max(pred$mathigh[1,])),
     ylab="Differences in tBW(95% CI)", xlab="Gestational week", main="Adjusted, Cold (<10th) vs reference (41st - 50th)")
segments(1:37, pred$matlow[1,], 1:37, pred$mathigh[1,])
abline(h=0, lty=3, col="red")

# Heat
plot(1:37, pred$matfit[10,], pch=16, ylim=c(min(pred$matlow[10,]), max(pred$mathigh[10,])), 
     ylab="Differences in tBW(95% CI)", xlab="Gestational week", main="Adjusted, Heat (>90th) vs reference (41st - 50th)")
segments(1:37, pred$matlow[10,], 1:37, pred$mathigh[10,])
abline(h=0, lty=3, col="red")

