# 10 2Stage ---

rm(list=(ls()))
options(scipen=999)
source("Code/0.2 Settings.R")

# Optimal use power computation 
options(future.globals.maxSize = 3000 * 1024^2)  
plan(multisession, workers = detectCores() - 4) # Parallelization

# Paths
data_path <- "Data/Output/"

## Prepare data  ----

data_temp <- "births_2011_2020_weeks_temp_analysis.RData"
load(paste0(data_path, data_temp))

time <- c("id", "week_gest_num")
vd <- c("tbw", "ltbw")
vc <- c("sex", "age_group_mom", "educ_group_mom", "job_group_mom", 
        "age_group_dad", "educ_group_dad", "job_group_dad",  "clim_zone")
trend <- c("year_week1", "month_week1")
clim_zones <- c("Desert", "Temperate dry, hot summer", "Temperate dry, warm summer", "Temperate, no dry season", "Cold steppe")

## Mean ----

vi <- c("temp_mean") 

data <- births_weeks_temp |>
  filter(week_gest_num <= 37) |>
  select(all_of(c(time, vd, vi, vc, trend))) |>
  group_by(clim_zone, week_gest_num) |>
  mutate(temp_mean_percentile_wcz = ntile(temp_mean, 100)) |> 
  ungroup() |> 
  select(-temp_mean) |>
  pivot_wider(names_from = "week_gest_num", 
              values_from = "temp_mean_percentile_wcz",
              names_prefix = "ptem_gw") 

datalist <- lapply(clim_zones, function(region) data[data$clim_zone==region,]) 
names(datalist) <- clim_zones

gc()

### First Stage: climatic zone estimation  ----

coef <- matrix(NA,length(datalist), 9,
               dimnames=list(clim_zones, paste0("b",seq(9))))

S <- vector("list", length(datalist))

names(S) <- clim_zones

for(i in seq_along(clim_zones)) {
  
  cat(i,"", clim_zones[[i]], "")
  
  df <- datalist[[i]]
  # Exposition matriz
  mat <- df |> 
    dplyr::select(starts_with("ptem_gw")) |> 
    as.matrix()
  
  lagknots <- equalknots(x = c(1, 37), nk = 2, fun = "ns")
  
  # Crossbasis 
  cb <- crossbasis(mat, 
                   lag = c(1, 37),
                   argvar = list(fun = "strata", breaks = seq(10, 90, by = 10), ref = 5),
                   arglag = list(fun = "ns", knots = lagknots))
  
  # GAM Model
  m <- gam(tbw ~ cb + sex + 
             age_group_mom + educ_group_mom + job_group_mom +
             age_group_dad + educ_group_dad + job_group_dad +
             s(year_week1) + s(month_week1),
           data = df, 
           na.action = na.exclude,
           family = gaussian())
  
  # Crossreduce (cumulative effect)
  cr <- crossreduce(cb, m, cen = 50)

  # Overtall cumulative summary
  coef[i, ] <- coef(cr)
  S[[i]] <- vcov(cr)
}


### Second Stage: Meta  ----

# Acumulative
model0 <- mixmeta(coef ~ 1, S, data = data, method = "reml", bscov = "unstr", random = ~ 1 | clim_zones)
blups0 <- blup(model0, vcov=TRUE)

### Plots with analysis  ----

xvar <-  seq(5,95, by=10)
bvar <- do.call("onebasis",c(list(x=xvar), attr(cb,"argvar")))
xlag <- 1:37
blag <- do.call("onebasis",c(list(x=xlag),attr(cb,"arglag")))
at = seq(5,95, by=10)
at

cz_f0 <- lapply(seq(nrow(coef)),function(i) crosspred(bvar, coef=coef[i,], vcov=S[[i]], at=at, cen=50))
cz_ov <- crosspred(bvar, coef=coef(model0), vcov=vcov(model0), at=at, cen=50)
cz_f0_blups <- lapply(seq(nrow(coef)),function(i) crosspred(bvar, coef=blups0[[i]][["blup"]], vcov=blups0[[i]][["vcov"]], at=at, cen=50))


overall <- data.frame(Zone="Pooled",
                      fit=cz_ov$matfit,
                      low=cz_ov$matlow,
                      high=cz_ov$mathigh, 
                      percentile=at) |> 
  rename(fit=lag0, 
         low=lag0.1, 
         high=lag0.2) 

df_cz <- data.frame()

for (i in seq_along(clim_zones)) {
  temp_df <- data.frame(
    Zone = rep(clim_zones[i], length(cz_f0_blups[[i]]$matfit)),
    matfit = cz_f0_blups[[i]]$matfit,
    matlow = cz_f0_blups[[i]]$matlow,
    mathigh = cz_f0_blups[[i]]$mathigh,
    percentile=at
  )
  # Concatenar al data frame final
  df_cz <- rbind(df_cz, temp_df)
}

df_cz <- df_cz |> 
  rename(fit=lag0, 
         low=lag0.1, 
         high=lag0.2) 

df_cz <- df_cz |> bind_rows(overall)
df_cz <- df_cz |> filter(percentile %in% c(5,95))
rownames(df_cz) <- NULL
df_cz <- df_cz |> 
  mutate(Zone=factor(Zone, levels=c("Pooled", rev(clim_zones)))) |> 
  mutate(label=paste0(Zone, "\n ", round(fit,1),
                      " (", round(low,1), ", ",
                      round(high,1), ")")) |> 
  mutate(order=c(10:1, 11,12)) 
  

g1 <- df_cz |> 
  filter(percentile==5) |> 
  ggplot(aes(x = fct_reorder(factor(label), order), #label,  # fct_reorder(factor(label)
                  y = fit)) +
  geom_point() +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.2) +
  coord_flip() +  
  scale_y_continuous(limits = c(-600,600), n.breaks = 7) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 5.5, linewidth = 0.5, color = "gray30", linetype = "solid") +
  geom_rect(aes(xmin = 5.5, xmax = 6.6, ymin = -Inf, ymax = Inf), 
            fill = "gray95", alpha = 0.1) +
  labs(x = NULL,
       y = "Differences in tBW and 95% CI (grams)", 
       title="Adjusted Mean",
       subtitle = "Cold (<10th) vs reference (41st - 50th)") +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.background = element_rect(color = NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12, hjust=0.5),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white"), 
        strip.text.x = element_text(size = 10, hjust = 0))


g2 <- df_cz |> 
  filter(percentile==95) |> 
  ggplot(aes(x = fct_reorder(factor(label), order), #label,  # fct_reorder(factor(label)
             y = fit)) +
  geom_point() +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.2) +
  coord_flip() +  
  scale_y_continuous(limits = c(-600,600), n.breaks = 7) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 5.5, linewidth = 0.5, color = "gray30", linetype = "solid") +
  geom_rect(aes(xmin = 5.5, xmax = 6.6, ymin = -Inf, ymax = Inf), 
            fill = "gray95", alpha = 0.1) +
  labs(x = NULL,
       y = "Differences in tBW and 95% CI (grams)", 
       title="Adjusted Mean",
       subtitle = "Heat (>90th) vs reference (41st - 50th)") +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.background = element_rect(color = NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12, hjust=0.5),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white"), 
        strip.text.x = element_text(size = 10, hjust = 0))

g1 + labs(tag="A.") | g2 + labs(tag="B.") 


## Min ----

vi <- c("temp_min") 

data <- births_weeks_temp |>
  filter(week_gest_num <= 37) |>
  select(all_of(c(time, vd, vi, vc, trend))) |>
  group_by(clim_zone, week_gest_num) |>
  mutate(temp_min_percentile_wcz = ntile(temp_min, 100)) |> 
  ungroup() |> 
  select(-temp_min) |>
  pivot_wider(names_from = "week_gest_num", 
              values_from = "temp_min_percentile_wcz",
              names_prefix = "ptem_gw") 

datalist <- lapply(clim_zones, function(region) data[data$clim_zone==region,]) 
names(datalist) <- clim_zones

### First Stage: climatic zone estimation  ----

coef <- matrix(NA,length(datalist), 9,
               dimnames=list(clim_zones, paste0("b",seq(9))))

S <- vector("list", length(datalist))

names(S) <- clim_zones

for(i in seq_along(clim_zones)) {
  
  cat(i,"", clim_zones[[i]], "")
  
  df <- datalist[[i]]
  # Exposition matriz
  mat <- df |> 
    dplyr::select(starts_with("ptem_gw")) |> 
    as.matrix()
  
  lagknots <- equalknots(x = c(1, 37), nk = 2, fun = "ns")
  
  # Crossbasis 
  cb <- crossbasis(mat, 
                   lag = c(1, 37),
                   argvar = list(fun = "strata", breaks = seq(10, 90, by = 10), ref = 5),
                   arglag = list(fun = "ns", knots = lagknots))
  
  # GAM Model
  m <- gam(tbw ~ cb + sex + 
             age_group_mom + educ_group_mom + job_group_mom +
             age_group_dad + educ_group_dad + job_group_dad +
             s(year_week1) + s(month_week1),
           data = df, 
           na.action = na.exclude,
           family = gaussian())
  
  # Crossreduce (cumulative effect)
  cr <- crossreduce(cb, m, cen = 50)
  
  # Overtall cumulative summary
  coef[i, ] <- coef(cr)
  S[[i]] <- vcov(cr)
}


### Second Stage: Meta  ----

# Acumulative
model1 <- mixmeta(coef ~ 1, S, data = data, method = "reml", bscov = "unstr", random = ~ 1 | clim_zones)
blups0 <- blup(model1, vcov=TRUE)

### Plots with analysis  ----
xvar <-  seq(5,95, by=10)
bvar <- do.call("onebasis",c(list(x=xvar), attr(cb,"argvar")))
xlag <- 1:37
blag <- do.call("onebasis",c(list(x=xlag),attr(cb,"arglag")))
at = seq(5,95, by=10)
at

cz_f0 <- lapply(seq(nrow(coef)),function(i) crosspred(bvar, coef=coef[i,], vcov=S[[i]], at=at, cen=50))
cz_ov <- crosspred(bvar, coef=coef(model1), vcov=vcov(model1), at=at, cen=50)
cz_f0_blups <- lapply(seq(nrow(coef)),function(i) crosspred(bvar, coef=blups0[[i]][["blup"]], vcov=blups0[[i]][["vcov"]], at=at, cen=50))


overall <- data.frame(Zone="Pooled",
                      fit=cz_ov$matfit,
                      low=cz_ov$matlow,
                      high=cz_ov$mathigh, 
                      percentile=at) |> 
  rename(fit=lag0, 
         low=lag0.1, 
         high=lag0.2) 

df_cz_min <- data.frame()

for (i in seq_along(clim_zones)) {
  temp_df <- data.frame(
    Zone = rep(clim_zones[i], length(cz_f0_blups[[i]]$matfit)),
    matfit = cz_f0_blups[[i]]$matfit,
    matlow = cz_f0_blups[[i]]$matlow,
    mathigh = cz_f0_blups[[i]]$mathigh,
    percentile=at
  )
  # Concatenar al data frame final
  df_cz_min <- rbind(df_cz_min, temp_df)
}

df_cz_min <- df_cz_min |> 
  rename(fit=lag0, 
         low=lag0.1, 
         high=lag0.2) 

df_cz_min <- df_cz_min |> bind_rows(overall)
df_cz_min <- df_cz_min |> filter(percentile %in% c(5,95))
rownames(df_cz_min) <- NULL
df_cz_min <- df_cz_min |> 
  mutate(Zone=factor(Zone, levels=c("Pooled", rev(clim_zones)))) |> 
  mutate(label=paste0(Zone, "\n ", round(fit,1),
                      " (", round(low,1), ", ",
                      round(high,1), ")")) |> 
  mutate(order=c(10:1, 11,12)) 

g3 <- df_cz_min |> 
  filter(percentile==5) |> 
  ggplot(aes(x = fct_reorder(factor(label), order), #label,  # fct_reorder(factor(label)
             y = fit)) +
  geom_point() +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.2) +
  coord_flip() +  
  scale_y_continuous(limits = c(-450,450), n.breaks = 7) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 5.5, linewidth = 0.5, color = "gray30", linetype = "solid") +
  geom_rect(aes(xmin = 5.5, xmax = 6.6, ymin = -Inf, ymax = Inf), 
            fill = "gray95", alpha = 0.1) +
  labs(x = NULL,
       y = "Differences in tBW and 95% CI (grams)", 
       title="Adjusted Minimum",
       subtitle="Cold (<10th) vs reference (41st - 50th)") +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.background = element_rect(color = NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12, hjust=0.5),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white"), 
        strip.text.x = element_text(size = 10, hjust = 0))

(g1 + labs(tag="A.") | g2 + labs(tag="B.")) / (g3 + labs(tag="C."))

## Max ----

vi <- c("temp_max") 

data <- births_weeks_temp |>
  filter(week_gest_num <= 37) |>
  select(all_of(c(time, vd, vi, vc, trend))) |>
  group_by(clim_zone, week_gest_num) |>
  mutate(temp_max_percentile_wcz = ntile(temp_max, 100)) |> 
  ungroup() |> 
  select(-temp_max) |>
  pivot_wider(names_from = "week_gest_num", 
              values_from = "temp_max_percentile_wcz",
              names_prefix = "ptem_gw") 

datalist <- lapply(clim_zones, function(region) data[data$clim_zone==region,]) 
names(datalist) <- clim_zones

### First Stage: climatic zone estimation  ----

coef <- matrix(NA,length(datalist), 9,
               dimnames=list(clim_zones, paste0("b",seq(9))))

S <- vector("list", length(datalist))

names(S) <- clim_zones

for(i in seq_along(clim_zones)) {
  
  cat(i,"", clim_zones[[i]])
  
  df <- datalist[[i]]
  # Exposition matriz
  mat <- df |> 
    dplyr::select(starts_with("ptem_gw")) |> 
    as.matrix()
  
  lagknots <- equalknots(x = c(1, 37), nk = 2, fun = "ns")
  
  # Crossbasis 
  cb <- crossbasis(mat, 
                   lag = c(1, 37),
                   argvar = list(fun = "strata", breaks = seq(10, 90, by = 10), ref = 5),
                   arglag = list(fun = "ns", knots = lagknots))
  
  # GAM Model
  m <- gam(tbw ~ cb + sex + 
             age_group_mom + educ_group_mom + job_group_mom +
             age_group_dad + educ_group_dad + job_group_dad +
             s(year_week1) + s(month_week1),
           data = df, 
           na.action = na.exclude,
           family = gaussian())
  
  # Crossreduce (cumulative effect)
  cr <- crossreduce(cb, m, cen = 50)
  
  # Overtall cumulative summary
  coef[i, ] <- coef(cr)
  S[[i]] <- vcov(cr)
}

### Second Stage: Meta  ----

# Acumulative
model2 <- mixmeta(coef ~ 1, S, data = data, method = "reml", bscov = "unstr", random = ~ 1 | clim_zones)
blups0 <- blup(model2, vcov=TRUE)

### Plots with analysis  ----
xvar <-  seq(5,95, by=10)
bvar <- do.call("onebasis",c(list(x=xvar), attr(cb,"argvar")))
xlag <- 1:37
blag <- do.call("onebasis",c(list(x=xlag),attr(cb,"arglag")))
at = seq(5,95, by=10)
at

cz_f0 <- lapply(seq(nrow(coef)),function(i) crosspred(bvar, coef=coef[i,], vcov=S[[i]], at=at, cen=50))
cz_ov <- crosspred(bvar, coef=coef(model1), vcov=vcov(model1), at=at, cen=50)
cz_f0_blups <- lapply(seq(nrow(coef)),function(i) crosspred(bvar, coef=blups0[[i]][["blup"]], vcov=blups0[[i]][["vcov"]], at=at, cen=50))


overall <- data.frame(Zone="Pooled",
                      fit=cz_ov$matfit,
                      low=cz_ov$matlow,
                      high=cz_ov$mathigh, 
                      percentile=at) |> 
  rename(fit=lag0, 
         low=lag0.1, 
         high=lag0.2) 

df_cz_max <- data.frame()

for (i in seq_along(clim_zones)) {
  temp_df <- data.frame(
    Zone = rep(clim_zones[i], length(cz_f0_blups[[i]]$matfit)),
    matfit = cz_f0_blups[[i]]$matfit,
    matlow = cz_f0_blups[[i]]$matlow,
    mathigh = cz_f0_blups[[i]]$mathigh,
    percentile=at
  )
  # Concatenar al data frame final
  df_cz_max <- rbind(df_cz_max, temp_df)
}

df_cz_max <- df_cz_max |> 
  rename(fit=lag0, 
         low=lag0.1, 
         high=lag0.2) 

df_cz_max <- df_cz_max |> bind_rows(overall)
df_cz_max <- df_cz_max |> filter(percentile %in% c(5,95))
rownames(df_cz_max) <- NULL
df_cz_max <- df_cz_max |> 
  mutate(Zone=factor(Zone, levels=c("Pooled", rev(clim_zones)))) |> 
  mutate(label=paste0(Zone, "\n ", round(fit,1),
                      " (", round(low,1), ", ",
                      round(high,1), ")")) |> 
  mutate(order=c(10:1, 11,12)) 

g4 <- df_cz_max |> 
  filter(percentile==5) |> 
  ggplot(aes(x = fct_reorder(factor(label), order), #label,  # fct_reorder(factor(label)
             y = fit)) +
  geom_point() +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.2) +
  coord_flip() +  
  scale_y_continuous(limits = c(-450,450), n.breaks = 7) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 5.5, linewidth = 0.5, color = "gray30", linetype = "solid") +
  geom_rect(aes(xmin = 5.5, xmax = 6.6, ymin = -Inf, ymax = Inf), 
            fill = "gray95", alpha = 0.1) +
  labs(x = NULL,
       y = "Differences in tBW and 95% CI (grams)", 
       title="Adjusted Maximum",
       subtitle = "Heat (>90th) vs reference (41st - 50th)") +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.background = element_rect(color = NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12, hjust=0.5),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white"), 
        strip.text.x = element_text(size = 10, hjust = 0))

### SAVING PLOTS ----

(g1 + labs(tag="A.") | g2 + labs(tag="B.")) / (g3 + labs(tag="C.") | g4 + labs(tag="D."))

ggsave(filename = paste0("Output_analysis/", "2Stage/", "2Stage_tBW", ".png"),
       res = 300,
       width = 23,
       height = 20,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)

bind_rows(
  df_cz |> mutate(temp="Mean"), 
  df_cz_min |> mutate(temp="Min"),
  df_cz_max |> mutate(temp="Max")
) |> 
  write.xlsx(paste0("Output_analysis/", "2Stage/", "2Stage_tBW", ".xlsx"))





