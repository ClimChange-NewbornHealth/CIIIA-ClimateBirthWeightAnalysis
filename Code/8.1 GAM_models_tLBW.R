# 8.0 GAM Models ---
# 6 hours in execution 
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

#### WIDE data ----

inicio <- Sys.time()

# Wide data by trimester analysis
births_weeks_temp <- births_weeks_temp |> 
  select(id, clim_zone, ltbw, trim_gest, sex,  
         age_group_mom, educ_group_mom, job_group_mom,
         age_group_dad, educ_group_dad, job_group_dad, 
         year_week1, month_week1,
         temp_mean_gest, 
         temp_min_gest,
         temp_max_gest,
         temp_mean_percentile_wcz,
         temp_min_percentile_wcz,
         temp_max_percentile_wcz,
         temp_mean_percentile_tcz,
         temp_min_percentile_tcz,
         temp_max_percentile_tcz
  ) 

births_weeks_temp_wide <-  births_weeks_temp |> 
  select(!c(temp_mean_percentile_wcz,
            temp_min_percentile_wcz,
            temp_max_percentile_wcz)) |> 
  distinct() |> 
  pivot_wider(names_from = trim_gest,
              values_from = c(temp_mean_percentile_tcz, temp_min_percentile_tcz, temp_max_percentile_tcz),
              values_fn = list(
                temp_mean_percentile_tcz = ~ .x[length(.x)],
                temp_min_percentile_tcz = ~ .x[length(.x)],
                temp_max_percentile_tcz = ~ .x[length(.x)]
              )) 

births_weeks_temp_wide <- births_weeks_temp_wide |> 
  group_by(clim_zone) |>
  mutate(temp_mean_percentile100_cz = ntile(temp_mean_gest, 100)) |>
  mutate(temp_min_percentile100_cz = ntile(temp_min_gest, 100)) |>
  mutate(temp_max_percentile100_cz = ntile(temp_max_gest, 100)) |> 
  ungroup()
  

# Prepare factors 

births_weeks_temp$temp_mean_percentile_wcz <- relevel(births_weeks_temp$temp_mean_percentile_wcz, ref = "41-50")
births_weeks_temp$temp_min_percentile_wcz <- relevel(births_weeks_temp$temp_min_percentile_wcz, ref = "41-50")
births_weeks_temp$temp_max_percentile_wcz <- relevel(births_weeks_temp$temp_max_percentile_wcz, ref = "41-50")

births_weeks_temp_wide$temp_mean_percentile_tcz_T1 <- relevel(births_weeks_temp_wide$temp_mean_percentile_tcz_T1, ref = "41-50")
births_weeks_temp_wide$temp_mean_percentile_tcz_T2 <- relevel(births_weeks_temp_wide$temp_mean_percentile_tcz_T2, ref = "41-50")
births_weeks_temp_wide$temp_mean_percentile_tcz_T3 <- relevel(births_weeks_temp_wide$temp_mean_percentile_tcz_T3, ref = "41-50")

births_weeks_temp_wide$temp_min_percentile_tcz_T1 <- relevel(births_weeks_temp_wide$temp_min_percentile_tcz_T1, ref = "41-50")
births_weeks_temp_wide$temp_min_percentile_tcz_T2 <- relevel(births_weeks_temp_wide$temp_min_percentile_tcz_T2, ref = "41-50")
births_weeks_temp_wide$temp_min_percentile_tcz_T3 <- relevel(births_weeks_temp_wide$temp_min_percentile_tcz_T3, ref = "41-50")

births_weeks_temp_wide$temp_max_percentile_tcz_T1 <- relevel(births_weeks_temp_wide$temp_max_percentile_tcz_T1, ref = "41-50")
births_weeks_temp_wide$temp_max_percentile_tcz_T2 <- relevel(births_weeks_temp_wide$temp_max_percentile_tcz_T2, ref = "41-50")
births_weeks_temp_wide$temp_max_percentile_tcz_T3 <- relevel(births_weeks_temp_wide$temp_max_percentile_tcz_T3, ref = "41-50")

#########################################################/
## GAM Models tLBW (climate zone) ----
#########################################################/

###########################################################################/
#### Unadjusted GAM tLBW ~ MEAN ----
###########################################################################/

# Lista para almacenar gráficos
gam_plots <- list()

# Lista de zonas climáticas únicas
clim_zones <- c("Desert", "Temperate dry, hot summer", "Temperate dry, warm summer", "Temperate, no dry season", "Cold steppe")

# Bucle para ajustar el modelo y generar gráficos por zona climática
for (i in seq_along(clim_zones)) {
  
  # Filtrar los datos por la zona climática actual
  zone <- clim_zones[i]
  zone_data <- births_weeks_temp_wide[births_weeks_temp_wide$clim_zone == zone, ]
  
  # Ajustar el modelo GAM
  gam0 <- gam(ltbw ~ s(temp_mean_gest),
              data = zone_data, 
              family = binomial(link = "logit")) 
  
  # Crear datos para predicción
  pred_data <- data.frame(temp_mean_gest = seq(min(zone_data$temp_mean_gest), 
                                               max(zone_data$temp_mean_gest), length.out = 100))
  
  # Obtener predicciones y calcular intervalos de confianza
  pred_data$predicted_ltbw <- predict(gam0, newdata = pred_data, se.fit = TRUE)$fit
  pred_data$se <- predict(gam0, newdata = pred_data, se.fit = TRUE)$se.fit
  pred_data <- pred_data |> 
    mutate(
      predicted_prob = plogis(predicted_ltbw),
      lower_ci = plogis(predicted_ltbw - 1.96 * se),
      upper_ci = plogis(predicted_ltbw + 1.96 * se)
    )
  
  # Generar el gráfico
  tags <- LETTERS[1:length(clim_zones)]
  
  p <- ggplot(pred_data, aes(x = temp_mean_gest, y = predicted_prob)) +
    geom_line(color = "black", size = 1) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "lightblue", alpha = 0.5) +
    #scale_y_continuous(limits = c(3000, 3800)) +
    #scale_x_continuous(n.breaks = 5) +
    labs(title = paste(zone),
         x = "Entire pregnacy average of the daily mean temperature (°C)",
         y = "Predicted probabilities tLBW and 95% CI (grams)",
         tag = paste0(tags[i], ".")) +
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
          strip.text.x = element_text(size=10, hjust = 0))
  
  # Almacenar el gráfico en la lista
  gam_plots[[zone]] <- p
}

# Add combined plot 
gam0 <- gam(ltbw ~ s(temp_mean_percentile100_cz),
            data = births_weeks_temp_wide, 
            family = binomial(link = "logit")) 

combined_pred_data <- data.frame(temp_mean_percentile100_cz = seq(0, 100, length.out = 100))

combined_pred_data$predicted_ltbw <- predict(gam0, newdata = combined_pred_data, se.fit = TRUE)$fit
combined_pred_data$se <- predict(gam0, newdata = combined_pred_data, se.fit = TRUE)$se.fit
combined_pred_data <- combined_pred_data |> 
  mutate(
    predicted_prob = plogis(predicted_ltbw),
    lower_ci = plogis(predicted_ltbw - 1.96 * se),
    upper_ci = plogis(predicted_ltbw + 1.96 * se)
    )

gam_plots[["Combined"]] <- ggplot(combined_pred_data, aes(x = temp_mean_percentile100_cz, y = predicted_prob)) +
  geom_line(color = "black", size = 1) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "lightblue", alpha = 0.5) +
  #scale_y_continuous(limits = c(0,1)) +
  labs(title = "Combined, all climatic zones percentiles",
       x = "Percentiles of entire pregnancy average of the daily mean temperature",
       y = "Predicted probabilities tLBW and 95% CI (grams)",
       tag = "F.") +
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
        strip.text.x = element_text(size=10, hjust = 0))

# See all plots
do.call(ggarrange, c(gam_plots, list(nrow = 2, ncol = 3)))

ggsave(filename = paste0(data_out, "fig/", "Unadjusted_GAM_models_ltbw_mean", ".png"),
       res = 300,
       width = 32,
       height = 16,
       units = 'cm',
       scaling = 0.75,
       device = ragg::agg_png)


###########################################################################/
#### Unadjusted GAM tLBW ~ MIN ----
###########################################################################/

# Lista para almacenar gráficos
gam_plots <- list()

# Lista de zonas climáticas únicas
clim_zones <- c("Desert", "Temperate dry, hot summer", "Temperate dry, warm summer", "Temperate, no dry season", "Cold steppe")

# Bucle para ajustar el modelo y generar gráficos por zona climática
for (i in seq_along(clim_zones)) {
  
  # Filtrar los datos por la zona climática actual
  zone <- clim_zones[i]
  zone_data <- births_weeks_temp_wide[births_weeks_temp_wide$clim_zone == zone, ]
  
  # Ajustar el modelo GAM
  gam0 <- gam(ltbw ~ s(temp_min_gest),
              data = zone_data, 
              family = binomial(link = "logit")) 
  
  # Crear datos para predicción
  pred_data <- data.frame(temp_min_gest = seq(min(zone_data$temp_min_gest), 
                                               max(zone_data$temp_min_gest), length.out = 100))
  
  # Obtener predicciones y calcular intervalos de confianza
  pred_data$predicted_ltbw <- predict(gam0, newdata = pred_data, se.fit = TRUE)$fit
  pred_data$se <- predict(gam0, newdata = pred_data, se.fit = TRUE)$se.fit
  pred_data <- pred_data |> 
    mutate(
      predicted_prob = plogis(predicted_ltbw),
      lower_ci = plogis(predicted_ltbw - 1.96 * se),
      upper_ci = plogis(predicted_ltbw + 1.96 * se)
    )
  
  # Generar el gráfico
  tags <- LETTERS[1:length(clim_zones)]
  
  p <- ggplot(pred_data, aes(x = temp_min_gest, y = predicted_prob)) +
    geom_line(color = "black", size = 1) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "lightblue", alpha = 0.5) +
    #scale_y_continuous(limits = c(3000, 3800)) +
    #scale_x_continuous(n.breaks = 5) +
    labs(title = paste(zone),
         x = "Entire pregnacy average of the daily min temperature (°C)",
         y = "Predicted probabilities tLBW and 95% CI (grams)",
         tag = paste0(tags[i], ".")) +
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
          strip.text.x = element_text(size=10, hjust = 0))
  
  # Almacenar el gráfico en la lista
  gam_plots[[zone]] <- p
}

# Add combined plot 
gam0 <- gam(ltbw ~ s(temp_min_percentile100_cz),
            data = births_weeks_temp_wide, 
            family = binomial(link = "logit")) 

combined_pred_data <- data.frame(temp_min_percentile100_cz = seq(0, 100, length.out = 100))

combined_pred_data$predicted_ltbw <- predict(gam0, newdata = combined_pred_data, se.fit = TRUE)$fit
combined_pred_data$se <- predict(gam0, newdata = combined_pred_data, se.fit = TRUE)$se.fit
combined_pred_data <- combined_pred_data |> 
  mutate(
    predicted_prob = plogis(predicted_ltbw),
    lower_ci = plogis(predicted_ltbw - 1.96 * se),
    upper_ci = plogis(predicted_ltbw + 1.96 * se)
  )

gam_plots[["Combined"]] <- ggplot(combined_pred_data, aes(x = temp_min_percentile100_cz, y = predicted_prob)) +
  geom_line(color = "black", size = 1) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "lightblue", alpha = 0.5) +
  #scale_y_continuous(limits = c(3000, 3800)) +
  labs(title = "Combined, all climatic zones percentiles",
       x = "Percentiles of entire pregnancy average of the daily min temperature",
       y = "Predicted probabilities tLBW and 95% CI (grams)",
       tag = "F.") +
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
        strip.text.x = element_text(size=10, hjust = 0))

# See all plots
do.call(ggarrange, c(gam_plots, list(nrow = 2, ncol = 3)))

ggsave(filename = paste0(data_out, "fig/", "Unadjusted_GAM_models_ltbw_min", ".png"),
       res = 300,
       width = 32,
       height = 16,
       units = 'cm',
       scaling = 0.75,
       device = ragg::agg_png)

###########################################################################/
#### Unadjusted GAM tLBW ~ MAX ----
###########################################################################/

# Lista para almacenar gráficos
gam_plots <- list()

# Lista de zonas climáticas únicas
clim_zones <- c("Desert", "Temperate dry, hot summer", "Temperate dry, warm summer", "Temperate, no dry season", "Cold steppe")

# Bucle para ajustar el modelo y generar gráficos por zona climática
for (i in seq_along(clim_zones)) {
  
  # Filtrar los datos por la zona climática actual
  zone <- clim_zones[i]
  zone_data <- births_weeks_temp_wide[births_weeks_temp_wide$clim_zone == zone, ]
  
  # Ajustar el modelo GAM
  gam0 <- gam(ltbw ~ s(temp_max_gest),
              data = zone_data, 
              family = binomial(link = "logit")) 
  
  # Crear datos para predicción
  pred_data <- data.frame(temp_max_gest = seq(min(zone_data$temp_max_gest), 
                                              max(zone_data$temp_max_gest), length.out = 100))
  
  # Obtener predicciones y calcular intervalos de confianza
  pred_data$predicted_ltbw <- predict(gam0, newdata = pred_data, se.fit = TRUE)$fit
  pred_data$se <- predict(gam0, newdata = pred_data, se.fit = TRUE)$se.fit
  pred_data <- pred_data |> 
    mutate(
      predicted_prob = plogis(predicted_ltbw),
      lower_ci = plogis(predicted_ltbw - 1.96 * se),
      upper_ci = plogis(predicted_ltbw + 1.96 * se)
    )
  
  # Generar el gráfico
  tags <- LETTERS[1:length(clim_zones)]
  
  p <- ggplot(pred_data, aes(x = temp_max_gest, y = predicted_prob)) +
    geom_line(color = "black", size = 1) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "lightblue", alpha = 0.5) +
    #scale_y_continuous(limits = c(3000, 3800)) +
    #scale_x_continuous(n.breaks = 5) +
    labs(title = paste(zone),
         x = "Entire pregnacy average of the daily max temperature (°C)",
         y = "Predicted probabilities tLBW and 95% CI (grams)",
         tag = paste0(tags[i], ".")) +
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
          strip.text.x = element_text(size=10, hjust = 0))
  
  # Almacenar el gráfico en la lista
  gam_plots[[zone]] <- p
}

# Add combined plot 
gam0 <- gam(ltbw ~ s(temp_max_percentile100_cz),
            data = births_weeks_temp_wide, 
            family = binomial(link = "logit")) 

combined_pred_data <- data.frame(temp_max_percentile100_cz = seq(0, 100, length.out = 100))

combined_pred_data$predicted_ltbw <- predict(gam0, newdata = combined_pred_data, se.fit = TRUE)$fit
combined_pred_data$se <- predict(gam0, newdata = combined_pred_data, se.fit = TRUE)$se.fit
combined_pred_data <- combined_pred_data |> 
  mutate(
    predicted_prob = plogis(predicted_ltbw),
    lower_ci = plogis(predicted_ltbw - 1.96 * se),
    upper_ci = plogis(predicted_ltbw + 1.96 * se)
  )

gam_plots[["Combined"]] <- ggplot(combined_pred_data, aes(x = temp_max_percentile100_cz, y = predicted_prob)) +
  geom_line(color = "black", size = 1) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "lightblue", alpha = 0.5) +
  #scale_y_continuous(limits = c(3000, 3800)) +
  labs(title = "Combined, all climatic zones percentiles",
       x = "Percentiles of entire pregnancy average of the daily max temperature",
       y = "Predicted probabilities tLBW and 95% CI (grams)",
       tag = "F.") +
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
        strip.text.x = element_text(size=10, hjust = 0))

# See all plots
do.call(ggarrange, c(gam_plots, list(nrow = 2, ncol = 3)))

ggsave(filename = paste0(data_out, "fig/", "Unadjusted_GAM_models_ltbw_max", ".png"),
       res = 300,
       width = 32,
       height = 16,
       units = 'cm',
       scaling = 0.75,
       device = ragg::agg_png)


###########################################################################/
#### Adjusted GAM tLBW ~ MEAM ----
###########################################################################/
rm(list = setdiff(ls(), c("births_weeks_temp", "births_weeks_temp_wide",
                          "data_path", "data_temp", "data_out"
                          )))

# Prepare data 
births_weeks_temp_wide_log <- births_weeks_temp |> 
  select(id, clim_zone, ltbw, trim_gest, sex,  
         age_group_mom, educ_group_mom, job_group_mom,
         age_group_dad, educ_group_dad, job_group_dad, 
         year_week1, month_week1,
         week_gest_num,
         temp_mean_percentile_wcz
  ) |> 
  distinct() |> 
  pivot_wider(names_from = week_gest_num,
              values_from = temp_mean_percentile_wcz
              ) 

births_weeks_temp_wide_log <- births_weeks_temp_wide_log |> bind_cols(fastDummies::dummy_cols(births_weeks_temp_wide_log$temp_mean_percentile_wcz))

# Unadjusted

system.time({
gam0_1 <- gam(ltbw ~ temp_mean_percentile_wcz,
            data = births_weeks_temp, 
            family = binomial(link = "logit"),
            gc.level = 0) 

gam0_2 <- gam(ltbw ~ temp_mean_percentile_tcz_T1 + temp_mean_percentile_tcz_T2 + temp_mean_percentile_tcz_T3,
            data = births_weeks_temp_wide, 
            family = binomial(link = "logit"),
            gc.level = 0) 
})

# Adjusted
system.time({
gam1 <- gam(ltbw ~ temp_mean_percentile_wcz + sex + 
              age_group_mom + educ_group_mom + job_group_mom +
              age_group_dad + educ_group_dad + job_group_dad +
              s(year_week1) + s(month_week1),  
            data = births_weeks_temp, 
            family = binomial(link = "logit"),
            gc.level = 0) 

gam2 <- gam(ltbw ~ temp_mean_percentile_tcz_T1 + temp_mean_percentile_tcz_T2 + temp_mean_percentile_tcz_T3+
              sex + 
              age_group_mom + educ_group_mom + job_group_mom +
              age_group_dad + educ_group_dad + job_group_dad +
              s(year_week1) + s(month_week1),  
            data = births_weeks_temp_wide, 
            family = binomial(link = "logit"),
            gc.level = 0) 
})


# Save results 
m0_1 <-  broom::tidy(gam0_1, parametric = TRUE, conf.int = TRUE, conf.level = 0.95) |> mutate(estimate=exp(estimate), conf.low=exp(conf.low), conf.high=exp(conf.high)) |> mutate_if(is.numeric, round, 2)
m0_2 <-  broom::tidy(gam0_2, parametric = TRUE, conf.int = TRUE, conf.level = 0.95) |> mutate(estimate=exp(estimate), conf.low=exp(conf.low), conf.high=exp(conf.high)) |> mutate_if(is.numeric, round, 2)
m1 <-  broom::tidy(gam1, parametric = TRUE, conf.int = TRUE, conf.level = 0.95) |> mutate(estimate=exp(estimate), conf.low=exp(conf.low), conf.high=exp(conf.high)) |> mutate_if(is.numeric, round, 2)
m2 <-  broom::tidy(gam2, parametric = TRUE, conf.int = TRUE, conf.level = 0.95) |> mutate(estimate=exp(estimate), conf.low=exp(conf.low), conf.high=exp(conf.high)) |> mutate_if(is.numeric, round, 2)

tabla <- bind_rows(
  m0_1 |>
    slice(2:10) |> 
    select(c(1:2, 6, 7)) |> 
    mutate(estimate=paste0(estimate, " ", "(", conf.low, "; ", conf.high, ")")) |> 
    select(1:2) |> 
    mutate(estimate=str_replace_all(estimate, ",", ".")) |> 
    mutate(term=str_replace_all(term, "temp_mean_percentile_wcz", "")) |> 
    rename("Unadjusted"=estimate) |> 
    left_join(
      m1 |> 
        slice(2:10) |> 
        select(c(1:2, 6, 7)) |> 
        mutate(estimate=paste0(estimate, " ", "(", conf.low, "; ", conf.high, ")")) |> 
        select(1:2) |> 
        mutate(estimate=str_replace_all(estimate, ",", ".")) |> 
        mutate(term=str_replace_all(term, "temp_mean_percentile_wcz", "")) |> 
        rename("Complete case adjusted"=estimate)
      , by="term") |> 
    rename("Temperature (centile category)"=term), 
  
  m0_2 |>
    slice(2:28) |> 
    select(c(1:2, 6, 7)) |> 
    mutate(estimate=paste0(estimate, " ", "(", conf.low, "; ", conf.high, ")")) |> 
    select(1:2) |> 
    mutate(estimate=str_replace_all(estimate, ",", ".")) |> 
    mutate(term=str_replace_all(term, "temp_mean_percentile_tcz_", "")) |> 
    rename("Unadjusted"=estimate) |> 
    mutate(t=str_extract(term, "^.{2}")) |> 
    mutate(term=str_remove(term, "^.{2}")) |> 
    relocate(t) |> 
    left_join(
      m2 |> 
        slice(2:28) |> 
        select(c(1:2, 6, 7)) |> 
        mutate(estimate=paste0(estimate, " ", "(", conf.low, "; ", conf.high, ")")) |> 
        select(1:2) |> 
        mutate(estimate=str_replace_all(estimate, ",", ".")) |> 
        mutate(term=str_replace_all(term, "temp_mean_percentile_tcz_", "")) |> 
        rename("Complete case adjusted"=estimate) |> 
        mutate(t=str_extract(term, "^.{2}")) |> 
        mutate(term=str_remove(term, "^.{2}")) |> 
        relocate(t)
      , by=c("t", "term")) |> 
    rename("Temperature (centile category)"=term) 
) |> 
  relocate(t)

tables_list <- list(
  "gam_unadj" = m0_1, 
  "gam_unadj_trimester" = m0_2,
  "gam_adj" = m1, 
  "gam_adj_trimester" = m2,
  "tabla_gam" = tabla
                   )

write.xlsx(tables_list, file =  paste0(data_out, "tab/", "GAM_models_ltbw_mean", ".xlsx"))

#### Graph Models reports ltbw ----

##### General gestational period ----

p50_1 <- data.frame(term="temp_mean_percentile_wcz41-50",
                  estimate=1, 
                  std.error=0, 
                  statistic=0, 
                  p.value=0, 
                  conf.low=1,
                  conf.high=1)

vis_data1 <- m1 %>% 
  slice(2:10) %>% 
  bind_rows(p50_1) %>% 
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

g1 <- ggplot(vis_data1, aes(x = term, y = estimate)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  geom_hline(yintercept = 1, linewidth = 0.5, color="black", linetype = "dashed") +
  scale_y_continuous(limits = c(0.8,1.2)) +
  labs(x = "Temperature centiles of average daily mean temperature",
       y = "tLBW (ORs and 95% CI)",
       tag="A.") +
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
        strip.text.x = element_text(size=10, hjust = 0))

g1

##### Trimester ----

p50_2 <- data.frame(term=c("temp_mean_percentile_tcz_T141-50", 
                         "temp_mean_percentile_tcz_T241-50", 
                         "temp_mean_percentile_tcz_T341-50"),
                  estimate=c(1,1,1),
                  std.error=c(0,0,0), 
                  statistic=c(0,0,0), 
                  p.value=c(0,0,0), 
                  conf.low=c(1,1,1),
                  conf.high=c(1,1,1))

vis_data2 <- m2 %>% 
  slice(2:28) %>% 
  bind_rows(p50_2) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_mean_percentile_tcz_T1<=10" ~ "<=10",
    term == "temp_mean_percentile_tcz_T111-20" ~ "11-20",
    term == "temp_mean_percentile_tcz_T121-30" ~ "21-30",
    term == "temp_mean_percentile_tcz_T131-40" ~ "31-40",
    term == "temp_mean_percentile_tcz_T141-50" ~ "41-50",
    term == "temp_mean_percentile_tcz_T151-60" ~ "51-60",
    term == "temp_mean_percentile_tcz_T161-70" ~ "61-70",
    term == "temp_mean_percentile_tcz_T171-80" ~ "71-80",
    term == "temp_mean_percentile_tcz_T181-90" ~ "81-90",
    term == "temp_mean_percentile_tcz_T1> 90" ~ "> 90",
    
    term == "temp_mean_percentile_tcz_T2<=10" ~ "<=10",
    term == "temp_mean_percentile_tcz_T211-20" ~ "11-20",
    term == "temp_mean_percentile_tcz_T221-30" ~ "21-30",
    term == "temp_mean_percentile_tcz_T231-40" ~ "31-40",
    term == "temp_mean_percentile_tcz_T241-50" ~ "41-50",
    term == "temp_mean_percentile_tcz_T251-60" ~ "51-60",
    term == "temp_mean_percentile_tcz_T261-70" ~ "61-70",
    term == "temp_mean_percentile_tcz_T271-80" ~ "71-80",
    term == "temp_mean_percentile_tcz_T281-90" ~ "81-90",
    term == "temp_mean_percentile_tcz_T2> 90" ~ "> 90",
    
    term == "temp_mean_percentile_tcz_T3<=10" ~ "<=10",
    term == "temp_mean_percentile_tcz_T311-20" ~ "11-20",
    term == "temp_mean_percentile_tcz_T321-30" ~ "21-30",
    term == "temp_mean_percentile_tcz_T331-40" ~ "31-40",
    term == "temp_mean_percentile_tcz_T341-50" ~ "41-50",
    term == "temp_mean_percentile_tcz_T351-60" ~ "51-60",
    term == "temp_mean_percentile_tcz_T361-70" ~ "61-70",
    term == "temp_mean_percentile_tcz_T371-80" ~ "71-80",
    term == "temp_mean_percentile_tcz_T381-90" ~ "81-90",
    term == "temp_mean_percentile_tcz_T3> 90" ~ "> 90"
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
  mutate(
    Trimester=c(rep("T1", 10), rep("T2", 10), rep("T3", 10))
  )

g2 <- ggplot(vis_data2, aes(x = term, y = estimate, color = Trimester, shape = Trimester, group = Trimester)) +
  geom_point(position = position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linewidth = 0.5, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("T1" = "black", "T2" = "grey", "T3" = "lightblue")) +
  scale_shape_manual(values = c("T1" = 16, "T2" = 17, "T3" = 15)) +
  scale_y_continuous(limits = c(0.3,1.7)) +
  labs(x = "Temperature centiles of average daily mean temperature",
       y = "tLBW (ORs and 95% CI)",
       tag="B.") +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.background = element_rect(color = NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white"), 
        strip.text.x = element_text(size = 10, hjust = 0))

g2

g1 | g2

ggsave(filename = paste0(data_out, "fig/", "Adjusted_GAM_models_ltbw_trim_mean", ".png"),
       res = 300,
       width = 28,
       height = 12,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)


###########################################################################/
#### Adjusted GAM tLBW ~ MIN ----
###########################################################################/
rm(list = setdiff(ls(), c("births_weeks_temp", "births_weeks_temp_wide",
                          "data_path", "data_temp", "data_out",
                          "g1", "g2"
)))

# Run models

# Unadjusted

system.time({
gam0_1 <- gam(ltbw ~ temp_min_percentile_wcz,
              data = births_weeks_temp, 
              family = binomial(link = "logit"),
              gc.level = 0) 

gam0_2 <- gam(ltbw ~ temp_min_percentile_tcz_T1 + temp_min_percentile_tcz_T2 + temp_min_percentile_tcz_T3,
              data = births_weeks_temp_wide, 
              family = binomial(link = "logit"),
              gc.level = 0) 
})

# Adjusted 
system.time({
gam1 <- gam(ltbw ~ temp_min_percentile_wcz + sex + 
              age_group_mom + educ_group_mom + job_group_mom +
              age_group_dad + educ_group_dad + job_group_dad +
              s(year_week1) + s(month_week1),  
            data = births_weeks_temp, 
            family = binomial(link = "logit"),
            gc.level = 0) 

gam2 <- gam(ltbw ~ temp_min_percentile_tcz_T1 + temp_min_percentile_tcz_T2 + temp_min_percentile_tcz_T3+
              sex + 
              age_group_mom + educ_group_mom + job_group_mom +
              age_group_dad + educ_group_dad + job_group_dad +
              s(year_week1) + s(month_week1),  
            data = births_weeks_temp_wide, 
            family = binomial(link = "logit"),
            gc.level = 0) 
})

# Save results 
m0_1 <-  broom::tidy(gam0_1, parametric = TRUE, conf.int = TRUE, conf.level = 0.95) |> mutate(estimate=exp(estimate), conf.low=exp(conf.low), conf.high=exp(conf.high)) |> mutate_if(is.numeric, round, 2)
m0_2 <-  broom::tidy(gam0_2, parametric = TRUE, conf.int = TRUE, conf.level = 0.95) |> mutate(estimate=exp(estimate), conf.low=exp(conf.low), conf.high=exp(conf.high)) |> mutate_if(is.numeric, round, 2)
m1 <-  broom::tidy(gam1, parametric = TRUE, conf.int = TRUE, conf.level = 0.95) |> mutate(estimate=exp(estimate), conf.low=exp(conf.low), conf.high=exp(conf.high)) |> mutate_if(is.numeric, round, 2)
m2 <-  broom::tidy(gam2, parametric = TRUE, conf.int = TRUE, conf.level = 0.95) |> mutate(estimate=exp(estimate), conf.low=exp(conf.low), conf.high=exp(conf.high)) |> mutate_if(is.numeric, round, 2)

tabla <- bind_rows(
  m0_1 |>
    slice(2:10) |> 
    select(c(1:2, 6, 7)) |> 
    mutate(estimate=paste0(estimate, " ", "(", conf.low, "; ", conf.high, ")")) |> 
    select(1:2) |> 
    mutate(estimate=str_replace_all(estimate, ",", ".")) |> 
    mutate(term=str_replace_all(term, "temp_min_percentile_wcz", "")) |> 
    rename("Unadjusted"=estimate) |> 
    left_join(
      m1 |> 
        slice(2:10) |> 
        select(c(1:2, 6, 7)) |> 
        mutate(estimate=paste0(estimate, " ", "(", conf.low, "; ", conf.high, ")")) |> 
        select(1:2) |> 
        mutate(estimate=str_replace_all(estimate, ",", ".")) |> 
        mutate(term=str_replace_all(term, "temp_min_percentile_wcz", "")) |> 
        rename("Complete case adjusted"=estimate)
      , by="term") |> 
    rename("Temperature (centile category)"=term), 
  
  m0_2 |>
    slice(2:28) |> 
    select(c(1:2, 6, 7)) |> 
    mutate(estimate=paste0(estimate, " ", "(", conf.low, "; ", conf.high, ")")) |> 
    select(1:2) |> 
    mutate(estimate=str_replace_all(estimate, ",", ".")) |> 
    mutate(term=str_replace_all(term, "temp_min_percentile_tcz_", "")) |> 
    rename("Unadjusted"=estimate) |> 
    mutate(t=str_extract(term, "^.{2}")) |> 
    mutate(term=str_remove(term, "^.{2}")) |> 
    relocate(t) |> 
    left_join(
      m2 |> 
        slice(2:28) |> 
        select(c(1:2, 6, 7)) |> 
        mutate(estimate=paste0(estimate, " ", "(", conf.low, "; ", conf.high, ")")) |> 
        select(1:2) |> 
        mutate(estimate=str_replace_all(estimate, ",", ".")) |> 
        mutate(term=str_replace_all(term, "temp_min_percentile_tcz_", "")) |> 
        rename("Complete case adjusted"=estimate) |> 
        mutate(t=str_extract(term, "^.{2}")) |> 
        mutate(term=str_remove(term, "^.{2}")) |> 
        relocate(t)
      , by=c("t", "term")) |> 
    rename("Temperature (centile category)"=term) 
) |> 
  relocate(t)

tables_list <- list(
  "gam_unadj" = m0_1, 
  "gam_unadj_trimester" = m0_2,
  "gam_adj" = m1, 
  "gam_adj_trimester" = m2,
  "tabla_gam" = tabla
)

write.xlsx(tables_list, file =  paste0(data_out, "tab/", "GAM_models_ltbw_min", ".xlsx"))

#### Graph Models reports ltbw ----

##### General gestational period ----

p50_1 <- data.frame(term="temp_min_percentile_wcz41-50",
                    estimate=1, 
                    std.error=0, 
                    statistic=0, 
                    p.value=0, 
                    conf.low=1,
                    conf.high=1)

vis_data1 <- m1 %>% 
  slice(2:10) %>% 
  bind_rows(p50_1) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_min_percentile_wcz<=10" ~ "<=10",
    term == "temp_min_percentile_wcz11-20" ~ "11-20",
    term == "temp_min_percentile_wcz21-30" ~ "21-30",
    term == "temp_min_percentile_wcz31-40" ~ "31-40",
    term == "temp_min_percentile_wcz41-50" ~ "41-50",
    term == "temp_min_percentile_wcz51-60" ~ "51-60",
    term == "temp_min_percentile_wcz61-70" ~ "61-70",
    term == "temp_min_percentile_wcz71-80" ~ "71-80",
    term == "temp_min_percentile_wcz81-90" ~ "81-90",
    term == "temp_min_percentile_wcz> 90" ~ "> 90"
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

g3 <- vis_data1 |> 
  filter(term %in% c("<=10", "11-20", "21-30", "31-40", "41-50")) |> 
  ggplot(aes(x = term, y = estimate)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  geom_hline(yintercept = 1, linewidth = 0.5, color="black", linetype = "dashed") +
  scale_y_continuous(limits = c(0.7,1.3)) +
  labs(x = "Temperature centiles of average daily minimum temperature",
       y = "tLBW (ORs and 95% CI)",
       tag="C.") +
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
        strip.text.x = element_text(size=10, hjust = 0))

g3


##### Trimester ----

p50_2 <- data.frame(term=c("temp_min_percentile_tcz_T141-50", 
                           "temp_min_percentile_tcz_T241-50", 
                           "temp_min_percentile_tcz_T341-50"),
                    estimate=c(1,1,1),
                    std.error=c(0,0,0), 
                    statistic=c(0,0,0), 
                    p.value=c(0,0,0), 
                    conf.low=c(1,1,1),
                    conf.high=c(1,1,1))

vis_data2 <- m2 %>% 
  slice(2:28) %>% 
  bind_rows(p50_2) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_min_percentile_tcz_T1<=10" ~ "<=10",
    term == "temp_min_percentile_tcz_T111-20" ~ "11-20",
    term == "temp_min_percentile_tcz_T121-30" ~ "21-30",
    term == "temp_min_percentile_tcz_T131-40" ~ "31-40",
    term == "temp_min_percentile_tcz_T141-50" ~ "41-50",
    term == "temp_min_percentile_tcz_T151-60" ~ "51-60",
    term == "temp_min_percentile_tcz_T161-70" ~ "61-70",
    term == "temp_min_percentile_tcz_T171-80" ~ "71-80",
    term == "temp_min_percentile_tcz_T181-90" ~ "81-90",
    term == "temp_min_percentile_tcz_T1> 90" ~ "> 90",
    
    term == "temp_min_percentile_tcz_T2<=10" ~ "<=10",
    term == "temp_min_percentile_tcz_T211-20" ~ "11-20",
    term == "temp_min_percentile_tcz_T221-30" ~ "21-30",
    term == "temp_min_percentile_tcz_T231-40" ~ "31-40",
    term == "temp_min_percentile_tcz_T241-50" ~ "41-50",
    term == "temp_min_percentile_tcz_T251-60" ~ "51-60",
    term == "temp_min_percentile_tcz_T261-70" ~ "61-70",
    term == "temp_min_percentile_tcz_T271-80" ~ "71-80",
    term == "temp_min_percentile_tcz_T281-90" ~ "81-90",
    term == "temp_min_percentile_tcz_T2> 90" ~ "> 90",
    
    term == "temp_min_percentile_tcz_T3<=10" ~ "<=10",
    term == "temp_min_percentile_tcz_T311-20" ~ "11-20",
    term == "temp_min_percentile_tcz_T321-30" ~ "21-30",
    term == "temp_min_percentile_tcz_T331-40" ~ "31-40",
    term == "temp_min_percentile_tcz_T341-50" ~ "41-50",
    term == "temp_min_percentile_tcz_T351-60" ~ "51-60",
    term == "temp_min_percentile_tcz_T361-70" ~ "61-70",
    term == "temp_min_percentile_tcz_T371-80" ~ "71-80",
    term == "temp_min_percentile_tcz_T381-90" ~ "81-90",
    term == "temp_min_percentile_tcz_T3> 90" ~ "> 90"
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
  mutate(
    Trimester=c(rep("T1", 10), rep("T2", 10), rep("T3", 10))
  )

g4 <- vis_data2 |> 
  filter(term %in% c("<=10", "11-20", "21-30", "31-40", "41-50")) |> 
  ggplot(aes(x = term, y = estimate, color = Trimester, shape = Trimester, group = Trimester)) +
  geom_point(position = position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linewidth = 0.5, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("T1" = "black", "T2" = "grey", "T3" = "lightblue")) +
  scale_shape_manual(values = c("T1" = 16, "T2" = 17, "T3" = 15)) +
  scale_y_continuous(limits = c(0.2,1.8)) +
  labs(x = "Temperature centiles of average daily minimum temperature",
       y = "tLBW (ORs and 95% CI)",
       tag="D.") +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.background = element_rect(color = NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white"), 
        strip.text.x = element_text(size = 10, hjust = 0))

g4

g3 | g4

ggsave(filename = paste0(data_out, "fig/", "Adjusted_GAM_models_ltbw_trim_min", ".png"),
       res = 300,
       width = 28,
       height = 12,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)


###########################################################################/
#### Adjusted GAM tLBW ~ MAX ----
###########################################################################/
rm(list = setdiff(ls(), c("births_weeks_temp", "births_weeks_temp_wide",
                          "data_path", "data_temp", "data_out",
                          "g1", "g2", "g3", "g4"
)))

# Run models

# Unadjusted
system.time({
gam0_1 <- gam(ltbw ~ temp_max_percentile_wcz,
              data = births_weeks_temp, 
              family = binomial(link = "logit"),
              gc.level = 0) 

gam0_2 <- gam(ltbw ~ temp_max_percentile_tcz_T1 + temp_max_percentile_tcz_T2 + temp_max_percentile_tcz_T3,
              data = births_weeks_temp_wide, 
              family = binomial(link = "logit"),
              gc.level = 0) 
})

# Adjusted
system.time({
gam1 <- gam(ltbw ~ temp_max_percentile_wcz + sex + 
              age_group_mom + educ_group_mom + job_group_mom +
              age_group_dad + educ_group_dad + job_group_dad +
              s(year_week1) + s(month_week1),  
            data = births_weeks_temp, 
            family = binomial(link = "logit"),
            gc.level = 0) 


gam2 <- gam(ltbw ~ temp_max_percentile_tcz_T1 + temp_max_percentile_tcz_T2 + temp_max_percentile_tcz_T3+
              sex + 
              age_group_mom + educ_group_mom + job_group_mom +
              age_group_dad + educ_group_dad + job_group_dad +
              s(year_week1) + s(month_week1),  
            data = births_weeks_temp_wide, 
            family = binomial(link = "logit"),
            gc.level = 0) 
})

# Save results 
m0_1 <-  broom::tidy(gam0_1, parametric = TRUE, conf.int = TRUE, conf.level = 0.95) |> mutate(estimate=exp(estimate), conf.low=exp(conf.low), conf.high=exp(conf.high)) |> mutate_if(is.numeric, round, 2)
m0_2 <-  broom::tidy(gam0_2, parametric = TRUE, conf.int = TRUE, conf.level = 0.95) |> mutate(estimate=exp(estimate), conf.low=exp(conf.low), conf.high=exp(conf.high)) |> mutate_if(is.numeric, round, 2)
m1 <-  broom::tidy(gam1, parametric = TRUE, conf.int = TRUE, conf.level = 0.95) |> mutate(estimate=exp(estimate), conf.low=exp(conf.low), conf.high=exp(conf.high)) |> mutate_if(is.numeric, round, 2)
m2 <-  broom::tidy(gam2, parametric = TRUE, conf.int = TRUE, conf.level = 0.95) |> mutate(estimate=exp(estimate), conf.low=exp(conf.low), conf.high=exp(conf.high)) |> mutate_if(is.numeric, round, 2)

tabla <- bind_rows(
  m0_1 |>
    slice(2:10) |> 
    select(c(1:2, 6, 7)) |> 
    mutate(estimate=paste0(estimate, " ", "(", conf.low, "; ", conf.high, ")")) |> 
    select(1:2) |> 
    mutate(estimate=str_replace_all(estimate, ",", ".")) |> 
    mutate(term=str_replace_all(term, "temp_max_percentile_wcz", "")) |> 
    rename("Unadjusted"=estimate) |> 
    left_join(
      m1 |> 
        slice(2:10) |> 
        select(c(1:2, 6, 7)) |> 
        mutate(estimate=paste0(estimate, " ", "(", conf.low, "; ", conf.high, ")")) |> 
        select(1:2) |> 
        mutate(estimate=str_replace_all(estimate, ",", ".")) |> 
        mutate(term=str_replace_all(term, "temp_max_percentile_wcz", "")) |> 
        rename("Complete case adjusted"=estimate)
      , by="term") |> 
    rename("Temperature (centile category)"=term), 
  
  m0_2 |>
    slice(2:28) |> 
    select(c(1:2, 6, 7)) |> 
    mutate(estimate=paste0(estimate, " ", "(", conf.low, "; ", conf.high, ")")) |> 
    select(1:2) |> 
    mutate(estimate=str_replace_all(estimate, ",", ".")) |> 
    mutate(term=str_replace_all(term, "temp_max_percentile_tcz_", "")) |> 
    rename("Unadjusted"=estimate) |> 
    mutate(t=str_extract(term, "^.{2}")) |> 
    mutate(term=str_remove(term, "^.{2}")) |> 
    relocate(t) |> 
    left_join(
      m2 |> 
        slice(2:28) |> 
        select(c(1:2, 6, 7)) |> 
        mutate(estimate=paste0(estimate, " ", "(", conf.low, "; ", conf.high, ")")) |> 
        select(1:2) |> 
        mutate(estimate=str_replace_all(estimate, ",", ".")) |> 
        mutate(term=str_replace_all(term, "temp_max_percentile_tcz_", "")) |> 
        rename("Complete case adjusted"=estimate) |> 
        mutate(t=str_extract(term, "^.{2}")) |> 
        mutate(term=str_remove(term, "^.{2}")) |> 
        relocate(t)
      , by=c("t", "term")) |> 
    rename("Temperature (centile category)"=term) 
) |> 
  relocate(t)

tables_list <- list(
  "gam_unadj" = m0_1, 
  "gam_unadj_trimester" = m0_2,
  "gam_adj" = m1, 
  "gam_adj_trimester" = m2,
  "tabla_gam" = tabla
)

write.xlsx(tables_list, file =  paste0(data_out, "tab/", "GAM_models_ltbw_max", ".xlsx"))

#### Graph Models reports ltbw ----

##### General gestational period ----

p50_1 <- data.frame(term="temp_max_percentile_wcz41-50",
                    estimate=1, 
                    std.error=0, 
                    statistic=0, 
                    p.value=0, 
                    conf.low=1,
                    conf.high=1)

vis_data1 <- m1 %>% 
  slice(2:10) %>% 
  bind_rows(p50_1) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_max_percentile_wcz<=10" ~ "<=10",
    term == "temp_max_percentile_wcz11-20" ~ "11-20",
    term == "temp_max_percentile_wcz21-30" ~ "21-30",
    term == "temp_max_percentile_wcz31-40" ~ "31-40",
    term == "temp_max_percentile_wcz41-50" ~ "41-50",
    term == "temp_max_percentile_wcz51-60" ~ "51-60",
    term == "temp_max_percentile_wcz61-70" ~ "61-70",
    term == "temp_max_percentile_wcz71-80" ~ "71-80",
    term == "temp_max_percentile_wcz81-90" ~ "81-90",
    term == "temp_max_percentile_wcz> 90" ~ "> 90"
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

g5 <- vis_data1 |> 
  filter(term %in% c("41-50", "51-60", "61-70", "71-80", "81-90", "> 90")) |> 
  ggplot(aes(x = term, y = estimate)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  geom_hline(yintercept = 1, linewidth = 0.5, color="black", linetype = "dashed") +
  scale_y_continuous(limits = c(0.8,1.2)) +
  labs(x = "Temperature centiles of average daily maximum temperature",
       y = "tLBW (ORs and 95% CI)",
       tag="E.") +
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
        strip.text.x = element_text(size=10, hjust = 0))

g5


##### Trimester ----

p50_2 <- data.frame(term=c("temp_max_percentile_tcz_T141-50", 
                           "temp_max_percentile_tcz_T241-50", 
                           "temp_max_percentile_tcz_T341-50"),
                    estimate=c(1,1,1),
                    std.error=c(0,0,0), 
                    statistic=c(0,0,0), 
                    p.value=c(0,0,0), 
                    conf.low=c(1,1,1),
                    conf.high=c(1,1,1))

vis_data2 <- m2 %>% 
  slice(2:28) %>% 
  bind_rows(p50_2) %>% 
  arrange(term) %>% 
  mutate(term = factor(case_when(
    term == "temp_max_percentile_tcz_T1<=10" ~ "<=10",
    term == "temp_max_percentile_tcz_T111-20" ~ "11-20",
    term == "temp_max_percentile_tcz_T121-30" ~ "21-30",
    term == "temp_max_percentile_tcz_T131-40" ~ "31-40",
    term == "temp_max_percentile_tcz_T141-50" ~ "41-50",
    term == "temp_max_percentile_tcz_T151-60" ~ "51-60",
    term == "temp_max_percentile_tcz_T161-70" ~ "61-70",
    term == "temp_max_percentile_tcz_T171-80" ~ "71-80",
    term == "temp_max_percentile_tcz_T181-90" ~ "81-90",
    term == "temp_max_percentile_tcz_T1> 90" ~ "> 90",
    
    term == "temp_max_percentile_tcz_T2<=10" ~ "<=10",
    term == "temp_max_percentile_tcz_T211-20" ~ "11-20",
    term == "temp_max_percentile_tcz_T221-30" ~ "21-30",
    term == "temp_max_percentile_tcz_T231-40" ~ "31-40",
    term == "temp_max_percentile_tcz_T241-50" ~ "41-50",
    term == "temp_max_percentile_tcz_T251-60" ~ "51-60",
    term == "temp_max_percentile_tcz_T261-70" ~ "61-70",
    term == "temp_max_percentile_tcz_T271-80" ~ "71-80",
    term == "temp_max_percentile_tcz_T281-90" ~ "81-90",
    term == "temp_max_percentile_tcz_T2> 90" ~ "> 90",
    
    term == "temp_max_percentile_tcz_T3<=10" ~ "<=10",
    term == "temp_max_percentile_tcz_T311-20" ~ "11-20",
    term == "temp_max_percentile_tcz_T321-30" ~ "21-30",
    term == "temp_max_percentile_tcz_T331-40" ~ "31-40",
    term == "temp_max_percentile_tcz_T341-50" ~ "41-50",
    term == "temp_max_percentile_tcz_T351-60" ~ "51-60",
    term == "temp_max_percentile_tcz_T361-70" ~ "61-70",
    term == "temp_max_percentile_tcz_T371-80" ~ "71-80",
    term == "temp_max_percentile_tcz_T381-90" ~ "81-90",
    term == "temp_max_percentile_tcz_T3> 90" ~ "> 90"
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
  mutate(
    Trimester=c(rep("T1", 10), rep("T2", 10), rep("T3", 10))
  )

g6 <- vis_data2 |> 
  filter(term %in% c("41-50", "51-60", "61-70", "71-80", "81-90",  "> 90")) |> 
  ggplot(aes(x = term, y = estimate, color = Trimester, shape = Trimester, group = Trimester)) +
  geom_point(position = position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linewidth = 0.5, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("T1" = "black", "T2" = "grey", "T3" = "lightblue")) +
  scale_shape_manual(values = c("T1" = 16, "T2" = 17, "T3" = 15)) +
  scale_y_continuous(limits = c(0.1,1.9)) +
  labs(x = "Temperature centiles of average daily maximum temperature",
       y = "tLBW (ORs and 95% CI)",
       tag="F.") +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.background = element_rect(color = NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = "white", color = "white"), 
        strip.text.x = element_text(size = 10, hjust = 0))

g6

g5 | g6

ggsave(filename = paste0(data_out, "fig/", "Adjusted_GAM_models_ltbw_trim_max", ".png"),
       res = 300,
       width = 28,
       height = 12,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)


###########################################################################/
#### Full PLots -----
###########################################################################/

(g1 | g2) / (g3 | g4) / (g5 | g6)


ggsave(filename = paste0(data_out, "fig/", "Adjusted_GAM_models_ltbw_trim_full", ".png"),
       res = 300,
       width = 25,
       height = 25,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)


fin <- Sys.time()
fin-inicio

