## Settings  ----

# Previous work
rm(list=(ls()))
options(scipen=999)
source("Code/0.2 Settings.R")

# Optimal use power computation 
options(future.globals.maxSize = 3000 * 1024^2)  
plan(multisession, workers = detectCores() - 4) # Parallelization

# Paths
load(paste0("Data/Output/births_2011_2020_weeks_temp_mod", ".RData"))
glimpse(births_weeks_temp)
zona <- unique(births_weeks_temp$clim_zone)

# Gam MODELS
library(mgcv)
library(broom)
library(ggplot2)
library(dplyr)
library(ragg)

births_weeks_temp$temp_mean_percentile_wz <- relevel(births_weeks_temp$temp_mean_percentile_wz, ref = "41-50")
births_weeks_temp$temp_mean_percentile_wcz <- relevel(births_weeks_temp$temp_mean_percentile_wcz, ref = "41-50")


# Define las zonas climáticas
clim_zones <- unique(births_weeks_temp$clim_zone)
#clim_zones <- clim_zones[1:5]


# Ajustar el modelo GAM con los datos filtrados

gm <- gam(tbw ~ temp_mean_percentile_wcz + sex + 
            age_mom + factor(educ_mom) + factor(job_mom) +
            age_dad + factor(educ_dad) + factor(job_dad) + 
            s(year_week1) + s(month_week1),  
          data = filter(births_weeks_temp, clim_zone != clim_zones[6]), 
          family=gaussian())
beepr::beep()

# Extraer resultados del modelo
m1 <- broom::tidy(gm, parametric = TRUE)

# Crear la fila de datos para el percentil 41-50
p50 <- data.frame(term="temp_mean_percentile_wcz41-50",
                  estimate=0, 
                  std.error=0, 
                  statistic=0, 
                  p.value=0)

# Preparar los datos para la visualización
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
  ))) 

# Crear la gráfica
g5 <- ggplot(vis_data, aes(x = term, y = estimate)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = lci, ymax = rci), width = 0.1) +
  #scale_y_continuous(limits = c(-30, 30), n.breaks = 6) +
  geom_hline(yintercept = 0, linewidth = 0.5, color="black", linetype = "dashed") +
  labs(x = "Temperature centiles",
       y = "Differences in tBW and 95% CI (grams)",
       title = paste("Se excluye:", clim_zones[6])) +
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



(g1 | g2 | g3) / (g4 | g5) 
g5

ggsave(filename = paste0("GAM_models_tbw_czone", ".png"),
       res = 300,
       width = 25,
       height = 10,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)




# Iterar a través de las zonas climáticas excluyendo una a la vez
for (i in seq_along(clim_zones)) {
  # Filtrar los datos excluyendo la zona actual
  zone <- clim_zones[i]
  data_filtered <- births_weeks_temp %>% filter(clim_zone != clim_zones[i])
  
  # Ajustar el modelo GAM con los datos filtrados
  gm <- gam(tbw ~ temp_mean_percentile_wcz + sex + 
              age_mom + factor(educ_mom) + factor(job_mom) +
              age_dad + factor(educ_dad) + factor(job_dad) + 
              s(year_week1) + s(month_week1),  
            data = data_filtered, 
            family=gaussian())
  
  # Extraer resultados del modelo
  m1 <- broom::tidy(gm, parametric = TRUE)
  
  # Crear la fila de datos para el percentil 41-50
  p50 <- data.frame(term="temp_mean_percentile_wz41-50",
                    estimate=0, 
                    std.error=0, 
                    statistic=0, 
                    p.value=0)
  
  # Preparar los datos para la visualización
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
    ))) 
  
  # Crear la gráfica
  g1 <- ggplot(vis_data, aes(x = term, y = estimate)) +
    geom_point(size=1.5) +
    geom_errorbar(aes(ymin = lci, ymax = rci), width = 0.1) +
    #scale_y_continuous(limits = c(-30, 30), n.breaks = 6) +
    geom_hline(yintercept = 0, linewidth = 0.5, color="black", linetype = "dashed") +
    labs(x = "Temperature centiles",
         y = "Differences in tBW and 95% CI (grams)",
         title = paste("Se excluye:", clim_zones[i])) +
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
  
  plot_list[[i]] <- g1
  
}

do.call(ggarrange, c(plot_list, list(nrow = 5, ncol = 3)))

plot_list[[1]]


# Guardar la gráfica
ggsave(filename = paste0("Figura_excluyendo_", gsub(" ", "_", clim_zones[i]), ".png"),
       plot = g1,
       res = 300,
       width = 25,
       height = 10,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)


gm <- gam(tbw ~ temp_mean_percentile_wz + sex + 
            age_mom + factor(educ_mom) + factor(job_mom) +
            age_dad + factor(educ_dad) + factor(job_dad) + 
            s(year_week1) + s(month_week1),  
          data = births_weeks_temp, 
          family=gaussian())

m1 <-  broom::tidy(gm, parametric = TRUE)

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



##### Vulnerability ----

gm_t1 <- gam(tbw ~ temp_mean_percentile_wcz + sex + 
               age_mom + factor(educ_mom) + factor(job_mom) +
               age_dad + factor(educ_dad) + factor(job_dad) + 
               s(year_week1) + s(month_week1),  
             data = filter(births_weeks_temp, vulnerability=="Low"),
             family=gaussian())

gm_t2 <- gam(tbw ~ temp_mean_percentile_wcz + sex + 
               age_mom + factor(educ_mom) + factor(job_mom) +
               age_dad + factor(educ_dad) + factor(job_dad) +
               s(year_week1) + s(month_week1),  
             data = filter(births_weeks_temp, vulnerability=="Medium low"),
             family=gaussian())

gm_t3 <- gam(tbw ~ temp_mean_percentile_wcz + sex + 
               age_mom + factor(educ_mom) + factor(job_mom) +
               age_dad + factor(educ_dad) + factor(job_dad) +
               s(year_week1) + s(month_week1),  
             data = filter(births_weeks_temp, vulnerability=="Medium high"),
             family=gaussian())



p50 <- data.frame(term="temp_mean_percentile_wcz41-50",
                  estimate=0, 
                  std.error=0, 
                  statistic=0, 
                  p.value=0)


mt1 <- broom::tidy(gm_t1, parametric = TRUE)
mt2 <- broom::tidy(gm_t2, parametric = TRUE)
mt3 <- broom::tidy(gm_t3, parametric = TRUE)


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
  mutate(Vulnerability="Low")

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
  mutate(Vulnerability="Medium low")

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
  mutate(Vulnerability="Medium high")

vis_data <- rbind(vis_t1, vis_t2, vis_t3)

g2 <- ggplot(vis_data, aes(x = term, y = estimate, color = Vulnerability, shape = Vulnerability, group = Vulnerability)) +
  geom_point(position = position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin = lci, ymax = rci), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("Low" = "black", "Medium low" = "grey", "Medium high" = "lightblue")) +
  scale_shape_manual(values = c("Low" = 16, "Medium low" = 17, "Medium high" = 15)) +
  labs(x = "Temperature centiles",
       y = "Differences in tBW and 95% CI (grams)"
       #tag="B)"
       ) +
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

g2


ggsave(filename = paste0("fig/", "GAM_models_tbw_zone_trim_wcz", ".png"),
       res = 300,
       width = 15,
       height = 10,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)


