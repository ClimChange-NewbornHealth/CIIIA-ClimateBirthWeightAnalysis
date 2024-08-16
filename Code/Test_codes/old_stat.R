# Code 8: Statistical Models ---

## Settings  ----
rm(list=(ls()))
source("Code/0.2 Settings.R")

path <- "Data/Output/"
data_temp <- "births_2011_2020_weeks_temp_final.RData"
load(paste0(path, data_temp))
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

births_weeks_temp <- births_weeks_temp %>% 
  filter(!id %in% ids_na) 

length(unique(births_weeks_temp$id)) # 330118

births_weeks_temp <- births_weeks_temp %>% drop_na(sex,
                                                   age_mom, educ_mom, job_mom
                                                   #age_dad, educ_dad, job_dad
                                                         )
length(unique(births_weeks_temp$id)) # 323551

rm(ids_con_na)

mean(births_weeks_temp$tbw)
sd(births_weeks_temp$tbw)


## GAM MODELS ----

### Temperature and weight all climate zones ----
glimpse(births_weeks_temp)
# Adjust label ref 
births_weeks_temp$temp_mean_percentile_wz <- relevel(births_weeks_temp$temp_mean_percentile_wz, ref = "41-50")
births_weeks_temp$temp_mean_percentile_wcz <- relevel(births_weeks_temp$temp_mean_percentile_wcz, ref = "41-50")

gam_model1 <- gam(tbw ~ temp_mean_percentile_wz + s(week_gest_num), data = births_weeks_temp, family=gaussian())
gam_model_log1 <- gam(ltbw ~ temp_mean_percentile_wz + s(week_gest_num), data = births_weeks_temp, family = binomial(link = "logit"))

summary(gam_model1)
summary(gam_model_log1)

# Graph results 
p50 <- data.frame(term="temp_mean_percentile_wz41-50",
                  estimate=0, 
                  std.error=0, 
                  statistic=0, 
                  p.value=0)


vis_data <- broom::tidy(gam_model1, parametric = TRUE) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error),
         ) %>% 
  filter(term!="(Intercept)") %>% 
  slice(1:9) %>% 
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
    term == "temp_mean_percentile_wz> 90" ~ "> 90",
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

p50 <- data.frame(term="temp_mean_percentile_wz41-50",
                  estimate=1, 
                  std.error=0, 
                  statistic=0, 
                  p.value=0)


vis_data <- broom::tidy(gam_model_log1, exponentiate=TRUE, parametric = TRUE) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error),
  ) %>% 
  filter(term!="(Intercept)") %>% 
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
    term == "temp_mean_percentile_wz> 90" ~ "> 90",
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


g2 <- ggplot(vis_data, aes(x = term, y = estimate)) +
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

g2

ggarrange(g1, g3, common.legend = FALSE, ncol = 2, legend="right")

ggsave(filename = paste0("Output_analysis/Fig/temp_bw_z",".png"),
       res = 300,
       width = 25,
       height = 10,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)


### Temperature by trimester ----

gam_model2 <- gam(tbw ~ temp_mean_percentile_wz + s(week_gest_num), data = filter(births_weeks_temp, trim_gest=="T1"), family=gaussian())
gam_model_log2 <- gam(ltbw ~ temp_mean_percentile_wz + s(week_gest_num), data = filter(births_weeks_temp, trim_gest=="T1"), family = binomial(link = "logit"))

gam_model3 <- gam(tbw ~ temp_mean_percentile_wz + s(week_gest_num), data = filter(births_weeks_temp, trim_gest=="T2"), family=gaussian())
gam_model_log3 <- gam(ltbw ~ temp_mean_percentile_wz + s(week_gest_num), data = filter(births_weeks_temp, trim_gest=="T2"), family = binomial(link = "logit"))

gam_model4 <- gam(tbw ~ temp_mean_percentile_wz + s(week_gest_num), data = filter(births_weeks_temp, trim_gest=="T3"), family=gaussian())
gam_model_log4 <- gam(ltbw ~ temp_mean_percentile_wz + s(week_gest_num), data = filter(births_weeks_temp, trim_gest=="T3"), family = binomial(link = "logit"))


p50 <- data.frame(term="temp_mean_percentile_wz41-50",
                  estimate=0, 
                  std.error=0, 
                  statistic=0, 
                  p.value=0)


vis_data1 <- broom::tidy(gam_model2, parametric = TRUE) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error),
  ) %>% 
  filter(term!="(Intercept)") %>% 
  arrange(term)

vis_data1 <- vis_data1 %>% 
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


vis_data2 <- broom::tidy(gam_model3, parametric = TRUE) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error),
  ) %>% 
  filter(term!="(Intercept)") %>% 
  arrange(term)

vis_data2 <- vis_data2 %>% 
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


vis_data3 <- broom::tidy(gam_model4, parametric = TRUE) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error),
  ) %>% 
  filter(term!="(Intercept)") %>% 
  arrange(term)

vis_data3 <- vis_data3 %>% 
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


vis_data1
vis_data2
vis_data3

vis_data <- rbind(vis_data1, vis_data2, vis_data3)

g3 <- ggplot(vis_data, aes(x = term, y = estimate, color = Trimester, shape = Trimester, group = Trimester)) +
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

g3


p50 <- data.frame(term="temp_mean_percentile_wz41-50",
                  estimate=1, 
                  std.error=0, 
                  statistic=0, 
                  p.value=0)

vis_data1 <- broom::tidy(gam_model_log1, exponentiate=TRUE, parametric = TRUE) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error),
  ) %>% 
  filter(term!="(Intercept)") %>% 
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
    term == "temp_mean_percentile_wz> 90" ~ "> 90",
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

vis_data2 <- broom::tidy(gam_model_log2, exponentiate=TRUE, parametric = TRUE) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error),
  ) %>% 
  filter(term!="(Intercept)") %>% 
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
    term == "temp_mean_percentile_wz> 90" ~ "> 90",
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
  ))  %>% 
  mutate(Trimester="T2")

vis_data3 <- broom::tidy(gam_model_log3, exponentiate=TRUE, parametric = TRUE) %>% 
  bind_rows(p50) %>% 
  mutate(lci = estimate - (1.96*std.error),
         rci = estimate + (1.96*std.error),
  ) %>% 
  filter(term!="(Intercept)") %>% 
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
    term == "temp_mean_percentile_wz> 90" ~ "> 90",
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
  ))  %>% 
  mutate(Trimester="T3")


vis_data1
vis_data2
vis_data3

vis_data <- rbind(vis_data1, vis_data2, vis_data3)

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


ggarrange(g2, g4, common.legend = TRUE, ncol = 2, legend="right")

ggsave(filename = paste0("Output_analysis/Fig/temp_bw_trim_z",".png"),
       res = 300,
       width = 25,
       height = 10,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)


### Others ----


gam_model1 <- gam(tbw ~ s(temp_mean) + s(week_gest_num), data = births_weeks_temp)

gam_models_zone <- births_weeks_temp %>%
  group_by(zona) %>%
  do(model = gam(peso ~ s(temp_mean) + s(week_gest_num), data = .))

# Gráfico para toda la zona
# Crear un nuevo dataframe para predicciones
pred_data <- data.frame(
  temp_mean = seq(min(births_weeks_temp$temp_mean, na.rm = TRUE), max(births_weeks_temp$temp_mean, na.rm = TRUE), length.out = 100),
  week_gest_num = median(births_weeks_temp$week_gest_num, na.rm = TRUE)  # Usar la mediana como ejemplo
)

# Predecir usando el modelo GAM
pred_data$predicted_weight <- as.numeric(predict(gam_model1, newdata = pred_data, type = "response", se=TRUE)$fit)
pred_data$predicted_weight_se <- as.numeric(predict(gam_model1, newdata = pred_data, type = "response", se=TRUE)$se.fit)

# Gráfico
ggplot(pred_data, aes(x = temp_mean)) +
  geom_line(aes(y = predicted_weight)) +
  geom_ribbon(aes(ymin = predicted_weight - 1.96 * predicted_weight_se, 
                  ymax = predicted_weight + 1.96 * predicted_weight_se),
              alpha = 0.2, fill = "blue") +
  labs(x="Temperatura media", y="Estimación Peso") + # Título específico por zona
  theme_light()

ggsave(filename = paste0("Output_analysis/Fig/Trends_GAM_full", "_",Sys.Date(),".png"),
       res = 300,
       width = 10,
       height = 5,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)



# Crear un gráfico para cada zona climática
plots <- lapply(seq_along(gam_models_zone$model), function(idx) {
  mod <- gam_models_zone$model[[idx]]
  zona_name <- gam_models_zone$zona[idx]  # Acceder al nombre de la zona
  
  # Crear datos para predicciones
  new_data <- data_frame(temp_mean = seq(min(births_weeks_temp$temp_mean, na.rm = TRUE),
                                         max(births_weeks_temp$temp_mean, na.rm = TRUE),
                                         length.out = 100),
                         week_gest_num = median(births_weeks_temp$week_gest_num, na.rm = TRUE))
  
  # Realizar predicciones
  predictions <- predict(mod, newdata = new_data, type = "response", se = TRUE)
  
  # Añadir los resultados de las predicciones a new_data
  new_data$predicted_fit = predictions$fit
  new_data$predicted_se = predictions$se.fit
  
  # Crear el gráfico
  ggplot(new_data, aes(x = temp_mean)) +
    geom_line(aes(y = predicted_fit)) +
    geom_ribbon(aes(ymin = predicted_fit - 1.96 * predicted_se, ymax = predicted_fit + 1.96 * predicted_se),
                alpha = 0.2, fill = "blue") +
    labs(x="Temperatura media", y="Estimación Peso",
         title=zona_name) + # Título específico por zona
    theme_light()
})



# Asegurarse de que `plots` es una lista de objetos ggplot
if(length(plots) > 1) {
  library(gridExtra)
  combined_plot <- gridExtra::grid.arrange(grobs = plots, ncol = 3)
  print(combined_plot)
} else {
  print(plots[[1]])
}

ggsave(combined_plot, 
       filename = paste0("Output_analysis/Fig/Trends_GAM", "_",Sys.Date(),".png"),
       res = 300,
       width = 25,
       height = 10,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)



### Humidity and weight 


## DLMNS MODELS ----