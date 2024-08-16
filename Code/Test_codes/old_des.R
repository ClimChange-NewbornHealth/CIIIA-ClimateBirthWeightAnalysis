# Code 7: Descriptive analysis ----

rm(list = ls())
source("Code/0.2 Settings.R")

# Optimal use power computation 
options(future.globals.maxSize = 3000 * 1024^2)  
plan(multisession, workers = detectCores() - 4) # Parallelization

path <- "Data/Output/"

## Temperature  ----

data_temp <- "births_2011_2020_weeks_temp_final.RData"
load(paste0(path, data_temp))
glimpse(births_weeks_temp)
length(unique(births_weeks_temp$id)) # 361913

### Remove NA ----

ids_con_na <- births_weeks_temp %>%
  group_by(id) %>%                       
  filter(any(is.na(temp_mean))) %>% 
  ungroup()

ids_na <- unique(ids_con_na$id)

(length(ids_na)/361913)*100 # 8,785261%

### Information NA ----
# Solo los partes de Sept/Oct
ids_con_na %>% 
  distinct(id, .keep_all = TRUE) %>% 
  group_by(year(week_gest)) %>% 
  summarise(n=n()) %>% 
  ungroup()

ids_con_na %>% 
  distinct(id, .keep_all = TRUE) %>% 
  group_by(year(week_gest), week_gest_num) %>% 
  summarise(n=n()) %>% 
  ungroup()

ids_con_na %>% 
  distinct(id, .keep_all = TRUE) %>% 
  group_by(week_gest_num) %>% 
  summarise(n=n()) %>% 
  ungroup()

ids_con_na %>% 
  distinct(id, .keep_all = TRUE) %>% 
  group_by(zona) %>% 
  summarise(n=n()) %>% 
  ungroup()

### Final data ----

(length(ids_na)/361913)*100 # 8,785261%

births_weeks_temp <- births_weeks_temp %>% 
  filter(!id %in% ids_na)

length(unique(births_weeks_temp$id)) # 330118

save(births_weeks_temp, file=paste0(path, "births_2011_2020_weeks_temp_models", ".RData"))

### Trends observations ----

id_n <- sample(unique(births_weeks_temp$id[births_weeks_temp$zone=="North"]), 5)
id_c <- sample(unique(births_weeks_temp$id[births_weeks_temp$zone=="Center"]), 5)
id_s <- sample(unique(births_weeks_temp$id[births_weeks_temp$zone=="South"]), 5)

trends <- births_weeks_temp %>% 
  filter(id %in% c(id_n, id_c, id_s))

pal <- c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", 
         "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#00A087B2", 
         "#91D1C2B2", "#DC0000B2", "#3C5488B2", "#F39B7FB2", "#E64B35B2")

g1 <- trends %>% 
  ggplot(aes(x=week_gest_num, y=temp_min, color=factor(id))) +
  geom_point(size=0.75) +
  geom_line() +
  labs(y="Minimum Temperature", x="Gestational Week") +
  scale_y_continuous(limits=c(-10, 40), breaks = seq(-10, 40, by=10)) +
  facet_wrap(~zone) +
  scale_color_manual(values = pal, name="ID") +
  theme_light() +
  theme(strip.background = element_blank(),
        strip.text = element_text(color = "black"),
        legend.position = "right")

# g2 <- trends %>% 
#   ggplot(aes(x=week_gest_num, y=temp_mean_min, color=factor(id))) +
#   geom_point(size=0.75) +
#   geom_line() +
#   labs(y="Temperatura min (mean)", x="Semana gestacional") +
#   facet_wrap(~zona) +
#   scale_color_manual(values = pal, name="ID") +
#   theme_light() +
#   theme(strip.background = element_blank(),
#         strip.text = element_text(color = "black"),
#         legend.position = "right")

g3 <- trends %>% 
  ggplot(aes(x=week_gest_num, y=temp_mean, color=factor(id))) +
  geom_point(size=0.75) +
  geom_line() +
  labs(y="Mean Temperature", x="Gestational Week") +
  scale_y_continuous(limits=c(-10, 40), breaks = seq(-10, 40, by=10)) +
  facet_wrap(~zone) +
  scale_color_manual(values = pal, name="ID") +
  theme_light() +
  theme(strip.background = element_blank(),
        strip.text = element_text(color = "black"),
        legend.position = "right")

# g4 <- trends %>% 
#   ggplot(aes(x=week_gest_num, y=temp_mean_max, color=factor(id))) +
#   geom_point(size=0.75) +
#   geom_line() +
#   labs(y="Temperatura max (mean)", x="Semana gestacional") +
#   facet_wrap(~zona) +
#   scale_color_manual(values = pal, name="ID") +
#   theme_light() +
#   theme(strip.background = element_blank(),
#         strip.text = element_text(color = "black"),
#         legend.position = "right")


g5 <- trends %>% 
  ggplot(aes(x=week_gest_num, y=temp_max, color=factor(id))) +
  geom_point(size=0.75) +
  geom_line() +
  labs(y="Maximum Temperature", x="Gestational Week") +
  scale_y_continuous(limits=c(-10, 40), breaks = seq(-10, 40, by=10)) +
  facet_wrap(~zone) +
  scale_color_manual(values = pal, name="ID") +
  theme_light() +
  theme(strip.background = element_blank(),
        strip.text = element_text(color = "black"),
        legend.position = "right")

ggarrange(g1, g3, g5, common.legend = TRUE, ncol = 1, legend="right")

ggsave(filename = paste0("Output_analysis/Fig/Trends_id",".png"),
       res = 300,
       width = 35,
       height = 20,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)

### Trends global temperature ----

# Temp - time-gestational 

temp <- births_weeks_temp %>% 
  group_by(zone, week_gest) %>% 
  summarise(temp_media = mean(temp_mean, na.rm = TRUE), 
            temp_min = mean(temp_min, na.rm = TRUE), 
            temp_max = mean(temp_max, na.rm = TRUE), 
            peso = mean(peso, na.rm = TRUE)) %>% 
  pivot_longer(!c(zone, week_gest, peso), 
               names_to = "temperatures", 
               values_to = "values")

ggplot(temp, aes(x = week_gest)) +
  geom_line(aes(y = values, 
                color=factor(temperatures, 
                             labels = c("Maximum", "Mean", "Minimum")
                             ))) + 
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y") +
  facet_wrap(~ factor(zone), scales = "free_x", ncol=1) + 
  labs(x = "", y = "Temperatura") +
  theme_light() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        strip.background = element_blank(),
        strip.text = element_text(color="black", face = "bold")) 

ggsave(filename = paste0("Output_analysis/Fig/Trends_temp_time", ".png"),
       res = 300,
       width = 30,
       height = 20,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)

### Summary stats ----

#### Total obs -----

births_weeks_temp %>% 
  distinct(id, .keep_all = TRUE) %>% 
  summarise(n=n()) # 330118

births_weeks_temp  %>% 
  ggbetweenstats(x=zona, 
                 y=peso, 
                 type = "parametric",
                 pairwise.comparisons = TRUE,
                 centrality.label.args = list(alpha = 0),
                 package = "ggsci",
                 palette = "default_igv",
                 point.args = list(alpha = 0),
                 #centrality.point.args = list(alpha = 0),
                 point.path = FALSE
  ) +
  ggplot2::labs(x="Climatic Zone", y="tWB") +
  theme_light() +
  theme(axis.text.y = element_text(size=8, angle=0), 
        legend.position = "none",
        legend.title = element_blank(),
        legend.background = element_rect(color=NA),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        axis.text.x = element_text(size=10, angle=0, vjust=0.70, hjust=0.70),
        plot.title = element_text(size=12, hjust=0, face="bold"),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=10, hjust = 0))

ggsave(filename = paste0("Output_analysis/Fig/tBW_zone", "_",Sys.Date(),".png"),
       res = 300,
       width = 25,
       height = 20,
       units = 'cm',
       scaling = 0.775,
       device = ragg::agg_png)

#### Zone obs -----
births_weeks_temp %>% 
  distinct(id, .keep_all = TRUE) %>% 
  group_by(zone) %>% 
  summarise(n=n()) # N: 96918 C: 160056 S: 73144 

#### Mean -----

# Entire Pregnacy average 
t1 <- births_weeks_temp %>% 
  summarise(
    n=n(), 
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

# Climate zones  
t2 <- births_weeks_temp %>% 
  group_by(zone) %>% 
  summarise(
    n=n(), 
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
t3 <- births_weeks_temp %>% 
  #filter(zone=="North") %>% 
  group_by(clim_zone, trim_gest) %>% 
  summarise(
    n=n(), 
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

tables <- list("all" = t1, 
               "zone" = t2,
               "zone-trim" = t3)

write.xlsx(tables, file =  paste0("Output_analysis/Tables/Descriptive_temp_mean", "_",Sys.Date(),".xlsx"))


#### Min -----

# Entire Pregnacy average 
t1 <- births_weeks_temp %>% 
  summarise(
    n=n(), 
    mean=mean(temp_min, na.rm = TRUE),
    min=min(temp_min, na.rm = TRUE),
    q05 = quantile(temp_min, probs = 0.05, na.rm = TRUE), 
    q10 = quantile(temp_min, probs = 0.10, na.rm = TRUE), 
    q20 = quantile(temp_min, probs = 0.20, na.rm = TRUE), 
    q30 = quantile(temp_min, probs = 0.30, na.rm = TRUE), 
    q40 = quantile(temp_min, probs = 0.40, na.rm = TRUE), 
    q50 = quantile(temp_min, probs = 0.50, na.rm = TRUE), 
    q60 = quantile(temp_min, probs = 0.60, na.rm = TRUE), 
    q70 = quantile(temp_min, probs = 0.70, na.rm = TRUE), 
    q80 = quantile(temp_min, probs = 0.80, na.rm = TRUE), 
    q90 = quantile(temp_min, probs = 0.90, na.rm = TRUE), 
    q95 = quantile(temp_min, probs = 0.95, na.rm = TRUE),
    max=max(temp_min, na.rm = TRUE)
  )

# Climate zones  
t2 <- births_weeks_temp %>% 
  group_by(zona) %>% 
  summarise(
    n=n(), 
    mean=mean(temp_min, na.rm = TRUE),
    min=min(temp_min, na.rm = TRUE),
    q05 = quantile(temp_min, probs = 0.05, na.rm = TRUE), 
    q10 = quantile(temp_min, probs = 0.10, na.rm = TRUE), 
    q20 = quantile(temp_min, probs = 0.20, na.rm = TRUE), 
    q30 = quantile(temp_min, probs = 0.30, na.rm = TRUE), 
    q40 = quantile(temp_min, probs = 0.40, na.rm = TRUE), 
    q50 = quantile(temp_min, probs = 0.50, na.rm = TRUE), 
    q60 = quantile(temp_min, probs = 0.60, na.rm = TRUE), 
    q70 = quantile(temp_min, probs = 0.70, na.rm = TRUE), 
    q80 = quantile(temp_min, probs = 0.80, na.rm = TRUE), 
    q90 = quantile(temp_min, probs = 0.90, na.rm = TRUE), 
    q95 = quantile(temp_min, probs = 0.95, na.rm = TRUE),
    max=max(temp_min, na.rm = TRUE)
  )

# Trimester average 
t3 <- births_weeks_temp %>% 
  group_by(zona, trim_gest) %>% 
  summarise(
    n=n(), 
    mean=mean(temp_min, na.rm = TRUE),
    min=min(temp_min, na.rm = TRUE),
    q05 = quantile(temp_min, probs = 0.05, na.rm = TRUE), 
    q10 = quantile(temp_min, probs = 0.10, na.rm = TRUE), 
    q20 = quantile(temp_min, probs = 0.20, na.rm = TRUE), 
    q30 = quantile(temp_min, probs = 0.30, na.rm = TRUE), 
    q40 = quantile(temp_min, probs = 0.40, na.rm = TRUE), 
    q50 = quantile(temp_min, probs = 0.50, na.rm = TRUE), 
    q60 = quantile(temp_min, probs = 0.60, na.rm = TRUE), 
    q70 = quantile(temp_min, probs = 0.70, na.rm = TRUE), 
    q80 = quantile(temp_min, probs = 0.80, na.rm = TRUE), 
    q90 = quantile(temp_min, probs = 0.90, na.rm = TRUE), 
    q95 = quantile(temp_min, probs = 0.95, na.rm = TRUE),
    max=max(temp_min, na.rm = TRUE)
  )

tables <- list("all" = t1, 
               "zone" = t2,
               "zone-trim" = t3)

write.xlsx(tables, file =  paste0("Output_analysis/Tables/Descriptive_temp_min", "_",Sys.Date(),".xlsx"))

#### Max -----

# Entire Pregnacy average 
t1 <- births_weeks_temp %>% 
  summarise(
    n=n(), 
    mean=mean(temp_max, na.rm = TRUE),
    min=min(temp_max, na.rm = TRUE),
    q05 = quantile(temp_max, probs = 0.05, na.rm = TRUE), 
    q10 = quantile(temp_max, probs = 0.10, na.rm = TRUE), 
    q20 = quantile(temp_max, probs = 0.20, na.rm = TRUE), 
    q30 = quantile(temp_max, probs = 0.30, na.rm = TRUE), 
    q40 = quantile(temp_max, probs = 0.40, na.rm = TRUE), 
    q50 = quantile(temp_max, probs = 0.50, na.rm = TRUE), 
    q60 = quantile(temp_max, probs = 0.60, na.rm = TRUE), 
    q70 = quantile(temp_max, probs = 0.70, na.rm = TRUE), 
    q80 = quantile(temp_max, probs = 0.80, na.rm = TRUE), 
    q90 = quantile(temp_max, probs = 0.90, na.rm = TRUE), 
    q95 = quantile(temp_max, probs = 0.95, na.rm = TRUE),
    max=max(temp_max, na.rm = TRUE)
  )

# Climate zones  
t2 <- births_weeks_temp %>% 
  group_by(zona) %>% 
  summarise(
    n=n(), 
    mean=mean(temp_max, na.rm = TRUE),
    min=min(temp_max, na.rm = TRUE),
    q05 = quantile(temp_max, probs = 0.05, na.rm = TRUE), 
    q10 = quantile(temp_max, probs = 0.10, na.rm = TRUE), 
    q20 = quantile(temp_max, probs = 0.20, na.rm = TRUE), 
    q30 = quantile(temp_max, probs = 0.30, na.rm = TRUE), 
    q40 = quantile(temp_max, probs = 0.40, na.rm = TRUE), 
    q50 = quantile(temp_max, probs = 0.50, na.rm = TRUE), 
    q60 = quantile(temp_max, probs = 0.60, na.rm = TRUE), 
    q70 = quantile(temp_max, probs = 0.70, na.rm = TRUE), 
    q80 = quantile(temp_max, probs = 0.80, na.rm = TRUE), 
    q90 = quantile(temp_max, probs = 0.90, na.rm = TRUE), 
    q95 = quantile(temp_max, probs = 0.95, na.rm = TRUE),
    max=max(temp_max, na.rm = TRUE)
  )

# Trimester average 
t3 <- births_weeks_temp %>% 
  group_by(zona, trim_gest) %>% 
  summarise(
    n=n(), 
    mean=mean(temp_max, na.rm = TRUE),
    min=min(temp_max, na.rm = TRUE),
    q05 = quantile(temp_max, probs = 0.05, na.rm = TRUE), 
    q10 = quantile(temp_max, probs = 0.10, na.rm = TRUE), 
    q20 = quantile(temp_max, probs = 0.20, na.rm = TRUE), 
    q30 = quantile(temp_max, probs = 0.30, na.rm = TRUE), 
    q40 = quantile(temp_max, probs = 0.40, na.rm = TRUE), 
    q50 = quantile(temp_max, probs = 0.50, na.rm = TRUE), 
    q60 = quantile(temp_max, probs = 0.60, na.rm = TRUE), 
    q70 = quantile(temp_max, probs = 0.70, na.rm = TRUE), 
    q80 = quantile(temp_max, probs = 0.80, na.rm = TRUE), 
    q90 = quantile(temp_max, probs = 0.90, na.rm = TRUE), 
    q95 = quantile(temp_max, probs = 0.95, na.rm = TRUE),
    max=max(temp_max, na.rm = TRUE)
  )

tables <- list("all" = t1, 
               "zone" = t2,
               "zone-trim" = t3)

write.xlsx(tables, file =  paste0("Output_analysis/Tables/Descriptive_temp_max", "_",Sys.Date(),".xlsx"))



