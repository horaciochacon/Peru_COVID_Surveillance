library(data.table)
library(tidyverse)
library(lubridate)
library(sf)
library(scales)
library(colorspace)
library(cowplot)
library(viridis)
source("R/functions.R")

# Reading data ------------------------------------------------------------
datasets <- c("molecular", "positives", "deaths", "suspected")
i <- 1

data <- fread(paste0("data/processed/", datasets[i],".gz"))
peru_dep  <- read_sf("data/departamentos/DEPARTAMENTOS.shp")   
peru_prov <- read_sf("data/provincias/PROVINCIAS.shp")


# Completeness ------------------------------------------------------------

# Time series -------------------------------------------------------------

fecha  <- c(
  "week", "week", "week", "week"
  )
var_by <- c("sexo", "metododx", "age_group", "flag_sospechoso")

ts <- data[
  ,.(missing = sum(missing), 
     .N,
     perc_miss = sum(missing)/.N),
  by = eval((fecha[i]))
]

ts_by <- data[
  ,.(missing = sum(missing), 
     .N, 
     perc_miss = sum(missing)/.N),
  by = eval(c((fecha[i]), (var_by[i])))
]

plot_count <- ts %>% 
  filter(year(.data[[fecha[i]]]) >= 2020) %>% 
  ggplot(aes(x = .data[[fecha[i]]])) + 
  geom_line(aes(y = N)) +
  labs(
    title = paste0("Case count - Weekly"," (", datasets[i], " dataset)"),
    y = "Number of tests", 
    x = "Date") +
  theme_bw()

ggsave(
  filename = paste0("output/plot_count_", datasets[i],".png"),
  plot = plot_count, device = "png"
  )

plot_missing <- ts %>% 
  filter(
    .data[[fecha[i]]] >= "2020-03-01"
  ) %>% 
  ggplot(aes(x = .data[[fecha[i]]])) + 
  geom_line(aes(y = perc_miss)) +
  labs(
    title = paste0(
      "Percentage of missing - Weekly",
      " (",
      datasets[i],
      " dataset)"
    ),
    y = "Percentage of missing",
    x = "Date") +
  theme_bw() 

ggsave(
  filename = paste0("output/plot_missing_", datasets[i],".png"),
  plot = plot_missing, device = "png"
)

plot_missing_by <- ts_by %>% 
  filter(
    .data[[fecha[i]]] >= "2020-03-01"
  ) %>% 
  ggplot(aes(x = .data[[fecha[i]]], col = as.factor(.data[[var_by[i]]]))) + 
  geom_line(aes(y = perc_miss)) +
  labs(
    col = var_by[i],
    title = paste0(
      "Percentage of missing - Weekly",
      " (",
      datasets[i],
      " dataset)"
      ),
    y = "Percentage of missing",
    x = "Date") +
  theme_bw()

ggsave(
  filename = paste0("output/plot_missing_by_", datasets[i],".png"),
  plot = plot_missing_by, device = "png"
)

# Maps --------------------------------------------------------------------
department <- c("departamento_muestra", "departamento", "departamento")
province   <- c("provincia_muestra", "provincia", "provincia")

departments <- data[
  ,.(missing = sum(missing), 
     .N, 
     perc_miss = sum(missing)/.N),
  by = eval((department[i]))
]

provinces <- data[
  ,.(missing = sum(missing), 
     .N, 
     perc_miss = sum(missing)/.N),
  by = eval(c((department[i]), (province[i])))
]

peru_dep <- peru_dep %>% 
  left_join(departments, by = c("DEPARTAMEN" = department[i]))


map_dep <- peru_dep %>%
  filter(DEPARTAMEN != "CALLAO") %>% 
  ggplot() +
  geom_sf(aes(fill = perc_miss), size = 0.05, color = "grey40") +
  theme_map() +
  theme(legend.position = "top") +
  scale_fill_viridis(
    option = "mako", 
    direction = -1,
    name = paste0("Missing % - ", datasets[i]),
    na.value = "white",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      reverse = TRUE,
      title.position = 'top',
      title.hjust = 0.5
    )
  )

ggsave(
  filename = paste0("output/map_dep_missing_", datasets[i],".png"),
  plot = map_dep, device = "png"
)


peru_prov <- peru_prov %>% 
  left_join(
    provinces, 
    by = c(
      "DEPARTAMEN" = department[i],
      "PROVINCIA" = province[i]
      )
  )

map_prov <- peru_prov %>%
  ggplot() +
  geom_sf(aes(fill = perc_miss), size = 0.05, color = "grey40") +
  theme_map() +
  theme(legend.position = "top") +
  scale_fill_viridis(
    option = "mako", 
    direction = -1,
    name = paste0("Missing % - ", datasets[i]),
    na.value = "white",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      reverse = TRUE,
      title.position = 'top',
      title.hjust = 0.5
    )
  )

ggsave(
  filename = paste0("output/map_prov_missing_", datasets[i],".png"),
  plot = map_prov, device = "png"
)




# Validity ----------------------------------------------------------------

# Time series -------------------------------------------------------------

fecha_eval  <- c(
  "fecha_muestra", "fecha_resultado", "fecha_fallecimiento", "fecha_contacto"
)

var_by <- c("sexo", "metododx", "age_group", "flag_sospechoso")
var_by2 <- c("institucion", "sexo", "clasificacion_def", NA)

data <- data %>% 
  mutate(
    wrong_date = !between(
      .data[[fecha_eval[i]]], as.Date("2020-03-01"), as.Date("2022-08-01")
    )
  )

group1 <- data %>% 
  group_by(.data[[var_by[i]]]) %>% 
  summarise(
    wrong_date = sum(wrong_date), 
    n = n() , 
    perc_wrong_date = wrong_date/n
      )

group2 <- data %>% 
  group_by(.data[[var_by2[i]]]) %>% 
  summarise(
    wrong_date = sum(wrong_date), 
    n = n() , 
    perc_wrong_date = wrong_date/n
  )


plot_wrong_dates1 <-  group1 %>% 
  filter(wrong_date >= 1) %>% 
  ggplot(aes(x = .data[[var_by[i]]], y = perc_wrong_date )) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = paste0(
      "Wrong dates",
      " (",
      datasets[i],
      " dataset)"
    ),
    y = "Wrong dates",
    x = var_by) +
  theme_bw() 

ggsave(
  filename = paste0("output/plot_wrong_dates1_", datasets[i],".png"),
  plot = plot_wrong_dates1, device = "png"
)


plot_wrong_dates2 <-  group2 %>% 
  filter(wrong_date >= 1) %>% 
  ggplot(aes(x = .data[[var_by2[i]]], y = perc_wrong_date )) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = paste0(
      "Wrong dates",
      " (",
      datasets[i],
      " dataset)"
    ),
    y = "Wrong dates",
    x = var_by2) +
  theme_bw() 

ggsave(
  filename = paste0("output/plot_wrong_dates2_", datasets[i],".png"),
  plot = plot_wrong_dates2, device = "png", scale = 2
)

