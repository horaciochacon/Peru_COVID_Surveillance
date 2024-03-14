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

suspected <- read_csv("data/processed/suspected.gz")
positive  <- read_csv("data/processed/positives.gz")
hospital  <- read_csv("data/processed/hospital.gz")

peru_dep  <- read_sf("data/departamentos/DEPARTAMENTOS.shp")   
peru_prov <- read_sf("data/provincias/PROVINCIAS.shp")


# Joining datasets --------------------------------------------------------

# Suspected - Positive
sus_pos <- suspected %>% 
  left_join(positive, by = "id_persona") %>% 
  select(id_persona, departamento, provincia, distrito, fecha_contacto, edad,
         sexo, fecha_sintomas, fecha_resultado, flag_sospechoso, metododx) %>% 
  filter(!is.na(fecha_resultado)) %>% 
  mutate(
    valid = as.numeric(fecha_resultado) <= (as.numeric(fecha_sintomas) + 30) &
         as.numeric(fecha_resultado) >= (as.numeric(fecha_sintomas) - 10)
  ) %>%
  filter(valid) %>%
  mutate(
    timediff_res_sympt = as.numeric(
      difftime(fecha_resultado, fecha_sintomas, units = "days")
      ),
    timediff_sympt_cont = as.numeric(
      difftime(fecha_contacto, fecha_sintomas, units = "days")
    )
  )

sus_pos_dpto <- sus_pos %>% 
  group_by(departamento) %>% 
  summarise(
    timediff_res_sympt_mean = mean(timediff_res_sympt, na.rm = TRUE), 
    timediff_sympt_cont_mean = mean(timediff_sympt_cont, na.rm = TRUE),
    )

sus_pos_prov<- sus_pos %>% 
  group_by(departamento,  provincia) %>% 
  summarise(
    timediff_res_sympt_mean = mean(timediff_res_sympt, na.rm = TRUE), 
    timediff_sympt_cont_mean = mean(timediff_sympt_cont, na.rm = TRUE),
  )

sus_pos_date <- sus_pos %>% 
  group_by(
    fecha_sintomas = floor_date(fecha_sintomas, unit = "weeks", week_start = 1)
    ) %>% 
  summarise(
    timediff_res_sympt_mean = mean(timediff_res_sympt, na.rm = TRUE), 
    timediff_sympt_cont_mean = mean(timediff_sympt_cont, na.rm = TRUE),
  )

sus_pos_dx <- sus_pos %>% 
  group_by(
    fecha_sintomas = floor_date(fecha_sintomas, unit = "weeks", week_start = 1),
    metododx
  ) %>% 
  summarise(
    timediff_res_sympt_mean = mean(timediff_res_sympt, na.rm = TRUE), 
    timediff_sympt_cont_mean = mean(timediff_sympt_cont, na.rm = TRUE),
  )


# Suspected - Hospital

sus_hos <- suspected %>% 
  left_join(hospital, by = "id_persona") %>% 
  select(id_persona, dep_domicilio, prov_domicilio, dist_domicilio,
         fecha_contacto, edad, sexo, fecha_sintomas, fecha_ingreso_hosp) %>% 
  filter(!is.na(fecha_ingreso_hosp)) %>% 
  mutate(
    valid = as.numeric(fecha_ingreso_hosp) <= (as.numeric(fecha_sintomas) + 60) &
      as.numeric(fecha_ingreso_hosp) >= (as.numeric(fecha_sintomas) - 60)
  ) %>%
  filter(valid, !is.na(fecha_sintomas)) %>%
  mutate(
    timediff = as.numeric(
      difftime(fecha_ingreso_hosp,fecha_sintomas, units = "days")
      )
  )

sus_hos_dpto <- sus_hos %>% 
  group_by(dep_domicilio) %>% 
  summarise(
    timediff_mean = mean(timediff, na.rm = TRUE), 
    timediff_median = median(timediff, na.rm = TRUE)
  )

sus_hos_prov <- sus_hos %>% 
  group_by(dep_domicilio, prov_domicilio) %>% 
  summarise(
    timediff_mean = mean(timediff, na.rm = TRUE), 
    timediff_median = median(timediff, na.rm = TRUE)
  )

sus_hos_sintomas <- sus_hos %>% 
  group_by(weefecha_sintomas) %>% 
  summarise(
    timediff_mean = mean(timediff, na.rm = TRUE), 
    timediff_median = median(timediff, na.rm = TRUE)
  )



# Plots -------------------------------------------------------------------

# Suspected - Positive

(sus_pos_dx %>% 
   pivot_longer(cols = 3:4) %>% 
   filter(year(fecha_sintomas) >= 2020) %>% 
   ggplot(aes(x = fecha_sintomas, y = value, col = name)) +
   facet_grid(metododx ~ .) +
   geom_line() +
   ylim(0, 20) +
   theme_bw() +
   scale_color_manual(
     name = "Indicator", 
     values = c(
       "timediff_res_sympt_mean" = "blue",
       "timediff_sympt_cont_mean" = "red"
     ),
     labels = c("Positive results - symptoms", "Contact - symptoms")
   ) +
   labs(
     title = "Timeliness indicators by diagnostic method",
     x = "Date of onset (symptoms)", 
     y = "Time (in days)"
   )
) %>% 
  ggsave(
    filename = "output/timeliness/plot_sus_pos_dx.png",
    plot = .
  )


(sus_pos_date %>% 
  pivot_longer(cols = 2:3) %>% 
  filter(year(fecha_sintomas) >= 2020) %>% 
  ggplot(aes(x = fecha_sintomas, y = value, col = name)) +
  geom_line() +
  ylim(0, 30) +
  theme_bw() +
  scale_color_manual(
    name = "Indicator", 
    values = c(
      "timediff_res_sympt_mean" = "blue",
      "timediff_sympt_cont_mean" = "red"
      ),
    labels = c("Positive results - symptoms", "Contact - symptoms")
    ) +
  labs(
    title = "Timeliness indicators",
    x = "Date of onset (symptoms)", 
    y = "Time (in days)"
    )
) %>% 
  ggsave(
    filename = "output/timeliness/plot_sus_pos_date.png",
    plot = .
  )

peru_dep <- peru_dep %>% 
  left_join(sus_pos_dpto, by = c("DEPARTAMEN" = "departamento"))

(peru_dep %>%
  ggplot() +
  geom_sf(aes(fill = timediff_res_sympt_mean), size = 0.05, color = "grey40") +
  theme_map() +
  ggtitle("Average time between symptoms and positive results") +
  scale_fill_viridis(
    option = "mako", 
    direction = -1,
    name = "Time\n(in days)",
    na.value = "white"
  )
) %>% 
  ggsave(
    filename = "output/timeliness/map_sus_pos_res_sympt_dep.png",
    plot = .
  )

(peru_dep %>%
  ggplot() +
  geom_sf(aes(fill = timediff_sympt_cont_mean), size = 0.05, color = "grey40") +
  theme_map() +
  ggtitle("Average time between symptoms and government contact") +
  scale_fill_viridis(
    option = "mako", 
    direction = -1,
    name = "Time\n(in days)",
    na.value = "white",
  )
) %>% 
  ggsave(
    filename = "output/timeliness/map_sus_pos_cont_sympt_dep.png",
    plot = .
  )

peru_prov <- peru_prov %>% 
  left_join(
    sus_pos_prov, 
    by = c(
      "DEPARTAMEN" = "departamento",
      "PROVINCIA" = "provincia"
    )
  )

(peru_prov %>%
    ggplot() +
    geom_sf(aes(fill = timediff_res_sympt_mean), size = 0.05, color = "grey40") +
    theme_map() +
    ggtitle("Average time between symptoms and positive results") +
    scale_fill_viridis(
      option = "mako", 
      direction = -1,
      name = "Time\n(in days)",
      na.value = "white"
    )
) %>% 
  ggsave(
    filename = "output/timeliness/map_sus_pos_res_sympt_prov.png",
    plot = .
  )

(peru_prov %>%
    ggplot() +
    geom_sf(aes(fill = timediff_sympt_cont_mean), size = 0.05, color = "grey40") +
    theme_map() +
    ggtitle("Average time between symptoms and government contact") +
    scale_fill_viridis(
      option = "mako", 
      direction = -1,
      name = "Time\n(in days)",
      na.value = "white",
    )
) %>% 
  ggsave(
    filename = "output/timeliness/map_sus_pos_cont_sympt_prov.png",
    plot = .
  )


# Suspected - Hospital

# Suspected - Positive
sus_hos_sintomas %>% 
  ggplot(aes(x = fecha_sintomas, y = timediff_mean)) +
  geom_line()

peru_dep <- peru_dep %>% 
  left_join(sus_hos_dpto, by = c("DEPARTAMEN" = "dep_domicilio"))

peru_dep %>%
  filter(DEPARTAMEN != "CALLAO") %>% 
  ggplot() +
  geom_sf(aes(fill = timediff_mean), size = 0.05, color = "grey40") +
  theme_map() +
  theme(legend.position = "top") +
  scale_fill_viridis(
    option = "mako", 
    direction = -1,
    name = "Time between simptoms and Hospitalization",
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

