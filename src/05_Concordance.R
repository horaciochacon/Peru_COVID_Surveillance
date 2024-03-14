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

molecular <- read_csv("data/processed/molecular.gz")
positive  <- read_csv("data/processed/positives.gz")

peru_dep  <- read_sf("data/departamentos/DEPARTAMENTOS.shp")   
peru_prov <- read_sf("data/provincias/PROVINCIAS.shp")


# Filtering datasets ------------------------------------------------------

molecular <- molecular %>%
  filter(resultado == "POSITIVO")

positive <- positive %>% 
  filter(metododx == "PCR") %>% 
  mutate(week = week - 3)


# Time series -------------------------------------------------------------

ts_molecular <- molecular %>% 
  filter(week > "2020-03-01", week < "2022-04-22") %>% 
  group_by(week) %>% 
  count()

ts_positive <- positive %>% 
  filter(week > "2020-03-01", week < "2022-04-22") %>% 
  group_by(week) %>% 
  count()


ggplot() +
  geom_line(data = ts_molecular, aes(x = week, y = n), col = "red") +
  geom_line(data = ts_positive, aes(x = week, y = n), col = "blue") +
  theme_bw()

# Compare by department and province --------------------------------------

dep_muestra_molecular <- molecular %>% 
  filter(week > "2020-03-01", week < "2022-04-22") %>% 
  group_by(departamento_muestra) %>% 
  count()
  
dep_positive <- positive %>% 
  filter(week > "2020-03-01", week < "2022-04-22") %>% 
  group_by(departamento) %>% 
  count()
