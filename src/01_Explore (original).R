library(tidyverse)
library(janitor)
library(dm)
library(VennDiagram)

deaths      <- read_delim("data/fallecidos_covid.csv", delim = ";") %>% 
  clean_names()
molecular   <- read_delim("data/pm21Septiembre2021.csv", delim = "|") %>% 
  clean_names()
positives   <- read_delim("data/positivos_covid.csv", delim = ";") %>% 
  clean_names()
suspected   <- read_csv("data/TB_F00_SICOVID.csv")
attentions  <- read_csv("data/TB_ATEN_COVID19.csv")
hospitalization    <- read_csv("data/TB_HOSP_VAC_FALLECIDOS.csv") %>% 
  select(eess_renaes:evolucion_hosp_ultimo)
facilities  <- hospitalization %>% 
  select(id_eess, eess_nombre) %>% 
  distinct()
people      <- read_delim("data/positivos_covid.csv", delim = ";") %>% 
  select(id_persona, EDAD, SEXO) %>% 
  group_by(id_persona) %>% 
  summarize(edad = first(EDAD), sexo = first(SEXO))

data_model <- dm(people, hospitalization, facilities, attentions, suspected,
                 positives)

mod <- data_model %>% 
  dm_add_pk(people, id_persona) %>% 
  dm_add_pk(facilities, id_eess) %>% 
  dm_add_fk(hospitalization, id_persona, people) %>% 
  dm_add_fk(hospitalization, id_eess, facilities) %>% 
  dm_add_fk(attentions, id_persona, people) %>% 
  dm_add_fk(attentions, id_eess, facilities) %>% 
  dm_add_fk(suspected, id_persona, people) %>% 
  dm_add_fk(positives, id_persona, people) %>% 
  dm_set_colors(
    "lightpink" = hospitalization,
    "lightblue" = attentions,
    "lightgreen" = suspected,
    "lightgrey" = facilities,
    "lightyellow" = positives
  )

mod %>% 
  dm_draw(rankdir = "RB", columnArrows = FALSE,view_type = "all")


data_model <- dm(molecular, deaths)

mod <- data_model %>% 
  dm_add_pk(molecular, uuid) %>% 
  dm_add_fk(deaths, uuid, molecular) %>% 
  dm_set_colors(
    "lightsteelblue" =  molecular,
    "lightcyan" = deaths,
  )

mod %>% 
  dm_draw(rankdir = "LR", columnArrows = FALSE, view_type = "all")


# Venn Diagram ------------------------------------------------------------

att <- attentions$id_persona %>% na.omit()
pos <- positives$id_persona %>% na.omit()
hos <- hospitalization$id_persona %>% na.omit()
sus <- suspected$id_persona %>% na.omit()

library(RColorBrewer)
myCol <- brewer.pal(3, "Pastel2")

# Chart
venn.diagram(
  x = list(att, pos, sus),
  category.names = c(
    "Attentions" , "Positives", "Triage"),
  filename = '#15_venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 1500 , 
  width = 1500 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  # cat.cex = 0.6,
  # cat.fontface = "bold",
  # cat.default.pos = "outer",
  # cat.fontfamily = "sans",
  # rotation = 1
  
)
