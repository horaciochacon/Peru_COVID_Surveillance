library(data.table)
library(janitor)
library(lubridate)
library(AMR)

# Functions definition ----------------------------------------------------

missing_field <-  function(x) {
  list(miss = sum(is.na(x), na.rm = T), 
       miss_perc = (sum(is.na(x), na.rm = T) / length(x)))
} 

miss <- function(x) {
  is.na(max(x))
}

# Reading the data --------------------------------------------------------

na_string <- c(NA_character_, "", "NULL")

molecular <- fread(
  "data/pm31Jul2022.csv",
  na.strings = na_string
) %>% 
  clean_names() %>%
  mutate(
    departamento_paciente = ifelse(
      provincia_paciente == "CALLAO", "CALLAO", departamento_paciente)
    )

positives <- fread(
  "data/positivos_covid.csv", 
  na.strings = na_string
) %>% clean_names()

deaths <- fread(
  "data/fallecidos_covid.csv", 
  na.strings = na_string
) %>% clean_names()

suspected <- fread(
  "data/TB_F00_SICOVID.csv", 
  na.strings = na_string
) %>% clean_names() 

hospital  <- fread("data/TB_HOSP_VAC_FALLECIDOS.csv", na.strings = na_string)
attention <- fread("data/TB_ATEN_COVID19.csv", na.strings = na_string)

# Formatting the datasets --------------------------------------------------

molecular[
  , ':='(fecha_corte = ymd(fecha_corte),
         fecha_muestra = ymd(fecha_muestra),
         age_group = age_groups(edad),
         missing = apply(.SD, 1, miss)
  )
][
  , ':='(week = floor_date(fecha_muestra, "weeks", week_start = 1))
]


positives[
          , ':='(fecha_corte = ymd(fecha_corte),
                 fecha_resultado = ymd(fecha_resultado),
                 missing = apply(.SD, 1, miss))
][
  , ':='(week = floor_date(fecha_resultado, "weeks", week_start = 1))
]

suspected[
  , ':='(fecha_contacto = dmy(fecha_contacto),
         fecha_sintomas = dmy(fecha_sintomas),
         flag_sospechoso = as.factor(flag_sospechoso),
         missing = apply(.SD, 1, miss)),
  .SDcols = -"fecha_sintomas"
][
  , ':='(week = floor_date(fecha_contacto, "weeks", week_start = 1))
]

deaths[
  , ':='(fecha_corte = ymd(fecha_corte),
         fecha_fallecimiento = ymd(fecha_fallecimiento),
         age_group = age_groups(edad_declarada),
         missing = apply(.SD, 1, miss))
][
  , ':='(week = floor_date(fecha_fallecimiento, "weeks", week_start = 1))
]

hospital[
  , c("fecha_dosis1", "fecha_dosis2", "fecha_dosis3",
      "fecha_ingreso_hosp", "fecha_ingreso_uci", "fecha_ingreso_ucin",
      "fecha_segumiento_hosp_ultimo") := lapply(.SD, dmy),
  .SDcols = c("fecha_dosis1", "fecha_dosis2", "fecha_dosis3",
              "fecha_ingreso_hosp", "fecha_ingreso_uci", "fecha_ingreso_ucin",
              "fecha_segumiento_hosp_ultimo")
][
  , missing := apply(.SD, 1, miss)
]

attention[
  , c("fecha_alta", "fecha_alta_voluntaria", "fecha_fallecido",
      "fecha_referido") := lapply(.SD, dmy),
  .SDcols = c("fecha_alta", "fecha_alta_voluntaria", "fecha_fallecido",
              "fecha_referido")
][
  , missing := apply(.SD, 1, miss)
]

# Writing data ------------------------------------------------------------

fwrite(molecular, "data/processed/molecular.gz", compress = "gzip")
fwrite(positives, "data/processed/positives.gz", compress = "gzip")
fwrite(suspected, "data/processed/suspected.gz", compress = "gzip")
fwrite(deaths,    "data/processed/deaths.gz",    compress = "gzip")
fwrite(hospital,  "data/processed/hospital.gz",  compress = "gzip")
fwrite(attention, "data/processed/attention.gz", compress = "gzip")


