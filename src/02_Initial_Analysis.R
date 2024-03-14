library(data.table)
library(magrittr)
library(janitor)
library(lubridate)
library(AMR)
options(na.rm = TRUE)

# Functions definition ----------------------------------------------------

missing_field <-  function(x) {
  list(miss = sum(is.na(x), na.rm = T), 
       miss_perc = (sum(is.na(x), na.rm = T) / length(x)))
} 

miss <- function(x) {
  is.na(max(x))
}

# Reading data ------------------------------------------------------------

molecular <- fread("data/processed/molecular.gz")
positives <- fread("data/processed/positives.gz")
suspected <- fread("data/processed/suspected.gz")
deaths    <- fread("data/processed/deaths.gz")
hospital  <- fread("data/processed/hospital.gz")
attention <- fread("data/processed/attention.gz")

# Completeness ------------------------------------------------------------

## Molecular
### 1a

molecular[
  ,.(missing = sum(missing), .N, perc_miss = sum(missing)/.N), 
  by = sexo
][order(-perc_miss)]

molecular[
  ,.(missing = sum(missing), .N, perc_miss = sum(missing)/.N), 
  by = age_group
][order(-perc_miss)]

molecular[
  ,.(missing = sum(missing), .N, perc_miss = sum(missing)/.N), 
  by = departamento_paciente
][order(-perc_miss)]

molecular[
  ,.(missing = sum(missing), .N, perc_miss = sum(missing)/.N), 
  by = departamento_muestra
][order(-perc_miss)]

molecular[
  ,.(missing = sum(missing), .N, perc_miss = sum(missing)/.N), 
  by = institucion
][order(-perc_miss)]

molecular[
  ,.(missing = sum(missing), .N, perc_miss = sum(missing)/.N), 
  by = .(year(fecha_muestra), month(fecha_muestra))
][order(-year, -month)]

### 1b

molecular[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing", "age_group")
]

molecular[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing", "age_group"),
  by = sexo
]

molecular[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing", "age_group"),
  by = age_group
]

molecular[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing", "age_group"),
  by = departamento_paciente
]

molecular[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing", "age_group"),
  by = departamento_muestra
]

molecular[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing", "age_group"),
  by = institucion
]

molecular[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing", "age_group"),
  by = .(year(fecha_muestra), month(fecha_muestra))
][order(-year, -month)]

## Positives
### 1a

positives[
  ,.(missing = sum(missing, na.rm = T), 
     .N, perc_miss = sum(missing, na.rm = T)/.N), 
  by = sexo
][order(-perc_miss)]

positives[
   ,.(missing = sum(missing, na.rm = T),
      .N, perc_miss = sum(missing, na.rm = T)/.N),  
  by = age_group
][order(-perc_miss)]

positives[
   ,.(missing = sum(missing, na.rm = T),  
      .N, perc_miss = sum(missing, na.rm = T)/.N),  
  by = departamento
][order(-perc_miss)]

positives[
   ,.(missing = sum(missing, na.rm = T), 
      .N, perc_miss = sum(missing, na.rm = T)/.N),  
  by = metododx
][order(-perc_miss)]

# positives[
#    ,.(missing = sum(missing, na.rm = T),   
#       .N, perc_miss = sum(missing, na.rm = T)/.N),  
#   by = .(year(fecha_resultado), month(fecha_resultado))
# ][order(-year, -month)]

### 1b

positives[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing", "age_group")
]

positives[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing", "age_group"),
  by = age_group
]

positives[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing", "age_group"),
  by = departamento
]

positives[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing", "age_group"),
  by = metododx
]

# positives[
#   , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
#   .SDcols = !c("missing", "age_group"),
#   by = .(year(fecha_resultado), month(fecha_resultado))
# ][order(-year, -month)]

## Suspected
### 1 a

suspected[
  ,.(missing = sum(missing), .N, perc_miss = sum(missing)/.N), 
  by = flag_sospechoso
][order(-perc_miss)]

suspected[
  ,.(missing = sum(missing), .N, perc_miss = sum(missing)/.N), 
  by = .(year(fecha_contacto), month(fecha_contacto))
][order(-year, -month)]

suspected[
  ,.(missing = sum(missing), .N, perc_miss = sum(missing)/.N), 
  by = .(year(fecha_sintomas), month(fecha_sintomas))
][order(-year, -month)]

### 1b

suspected[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing")
]

suspected[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !"missing",
  by = .(year(fecha_contacto), month(fecha_contacto))
][order(-year, -month)]

suspected[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !"missing",
  by = .(year(fecha_sintomas), month(fecha_sintomas))
][order(-year, -month)]


## Deaths
### 1a

deaths[
  ,.(missing = sum(missing), .N, perc_miss = sum(missing)/.N), 
  by = sexo
][order(-perc_miss)]

deaths[
  ,.(missing = sum(missing), .N, perc_miss = sum(missing)/.N), 
  by = age_group
][order(-perc_miss)]

deaths[
  ,.(missing = sum(missing), .N, perc_miss = sum(missing)/.N), 
  by = departamento
][order(-perc_miss)]

deaths[
  ,.(missing = sum(missing), .N, perc_miss = sum(missing)/.N), 
  by = clasificacion_def
][order(-perc_miss)]

deaths[
  ,.(missing = sum(missing), .N, perc_miss = sum(missing)/.N), 
  by = .(year(fecha_fallecimiento), month(fecha_fallecimiento))
][order(-year, -month)]

### 1b

deaths[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing")
]

deaths[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing"),
  by =  sexo
]

deaths[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing"),
  by =  age_group
]

deaths[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing"),
  by =  departamento
]

deaths[
  , c(unlist(lapply(.SD, missing_field)), list(n = .N)),
  .SDcols = !c("missing"),
  by = .(year(fecha_fallecimiento), month(fecha_fallecimiento))
][order(-year, -month)]




