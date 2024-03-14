
summary_dataset <- function(x) {
  x[
    , c(
      null = lapply(.SD, function(x) sum(x == "NULL", na.rm = TRUE)),
      na = lapply(.SD, function(x) sum(is.na(x), na.rm = TRUE)),
      empty = lapply(.SD, function(x) sum(x == "", na.rm = TRUE)),
      undefined = lapply(.SD, function(x) sum(x == "[NO DEFINIDO]", na.rm = T))
    )
  ] %>% 
    melt(
      measure.vars = measure(value.name, var, sep="."),
    )
}

summary_dataset_empty <- function(x) {
  x[
    , c(
      missing = lapply(
        .SD, function(x) {
          sum(x %in% c("NULL", "", "[NO DEFINIDO]") | is.na(x), na.rm = TRUE)
          } 
        ),
      prop = missing / .N
    )
  ] %>% 
    melt(
      measure.vars = measure(value.name, var, sep="."),
    )
}

# Functions definition ----------------------------------------------------

missing_field <-  function(x) {
  list(miss = sum(is.na(x), na.rm = T), 
       miss_perc = (sum(is.na(x), na.rm = T) / length(x)))
} 

miss <- function(x) {
  is.na(max(x))
}
