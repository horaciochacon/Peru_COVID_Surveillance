
ts_pcr <- molecular[
  ,.(no_miss = .N - sum(missing), 
     perc_no_miss = (.N - sum(missing))/.N,
     perc_miss = sum(missing)/.N),
  by = week
]

ts_pcr %>% 
  filter(week >= "2020-03-01") %>% 
  ggplot(aes(x = week)) + 
  geom_line(aes(y = perc_no_miss)) +
  labs(
    y = "Percentage of no missing",
    x = "Date") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() 

ts_pos <- positives[
  ,.(no_miss = .N - sum(missing), 
     perc_no_miss = (.N - sum(missing))/.N,
     perc_miss = sum(missing)/.N),
  by = week
]

ts_pos %>% 
  filter(week >= "2020-03-01") %>% 
  ggplot(aes(x = week)) + 
  geom_line(aes(y = perc_no_miss)) +
  labs(
    y = "Percentage of no missing",
    x = "Date") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() 

ts_deaths <- deaths[
  ,.(no_miss = .N - sum(missing), 
     perc_no_miss = (.N - sum(missing))/.N,
     perc_miss = sum(missing)/.N),
  by = week
]

ts_deaths %>% 
  filter(week >= "2020-03-10") %>% 
  ggplot(aes(x = week, y = perc_no_miss)) + 
  geom_line() +
  geom_point(size = 0.5) +
  labs(
    y = "Percentage of no missing",
    x = "Date") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() 

# Plot completeness

ts_pcr %>% 
  filter(week >= "2020-03-01") %>% 
  ggplot(aes(x = week, y = perc_no_miss)) +
  geom_line(aes(color = "PCR")) +
  geom_smooth(aes(color = "PCR"), method = 'gam', formula = y ~ s(x, k = 30, bs = "cs")) +
  geom_line(aes(color = "Positives"), data = ts_pos) +
  geom_smooth(aes(color = "Positives"), method = 'gam', formula = y ~ s(x, k = 30, bs = "cs"), data = ts_pos) +
  geom_line(aes(y = perc_no_miss, color = "Deaths"), data = ts_deaths) +
  geom_smooth(aes(color = "Deaths"), method = 'gam', formula = y ~ s(x, k = 30, bs = "cs"), data = ts_deaths) +
  labs(
    y = "Percentage of completeness",
    x = "Date",
    color = "Source"
    ) +
  scale_y_continuous(limits = c(0.85,1.005), labels = scales::percent) +
  theme_bw() 

# Plot time concordance

ts_molecular %>% 
  left_join(ts_positives) %>% 
  mutate(conc_ratio =PCR/Positives) %>% 
  ggplot(aes(x = week, y = conc_ratio)) +
  geom_line() +
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 100, bs = "cs")) +
  labs(
    y = "Concordance ratio",
    x = "Date",
  ) +
  theme_bw()

# Scatter plot

dep_molecular %>% 
  left_join(dep_positive, by = "departamento") %>% 
  mutate(`Concordance ratio` = pcr / pos) %>%
  filter(departamento != "LIMA") %>% 
  ggplot(aes(x = pos, y = `Concordance ratio`)) +
  geom_point()

# TImes

sus_hos_agg <- sus_hos %>% 
  select(fecha_sintomas, timediff) %>% 
  filter(year(fecha_sintomas) >= 2020) %>%
  group_by(
    fecha_sintomas = floor_date(fecha_sintomas, unit = "weeks", week_start = 1)
  ) %>% 
  summarise(timediff = mean(timediff, na.rm = TRUE))

g <- (sus_pos_date %>% 
    select(fecha_sintomas,timediff_res_sympt_mean,timediff_sympt_cont_mean) %>% 
    left_join(sus_hos_agg) %>% 
    pivot_longer(cols = 2:4) %>% 
    filter(year(fecha_sintomas) >= 2020) %>% 
    ggplot(aes(x = fecha_sintomas, y = value, col = name)) +
    geom_smooth(
      method = 'gam', formula = y ~ s(x, k = 30, bs = "cs") ,
      size = 0.3,
      alpha = 0.1,
      se = FALSE
    ) +
    geom_smooth(
      method = 'gam', formula = y ~ s(x, k = 30, bs = "cs") ,
      size = 0,
      alpha = 0.1,
      se = TRUE
      ) +
    geom_line() +
    ylim(0, 25) +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_color_manual(
      name = "Indicator", 
      values = c(
        "timediff_res_sympt_mean" = "blue",
        "timediff_sympt_cont_mean" = "red",
        "timediff" = "purple"
      ),
      labels = c("Delay to attention", "Delay to lab results", "Delay to report" )
    ) +
    labs(
      x = "Date of onset (symptoms)", 
      y = "Time (in days)",
      caption = "GAM smoothing - Formula: 'y ~ s(x, k = 30, bs = `cs`)'"
    )
) 

ggsave("output/timeliness/smooth.png",plot = g, scale = 0.5)
