library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(patchwork)


# based on figure 1 and related text in
# "National Plan to Transition to Australia’s National COVID-19 Response: Economic Impact Analysis"
# https://treasury.gov.au/sites/default/files/2021-08/PDF_Economic_Impacts_COVID-19_Response_196731.pdf
tsy <- tribble(
  ~vax, ~transmission,      ~ttiq, ~ongoing_phsm, ~phsm, ~cost,
    50,    "minimised", "Optimal",     "Baseline", "Strict",  570,
    50,    "minimised", "Optimal",        "Light", "Strict", 1060,
    50,      "managed", "Partial",     "Baseline", "Strict", 2690,
    60,    "minimised", "Optimal",     "Baseline", "Strict",  430,
    60,    "minimised", "Optimal",        "Light", "Strict",  730,
    60,      "managed", "Partial",     "Baseline", "Strict", 2120,
    70,    "minimised", "Optimal",     "Baseline",  "Light",  200,
    70,    "minimised", "Optimal",        "Light",  "Light",  660,
    70,      "managed", "Partial",     "Baseline", "Strict", 1640,
    80,    "minimised", "Optimal",     "Baseline",  "Light",  140,
    80,    "minimised", "Optimal",        "Light",  "Light",  660,
    80,      "managed", "Partial",     "Baseline",  "Light",  590
)


# data from tables S2.2, S2.3, S2.4, S2.5 in
# "Doherty Modelling Report Revised, 2021/08/10"
# http://www.doherty.edu.au/uploads/content_doc/DohertyModelling_NationalPlan_and_Addendum_20210810.pdf
til_partial_baseline <- tribble(
  ~vax, ~allocation, ~Light, ~Moderate, ~Strict,
  50,    "Oldest first", NA, NA, 89,
  50, "40+ years first", NA, NA, 93,
  50,      "All adults", NA, NA, 84,
  60,    "Oldest first", NA, NA, 67,
  60, "40+ years first", NA, NA, 78,
  60,      "All adults", NA, NA, 65,
  70,    "Oldest first", NA, 77, 47,
  70, "40+ years first", NA, 99, 60,
  70,      "All adults", NA, 81, 49,
  80,    "Oldest first", 82, 47, 29,
  80, "40+ years first", NA, 59, 36,
  80,      "All adults", 89, 51, 31
) %>%
  mutate(
    ttiq = "Partial",
    ongoing_phsm = "Baseline"
  ) %>%
  pivot_longer(
    cols = Light:Strict,
    names_to = "phsm",
    values_to = "til"
  )

til_optimal_baseline <- tribble(
  ~vax, ~allocation, ~Light, ~Moderate, ~Strict,
  50,    "Oldest first", NA, NA, 63,
  50, "40+ years first", NA, NA, 67,
  50,      "All adults", NA, 94, 58,
  60,    "Oldest first", NA, 67, 41,
  60, "40+ years first", NA, 86, 52,
  60,      "All adults", NA, 64, 39,
  70,    "Oldest first", 60, 34, 21,
  70, "40+ years first", 97, 56, 34,
  70,      "All adults", 67, 38, 23,
  80,    "Oldest first",  7,  4,  3,
  80, "40+ years first", 29, 17, 10,
  80,      "All adults", 15,  8,  5
) %>%
  mutate(
    ttiq = "Optimal",
    ongoing_phsm = "Baseline"
  ) %>%
  pivot_longer(
    cols = Light:Strict,
    names_to = "phsm",
    values_to = "til"
  )



til_partial_light <- tribble(
  ~vax, ~allocation, ~Moderate, ~Strict,
  50,    "Oldest first", NA, 82,
  50, "40+ years first", NA, 89,
  50,      "All adults", NA, 75,
  60,    "Oldest first", NA, 49,
  60, "40+ years first", NA, 67,
  60,      "All adults", NA, 46,
  70,    "Oldest first", 46, 18,
  70, "40+ years first", 97, 39,
  70,      "All adults", 55, 22,
  80,    "Oldest first",  0,  0,
  80, "40+ years first",  4,  2,
  80,      "All adults",  0,  0,
) %>%
  mutate(
    ttiq = "Partial",
    ongoing_phsm = "Light"
  ) %>%
  pivot_longer(
    cols = Moderate:Strict,
    names_to = "phsm",
    values_to = "til"
  )


til_optimal_light <- tribble(
  ~vax, ~allocation, ~Moderate, ~Strict,
  50,    "Oldest first", NA, 42,
  50, "40+ years first", NA, 49,
  50,      "All adults", 87, 35,
  60,    "Oldest first", 23,  9,
  60, "40+ years first", 66, 27,
  60,      "All adults", 15 , 6,
  70,    "Oldest first",  0,  0,
  70, "40+ years first",  0,  0,
  70,      "All adults",  0,  0,
  80,    "Oldest first",  0,  0,
  80, "40+ years first",  0,  0,
  80,      "All adults",  0,  0
) %>%
  mutate(
    ttiq = "Optimal",
    ongoing_phsm = "Light"
  ) %>%
  pivot_longer(
    cols = Moderate:Strict,
    names_to = "phsm",
    values_to = "til"
  )


til <- bind_rows(
  til_partial_baseline,
  til_optimal_baseline,
  til_partial_light,
  til_optimal_light
)


dat_cost <- tsy %>%
  mutate(
    scenario = sprintf(
      "%s TTIQ & %s PHSM",
      ttiq,
      ongoing_phsm
    ) %>% 
      factor(
        levels = c(
          "Partial TTIQ & Baseline PHSM",
          "Optimal TTIQ & Light PHSM",
          "Optimal TTIQ & Baseline PHSM"
        )
      )
  ) %>%
  mutate(
    vax = sprintf(
      "%s%% vaccination\ncoverage",
      vax
    )
  )


dat_til <- left_join(
  x = dat_cost,
  y = til %>%
    filter(allocation == "All adults") %>%
    mutate(
      vax = sprintf(
        "%s%% vaccination\ncoverage",
        vax
      )
    ),
  by = c(
    "vax",
    "ttiq",
    "ongoing_phsm",
    "phsm"
  )
) %>%
  select(
    -transmission,
    -allocation,
    -cost
  ) %>%
  mutate(
    time_1 = ifelse(
      is.na(til),
      0,
      til
    ),
    time_2 = 100 - time_1,
    phsm_1 = phsm,
    phsm_2 = ongoing_phsm
  ) %>%
  select(-phsm, -til) %>%
  pivot_longer(
    time_1:phsm_2,
    names_to = c(".value", "x"),
    names_sep = "_",
  ) %>%
  select(-x) %>%
  filter(
    time != 0
  )

dat_til %>%
  print(n = 24)


plot_cost <- dat_cost %>%
  ggplot() +
  geom_col(
    aes(
      x = cost,
      y = scenario
    ),
    colour = "grey1",
    fill = "grey70",
    position = "dodge"
  ) + 
  facet_grid(
    vax ~ .
  ) +
  labs(
    x = "Cost (AUD M/week)\nof PHSMs",
    y = element_blank()
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    strip.text.y = element_blank(),
    #text = element_text(size = 12),
    strip.placement = "outside"
  ) +
  xlim(0, 3000)

plot_cost

plot_til <- dat_til %>%
  ggplot() +
  geom_col(
    aes(
      x = time,
      y = scenario,
      fill = phsm,
    ),
    colour = "grey1",
    position = "stack"
  ) + 
  facet_grid(
    vax ~ .,
    switch = "y"
  ) +
  # scale_fill_manual(
  #   #values = c("cornflowerblue", "yellow", "tomato")#,
  #   values = c("palegreen3", "yellow2", "grey30")
  # ) +
  scale_fill_brewer(palette = "YlGn") +
  #scale_fill_viridis_d() +
  labs(
    x = "Percentage of time\nunder PHSM",
    #y = "",
    fill = "PHSM",
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0),
    axis.title.y = element_blank(),
    strip.placement = "outside"#,
    #text = element_text(size = 12)
  ) +
  lims(x = c(0, 100))

plot_til

  

figure_til_cost <- plot_til +
  plot_cost +
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect")
figure_til_cost


png(
  filename = "figures/figure_til_cost.png",
  height = 1000,
  width = 1600,
  res = 200
)
figure_til_cost
dev.off()


png(
  filename = "figures/figure_til_cost_bottom.png",
  height = 1000,
  width = 1600,
  res = 200
)
plot_til +
  theme(legend.position = "bottom") +
  plot_cost +
  plot_annotation(tag_levels = "a")
dev.off()


# 
# ## all time-in-lockdown results
# til %>%
#   filter(allocation == "All adults") %>%
#   mutate(
#     constrain = if_else(
#       is.na(til),
#       "unable",
#       "able"
#     ),
#     til = if_else(
#       is.na(til),
#       100,
#       til
#     ),
#     scenario = sprintf(
#       "%s/%s",
#       ttiq,
#       ongoing_phsm
#     )
#   ) %>%
#   ggplot() +
#   geom_col(
#     aes(
#       x = til,
#       y = scenario,
#       fill = phsm,
#       alpha = constrain
#     ),
#     position = "dodge"
#   ) + 
#   scale_alpha_manual(values = c(1, 0.2)) +
#   facet_grid(
#     vax ~.
#   )
