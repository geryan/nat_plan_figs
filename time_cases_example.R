library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(patchwork)



x <- seq.Date(
  from = "2021-07-01",
  to = 
)


tp_baseline <- 1.5

prop_under_strict <- 0.49

tp_strict <- 10^(((prop_under_strict - 1)*log10(tp_baseline))/prop_under_strict)

tp_strict
# 0.65

n0 <- 1000

generation_interval <- 5

gr_baseline <- tp_baseline^(1/generation_interval)
gr_strict <- tp_strict^(1/generation_interval)

gr_baseline
gr_strict


short_interval <- 7

long_interval <- 28

total_days <- 200

dates <- 0:total_days

short_timeline <- c(
  "Baseline",
  rep(
    c(
      rep(
        "Baseline",
        times = short_interval
      ),
      rep(
        "Strict",
        times = short_interval
      )
    ),
    length.out = total_days
  )
)

table(short_timeline)


long_timeline <- c(
  "Baseline",
  rep(
    c(
      rep(
        "Baseline",
        times = long_interval
      ),
      rep(
        "Strict",
        times = long_interval
      )
    ),
    length.out = total_days + 10
  )[11:210] # modify so meets the correct proportion of time under each PHSM per prop_under_strict
)

table(long_timeline)

initial_cases <- 1000

dat <- tibble(
  date = dates,
  short = short_timeline,
  long = long_timeline
) %>%
  pivot_longer(
    cols = -date,
    names_to = "block",
    values_to = "phsm"
  ) %>%
  mutate(
    growth_rate = if_else(
      phsm == "Baseline",
      gr_baseline,
      gr_strict
    ),
    growth_rate = case_when(
      date == 0 ~ 1, # this so that the acumulate function below keeps the cases correct on day 0
      phsm == "Baseline" ~ gr_baseline,
      TRUE ~ gr_strict
    )
  ) %>%
  arrange(
    block,
    date
  ) %>%
  group_by(block) %>%
  mutate(
    cases = round(
      initial_cases * 
        accumulate(
        .x = growth_rate,
        .f = ~ .x * .y
      )
    )
  ) %>%
  ungroup %>%
  mutate(
    phsm = ordered(
      phsm,
      levels = c("Baseline", "Strict")
    ),
    block = if_else(
      block == "long",
      "Long\nperiods",
      "Short\nperiods"
    )
  )

dat

plot_1 <- dat %>%
  ggplot() +
  geom_line(
    aes(
      x = date,
      y = phsm,
      group = block
    ),
    colour = "darkgoldenrod"
  ) +
  facet_grid(
    block ~ .,
    #switch = "y"
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_blank(),
    strip.placement = "outside",
    axis.text.x = element_blank()
  ) +
  labs(
    x = "Time",
    y = "PHSM bundle"
  )

plot_1

plot_2 <- dat %>%
  ggplot() +
  geom_line(
    aes(
      x = date,
      y = cases
    ),
    colour = "darkorchid"
  ) +
  facet_grid(
    block ~ .#,
    #switch = "y"
  ) +
  geom_hline(
    yintercept = initial_cases,
    linetype = "dashed"
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0),
    strip.placement = "outside",
    axis.text = element_blank()
  ) +
  labs(
    x = "Time",
    y = "Cases"
  )


plot_2



plot_12 <- plot_1 + plot_2

plot_12

png(
  filename = "figures/figure_time_cases_leftright.png",
  height = 1000,
  width = 1600,
  res = 200
)
plot_12
dev.off()



plot_3 <- dat %>%
  ggplot() +
  geom_line(
    aes(
      x = date,
      y = phsm,
      group = block
    ),
    colour = "darkgoldenrod"
  ) +
  facet_grid(
    . ~ block,
    #switch = "y"
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_blank(),
    strip.placement = "outside",
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(
    y = "PHSM bundle"
  )

plot_3

plot_4 <- dat %>%
  ggplot() +
  geom_line(
    aes(
      x = date,
      y = cases
    ),
    colour = "darkorchid"
  ) +
  facet_grid(
    . ~ block
  ) +
  geom_hline(
    yintercept = initial_cases,
    linetype = "dashed"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_blank(),
    strip.placement = "outside",
    axis.text = element_blank()
  ) +
  labs(
    x = "Time",
    y = "Cases"
  )


plot_4



plot_34 <- plot_3 / plot_4

plot_34

png(
  filename = "figures/figure_time_cases_topbottom.png",
  height = 1000,
  width = 1600,
  res = 200
)
plot_34
dev.off()
