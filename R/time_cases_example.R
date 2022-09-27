library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(patchwork)



# input TP and proportion under increased restructions

# based on 70% vaccination
# tp_baseline <- 1.5
# prop_under_strict <- 0.49

# or 80% vaccination, reversion to baseline phsm, all adults allocation, partial TTIQ
tp_baseline <- 1.3
prop_under_strict <- 0.31

# calculate TP under strict restrictions
tp_strict <- 10^(((prop_under_strict - 1)*log10(tp_baseline))/prop_under_strict)
tp_strict
# 0.65

# initial cases and generation interval
n0 <- 1000
generation_interval <- 5

# calculate daily growth rates under restrictions
gr_baseline <- tp_baseline^(1/generation_interval)
gr_strict <- tp_strict^(1/generation_interval)

gr_baseline
gr_strict


# compute the number of cases at time t (can be non-integer) since the start of
# the timeseries, given the switching interval, the growth rates under baseline
# and strict, and the initial number of cases, and assuming that the timeseries
# starts with a full period of either strict or baseline restrictions
# can also return the PHSM regime associated with each time t
cases <- function(
  t,
  gr_baseline,
  gr_strict,
  proportion_strict = 0.5,
  cycle = 14,
  n0 = 1000,
  strict_first = FALSE,
  return_phsm = FALSE
) {
  
  strict_interval <- cycle * proportion_strict
  baseline_interval <- cycle * (1 - proportion_strict)
  
  # how many complete cycles of baseline and strict so far
  whole_cycles <- t %/% cycle
  
  # calculate the remainder in days
  # remainder <- t - (whole_cycles * cycle)
  remainder <- t %% cycle
  
  # how many days of baseline and strict, in
  # this remainder period
  if(strict_first) {
    extra_strict_days <- pmin(remainder, strict_interval)
    extra_baseline_days <- remainder - extra_strict_days
  } else {
    extra_baseline_days <- pmin(remainder, baseline_interval)
    extra_strict_days <- remainder - extra_baseline_days
  }
  
  # how many days of each
  baseline_days <- whole_cycles * baseline_interval + extra_baseline_days
  strict_days <- whole_cycles * strict_interval + extra_strict_days
  
  # calculate cases by this point
  ncases <- n0 * gr_baseline ^ baseline_days * gr_strict ^ strict_days
  
  if(return_phsm){
    
    phsm <- ifelse(
      extra_baseline_days > lag(extra_baseline_days),
      "Baseline",
      "Strict"
    ) %>%
      as.factor()
    
    phsm[1] <- phsm[2]
    
    return(tibble(phsm, cases = ncases))
    
  } else {
    return(cases)
  }
  
}

t <- seq(0, 150, length.out = 1000)

n_cases_long <- cases(
  t,
  gr_baseline = gr_baseline,
  gr_strict = gr_strict,
  proportion_strict = prop_under_strict,
  cycle = 50,
  n0 = 1000,
  strict_first = TRUE,
  return_phsm = TRUE
)

n_cases_short <- cases(
  t,
  gr_baseline = gr_baseline,
  gr_strict = gr_strict,
  proportion_strict = prop_under_strict,
  cycle = 15,
  n0 = 1000,
  strict_first = TRUE,
  return_phsm = TRUE
)


dat <- bind_rows(
  n_cases_long %>% 
    mutate(
      intervals = "Long\nIntervals",
      date = t
    ),
  n_cases_short %>%
    mutate(
      intervals = "Short\nIntervals",
      date = t
    )
)

# colour1 <- "darkgoldenrod"
# colour1 <- "goldenrod1"
# colour1 <- "mediumvioletred"
colour1 <- "seagreen"
colour2 <- "darkorchid"

# colour1 <- "dodgerblue2"
# colour2 <- "firebrick2"

linesize <- 1.5

plot_1 <- dat %>%
  ggplot() +
  geom_line(
    aes(
      x = date,
      y = phsm,
      group = intervals
    ),
    colour = colour1,
    size = linesize
  ) +
  facet_grid(
    intervals ~ .,
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
    colour = colour2,
    size = linesize
  ) +
  facet_grid(
    intervals ~ .#,
    #switch = "y"
    #scales = "free"
  ) +
  geom_hline(
    yintercept = initial_cases,
    linetype = "dashed"
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0),
    strip.placement = "outside",
    axis.text.x = element_blank()
  ) +
  labs(
    x = "Time",
    y = "Cases"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1000, 200),
    labels = c(0, rep("", 4), expression("N"[0])),
    limits = c(0, 1000)
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
      group = intervals
    ),
    colour = colour1,
    size = linesize
  ) +
  facet_grid(
    . ~ intervals,
    #switch = "y"
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_blank(),
    strip.placement = "outside",
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      angle = 0,
      vjust = 0.5
    )
  ) +
  labs(
    y = "PHSM\nbundle"
  )

plot_3

plot_4 <- dat %>%
  ggplot() +
  geom_line(
    aes(
      x = date,
      y = cases
    ),
    colour = colour2,
    size = linesize
  ) +
  facet_grid(
    . ~ intervals
  ) +
  geom_hline(
    yintercept = initial_cases,
    linetype = "dashed"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_blank(),
    strip.placement = "outside",
    axis.text.x = element_blank(),
    axis.title.y = element_text(
      angle = 0,
      vjust = 0.5
    )
  ) +
  labs(
    x = "Time",
    y = "Cases"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1000, 200),
    labels = c(0, rep("", 4), expression("N"[0])),
    limits = c(0, 1000)
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


