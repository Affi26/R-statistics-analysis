
# Skript som används för att genera grafer till till "projekt.tex" filen.

setwd("C:/R-statistics-analysis")

library(ggplot2)

##############################

# Normalfördelning 1

x <- seq(-5, 5, length.out = 1000)

df <- data.frame(
  x = x,
  density = dnorm(x, mean = 0, sd = 1)
)

# Positions for vertical lines
vline_positions <- c(-3, -2, -1, 0, 1, 2, 3)

# Data frame for vertical segments
vline_df <- data.frame(
  x = vline_positions,
  y_start = -0.01,
  y_end = dnorm(vline_positions, mean = 0, sd = 1)
)

ggplot(df, aes(x = x, y = density)) +
  geom_line(linewidth = 1.2, color = "black") +
  geom_segment(
    data = vline_df,
    aes(x = x, xend = x, y = y_start, yend = y_end),
    linetype = c(2, 2, 2, 1, 2, 2, 2),    # dashed for -1 and 1, solid for 0
    linewidth = 1,
    color = "red"
  ) +
  scale_x_continuous(
    breaks = c(-3, -2, -1, 0, 1, 2, 3),
    labels = expression(-3*sigma, -2*sigma, -sigma, mu, +sigma, +2*sigma, +3*sigma)
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14)
  )
##############################



##############################

# Normalfördelning 2

x <- seq(-5, 5, length.out = 1000)

df <- data.frame(
  x = x,
  density = dnorm(x, mean = 0, sd = 1)
)

# Positions for vertical lines
vline_positions <- c(-2, 2)

# Data frame for vertical segments
vline_df <- data.frame(
  x = vline_positions,
  y_start = -0.01,
  y_end = dnorm(vline_positions, mean = 0, sd = 1)
)

ggplot(df, aes(x = x, y = density)) +
  geom_line(linewidth = 1.2, color = "black") +
  geom_ribbon(
    data = subset(df, x >= -2 & x <= 2),
    aes(x = x, ymin = 0, ymax = density),
    alpha = 0.1,
    inherit.aes = FALSE,
    fill = "red"
  ) +
  geom_segment(
    data = vline_df,
    aes(x = x, xend = x, y = y_start, yend = y_end),
    linetype = c(2, 2), # all dashed lines
    linewidth = 1,
    color = "red"
  ) +
  scale_x_continuous(
    breaks = c(-2, 2),
    labels = expression(-2*sigma, +2*sigma)
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14)
  ) +
  annotate("text", x = 0, y = 0.15, label = "95%", color = "red", size = 6)
##############################



##############################

# GVS 1

set.seed(12)

n <- 10

df_manual <- data.frame(
  experiment = 1:10,
  avg = c(3.4, 3.7, 3.1, 3.9, 3.5, 3.2, 3.8, 3.6, 3.3, 3.7)
)
ggplot(df_manual, aes(x = avg)) +
  geom_histogram(
    bins = 5,
    fill = "steelblue",
    color = "black"
  ) +
  labs(
    x = NULL,
    y = NULL,
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(size = 14)
  )
##############################



##############################
# GVS 2

set.seed(12)

n <- 10
B_vals <- c(5, 10, 20, 30, 50, 100, 200, 500, 1000)

df <- data.frame()

for (B in B_vals) {
  sample_means <- numeric(B)
  
  for (i in 1:B) {
    x <- sample(1:6, size = n, replace = TRUE)
    sample_means[i] <- mean(x)
  }
  
  df <- rbind(
    df,
    data.frame(
      value = sample_means,
      experiments = factor(B, labels = paste("Antal stickprov: ", B))
    )
  )
}

library(dplyr)

stats <- df %>%
  group_by(experiments) %>%
  summarise(
    mean_val = mean(value),
    sd_val = sd(value)
  )

# Create a grid for normal curves
normal_curves <- stats %>%
  rowwise() %>%
  do({
    data.frame(
      experiments = .$experiments,
      x = seq(min(df$value), max(df$value), length.out = 100),
      y = dnorm(
        seq(min(df$value),
            max(df$value),
            length.out = 100),
        mean = .$mean_val,
        sd = .$sd_val)
    )
  }) %>%
  ungroup()


ggplot(df, aes(x = value)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 10,
    fill = "steelblue",
    color = "black",
    alpha = 0.6
  ) +
  geom_line(
    data = normal_curves,
    aes(x = x, y = y),
    color = "red",
    size = 1
  ) +
  facet_wrap(~ experiments, ncol = 3, strip.position = "top") +
  labs(
    x = NULL,
    y = "Relativ frekvens"
  ) +
  scale_x_continuous(breaks = seq(2, 5, by = 1.5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    strip.text = element_text(size = 10, margin = margin(t = 0, b = 0)),
    panel.spacing.y = unit(2, "lines")  # Increase vertical space between rows
  )
##############################



##############################

# Signifikans 1

set.seed(6)

x_12 <- rnorm(n = 100, mean = 151, sd = 2)

ggplot(data.frame(x = x_12), aes(x = x)) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "black",
    alpha = 0.6
  ) +
  coord_cartesian(
    xlim = c(145, 160),
    ylim = c(0, 30)
  ) +
  labs(
    x = "längd",
    y = "frekvens",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 10)
  )
mean(x_12)
sd(x_12)
##############################



##############################

# Signifikans 2

set.seed(9)

x_18 <- rnorm(n = 100, mean = 170, sd = 2)

ggplot(data.frame(x = x_18), aes(x = x)) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "black",
    alpha = 0.6
  ) +
  coord_cartesian(
    xlim = c(160, 180),
    ylim = c(0, 30)
  ) +
  labs(
    x = "längd",
    y = "frekvens",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 10)
  )

mean(x_18)
sd(x_18)
##############################



##############################

# Signifikans 3

x_delta = x_12 - x_18

ggplot(data.frame(x = x_delta), aes(x = x)) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "black",
    alpha = 0.6
  ) +
  coord_cartesian(
    xlim = c(-30, -10),
    ylim = c(0, 30)
  ) +
  labs(
    x = "skillnad",
    y = "frekvens",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 10)
  )
mean(x_delta)
sd(x_delta)
##############################



##############################

# Signifikans 4

x_delta_0 <- x_delta - mean(x_delta)

ggplot(data.frame(x = x_delta_0), aes(x = x)) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "black",
    alpha = 0.6
  ) +
  coord_cartesian(
    xlim = c(-10, 10),
    ylim = c(0, 30)
  ) +
  labs(
    x = "skillnad",
    y = "frekvens",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 10)
  )
mean(x_delta_0)
sd(x_delta_0)
##############################



##############################

# Signifikans 5

x <- seq(-30, 10, length.out = 1000)

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = -18.9, sd = 2.9),
    dnorm(x, mean = 0, sd = 2.9)
  ),
  distribution = factor(rep(c("Mean -18", "Mean 0"), each = length(x)))
)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(-18.9, 0),
    labels = c(
      expression(mu[delta] == -18.9),
      expression(mu[delta] == 0)
    )
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    color = "Distribution"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14)
  ) +
  geom_segment(aes(x = -18.9, xend = -18.9, y = 0, yend = max(df$density)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(df$density)), color = "red", linetype = "dashed", linewidth = 1)
##############################

