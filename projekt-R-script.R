
# Skript som används för att genera grafer till till "projekt.tex" filen.

setwd("C:/R-statistics-analysis")

library(ggplot2)
library(ggpattern)
library(dplyr)
library(deSolve)
library(reshape2)
library(patchwork)
library(tidyr)
library(kableExtra)
library(knitr)
library(lamW)
library(tibble)


##############################

# Vetenskap 1

delta_X_2 <- seq(-1, 1, by = 0.01)

a_1 <- 10 - 5 * delta_X_2

plot(delta_X_2, a_1, type = "l",
     xlab = expression(Delta~X[2]),
     ylab = expression(a[1]))
grid()
##############################



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
  y_start = 0,
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
    labels = expression(-3*sigma, -2*sigma, -sigma, mu, +sigma, +2*sigma, +3*sigma),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
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
  y_start = 0,
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
    labels = expression(-2*sigma, +2*sigma),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
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

# Histogram 1

set.seed(1)

# Generate random data
x <- rnorm(n = 100, mean = 0, sd = 1)

ggplot(data.frame(x = x), aes(x = x)) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "black",
    alpha = 0.6
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "värde",
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

##############################



##############################

# Histogram 2

set.seed(1)

# Generate exponential data
x <- rexp(n = 500, rate = 1)   # rate = λ

ggplot(data.frame(x = x), aes(x = x)) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "black",
    alpha = 0.6
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "värde",
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

##############################



##############################

# Boxdiagram 1

set.seed(4)

# Symmetric data
x_sym <- rnorm(10, mean = 0, sd = 1)

ggplot(data.frame(x = x_sym), aes(y = x)) +
  geom_boxplot(
    fill = "steelblue",
    alpha = 0.6,
    width = 0.3
  ) +
  labs(
    x = NULL,
    y = "värde",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 10)
  )

##############################



##############################

# Boxdiagram 2

set.seed(1)

# Skewed data + artificial outliers
x_skew <- c(rnorm(10, mean = 0, sd = 1), 6, 7, 8)

ggplot(data.frame(x = x_skew), aes(y = x)) +
  geom_boxplot(
    fill = "steelblue",
    alpha = 0.6,
    width = 0.3
  ) +
  labs(
    x = NULL,
    y = "värde",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 10)
  )

##############################



##############################

# medelvärde och felmarginal 1

set.seed(1)

# Example data
df <- data.frame(
  group = rep(c("A", "B"), each = 20),
  value = c(rnorm(20, mean = 6, sd = 0.8),
            rnorm(20, mean = 7, sd = 1.1))
)

# Compute mean and SD for each group
summary_df <- aggregate(value ~ group, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))
summary_df <- do.call(data.frame, summary_df)
names(summary_df) <- c("group", "mean", "sd")

# Plot mean ± SD
ggplot(summary_df, aes(x = group, y = mean)) +
  # mean points
  geom_point(size = 3, color = "steelblue") +
  # error bars
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "steelblue") +
  labs(
    x = NULL,
    y = "värde",
    title = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12)
  )

##############################



##############################

# medelvärde och felmarginal 2

set.seed(1)

# Example data
df <- data.frame(
  group = rep(c("A", "B"), each = 20),
  value = c(rnorm(20, mean = 6, sd = 0.8),
            rnorm(20, mean = 7, sd = 1.1))
)

# Compute mean and SD for each group
summary_df <- aggregate(value ~ group, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))
summary_df <- do.call(data.frame, summary_df)
names(summary_df) <- c("group", "mean", "sd")

# Plot mean ± SD
ggplot(summary_df, aes(x = group, y = mean)) +
  # Raw data points with slight horizontal jitter
  geom_jitter(data = df, aes(x = group, y = value),
              width = 0.1, height = 0, color = "black", alpha = 0.5, size = 2) +
  # mean points
  geom_point(size = 3, color = "red") +
  # error bars
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  labs(
    x = NULL,
    y = "värde",
    title = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12)
  )

##############################



##############################

# Tidsserie 1

set.seed(5)

# Simulate example data: 7 time points, 7 replicates each
time <- rep(1:5, each = 10)
values <- c(
  rnorm(10, mean = 3.3, sd = 0.3),
  rnorm(10, mean = 4.5, sd = 0.2),
  rnorm(10, mean = 3.4, sd = 0.3),
  rnorm(10, mean = 4.0, sd = 0.2),
  rnorm(10, mean = 3.7, sd = 0.2)
)

df <- data.frame(time = time, value = values)

# Compute mean and SD for each time point
summary_df <- aggregate(value ~ time, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))
summary_df <- do.call(data.frame, summary_df)
names(summary_df) <- c("time", "mean", "sd")

# Plot mean ± SD over time
ggplot(summary_df, aes(x = time, y = mean)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "steelblue") +
  labs(
    x = "Tidpunkt",
    y = "Värde",
    title = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12)
  )

##############################



##############################

# Stapeldiagram 1

set.seed(1)

# Example data: one group
x <- rnorm(20, mean = 15, sd = 3)

# Compute mean and SD
mean_x <- mean(x)
sd_x   <- sd(x)

# Create a small data frame for plotting
df <- data.frame(
  group = "A",
  mean = mean_x,
  sd = sd_x
)

# Bar chart with error bars
ggplot(df, aes(x = group, y = mean)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7, width = 0.1) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.05, color = "black", linewidth = 1) +
  labs(
    x = NULL,
    y = "värde",
    title = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12)
  )

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
  scale_x_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
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
  scale_x_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    strip.text = element_text(face = "bold", size = 10, margin = margin(t = 0, b = 0)),
    panel.spacing.y = unit(2, "lines")  # Increase vertical space between rows
  )
##############################



##############################

# Signifikans 1

set.seed(7)

x_12 <- rnorm(n = 100, mean = 151, sd = 2)


ggplot(data.frame(x = x_12), aes(x = x)) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "black",
    alpha = 0.6
  ) +
  coord_cartesian(
    xlim = c(138, 172),
    ylim = c(0, 30)
  ) +
  labs(
    x = "längd",
    y = "frekvens",
    title = NULL
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
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

set.seed(11)

x_15 <- rnorm(n = 100, mean = 157, sd = 2)

ggplot(data.frame(x = x_15), aes(x = x)) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "black",
    alpha = 0.6
  ) +
  coord_cartesian(
    xlim = c(138, 172),
    ylim = c(0, 30)
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
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

mean(x_15)
sd(x_15)
##############################



##############################

# Signifikans 3

mu_delta <- mean(x_12) - mean(x_15)
se_delta <- sqrt(sd(x_12)^2/100 + sd(x_15)^2/100)

x <- seq(-6.7, -4.3, length.out = 1000)

df <- data.frame(
  x = x,
  density = dnorm(x, mean = mu_delta, sd = se_delta)
)

ggplot(df, aes(x, density)) +
  geom_line(
    linewidth = 1.2, 
    colour = "steelblue"
  ) +
  coord_cartesian(
    xlim = c(-6.7, -4.3)
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "skillnad",
    y = "täthet",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 10)
  )
mu_delta
se_delta
##############################



##############################

# Signifikans 4

mu_delta_0 = mu_delta - mu_delta
se_delta_0 = se_delta

x <- seq(-6.7-mu_delta, -4.3-mu_delta, length.out = 1000)

df_0 <- data.frame(
  x = x,
  density = dnorm(x, mean = mu_delta_0, sd = se_delta_0)
)

ggplot(df_0, aes(x, density)) +
  geom_line(
    linewidth = 1.2, 
    colour = "steelblue"
  ) +
  coord_cartesian(
    xlim = c(-6.7-mu_delta, -4.3-mu_delta)
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "skillnad",
    y = "täthet",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 10)
  )
##############################



##############################

# Signifikans 5

x <- seq(-7.3, 1.8, length.out = 1000)

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = mu_delta, sd = se_delta),
    dnorm(x, mean = mu_delta_0, sd = se_delta_0)
  ),
  distribution = factor(rep(c("mu_delta", "mu_delta_0"), each = length(x)))
)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(mu_delta, mu_delta_0),
    labels = c(
      bquote(H[1]: mu[delta] == .(round(mu_delta,2)) ~ "\u2260" ~ 0),
      bquote(H[0]: mu[delta] == .(round(mu_delta_0,2)))
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
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
  geom_segment(aes(x = mu_delta, xend = mu_delta, y = 0, yend = max(df$density)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_segment(aes(x = mu_delta_0, xend = mu_delta_0, y = 0, yend = max(df$density)), color = "red", linetype = "dashed", linewidth = 1)
##############################



##############################

# Inferens 1

x <- seq(-2.2, 3.2, length.out = 1000)

mu_delta = 1
mu_delta_0 = 0
se_delta = 0.45

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = mu_delta_0, sd = se_delta),
    dnorm(x, mean = mu_delta, sd = se_delta)
  ),
  distribution = factor(rep(c("mu_delta_0", "mu_delta"), each = length(x)))
)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(mu_delta_0, mu_delta),
    labels = c(
      bquote(mu[delta] == .(round(mu_delta_0,2))),
      bquote(mu[delta] == .(round(mu_delta,2)))
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
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
  geom_segment(aes(x = mu_delta_0, xend = mu_delta_0, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_delta, xend = mu_delta, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1)
##############################



##############################

# Inferens 2

x <- seq(-2, 3, length.out = 1000)

mu_delta = 1
mu_delta_0 = 0
se_delta = 0.45

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = mu_delta_0, sd = se_delta),
    dnorm(x, mean = mu_delta, sd = se_delta)
  ),
  distribution = factor(rep(c("mu_delta_0", "mu_delta"), each = length(x)))
)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(mu_delta_0, mu_delta),
    labels = c(
      bquote(mu[delta] == .(round(mu_delta_0,2))),
      bquote(mu[delta] == .(round(mu_delta,2)))
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  geom_ribbon(
    data = subset(
      df,
      x <= mu_delta_0 - 1.96*se_delta &
        distribution == "mu_delta_0"
    ),
    aes(x=x, ymin = 0, ymax = density),
    fill = "orange",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_ribbon(
    data = subset(
      df,
      x >= mu_delta_0 + 1.96*se_delta &
        distribution == "mu_delta_0"
    ),
    aes(x=x, ymin = 0, ymax = density),
    fill = "orange",
    alpha = 0.5,
    inherit.aes = FALSE
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
  geom_segment(aes(x = mu_delta_0, xend = mu_delta_0, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_delta, xend = mu_delta, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  annotate("text", x = 1.5, y = 0.1, label = expression(alpha/2 == 0.025), color = "orange", size = 4) +
  annotate("text", x = -1.5, y = 0.1, label = expression(alpha/2 == 0.025), color = "orange", size = 4)
##############################



##############################

# Inferens 3

x <- seq(-2, 3, length.out = 1000)

mu_delta = 1
mu_delta_0 = 0
se_delta = 0.45

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = mu_delta_0, sd = se_delta),
    dnorm(x, mean = mu_delta, sd = se_delta)
  ),
  distribution = factor(rep(c("mu_delta_0", "mu_delta"), each = length(x)))
)

dens_1 = dnorm((mu_delta_0-1.96*se_delta), mean = mu_delta_0, sd = se_delta)
dens_2 = dnorm((mu_delta_0+1.96*se_delta), mean = mu_delta, sd = se_delta)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(mu_delta_0, mu_delta),
    labels = c(
      bquote(mu[delta] == .(round(mu_delta_0,2))),
      bquote(mu[delta] == .(round(mu_delta,2)))
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  geom_ribbon(
    data = subset(
      df,
      x >= mu_delta_0 - 1.96*se_delta & x <= mu_delta_0 + 1.96*se_delta &
        distribution == "mu_delta"
    ),
    aes(x=x, ymin = 0, ymax = density),
    fill = "orange",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "mu_delta_0" & x >= mu_delta_0+1.96*se_delta),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "crosshatch",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "mu_delta_0" & x <= mu_delta_0-1.96*se_delta),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "crosshatch",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
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
  geom_segment(aes(x = mu_delta_0, xend = mu_delta_0, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_delta, xend = mu_delta, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_delta_0-1.96*se_delta, xend = mu_delta_0-1.96*se_delta, y = 0, yend = dens_1), color = "brown", linetype = "dashed", linewidth = 1) +
  geom_segment(aes(x = mu_delta_0+1.96*se_delta, xend = mu_delta_0+1.96*se_delta, y = 0, yend = dens_2), color = "brown", linetype = "dashed", linewidth = 1) +
  annotate("text", x = -0.4, y = 0.2, label = expression(beta %~~% 0.39), color = "orange", size = 5) +
  annotate("text", x = 1.5, y = 0.1, label = expression(alpha/2 == 0.025), color = "brown", size = 4) +
  annotate("text", x = -1.5, y = 0.1, label = expression(alpha/2 == 0.025), color = "brown", size = 4)
##############################



##############################

# Inferens 4

x <- seq(-2, 3, length.out = 1000)

mu_delta = 1
mu_delta_0 = 0
se_delta = 0.45

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = mu_delta_0, sd = se_delta),
    dnorm(x, mean = mu_delta, sd = se_delta)
  ),
  distribution = factor(rep(c("mu_delta_0", "mu_delta"), each = length(x)))
)

dens_1 = dnorm((mu_delta_0-1.96*se_delta), mean = mu_delta_0, sd = se_delta)
dens_2 = dnorm((mu_delta_0+1.96*se_delta), mean = mu_delta, sd = se_delta)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(mu_delta_0, mu_delta),
    labels = c(
      bquote(mu[delta] == .(round(mu_delta_0,2))),
      bquote(mu[delta] == .(round(mu_delta,2)))
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  geom_ribbon(
    data = subset(
      df,
      x >= mu_delta_0 + 1.96*se_delta &
        distribution == "mu_delta"
    ),
    aes(x=x, ymin = 0, ymax = density),
    fill = "orange",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "mu_delta_0" & x >= mu_delta_0+1.96*se_delta),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "crosshatch",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "mu_delta_0" & x <= mu_delta_0-1.96*se_delta),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "crosshatch",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
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
  geom_segment(aes(x = mu_delta_0, xend = mu_delta_0, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_delta, xend = mu_delta, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_delta_0-1.96*se_delta, xend = mu_delta_0-1.96*se_delta, y = 0, yend = dens_1), color = "brown", linetype = "dashed", linewidth = 1) +
  geom_segment(aes(x = mu_delta_0+1.96*se_delta, xend = mu_delta_0+1.96*se_delta, y = 0, yend = dens_2), color = "brown", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 2.3, y = 0.2, label = expression(1 - beta %~~% 0.61), color = "orange", size = 5) +
  annotate("text", x = 1.5, y = 0.1, label = expression(alpha/2 == 0.025), color = "brown", size = 4) +
  annotate("text", x = -1.5, y = 0.1, label = expression(alpha/2 == 0.025), color = "brown", size = 4)
##############################



##############################

# Inferens 5

x <- seq(-2, 3, length.out = 1000)

mu_delta = 1.25
mu_delta_0 = 0
se_delta = 0.45

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = mu_delta_0, sd = se_delta),
    dnorm(x, mean = mu_delta, sd = se_delta)
  ),
  distribution = factor(rep(c("mu_delta_0", "mu_delta"), each = length(x)))
)

dens_1 = dnorm((mu_delta_0-1.96*se_delta), mean = mu_delta_0, sd = se_delta)
dens_2 = dnorm((mu_delta_0+1.96*se_delta), mean = mu_delta, sd = se_delta)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(mu_delta_0, mu_delta),
    labels = c(
      bquote(mu[delta] == .(round(mu_delta_0,2))),
      bquote(mu[delta] == .(round(mu_delta,2)))
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  geom_ribbon(
    data = subset(
      df,
      x >= mu_delta_0 + 1.96*se_delta &
        distribution == "mu_delta"
    ),
    aes(x=x, ymin = 0, ymax = density),
    fill = "orange",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "mu_delta_0" & x >= mu_delta_0+1.96*se_delta),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "crosshatch",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "mu_delta_0" & x <= mu_delta_0-1.96*se_delta),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "crosshatch",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
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
  geom_segment(aes(x = mu_delta_0, xend = mu_delta_0, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_delta, xend = mu_delta, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_delta_0-1.96*se_delta, xend = mu_delta_0-1.96*se_delta, y = 0, yend = dens_1), color = "brown", linetype = "dashed", linewidth = 1) +
  geom_segment(aes(x = mu_delta_0+1.96*se_delta, xend = mu_delta_0+1.96*se_delta, y = 0, yend = dens_2), color = "brown", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 2.5, y = 0.2, label = expression(1 - beta %~~% 0.80), color = "orange", size = 5) +
  annotate("text", x = 1.7, y = 0.1, label = expression(alpha/2 == 0.025), color = "brown", size = 4) +
  annotate("text", x = -1.5, y = 0.1, label = expression(alpha/2 == 0.025), color = "brown", size = 4)
##############################



##############################

# Inferens 6

x <- seq(-2, 3, length.out = 1000)

mu_delta = 1.5
mu_delta_0 = 0
se_delta = 0.45

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = mu_delta_0, sd = se_delta),
    dnorm(x, mean = mu_delta, sd = se_delta)
  ),
  distribution = factor(rep(c("mu_delta_0", "mu_delta"), each = length(x)))
)

dens_1 = dnorm((mu_delta_0-1.96*se_delta), mean = mu_delta_0, sd = se_delta)
dens_2 = dnorm((mu_delta_0+1.96*se_delta), mean = mu_delta, sd = se_delta)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(mu_delta_0, mu_delta),
    labels = c(
      bquote(mu[delta] == .(round(mu_delta_0,2))),
      bquote(mu[delta] == .(round(mu_delta,2)))
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  geom_ribbon(
    data = subset(
      df,
      x >= mu_delta_0 + 1.96*se_delta &
        distribution == "mu_delta"
    ),
    aes(x=x, ymin = 0, ymax = density),
    fill = "orange",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "mu_delta_0" & x >= mu_delta_0+1.96*se_delta),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "crosshatch",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "mu_delta_0" & x <= mu_delta_0-1.96*se_delta),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "crosshatch",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
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
  geom_segment(aes(x = mu_delta_0, xend = mu_delta_0, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_delta, xend = mu_delta, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_delta_0-1.96*se_delta, xend = mu_delta_0-1.96*se_delta, y = 0, yend = dens_1), color = "brown", linetype = "dashed", linewidth = 1) +
  geom_segment(aes(x = mu_delta_0+1.96*se_delta, xend = mu_delta_0+1.96*se_delta, y = 0, yend = dens_2), color = "brown", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 2.8, y = 0.2, label = expression(1 - beta %~~% 0.92), color = "orange", size = 5) +
  annotate("text", x = 1.92, y = 0.07, label = expression(alpha/2 == 0.025), color = "brown", size = 4) +
  annotate("text", x = -1.5, y = 0.1, label = expression(alpha/2 == 0.025), color = "brown", size = 4)
##############################



##############################

# Inferens 7

set.seed(123)

n <- 10
B <- 9 * 12

sim <- lapply(1:B, function(i){
  
  x <- rnorm(n, 0, 1)
  y <- rnorm(n, 1, 1)
  
  # Two-sample z-test (known sigma = 1)
  z <- (mean(x) - mean(y)) / sqrt(2 / n)
  p <- 2 * (1 - pnorm(abs(z)))
  
  data.frame(
    value = c(x, y),
    group = rep(c("Population 1", "Population 2"), each = n),
    experiment = i,
    p = p,
    significant = p < 0.05
  )
  
})

df <- do.call(rbind, sim)

# Label for each facet
df$lab <- sprintf(
  "%s Experiment %d\np = %.3f",
  ifelse(df$significant, "✔", "✖"),
  df$experiment,
  df$p
)

# Which page (9 experiments/page)
df$page <- ceiling(df$experiment / 9)

df_summary <- df |>
  group_by(page, experiment, lab, group) |>
  summarise(
    mean = mean(value),
    sd = sd(value),
    .groups = "drop"
  )

df_summary$xpos <- ifelse(df_summary$group == "Population 1", 1.30, 2.30)


plot_page <- function(page_no){
  
  ggplot(
    subset(df, page == page_no),
    aes(group, value, fill = group)
  ) +
    
    geom_jitter(
      width = 0.08,
      size = 2,
      shape = 21,
      stroke = 1.0,
      colour = "black"
    ) +
    
    geom_errorbar(
      data = subset(df_summary, page == page_no),
      aes(
        x = xpos,
        ymin = mean - sd,
        ymax = mean + sd
      ),
      width = 0.1,
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +
    
    geom_point(
      data = subset(df_summary, page == page_no),
      aes(
        x = xpos,
        y = mean,
        fill = group
      ),
      shape = 21,
      size = 3,
      colour = "black",
      stroke = 0.6,
      inherit.aes = FALSE
    ) +
    
    facet_wrap(
      ~lab,
      ncol = 3,
      axes = "all"
    ) +
    
    scale_fill_manual(
      name = NULL,
      values = c(
        "Population 1" = "#E64B35", # coral
        "Population 2" = "#4DBBD5" # bright cyan
      ),
      labels = c(
        "Population 1" = "N(0,1)",
        "Population 2" = "N(1,1)"
      )
    ) +
    
    labs(
      x = NULL,
      y = NULL
    ) +
    
    theme_minimal(base_size = 12) +
    
    theme(
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(face = "bold"),
      axis.line = element_line(colour = "black"),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.ticks.x = element_blank()
      #axis.line.x = element_blank()
    )
  
}

pvalues <- sapply(sim, function(d) unique(d$p))
n_significant <- sum(pvalues < 0.05)
n_total <- length(pvalues)
power_empirical <- n_significant / n_total

se <- sqrt(2 / n) 
lambda <- 1 / se 
z_alpha <- qnorm(1 - 0.05/2) 
power_theoretical <- pnorm(-z_alpha - lambda) + (1 - pnorm(z_alpha - lambda))

power_theoretical
power_empirical

plot_page(1) # inferens_7_1
plot_page(2) # inferens_7_2
plot_page(3) # inferens_7_3
plot_page(4) # inferens_7_4
plot_page(5) # inferens_7_5
plot_page(6) # inferens_7_6
plot_page(7) # inferens_7_7
plot_page(8) # inferens_7_8
plot_page(9) # inferens_7_9
plot_page(10) # inferens_7_10
plot_page(11) # inferens_7_11
plot_page(12) # inferens_7_12

# make images 450 x 450


#dir.create("figures", showWarnings = FALSE)

for(page_no in 1:12){
  
  ggsave(
    filename = sprintf(
      "C:/vetenskap-projekt/del_II_statistik/bilder/inferens_7_%d.png",
      page_no
    ),
    plot = plot_page(page_no),
    width = 1500,
    height = 1500,
    units ="px"
  )
  
}

##############################



##############################

# SD vs SE exempel 1

# Parameters
mu <- 100
sigma <- 20
sample_sizes <- c(10, 20, 50, 100)

# Colors
cols <- c(
  "10" = "brown",
  "20" = "blue",
  "50" = "green",
  "100" = "red"
)

# X-axis values
x <- seq(80, 120, length.out = 1000)

# Create plotting data
plot_data <- data.frame()

for (n in sample_sizes) {
  
  se <- sigma / sqrt(n)
  
  plot_data <- rbind(
    plot_data,
    data.frame(
      x = x,
      density = dnorm(x, mean = mu, sd = se),
      n = factor(n)
    )
  )
}

# Plot
ggplot(plot_data, aes(x = x, y = density, colour = n)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = mu,
             linetype = "dashed",
             colour = "black",
             linewidth = 0.8) +
  scale_color_manual(values = cols, name = "Stickprovsstorlek (n)") +
  labs(
    title = NULL,
    x = "Stickprovsmedelvärde",
    y = NULL
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14)
  )
##############################



##############################

# SD vs SE exempel 2

# Parameters
mu_A <- 100
mu_B <- 110
sigma <- 20
n <- 35
se <- sigma / sqrt(n)

# 95% confidence limits
A_lower <- mu_A - 1.96 * se
A_upper <- mu_A + 1.96 * se

B_lower <- mu_B - 1.96 * se
B_upper <- mu_B + 1.96 * se

#shade_A = x >= A_lower & x <= A_upper
#shade_B = x >= B_lower & x <= B_upper


# X values
x <- seq(80, 130, length.out = 1000)

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = mu_A, sd = se),
    dnorm(x, mean = mu_B, sd = se)
  ),
  distribution = factor(rep(c("Grupp A", "Grupp B"), each = length(x))),
  shade = c(
    x >= A_lower & x <= A_upper,
    x >= B_lower & x <= B_upper
  )
)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(mu_A, mu_B),
    labels = c(
      expression(mu[A] == 100),
      expression(mu[B] == 110)
    )
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  geom_ribbon(
    data = subset(df, shade),
    aes(ymin = 0, ymax = density, fill = distribution),
    alpha = 0.3,
    colour = NA
  ) +
  scale_fill_manual(
    values = c("Grupp A" = "red",
               "Grupp B" = "blue"),
    name = expression(paste("95% CI (n = 35)"))
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
  geom_segment(aes(x = mu_A, xend = mu_A, y = 0, yend = max(df$density)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_segment(aes(x = mu_B, xend = mu_B, y = 0, yend = max(df$density)), color = "blue", linetype = "dashed", linewidth = 1)
##############################



##############################

# SD vs SE exempel 3

x <- seq(-20, 30, length.out = 1000)


# Parameters
mu_A <- 100
mu_B <- 110
sigma <- 20
n <- 35
se <- sigma / sqrt(n)

mu_delta_0 = 0
mu_delta = mu_B - mu_A
se_delta = sqrt(se^2+se^2)


df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = mu_delta_0, sd = se_delta),
    dnorm(x, mean = mu_delta, sd = se_delta)
  ),
  distribution = factor(rep(c("mu_delta_0", "mu_delta"), each = length(x)))
)


crit_val <- mu_delta_0 + 1.96*se_delta
shade_alpha <- subset(df, distribution == "mu_delta_0" & x >= crit_val)

dens_1 <- dnorm(-crit_val, mean = mu_delta_0, sd = se_delta)
dens_2 <- dnorm(crit_val, mean = mu_delta, sd = se_delta)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_ribbon(
    data = subset(df, x >= crit_val & distribution == "mu_delta"),
    aes(x = x, ymin = 0, ymax = density),
    alpha = 0.5,
    inherit.aes = FALSE,
    fill = "orange"
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "mu_delta_0" & x >= crit_val),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "crosshatch",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "mu_delta_0" & x <= -crit_val),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "crosshatch",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
  ) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(mu_delta_0, mu_delta),
    labels = c(
      bquote(mu[delta]==.(round(mu_delta_0,2))),
      bquote(mu[delta]==.(round(mu_delta,2))
    ))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
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
  geom_segment(aes(x = mu_delta_0, xend = mu_delta_0, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_delta, xend = mu_delta, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  annotate("text", x = 20, y = 0.06, label = expression(1 - beta %~~% 0.553), color = "orange", size = 5) +
  
  geom_segment(aes(x = crit_val, xend = crit_val, y = 0, yend = dens_2),
               color = "brown", linetype = "dashed", linewidth = 1) +
  geom_segment(aes(x = -crit_val, xend = -crit_val, y = 0, yend = dens_1),
               color = "brown", linetype = "dashed", linewidth = 1)
##############################



##############################

# SD vs SE exempel 4

x <- seq(-20, 30, length.out = 1000)


# Parameters
mu_A <- 100
mu_B <- 110
sigma <- 20
n <- 70
se <- sigma / sqrt(n)

mu_delta_0 = 0
mu_delta = mu_B - mu_A
se_delta = sqrt(se^2+se^2)


df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = mu_delta_0, sd = se_delta),
    dnorm(x, mean = mu_delta, sd = se_delta)
  ),
  distribution = factor(rep(c("mu_delta_0", "mu_delta"), each = length(x)))
)


crit_val <- mu_delta_0 + 1.96*se_delta
shade_alpha <- subset(df, distribution == "mu_delta_0" & x >= crit_val)

dens_1 <- dnorm(-crit_val, mean = mu_delta_0, sd = se_delta)
dens_2 <- dnorm(crit_val, mean = mu_delta, sd = se_delta)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_ribbon(
    data = subset(df, x >= crit_val & distribution == "mu_delta"),
    aes(x = x, ymin = 0, ymax = density),
    alpha = 0.5,
    inherit.aes = FALSE,
    fill = "orange"
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "mu_delta_0" & x >= crit_val),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "crosshatch",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "mu_delta_0" & x <= -crit_val),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "crosshatch",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
  ) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(mu_delta_0, mu_delta),
    labels = c(
      bquote(mu[delta]==.(round(mu_delta_0,2))),
      bquote(mu[delta]==.(round(mu_delta,2))
      ))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
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
  geom_segment(aes(x = mu_delta_0, xend = mu_delta_0, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_delta, xend = mu_delta, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  annotate("text", x = 20, y = 0.06, label = expression(1 - beta %~~% 0.841), color = "orange", size = 5) +
  
  geom_segment(aes(x = crit_val, xend = crit_val, y = 0, yend = dens_2),
               color = "brown", linetype = "dashed", linewidth = 1) +
  geom_segment(aes(x = -crit_val, xend = -crit_val, y = 0, yend = dens_1),
               color = "brown", linetype = "dashed", linewidth = 1)
##############################



##############################

# SD vs SE exempel 5

# Parameters
mu_A <- 100
mu_B <- 110
sigma <- 20
n <- 70
se <- sigma / sqrt(n)

# X values
x <- seq(35, 175, length.out = 1000)

summary_df <- data.frame(
  group = c("Grupp A", "Grupp B", "Grupp A", "Grupp B"),
  type  = c("sample", "sample", "mean", "mean"),
  mean  = c(mu_A, mu_B, mu_A, mu_B),
  sd    = c(sigma, sigma, se, se)
)

summary_df$type <- factor(
  summary_df$type,
  levels = c("sample", "mean")
)

df_dist <- do.call(rbind, lapply(1:nrow(summary_df), function(i) {
  
  dens <- dnorm(
    x,
    mean = summary_df$mean[i],
    sd   = summary_df$sd[i]
  )
  
  data.frame(
    x = x,
    density = dens / max(dens),
    group = summary_df$group[i],
    type = summary_df$type[i]
  )
}))


ggplot(df_dist, aes(x = x, y = density, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_area(alpha = 0.2, position = "identity") +
  
  facet_wrap(~ type, nrow = 1,
             labeller = labeller(
               type = c(
                 "sample"   = "Stickprov (SD)",
                 "mean" = "Medelvärden (SE)"
               )
             )) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12)
  ) + 
  scale_color_manual(
    name = NULL,
    values = c("Grupp A" = "red", "Grupp B" = "blue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Grupp A" = "red", "Grupp B" = "blue")
  ) +
  geom_segment(aes(x = mu_A, xend = mu_A, y = 0, yend = max(df_dist$density)), color = "red", linetype = 2, linewidth = 1) +
  geom_segment(aes(x = mu_B, xend = mu_B, y = 0, yend = max(df_dist$density)), color = "blue", linetype = 2, linewidth = 1)
##############################



##############################

# Pålitlighet 1

x <- seq(-5, 5, length.out = 1000)

df <- data.frame(
  x = x,
  density = dnorm(x, mean = 0, sd = 1)
)

# Positions for vertical lines
vline_positions <- c(-2, -1, 0, 1, 2)

# Data frame for vertical segments
vline_df <- data.frame(
  x = vline_positions,
  y_start = 0,
  y_end = dnorm(vline_positions, mean = 0, sd = 1)
)

ggplot(df, aes(x = x, y = density)) +
  geom_line(linewidth = 1.2, color = "black") +
  geom_segment(
    data = vline_df,
    aes(x = x, xend = x, y = y_start, yend = y_end),
    linetype = c(2, 2, 1, 2, 2),    # dashed for -1 and 1, solid for 0
    linewidth = 1,
    color = "black"
  ) +
  geom_ribbon(
    data = subset(df, x <= -2),
    aes(x = x, ymin = 0, ymax = density),
    inherit.aes = FALSE,
    fill = "red",
    alpha = 0.5
  ) +
  geom_ribbon(
    data = subset(df, x >= 2),
    aes(x = x, ymin = 0, ymax = density),
    inherit.aes = FALSE,
    fill = "red",
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = c(-2, -1, 0, 1, 2),
    labels = expression(-2*s[delta], -s[delta], bar(delta), +s[delta], +2*s[delta])
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
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
  annotate("text", x = 2.9, y = 0.06, label = expression(p < 0.025), color = "red", size = 5) +
  annotate("text", x = -2.8, y = 0.06, label = expression(p < 0.025), color = "red", size = 5)

##############################



##############################

# Pålitlighet 2

x <- seq(-4, 6, length.out = 1000)

mu_A <- 0
mu_B <- 2
sigma <- 1

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = mu_A, sd = sigma),
    dnorm(x, mean = mu_B, sd = sigma)
  ),
  distribution = factor(rep(c("Mean A", "Mean B"), each = length(x)))
)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_ribbon(
    data = subset(df, x <= -2 & distribution == "Mean A"),
    aes(x = x, ymin = 0, ymax = density),
    alpha = 0.5,
    inherit.aes = FALSE,
    fill = "orange"
  ) +
  geom_ribbon(
    data = subset(df, x >= 2 & distribution == "Mean A"),
    aes(x = x, ymin = 0, ymax = density),
    alpha = 0.5,
    inherit.aes = FALSE,
    fill = "orange"
  ) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(0, 2),
    labels = c(
      expression(mu[A]),
      expression(mu[B])
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
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
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = 2, xend = 2, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  annotate("text", x = 2.9, y = 0.06, label = expression(p < 0.025), color = "orange", size = 5) +
  annotate("text", x = -2.8, y = 0.06, label = expression(p < 0.025), color = "orange", size = 5) +
  annotate("segment",
           x = mu_A, xend = mu_A + sigma,
           y = 0.22, yend = 0.22,
           arrow = arrow(length = unit(0.15, "cm"))) +
  
  annotate("segment",
           x = mu_A, xend = mu_A - sigma,
           y = 0.22, yend = 0.22,
           arrow = arrow(length = unit(0.15, "cm"))) +
  
  # Arrow: ±2 SD
  annotate("segment",
           x = mu_A, xend = mu_A + 2*sigma,
           y = 0.04, yend = 0.04,
           arrow = arrow(length = unit(0.15, "cm"))) +
  
  annotate("segment",
           x = mu_A, xend = mu_A - 2*sigma,
           y = 0.04, yend = 0.04,
           arrow = arrow(length = unit(0.15, "cm"))) +
  # Double arrow for delta:
  annotate("segment",
           x = mu_A + 1*sigma, xend = mu_A + 2*sigma,
           y = 0.35, yend = 0.35,
           arrow = arrow(length = unit(0.15, "cm"))) +
  
  annotate("segment",
           x = mu_A + 1*sigma, xend = mu_A,
           y = 0.35, yend = 0.35,
           arrow = arrow(length = unit(0.15, "cm"))) +
  # Labels
  annotate("text", x = mu_A + 0.5*sigma, y = 0.24, label = expression(1*sigma)) +
  annotate("text", x = mu_A - 0.5*sigma, y = 0.24, label = expression(1*sigma)) +
  annotate("text", x = mu_A + sigma, y = 0.06, label = expression(2*sigma)) +
  annotate("text", x = mu_A - sigma, y = 0.06, label = expression(2*sigma)) +
  annotate("text", x = mu_A + sigma, y = 0.37, label = expression(delta))

##############################



##############################

# Överlappning 1

x <- seq(-4, 6, length.out = 1000)

mu_A <- 0
mu_B <- 2
sigma <- 1

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = mu_A, sd = sigma),
    dnorm(x, mean = mu_B, sd = sigma)
  ),
  distribution = factor(rep(c("Mean A", "Mean B"), each = length(x)))
)

# Densities
dA <- dnorm(x, mean = mu_A, sd = sigma)
dB <- dnorm(x, mean = mu_B, sd = sigma)

# Pointwise minimum
overlap <- pmin(dA, dB)

# Numerical integration
dx <- diff(x)[1]
OVL <- sum(overlap) * dx

overlap_df <- data.frame(
  x = x,
  overlap = overlap
)


ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_ribbon(
    data = overlap_df,
    aes(x = x, ymin = 0, ymax = overlap),
    inherit.aes = FALSE,
    fill = "orange",
    alpha = 0.5
  ) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(mu_A, mu_B),
    labels = c(
      expression(mu[A] == 0),
      expression(mu[B] == 2)
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.03))
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
  geom_segment(aes(x = mu_A, xend = mu_A, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_B, xend = mu_B, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  annotate("text", x = 1, y = 0.42, label = expression(OVL %~~% 0.32), color = "orange", size = 5)
##############################


##############################

# P(B>A) 1

x <- seq(-5, 7, length.out = 100)

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = 0, sd = 1),
    dnorm(x, mean = 2, sd = 1)
  ),
  distribution = factor(rep(c("Mean 0", "Mean 2"), each = length(x)))
)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(0, 2),
    labels = c(
      expression(mu[A] == 0),
      expression(mu[B] == 2)
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
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
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(df$density)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_segment(aes(x = 2, xend = 2, y = 0, yend = max(df$density)), color = "red", linetype = "dashed", linewidth = 1)
##############################



##############################

# P(B>A) 2

mu_diff <- 2 - 0
sd_diff <- sqrt(1^2 + 1^2)

x <- seq(-3, 7, length.out = 400)
density <- dnorm(x, mean = mu_diff, sd = sd_diff)

df_diff <- data.frame(x = x, density = density)

df_shade <- subset(df_diff, x >= 0)
p_superiority <- 1 - pnorm(0, mean = mu_diff, sd = sd_diff)

ggplot(df_diff, aes(x = x, y = density)) +
  geom_line(linewidth = 1.2, color = "black") +
  
  geom_ribbon(
    data = df_shade,
    aes(ymin = 0, ymax = density),
    fill = "orange",
    alpha = 0.35
  ) +
  
  # Mean difference line (red)
  geom_segment(
    aes(
      x = mu_diff, xend = mu_diff,
      y = 0, yend = dnorm(mu_diff, mu_diff, sd_diff)
    ),
    color = "red", linetype = "dashed", linewidth = 1.2
  ) +
  
  # Zero reference line (grey)
  geom_segment(
    aes(
      x = 0, xend = 0,
      y = 0, yend = dnorm(0, mu_diff, sd_diff)
    ),
    color = "grey40", linetype = "dashed", linewidth = 1.2
  ) +
  
  annotate(
    "text",
    x = 5, y = max(df_diff$density) * 0.85,
    label = paste0("P(B > A) \u2248 ", sprintf("%.2f", p_superiority)),
    size = 6,
    color = "orange"
  ) +
  
  scale_x_continuous(
    breaks = c(0, mu_diff),
    labels = c(
      expression(0),
      expression(mu[delta] == 2)
    )
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14)
  )
##############################



##############################

# RI & OR exempel 1

# Parameters
p0 <- 0.5
z0 <- qnorm(p0)

d <- 0.8
z1 <- z0 + d
p1 <- pnorm(z1)

x <- seq(-4, 4, length.out = 100)

theme_common <- theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text = element_text(face = "bold")
  )

p_ref <- ggplot(data.frame(x = x), aes(x)) +
  
  geom_ribbon(
    data = subset(data.frame(x = x), x <= z0),
    aes(
      ymin = 0,
      ymax = dnorm(x)
    ),
    fill = "steelblue",
    alpha = 0.35
  ) +
  
  geom_line(aes(y = dnorm(x)), linewidth = 1.2) +
  
  annotate(
    "segment",
    x = z0,
    xend = z0,
    y = 0,
    yend = dnorm(z0),
    linewidth = 1,
    linetype = "solid"
  ) +
  
  annotate(
    "text",
    x = z0 - 1,
    y = 0.04,
    label = expression(p[0]),
    size = 6
  ) +
  
  scale_x_continuous(
    breaks = z0,
    labels = expression(z[0]),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  
  coord_cartesian(xlim = c(-4, 4)) +
  
  theme_common


p_trt <- ggplot(data.frame(x = x), aes(x)) +
  
  geom_ribbon(
    data = subset(data.frame(x = x),
                  x <= z1),
    aes(
      ymin = 0,
      ymax = dnorm(x)
    ),
    fill = "orange",
    alpha = 0.45
  ) +
  
  geom_line(aes(y = dnorm(x)), linewidth = 1.2) +
  
  annotate(
    "segment",
    x = z0,
    xend = z0,
    y = 0,
    yend = dnorm(z0),
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  annotate(
    "segment",
    x = z1,
    xend = z1,
    y = 0,
    yend = dnorm(z1),
    linewidth = 1
  ) +
  
  annotate(
    "segment",
    x = z0,
    xend = z1,
    y = 0.05,
    yend = 0.05,
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  
  annotate(
    "text",
    x = (z0 + z1) / 2,
    y = 0.065,
    label = "d"
  ) +
  
  annotate(
    "text",
    x = z0 - 1,
    y = 0.04,
    label = expression(p[1]),
    size = 6
  ) +
  
  scale_x_continuous(
    breaks = c(z0, z1),
    labels = c(expression(z[0]), expression(z[1])),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  
  coord_cartesian(xlim = c(-4, 4)) +
  
  theme_common


p_ref | p_trt

##############################



##############################

# RI & OR exempel 2

p0 <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)
d  <- c(0.2, 0.5, 0.8, 1.0)

tab <- expand.grid(
  p0 = p0,
  d = d
)

tab$z0 <- qnorm(tab$p0)
tab$z1 <- tab$z0 + tab$d
tab$p1 <- pnorm(tab$z1)
tab$ID <- tab$p1 - tab$p0
tab$RI <- tab$p1 / tab$p0
tab$OR <- (tab$p1 / (1 - tab$p1)) / (tab$p0 / (1 - tab$p0))

tab

make_table <- function(d_value){
  
  tab |>
    filter(d == d_value) |>
    select(p0, z0, z1, p1, ID, RI, OR) |>
    kable(
      format = "latex",
      booktabs = TRUE,
      linesep = "",
      digits = 4,
      escape = FALSE,
      align = "rrrrrrr",
      table.envir = "table",
      position = "H",
      col.names = c(
        "$p_0$",
        "$z_0$",
        "$z_1$",
        "$p_1$",
        "ID",
        "RI",
        "OR"
      )
    ) |>
    add_header_above(
      setNames(7, sprintf("Cohen's $d = %.1f$", d_value)),
      escape = FALSE
    ) |>
    kable_styling()
  
}

d_values <- d

for (i in d_values) {
  print(make_table(i))
}

##############################



##############################

# RI & OR exempel 3

p0 <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)
d  <- c(-0.2, -0.5, -0.8, -1.0)

tab <- expand.grid(
  p0 = p0,
  d = d
)

tab$z0 <- qnorm(tab$p0)
tab$z1 <- tab$z0 + tab$d
tab$p1 <- pnorm(tab$z1)
tab$ID <- tab$p1 - tab$p0
tab$RI <- tab$p1 / tab$p0
tab$OR <- (tab$p1 / (1 - tab$p1)) / (tab$p0 / (1 - tab$p0))

tab

make_table <- function(d_value){
  
  tab |>
    filter(d == d_value) |>
    select(p0, z0, z1, p1, ID, RI, OR) |>
    kable(
      format = "latex",
      booktabs = TRUE,
      linesep = "",
      digits = 4,
      escape = FALSE,
      align = "rrrrrrr",
      table.envir = "table",
      position = "H",
      col.names = c(
        "$p_0$",
        "$z_0$",
        "$z_1$",
        "$p_1$",
        "ID",
        "RI",
        "OR"
      )
    ) |>
    add_header_above(
      setNames(7, sprintf("Cohen's $d = %.1f$", d_value)),
      escape = FALSE
    ) |>
    kable_styling()
  
}

d_values <- d

for (i in d_values) {
  print(make_table(i))
}

##############################



##############################

# Regression 1

# -----------------------------
# Parameters
# -----------------------------

beta0 <- 1
beta1 <- 0.35
sigma <- 0.5

x_values <- 1:6

# Regression line
x_line <- seq(0.5, 8.5, length.out = 500)

df_line <- data.frame(
  x = x_line,
  y = beta0 + beta1 * x_line
)


# -----------------------------
# Conditional distributions
# -----------------------------

z <- seq(-3.5, 3.5, length.out = 300)

# Controls how wide the distributions appear horizontally
density_scale <- 2.5

df_dist <- do.call(
  rbind,
  lapply(x_values, function(xi) {
    
    mu <- beta0 + beta1 * xi
    
    data.frame(
      x = xi + density_scale * dnorm(z),
      y = mu + sigma * z,
      xi = xi
    )
    
  })
)


# -----------------------------
# Example observations
# -----------------------------

set.seed(123)

df_obs <- data.frame(
  x = x_values,
  y = beta0 + beta1 * x_values +
    rnorm(length(x_values), 0, sigma)
)

# Observation at x3
y3_obs <- df_obs$y[df_obs$x == x_values[3]]

# Expected value at x3
y3_hat <- beta0 + beta1 * x_values[3]


# -----------------------------
# Plot
# -----------------------------

ggplot() +
  
  # Regression line
  geom_line(
    data = df_line,
    aes(x, y),
    linewidth = 1
  ) +
  
  annotate(
    "text",
    x = 4.8,
    y = beta0 + beta1 * 4.8 + 0.15,
    label = expression(mu["Y|x"] == beta[0] + beta[1] * x),
    hjust = -1.2,
    vjust = -0.2,
    angle = atan(beta1 - 0.15) * 180 / pi
  ) +
  
  # Conditional normal distributions
  geom_path(
    data = df_dist,
    aes(x, y, group = xi),
    colour = "deepskyblue2",
    linewidth = 1
  ) +
  
  # Vertical reference lines
  geom_segment(
    data = data.frame(x = x_values),
    aes(
      x = x,
      xend = x,
      y = beta0 + beta1 * x - 3.5 * sigma,
      yend = beta0 + beta1 * x + 3.5 * sigma
    ),
    linetype = "dashed",
    linewidth = 0.5
  ) +
  
  # Observations
  geom_point(
    data = df_obs,
    aes(x, y),
    size = 3
  ) +
  
  # x labels
  scale_x_continuous(
    breaks = x_values,
    labels = as.expression(
      lapply(x_values, function(i) {
        bquote(x[.(i)])
      })
    )
  ) +
  
  labs(
    x = "x",
    y = "y"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  coord_cartesian(
    ylim = c(0, 6)
  ) +
  
  # show sigma^2 notation on graph
  
  annotate(
    "segment",
    x = x_values[1] - 0.35,
    xend = x_values[1] - 0.35,
    y = beta0 + beta1*x_values[1] - 3 * sigma,
    yend = beta0 + beta1*x_values[1] + 3 * sigma,
    linewidth = 0.6,
    linetype = "solid",
    color = "orange"
  ) +
  
  annotate(
    "segment",
    x = x_values[1] - 0.1,
    xend = x_values[1] - 0.35,
    y = beta0 + beta1*x_values[1] - 3 * sigma,
    yend = beta0 + beta1*x_values[1] - 3 * sigma,
    linewidth = 0.6,
    linetype = "solid",
    color = "orange"
  ) +
  
  annotate(
    "segment",
    x = x_values[1] - 0.1,
    xend = x_values[1] - 0.35,
    y = beta0 + beta1*x_values[1] + 3 * sigma,
    yend = beta0 + beta1*x_values[1] + 3 * sigma,
    linewidth = 0.6,
    linetype = "solid",
    color = "orange"
  ) +
  
  annotate(
    "text",
    x = x_values[1] - 0.1,
    y = beta0 + beta1*x_values[1] + 2.0,
    label = expression(sigma^2),
    hjust = 1,
    color = "orange"
  ) +
  
  # show epsilon notation on graph
  annotate(
    "segment",
    x = x_values[3] - 0.15,
    xend = x_values[3] - 0.15,
    y = beta0 + beta1*x_values[3],
    yend = beta0 + beta1*x_values[3] + 1.6 * sigma,
    linewidth = 0.6,
    linetype = "solid",
    color = "red"
  ) +
  
  annotate(
    "segment",
    x = x_values[3] - 0.05,
    xend = x_values[3] - 0.15,
    y = beta0 + beta1*x_values[3],
    yend = beta0 + beta1*x_values[3],
    linewidth = 0.6,
    linetype = "solid",
    color = "red"
  ) +
  
  annotate(
    "segment",
    x = x_values[3] - 0.05,
    xend = x_values[3] - 0.15,
    y = beta0 + beta1*x_values[3] + 1.6 * sigma,
    yend = beta0 + beta1*x_values[3] + 1.6 * sigma,
    linewidth = 0.6,
    linetype = "solid",
    color = "red"
  ) +
  
  annotate(
    "text",
    x = x_values[3] - 0.15,
    y = (y3_hat + y3_obs) / 2 + 0.7,
    label = expression(epsilon),
    hjust = 1,
    color = "red"
  )
##############################



##############################

# Regression 2

# Example data
set.seed(123)

# Parameters
beta0 <- 3
beta1 <- 0.8
sigma <- 4

# x values
x <- seq(2, 30, length.out = 25)

# Generate y values
y <- beta0 + beta1 * x + rnorm(
  length(x),
  mean = 0,
  sd = sigma
)

df <- data.frame(
  x = x,
  y = y
)

model <- lm(y ~ x, data = df)

summary(model)
coef(model)

x_new <- seq(
  min(df$x),
  max(df$x),
  length.out = 500
)

newdata <- data.frame(x = x_new)

pred <- predict(
  model,
  newdata = newdata,
  interval = "confidence",
  level = 0.95
)

pred_pi <- predict(
  model,
  newdata = newdata,
  interval = "prediction",
  level = 0.95
)

df_pred <- data.frame(
  x = x_new,
  fit = pred[, "fit"],
  conf_low = pred[, "lwr"],
  conf_high = pred[, "upr"],
  pred_low = pred_pi[, "lwr"],
  pred_high = pred_pi[, "upr"]
)

ggplot() +
  
  # 95% prediction interval
  geom_line(
    data = df_pred,
    aes(
      x = x,
      y = pred_low,
      colour = "95%-Prediktionsintervall"
    ),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  geom_line(
    data = df_pred,
    aes(
      x = x,
      y = pred_high,
      colour = "95%-Prediktionsintervall"
    ),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  # 95% confidence interval for mean response
  geom_line(
    data = df_pred,
    aes(
      x = x,
      y = conf_low,
      colour = "95%-KI för medelrespons"
    ),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  geom_line(
    data = df_pred,
    aes(
      x = x,
      y = conf_high,
      colour = "95%-KI för medelrespons"
    ),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  # Raw observations
  geom_point(
    data = df,
    aes(
      x = x,
      y = y,
      colour = "Observationer"
    ),
    size = 2.5
  ) +
  
  # Regression line
  geom_line(
    data = df_pred,
    aes(
      x = x,
      y = fit,
      colour = "Regressionslinje"
    ),
    linewidth = 1.2
  ) +
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Observationer" = "black",
      "Regressionslinje" = "blue",
      "95%-KI för medelrespons" = "red",
      "95%-Prediktionsintervall" = "orange"
    )
  ) +
  
  labs(
    x = "x",
    y = "y"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "bottom"
  )

##############################



##############################

# Regression 3

set.seed(123)

beta0 <- 2
beta1 <- 0.8
sigma <- 2

x <- 1:8

epsilon <- rnorm(
  length(x),
  mean = 0,
  sd = sigma
)

y <- beta0 + beta1*x + epsilon

df <- data.frame(
  x = x,
  epsilon = epsilon,
  y = y
)

df

kable(
  df |>
    dplyr::select(x, y, epsilon),
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  col.names = c(
    "$x_i$",
    "$y_i$",
    "$\\epsilon_i$"
  ),
  escape = FALSE,
  table.envir = "table",
  position = "H",
  caption = "Exempeldata för linjär regression."
)

model <- lm(y ~ x, data = df)

coef(model)

beta0_hat <- coef(model)[1]
beta1_hat <- coef(model)[2]

beta0_hat
beta1_hat


kable(
  data.frame(
    parameter = c("$\\hat{\\beta}_0$", "$\\hat{\\beta}_1$"),
    estimate = c(beta0_hat, beta1_hat)
  ),
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  escape = FALSE,
  col.names = c(
    "Parameter",
    "Estimat"
  ),
  table.envir = "table",
  position = "H",
  caption = "Skattade regressionsparametrar."
)

df$y_hat <- beta0_hat + beta1_hat * df$x

y_bar <- mean(df$y)

kable(
  df |>
    dplyr::select(x, y, y_hat) |>
    dplyr::mutate(y_bar = y_bar) |>
    dplyr::select(x, y, y_bar, y_hat),
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  escape = FALSE,
  col.names = c(
    "$x_i$",
    "$y_i$",
    "$\\bar{y}$",
    "$\\hat{y}_i$"
  ),
  table.envir = "table",
  position = "H",
  caption = "Observerade, medelvärdesbaserade och predikterade responsvärden."
)

SS_tot <- sum((df$y - y_bar)^2)

SS_res <- sum((df$y - df$y_hat)^2)

R2 <- 1 - SS_res / SS_tot

kable(
  data.frame(
    quantity = c(
      "$SS_{\\mathrm{tot}}$",
      "$SS_{\\mathrm{res}}$",
      "$R^2$"
    ),
    value = c(
      SS_tot,
      SS_res,
      R2
    )
  ),
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  escape = FALSE,
  col.names = c(
    "Storhet",
    "Värde"
  ),
  table.envir = "table",
  position = "H",
  caption = "Variationsuppdelning och förklaringsgrad."
)

n <- nrow(df)

df_res <- n - 2

s2 <- SS_res / df_res
s <- sqrt(s2)

x_bar <- mean(df$x)

Sxx <- sum((df$x - x_bar)^2)

SE_beta1 <- s / sqrt(Sxx)

SE_beta0 <- s * sqrt(
  1/n + x_bar^2/Sxx
)

t_crit <- qt(0.975, df = df_res)

beta0_CI <- c(
  beta0_hat - t_crit * SE_beta0,
  beta0_hat + t_crit * SE_beta0
)

beta1_CI <- c(
  beta1_hat - t_crit * SE_beta1,
  beta1_hat + t_crit * SE_beta1
)

kable(
  data.frame(
    parameter = c(
      "$\\hat{\\beta}_0$",
      "$\\hat{\\beta}_1$"
    ),
    estimate = c(
      beta0_hat,
      beta1_hat
    ),
    SE = c(
      SE_beta0,
      SE_beta1
    ),
    lower = c(
      beta0_CI[1],
      beta1_CI[1]
    ),
    upper = c(
      beta0_CI[2],
      beta1_CI[2]
    )
  ),
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  escape = FALSE,
  col.names = c(
    "Parameter",
    "Skattning",
    "SE",
    "Nedre gräns",
    "Övre gräns"
  ),
  table.envir = "table",
  position = "H",
  caption = "95~\\% konfidensintervall för regressionsparametrarna."
)

x0 <- 5

y_hat_0 <- beta0_hat + beta1_hat * x0

SE_mean <- s * sqrt(
  1/n + (x0 - x_bar)^2/Sxx
)

CI_mean <- c(
  y_hat_0 - t_crit * SE_mean,
  y_hat_0 + t_crit * SE_mean
)

kable(
  data.frame(
    quantity = c(
      "$x_0$",
      "$\\hat{y}_0$",
      "$SE(\\hat{y}_0)$",
      "$t_{0.975,df_{\\mathrm{res}}}$",
      "Nedre gräns",
      "Övre gräns"
    ),
    value = c(
      x0,
      y_hat_0,
      SE_mean,
      t_crit,
      CI_mean[1],
      CI_mean[2]
    )
  ),
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  escape = FALSE,
  col.names = c(
    "Storhet",
    "Värde"
  ),
  table.envir = "table",
  position = "H",
  caption = "95~\\% konfidensintervall för medelresponsen vid $x_0=5$."
)

SE_pred <- s * sqrt(
  1 +
    1/n +
    (x0 - x_bar)^2/Sxx
)

PI <- c(
  y_hat_0 - t_crit * SE_pred,
  y_hat_0 + t_crit * SE_pred
)

kable(
  data.frame(
    quantity = c(
      "$x_0$",
      "$\\hat{y}_0$",
      "$SE_{\\mathrm{pred}}$",
      "$t_{0.975,df_{\\mathrm{res}}}$",
      "Nedre gräns",
      "Övre gräns"
    ),
    value = c(
      x0,
      y_hat_0,
      SE_pred,
      t_crit,
      PI[1],
      PI[2]
    )
  ),
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  escape = FALSE,
  col.names = c(
    "Storhet",
    "Värde"
  ),
  table.envir = "table",
  position = "H",
  caption = "95~\\% prediktionsintervall vid $x_0=5$."
)

# Grid of x-values for plotting
x_grid <- seq(
  min(df$x) - 0.5,
  max(df$x) + 0.5,
  length.out = 300
)

# Fitted mean response
y_fit <- beta0_hat + beta1_hat * x_grid

# SE for mean response
SE_mean_grid <- s * sqrt(
  1/n +
    (x_grid - x_bar)^2 / Sxx
)

# SE for prediction
SE_pred_grid <- s * sqrt(
  1 +
    1/n +
    (x_grid - x_bar)^2 / Sxx
)

# Confidence and prediction intervals
df_plot <- data.frame(
  x = x_grid,
  fit = y_fit,
  conf_low = y_fit - t_crit * SE_mean_grid,
  conf_high = y_fit + t_crit * SE_mean_grid,
  pred_low = y_fit - t_crit * SE_pred_grid,
  pred_high = y_fit + t_crit * SE_pred_grid
)

# Plot
ggplot() +
  
  # 95% prediction interval
  geom_line(
    data = df_plot,
    aes(x = x, y = pred_low, colour = "95%-Prediktionsintervall"),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  geom_line(
    data = df_plot,
    aes(x = x, y = pred_high, colour = "95%-Prediktionsintervall"),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  # 95% confidence interval for mean response
  geom_line(
    data = df_plot,
    aes(x = x, y = conf_low, colour = "95%-KI för medelrespons"),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  geom_line(
    data = df_plot,
    aes(x = x, y = conf_high, colour = "95%-KI för medelrespons"),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  # Observations
  geom_point(
    data = df,
    aes(x = x, y = y, colour = "Observationer"),
    size = 2.8
  ) +
  
  # Regression line
  geom_line(
    data = df_plot,
    aes(x = x, y = fit, colour = "Regressionslinje"),
    linewidth = 1.2
  ) +
  
  # regression euqation and R2
  annotate(
    "text",
    x = max(df$x) - 1,
    y = -1,
    label = sprintf(
      "atop(hat(y) == %.3f + %.3f*x, R^2 == %.3f)",
      beta0_hat,
      beta1_hat,
      R2
    ),
    parse = TRUE,
    hjust = 0,
    size = 5
  ) +
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Observationer" = "black",
      "Regressionslinje" = "blue",
      "95%-KI för medelrespons" = "red",
      "95%-Prediktionsintervall" = "orange"
    )
  ) +
  
  labs(
    x = "x",
    y = "y"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "bottom"
  )

##############################



##############################

# Regression 4

set.seed(123)

# Example data
x <- 0:10
y <- 2 + 3*x + rnorm(10, 0, 15)

df <- data.frame(x = x, y = y)

# Linear regression
model <- lm(y ~ x, data = df)

# Observed x-range
x_min <- min(df$x)
x_max <- max(df$x)

# Extended x-range for extrapolation
x_grid <- seq(
  x_min - 30,
  x_max + 30,
  length.out = 400
)

# Regression quantities
n <- nrow(df)
x_bar <- mean(df$x)
Sxx <- sum((df$x - x_bar)^2)

s <- summary(model)$sigma
df_res <- df.residual(model)

t_crit <- qt(0.975, df_res)

beta0_hat <- coef(model)[1]
beta1_hat <- coef(model)[2]

# Fitted values
fit <- beta0_hat + beta1_hat*x_grid

# SE for mean response
SE_mean <- s * sqrt(
  1/n +
    (x_grid - x_bar)^2/Sxx
)

# SE for prediction
SE_pred <- s * sqrt(
  1 +
    1/n +
    (x_grid - x_bar)^2/Sxx
)

# Intervals
df_plot <- data.frame(
  x = x_grid,
  fit = fit,
  conf_low = fit - t_crit*SE_mean,
  conf_high = fit + t_crit*SE_mean,
  pred_low = fit - t_crit*SE_pred,
  pred_high = fit + t_crit*SE_pred
)

# Plot
ggplot() +
  
  # Prediction interval
  geom_line(
    data = df_plot,
    aes(x, pred_low, colour = "95%-Prediktionsintervall"),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  geom_line(
    data = df_plot,
    aes(x, pred_high, colour = "95%-Prediktionsintervall"),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  # CI for mean response
  geom_line(
    data = df_plot,
    aes(x, conf_low, colour = "95%-KI för medelrespons"),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  geom_line(
    data = df_plot,
    aes(x, conf_high, colour = "95%-KI för medelrespons"),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  # Regression line
  geom_line(
    data = df_plot,
    aes(x, fit, colour = "Regressionslinje"),
    linewidth = 1.2
  ) +
  
  # Observed data
  geom_point(
    data = df,
    aes(x, y, colour = "Observationer"),
    size = 2.5
  ) +
  
  # Boundaries of observed data
  geom_vline(
    xintercept = c(x_min, x_max),
    linetype = "dotted",
    colour = "grey40"
  ) +
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Observationer" = "black",
      "Regressionslinje" = "blue",
      "95%-KI för medelrespons" = "red",
      "95%-Prediktionsintervall" = "orange"
    )
  ) +
  
  labs(
    x = "x",
    y = "y"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "bottom"
  )
##############################



##############################

# Regression 5

set.seed(123)

# Example binary data
n <- 20

x <- seq(-3, 3, length.out = n)

# Generate binary outcome from a logistic model
p <- plogis(-0.1 + 2*x)

y <- rbinom(
  n = n,
  size = 1,
  prob = p
)

df <- data.frame(
  x = x,
  y = y
)

# Logistic regression
model <- glm(
  y ~ x,
  data = df,
  family = binomial(link = "logit")
)

summary(model)

# Prediction grid
x_grid <- seq(
  min(df$x),
  max(df$x),
  length.out = 300
)

df_pred <- data.frame(
  x = x_grid
)

df_pred$p <- predict(
  model,
  newdata = df_pred,
  type = "response"
)

# Extract coefficients
beta0 <- coef(model)[1]
beta1 <- coef(model)[2]

# Regression equation
eq_label <- sprintf(
  "hat(p)(x)[Y==1] == frac(1, 1 + e^{-(%.3f %+.3f*x)})",
  beta0,
  beta1
)

ggplot(df, aes(x = x, y = y)) +
  
  # Binary observations
  geom_jitter(
    width = 0,
    height = 0,
    size = 2.5,
    colour = "black"
  ) +
  
  # Fitted logistic curve
  geom_line(
    data = df_pred,
    aes(x = x, y = p),
    colour = "blue",
    linewidth = 1.2,
    alpha = 0.5
  ) +
  
  annotate(
    "text",
    x = min(df$x) + 0.2,
    y = 0.9,
    label = eq_label,
    parse = TRUE,
    hjust = 0,
    size = 5
  ) +
  
  labs(
    x = "x",
    y = expression(p[Y==1])
  ) +
  
  scale_x_continuous(
    limits = c(-3.05, 3.05),
    breaks = c(-3.0, -2.0, -1.0, 0, 1.0, 2.0, 3.0)
  ) +
  
  scale_y_continuous(
    limits = c(-0.05, 1.05),
    breaks = c(0, 0.25, 0.5, 0.75, 1)
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black")
  )

##############################



##############################

# Robusthet 1

set.seed(123)

# Base observations
n <- 8

base <- rnorm(
  n,
  mean = 70,
  sd = 5
)

# Dataset without extreme observations
df_normal <- data.frame(
  value = base,
  scenario = "Inga extrema observationer"
)

# Dataset with extreme observations
df_extreme <- data.frame(
  value = c(
    base,
    100,
    105
  ),
  scenario = "Med extrema observationer"
)

# Combine datasets
df <- bind_rows(
  df_normal,
  df_extreme
)

# Calculate mean and median
df_summary <- df |>
  group_by(scenario) |>
  summarise(
    mean = mean(value),
    median = median(value),
    .groups = "drop"
  )

df_summary

ggplot(
  df,
  aes(x = 0, y = value)
) +
  
  geom_jitter(
    width = 0.02,
    height = 0,
    size = 3,
    shape = 21,
    fill = "white",
    colour = "black",
    stroke = 1
  ) +
  
  # Mean
  geom_segment(
    data = df_summary,
    aes(
      x = -0.04,
      xend = 0.04,
      y = mean,
      yend = mean,
      colour = "Medelvärde"
    ),
    linewidth = 1.2
  ) +
  
  # Median
  geom_segment(
    data = df_summary,
    aes(
      x = -0.04,
      xend = 0.04,
      y = median,
      yend = median,
      colour = "Median"
    ),
    linewidth = 1.2
  ) +
  
  scale_x_continuous(
    limits = c(-0.1, 0.1),
    breaks = NULL
  ) +
  
  facet_wrap(
    ~scenario,
    ncol = 2
  ) +
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Medelvärde" = "red",
      "Median" = "blue"
    )
  ) +
  
  labs(
    x = NULL,
    y = "Observation"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.text = element_text(face = "bold")
  )


##############################



##############################

# Robusthet 2

set.seed(123)

# Base observations
n <- 10

x_base <- 1:n

y_base <- 0.5 * x_base +
  rnorm(n, mean = 0, sd = 0.8)


# ----------------------------
# Dataset without extremes
# ----------------------------

df_normal <- data.frame(
  x = x_base,
  y = y_base,
  extreme = FALSE,
  scenario = "Utan extrema observationer"
)


# ----------------------------
# Dataset with extremes
# ----------------------------

df_extreme <- data.frame(
  x = c(x_base, 0, 12),
  y = c(y_base, 6.5, -1),
  extreme = c(
    rep(FALSE, n),
    TRUE,
    TRUE
  ),
  scenario = "Med extrema observationer"
)


# ----------------------------
# Combine
# ----------------------------

df <- bind_rows(
  df_normal,
  df_extreme
)

# Regression and correlation statistics
df_summary <- df |>
  group_by(scenario) |>
  summarise(
    slope = coef(lm(y ~ x))[2],
    intercept = coef(lm(y ~ x))[1],
    r = cor(x, y),
    R2 = summary(lm(y ~ x))$r.squared,
    .groups = "drop"
  )

df_summary


df_pred <- df |>
  group_by(scenario) |>
  group_modify(~ {
    
    model <- lm(y ~ x, data = .x)
    
    x_grid <- seq(
      min(.x$x) - 0.5,
      max(.x$x) + 0.5,
      length.out = 100
    )
    
    data.frame(
      x = x_grid,
      y = predict(
        model,
        newdata = data.frame(x = x_grid)
      )
    )
    
  })

df_summary$label <- sprintf(
  "r = %.2f\nR² = %.2f\nlutning = %.2f",
  df_summary$r,
  df_summary$R2,
  df_summary$slope
)

ggplot(
  df,
  aes(x = x, y = y)
) +
  
  geom_point(
    aes(colour = extreme),
    size = 3,
    shape = 21,
    fill = "white",
    stroke = 1
  ) +
  
  geom_line(
    data = df_pred,
    aes(x = x, y = y),
    colour = "blue",
    linewidth = 1.2
  ) +
  
  geom_text(
    data = df_summary,
    aes(
      x = 1,
      y = max(df$y),
      label = label
    ),
    hjust = 0,
    vjust = 1,
    inherit.aes = FALSE,
    size = 4.5
  ) +
  
  facet_wrap(
    ~scenario,
    ncol = 2
  ) +
  
  scale_colour_manual(
    values = c(
      "FALSE" = "black",
      "TRUE" = "red"
    ),
    labels = c(
      "FALSE" = "Observation",
      "TRUE" = "Extrem observation"
    ),
    name = NULL
  ) +
  
  labs(
    x = "x",
    y = "y"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )


##############################



##############################

# Reaktionshastighet 1


# Definiera reaktionsparametrar
k_plus <- 2.0    # Hastighetskonstant för framåtreaktionen
k_minus <- 1.0   # Hastighetskonstant för bakåtreaktionen

# ODE-funktion som beskriver systemet
reaction_ode <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Reaktionshastigheter
    rate_forward <- k_plus * A * B
    rate_backward <- k_minus * C * D
    
    # Derivator
    dA <- -rate_forward + rate_backward
    dB <- -rate_forward + rate_backward
    dC <- rate_forward - rate_backward
    dD <- rate_forward - rate_backward
    
    list(c(dA, dB, dC, dD))
  })
}

# Startvärden för koncentrationer
state_init <- c(A = 1, B = 1.5, C = 0.5, D = 1.2)

# Parametrar
parameters <- c(k_plus = k_plus, k_minus = k_minus)

# Tidsvektor för simulering
times <- seq(0, 5, by = 0.1)

# Lös ODE-systemet
out <- ode(y = state_init, times = times, func = reaction_ode, parms = parameters)

# Konvertera till data.frame för enklare hantering
out_df <- as.data.frame(out)

out_melt <- melt(out_df, id = "time")

ggplot(out_melt, aes(x = time, y = value, color = variable)) +
  geom_line(size=1) +
  labs(title = NULL,
       x = "Tid",
       y = "Koncentration",
       color = "Molekyl") +
  scale_y_continuous(breaks = seq(0, 2, by = 0.1)) +  # justera här efter behov
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +  # justera här efter behov
  theme_minimal()
##############################



##############################

# Reaktionshastighet 2

# Parametrar
k_plus <- 2.0
k_minus <- 1.0
addition_rate <- 0.1  # Hastighet för linjär ökning av B efter tid = 5

# ODE-system
reaction_ode <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    rate_forward <- k_plus * A * B
    rate_backward <- k_minus * C * D
    
    # Tillförsel av B endast mellan t=5 och t=7
    addition <- ifelse(t > 5 & t < 7, addition_rate, 0)
    
    dA <- -rate_forward + rate_backward
    dB <- -rate_forward + rate_backward + addition
    dC <- rate_forward - rate_backward
    dD <- rate_forward - rate_backward
    
    list(c(dA, dB, dC, dD))
  })
}

# Startkoncentrationer
state_init <- c(A = 1, B = 1.5, C = 0.5, D = 1.2)
parameters <- c(k_plus = k_plus, k_minus = k_minus)

# Tidsvektor
times <- seq(0, 8, by = 0.1)

# Lös ODE
out <- ode(y = state_init, times = times, func = reaction_ode, parms = parameters)
out_df <- as.data.frame(out)
out_melt <- melt(out_df, id = "time")

# Plot
ggplot(out_melt, aes(x = time, y = value, color = variable)) +
  geom_line(size = 1) +
  labs(title = NULL,
       x = "Tid",
       y = "Koncentration",
       color = "Molekyl") +
  scale_y_continuous(breaks = seq(0, 2, by = 0.1)) +  # justera här efter behov
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +  # justera här efter behov
  theme_minimal()
##############################



##############################

# Reaktionshastighet 3

# Constants
EA <- 80e3        # J/mol
R  <- 8.314       # J/mol/K

# Temperature in Kelvin (for calculation)
T_K <- seq(273.15, 350, length.out = 500)

# Convert to Celsius for plotting
T_C <- T_K - 273.15

# Exponential Arrhenius term (dimensionless)
prob_factor <- exp(-EA / (R * T_K))

df <- data.frame(T_C = T_C, prob_factor = prob_factor)

ggplot(df, aes(x = T_C, y = prob_factor)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  labs(
    title = expression(exp(-E[A]/(R*T))),
    x = "Temperatur (°C)",
    y = NULL
  ) +
  theme_minimal(base_size = 14)

##############################



##############################
# Energi- & massbalans modell 1

set.seed(1)

# Parameters

days <- 720
time <- 0:days

# Initial conditions
w0  <- 95        # Initial body weight (kg)
FM0 <- 19        # Initial fat mass (kg)

# Daily intake
EPM  <- 0.280    # Energy-providing mass (kg/day)
nEPM <- 1.474    # Non-energy-providing mass (kg/day)

M <- EPM + nEPM  # Total daily mass intake

# Relative daily mass excretion
R <- 0.018       # Fraction (%) of body weight excreted per day


# Equation (2)
# Analytical solution

weight_analytical <- function(time, w0, M, R){
  
  M / R +
    (w0 - M / R) *
    (1 - R)^time
  
}


# Equation (4)
# Recursive solution

simulate_weight <- function(days, w0, M, R){
  
  weight <- numeric(days + 1)
  
  weight[1] <- w0
  
  for(k in 1:days){
    
    weight[k + 1] <-
      M +
      (1 - R) * weight[k]
    
  }
  
  return(weight)
  
}


# Recursive solution with random intake

simulate_random_weight <- function(days,
                                   w0,
                                   M,
                                   R,
                                   sigma){
  
  weight <- numeric(days + 1)
  
  weight[1] <- w0
  
  daily_M <- rnorm(
    days,
    mean = M,
    sd = sigma
  )
  
  for(k in 1:days){
    
    weight[k + 1] <-
      daily_M[k] +
      (1 - R) * weight[k]
    
  }
  
  return(
    list(
      weight = weight,
      intake = daily_M
    )
  )
  
}


# Equation (6)
# Fat mass

fat_mass <- function(weight, w0, FM0){
  
  10.4 *
    lambertW0(
      (FM0 / 10.4) *
        exp(
          (weight - w0 + FM0) / 10.4
        )
    )
  
}


# Equation (7)
# Fat-free mass

ffm_mass <- function(weight, fat){
  
  weight - fat
  
}


# Simulation

# Recursive simulation
weight <- simulate_weight(
  days = days,
  w0 = w0,
  M = M,
  R = R
)

# Analytical solution
weight_det <- weight_analytical(
  time = time,
  w0 = w0,
  M = M,
  R = R
)

# Fat mass
fat <- fat_mass(
  weight = weight,
  w0 = w0,
  FM0 = FM0
)

fat_det <- fat_mass(
  weight = weight_det,
  w0 = w0,
  FM0 = FM0
)

# Fat-free mass
ffm <- ffm_mass(weight, fat)
ffm_det <- ffm_mass(weight_det, fat_det)

# Net cumulative weight change
ncw <- weight - w0
ncw_det <- weight_det - w0


# Store results

results <- tibble(
  day = time,
  weight = weight,
  weight_det = weight_det,
  fat = fat,
  fat_det = fat_det,
  ffm = ffm,
  ffm_det = ffm_det,
  ncw = ncw,
  ncw_det = ncw_det
)


# Random simulation

sigma <- 0.05 # variation of daily intake by 50 g

sim <- simulate_random_weight(
  days = days,
  w0 = w0,
  M = M,
  R = R,
  sigma = sigma
)

weight_random <- sim$weight
daily_M <- sim$intake

fat_random <- fat_mass(
  weight_random,
  w0,
  FM0
)

ffm_random <- ffm_mass(
  weight_random,
  fat_random
)

results_random <- tibble(
  day = time,
  weight = weight_random,
  fat = fat_random,
  ffm = ffm_random,
  ncw = weight_random - w0
)

p_Mk <- ggplot(
  tibble(
    day = 1:days,
    intake = daily_M
  ),
  aes(day, intake)
) +
  
  geom_line(colour = "grey30") +
  
  geom_hline(
    yintercept = M,
    linetype = "dashed",
    linewidth = 1,
    colour = "red"
  ) +
  
  labs(
    title = "Slumpmässigt dagligt massaintag",
    x = "Dagar",
    y = "Massaintag (kg/dag)"
  ) +
  
  theme_classic()


p1 <- ggplot(results_random, aes(day)) +
  
  geom_line(
    aes(y = weight),
    colour = "grey30",
    linewidth = 0.8
  ) +
  
  geom_line(
    data = results,
    aes(y = weight_det),
    colour = "black",
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  labs(
    title = "Kroppsvikt",
    x = "Dagar",
    y = "Vikt (kg)"
  ) +
  
  theme_classic()

p2 <- ggplot(results_random, aes(day)) +
  
  geom_line(
    aes(y = fat),
    colour = "grey30",
    linewidth = 0.8
  ) +
  
  geom_line(
    data = results,
    aes(y = fat_det),
    colour = "black",
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  labs(
    title = "Fettmassa",
    x = "Dagar",
    y = "Fettmassa (kg)"
  ) +
  
  theme_classic()


p3 <- ggplot(results_random, aes(day)) +
  
  geom_line(
    aes(y = ffm),
    colour = "grey30",
    linewidth = 0.8
  ) +
  
  geom_line(
    data = results,
    aes(y = ffm_det),
    colour = "black",
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  labs(
    title = "Fettfri massa",
    x = "Dagar",
    y = "Fettfri massa (kg)"
  ) +
  
  theme_classic()


p4 <- ggplot(results_random, aes(day)) +
  
  geom_line(
    aes(y = ncw),
    colour = "grey30",
    linewidth = 0.8
  ) +
  
  geom_line(
    data = results,
    aes(y = ncw_det),
    colour = "black",
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  labs(
    title = "Kumulativ viktförändring",
    x = "Dagar",
    y = "Viktförändring (kg)"
  ) +
  
  theme_classic()

(p1 | p2) /
  (p3 | p4)

p_Mk



##############################



##############################
# Energi- & massbalans modell 2

EI <- 1500     # kcal/day

epm_from_energy <- function(EI,
                            fat,
                            carb,
                            protein){
  
  fat_g  <- EI * fat     / 9
  carb_g <- EI * carb    / 4
  prot_g <- EI * protein / 4
  
  (fat_g + carb_g + prot_g)/1000
  
}


diets <- tibble(
  
  diet = c(
    
    "Högkolhydrat\n15/55/30",
    
    "Balanserad\n35/35/30",
    
    "Högfett\n55/15/30"
    
  ),
  
  fat = c(0.15,0.35,0.55),
  
  carb = c(0.55,0.35,0.15),
  
  protein = c(0.30,0.30,0.30)
  
)


diets$EPM <- mapply(
  
  epm_from_energy,
  
  EI,
  
  diets$fat,
  
  diets$carb,
  
  diets$protein
  
)

diets$M <- diets$EPM + nEPM


cat("\n")
cat("======================================================\n")
cat("Kostsammansättningar\n")
cat("======================================================\n\n")

for(i in 1:nrow(diets)){
  
  fat_g  <- EI * diets$fat[i] / 9
  carb_g <- EI * diets$carb[i] / 4
  prot_g <- EI * diets$protein[i] / 4
  
  cat(diets$diet[i], "\n")
  
  cat(sprintf("Energiintag               : %4d kcal/dag\n", EI))
  
  cat(sprintf("Fett                      : %6.1f g/dag\n", fat_g))
  cat(sprintf("Kolhydrater               : %6.1f g/dag\n", carb_g))
  cat(sprintf("Protein                   : %6.1f g/dag\n", prot_g))
  
  cat(sprintf("Energigivande massa (EPM) : %.3f kg/dag\n", diets$EPM[i]))
  cat(sprintf("Totalt massaintag (M)     : %.3f kg/dag\n", diets$M[i]))
  cat(sprintf("Jämviktstillstånd (M/R)   : %.2f kg\n\n",
              diets$M[i] / R))
  
}


sigma <- 0.05   # kg/day


simulate_mass_balance <- function(time,
                                  w0,
                                  FM0,
                                  M,
                                  R,
                                  diet){
  
  sim <- simulate_random_weight(
    days = days,
    w0 = w0,
    M = M,
    R = R,
    sigma = sigma
  )
  
  weight <- sim$weight
  daily_M <- sim$intake
  
  fat <- fat_mass(
    weight,
    w0,
    FM0
  )
  
  ffm <- weight - fat
  
  tibble(
    
    day = time,
    
    diet = diet,
    
    weight = weight,
    
    fat = fat,
    
    ffm = ffm,
    
    ncw = weight - w0
    
  )
  
}

diets$equilibrium <- diets$M / R

results <- bind_rows(
  
  simulate_mass_balance(
    time,
    w0,
    FM0,
    diets$M[1],
    R,
    diets$diet[1]
  ),
  
  simulate_mass_balance(
    time,
    w0,
    FM0,
    diets$M[2],
    R,
    diets$diet[2]
  ),
  
  simulate_mass_balance(
    time,
    w0,
    FM0,
    diets$M[3],
    R,
    diets$diet[3]
  )
  
)


p1 <- ggplot(
  results,
  aes(day,
      weight,
      colour=diet)
  ) +
  
  geom_hline(
    data = diets,
    aes(
      yintercept = equilibrium,
      colour = diet
    ),
    linetype = "dashed",
    linewidth = 0.8,
    alpha = 0.8
  ) +
  
  geom_line(linewidth=1)+
  
  labs(
    title = "Kroppsvikt",
    x = "Dagar",
    y = "Vikt (kg)",
    colour = "Diet"
  )+
  
  theme_classic()


p2 <- ggplot(
  results,
  aes(day,
      ncw,
      colour=diet)
  ) +
  
  geom_line(linewidth=1)+
  
  labs(
    title = "Kumulativ viktförändring",
    x = "Dagar",
    y = "Viktförändring (kg)",
    colour = "Diet"
  )+
  
  theme_classic()


p3 <- ggplot(
  results,
  aes(day,
      fat,
      colour=diet)
  ) +
  
  geom_line(linewidth=1)+
  
  labs(
    title = "Fettmassa",
    x = "Dagar",
    y = "Fettmassa (kg)",
    colour = "Diet"
  )+
  
  theme_classic()


p4 <- ggplot(
  results,
  aes(day,
      ffm,
      colour=diet)
  ) +
  
  geom_line(linewidth=1)+
  
  labs(
    title = "Fettfri massa",
    x = "Dagar",
    y = "Fettfri massa (kg)",
    colour = "Diet"
  )+
  
  theme_classic()



(p1 | p2) /
  (p3 | p4) +
  plot_layout(guides = "collect")


##############################



##############################

# Exempel 1.1

# Using 95% CIs (SE) for mean estimates and 95% interval for sample data (SD)

# Compute summaries
summary_df <- data.frame(
  group = c("mean_A", "mean_B", "A", "B"),
  mean  = c(0.875, 1.400, 0.875, 1.400),
  sd    = 1.96*c(0.125, 0.200, 0.250, 0.400)
)

summary_df$type <- ifelse(grepl("mean", summary_df$group),
                          "mean",
                          "raw")

# Plot
ggplot(summary_df, aes(x = group, y = mean, fill = type)) +
  geom_bar(stat = "identity",
           alpha = 0.7,
           width = 0.2) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.05,
                color = "black",
                linewidth = 1) +
  scale_fill_manual(values = c(
    "raw" = "red",
    "mean" = "green"
  )) +
  scale_y_continuous(
    breaks = seq(0, 4, by = 2),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_x_discrete(labels = c(
    "mean_A" = expression(mu[5.5]),
    "mean_B" = expression(mu[27.5]),
    "A" = "5.5 mM",
    "B" = "27.5 mM"
  )) +
  labs(
    x = NULL,
    y = "Aktivitet",
    title = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12)
  ) + coord_cartesian(ylim = c(0, 4.0))

##############################



##############################

# Exempel 1.2

summary_df <- data.frame(
  group = c("A", "B", "A", "B"),
  type  = c("sample", "sample", "mean", "mean"),
  mean  = c(0.875, 1.400, 0.875, 1.400),
  sd    = c(0.250, 0.400, 0.125, 0.200)   # SD for samples, SE for means
)

summary_df$type <- factor(summary_df$type,
                          levels = c("sample", "mean"))

x <- seq(0, 4, length.out = 500)

df_dist <- do.call(rbind, lapply(1:nrow(summary_df), function(i) {
  
  m <- summary_df$mean[i]
  s <- summary_df$sd[i]
  
  if(summary_df$type[i] == "sample") {
    
    sdlog <- sqrt(log(1 + (s^2/m^2)))
    meanlog <- log(m) - 0.5*sdlog^2
    
    dens <- dlnorm(x,
                   meanlog = meanlog,
                   sdlog = sdlog)
    
  } else {
    
    dens <- dnorm(x,
                  mean = m,
                  sd = s)
    
  }
  
  data.frame(
    x = x,
    density = dens,
    group = summary_df$group[i],
    type = summary_df$type[i]
  )
}))


ggplot(df_dist, aes(x = x, y = density, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_area(alpha = 0.2, position = "identity") +
  
  facet_wrap(~ type, nrow = 1,
             labeller = labeller(
               type = c(
                 "sample"   = "Stickprov (SD)",
                 "mean" = "Medelvärden (SE)"
               )
             )) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  labs(
    x = "Aktivitet",
    y = "Täthet"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12)
  ) + 
  scale_color_manual(
    name = "Glukoskoncentration",
    values = c("A" = "steelblue", "B" = "orange"),
    labels = c("A" = "5.5 mM", "B" = "27.5 mM")
  ) +
  scale_fill_manual(
    name = "Glukoskoncentration",
    values = c("A" = "steelblue", "B" = "orange"),
    labels = c("A" = "5.5 mM", "B" = "27.5 mM")
  )

##############################



##############################

# Exempel 1.3

# Medelvärden (SE)

x <- seq(0, 2000, length.out = 1000)

df <- data.frame(
  x = rep(x, 4),
  density = c(
    dnorm(x, mean = 35, sd = 6),
    dnorm(x, mean = 910, sd = 87),
    dnorm(x, mean = 942, sd = 106),
    dnorm(x, mean = 956, sd = 95)
  ),
  distribution = factor(
    rep(c("Glucose", "Glucose + acetate", "Glucose + octanoate", "Glucose + palmitate"), each = length(x))
  )
)

# Create the two panels
df_left <- subset(
  df,
  distribution != "Glucose" &
    x >= 300 & x <= 1600
)

df_right <- subset(
  df,
  distribution == "Glucose" &
    x >= 0 & x <= 70
)

df_plot <- rbind(
  transform(df_left, panel = "Glukos + Fettsyror"),
  transform(df_right, panel = "Glukos")
)

ggplot(df_plot, aes(x = x, y = density, color = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c(
      "Glucose" = "darkgrey",
      "Glucose + acetate" = "red",
      "Glucose + octanoate" = "blue",
      "Glucose + palmitate" = "green"
    )
  ) +
  facet_wrap(
    ~panel,
    scales = "free",
    nrow = 1
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Medelvärden (SE)",
    x = "AMP-koncentration, nmol/g",
    y = NULL,
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14)
  )
##############################



##############################

# Exempel 1.4

# Stickprov (SD)

x <- seq(0, 2000, length.out = 1000)

# Convert arithmetic mean and SD to lognormal parameters
ln_params <- function(mean, sd) {
  sdlog <- sqrt(log(1 + (sd^2 / mean^2)))
  meanlog <- log(mean) - sdlog^2 / 2
  list(meanlog = meanlog, sdlog = sdlog)
}

p1 <- ln_params(35, 2*6)
p2 <- ln_params(910, 2*87)
p3 <- ln_params(942, 2*106)
p4 <- ln_params(956, 2*95)

df <- data.frame(
  x = rep(x, 4),
  density = c(
    dlnorm(x, meanlog = p1$meanlog, sdlog = p1$sdlog),
    dlnorm(x, meanlog = p2$meanlog, sdlog = p2$sdlog),
    dlnorm(x, meanlog = p3$meanlog, sdlog = p3$sdlog),
    dlnorm(x, meanlog = p4$meanlog, sdlog = p4$sdlog)
  ),
  distribution = factor(
    rep(c("Glucose", "Glucose + acetate", "Glucose + octanoate", "Glucose + palmitate"), each = length(x))
  )
)

# Create the two panels
df_left <- subset(
  df,
  distribution != "Glucose" &
    x >= 300 & x <= 1600
)

df_right <- subset(
  df,
  distribution == "Glucose" &
    x >= 0 & x <= 70
)

df_plot <- rbind(
  transform(df_left, panel = "Glukos + Fettsyror"),
  transform(df_right, panel = "Glukos")
)

ggplot(df_plot, aes(x = x, y = density, color = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c(
      "Glucose" = "darkgrey",
      "Glucose + acetate" = "red",
      "Glucose + octanoate" = "blue",
      "Glucose + palmitate" = "green"
    )
  ) +
  facet_wrap(
    ~panel,
    scales = "free",
    nrow = 1
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    title = "Stickprov (SD)",
    x = "AMP-koncentration, nmol/g",
    y = NULL,
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14)
  )

##############################



##############################

# Exempel 1.5

summary_df <- data.frame(
  group = c("A", "B", "C", "A", "B", "C"),
  type  = c("sample", "sample", "sample", "mean", "mean", "mean"),
  mean  = c(910, 942, 956, 910, 942, 956),
  sd    = c(2*87, 2*106, 2*95, 87, 106, 95)   # SD for samples, SE for means
)

summary_df$type <- factor(summary_df$type,
                          levels = c("sample", "mean"))

x <- seq(400, 1600, length.out = 100)

ln_params <- function(mean, sd) {
  sdlog <- sqrt(log(1 + (sd^2 / mean^2)))
  meanlog <- log(mean) - sdlog^2 / 2
  list(meanlog = meanlog, sdlog = sdlog)
}

df_dist <- do.call(rbind, lapply(1:nrow(summary_df), function(i) {
  
  if (summary_df$type[i] == "sample") {
    
    p <- ln_params(summary_df$mean[i], summary_df$sd[i])
    
    density <- dlnorm(
      x,
      meanlog = p$meanlog,
      sdlog = p$sdlog
    )
    
  } else {
    
    density <- dnorm(
      x,
      mean = summary_df$mean[i],
      sd = summary_df$sd[i]
    )
    
  }
  
  data.frame(
    x = x,
    density = density,
    group = summary_df$group[i],
    type = summary_df$type[i]
  )
}))


ggplot(df_dist, aes(x = x, y = density, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ type, nrow = 1,
             labeller = labeller(
               type = c(
                 "sample"   = "Stickprov (SD)",
                 "mean" = "Medelvärden (SE)"
               )
             )) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  labs(
    x = "AMP-koncentration, ng/mol",
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12)
  ) + 
  scale_color_manual(
    name = NULL,
    values = c("A" = "red", "B" = "blue", "C" = "green"),
    labels = c("A" = "Glucose + acetate",
               "B" = "Glucose + octanoate",
               "C" = "Glucose + palmitate")
  )

##############################



##############################

# Exempel 1.6

summary_df <- data.frame(
  group = c("A", "B",  "A", "B", "A", "B"),
  type  = c("Empty", "Empty",
            "Mouse ChREBP", "Mouse ChREBP",
            "Rat ChREBP", "Rat ChREBP"),
  mean  = c(0.875, 1.400,
            0.940, 3.440,
            1.125, 3.630),
  sd    = c(0.125, 0.200,
            0.190, 0.420, 
            0.250, 0.230)
)

summary_df$type <- factor(summary_df$type,
                          levels = c("Empty",
                                     "Mouse ChREBP",
                                     "Rat ChREBP"))

x <- seq(0, 6, length.out = 500)

df_dist <- do.call(rbind, lapply(1:nrow(summary_df), function(i) {
  data.frame(
    x = x,
    density = dnorm(x,
                    mean = summary_df$mean[i],
                    sd   = summary_df$sd[i]),
    group = summary_df$group[i],
    type  = summary_df$type[i]
  )
}))


ggplot(df_dist, aes(x = x, y = density, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_area(alpha = 0.2, position = "identity") +
  
  facet_wrap(~ type, nrow = 1,
             labeller = labeller(
               type = c(
                 "Empty"   = "Empty vector",
                 "Mouse ChREBP",
                 "Rat ChREBP"
               )
             )) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  labs(
    x = "Aktivitet",
    y = "Täthet"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12)
  ) + 
  scale_color_manual(
    name = "Glukoskoncentration",
    values = c("A" = "steelblue", "B" = "orange"),
    labels = c("A" = "5.5 mM", "B" = "27.5 mM")
  ) +
  scale_fill_manual(
    name = "Glukoskoncentration",
    values = c("A" = "steelblue", "B" = "orange"),
    labels = c("A" = "5.5 mM", "B" = "27.5 mM")
  ) + coord_cartesian(ylim = c(0, 3.2))

##############################



##############################

# Exempel 1.7

summary_df <- data.frame(
  group = c("A", "B",  "A", "B", "A", "B"),
  type  = c("Empty", "Empty",
            "Mouse ChREBP", "Mouse ChREBP",
            "Rat ChREBP", "Rat ChREBP"),
  mean  = c(0.875, 1.400,
            0.940, 3.440,
            1.125, 3.630),
  sd    = 2*c(0.125, 0.200,
            0.190, 0.420, 
            0.250, 0.230)
)

summary_df$type <- factor(summary_df$type,
                          levels = c("Empty",
                                     "Mouse ChREBP",
                                     "Rat ChREBP"))

x <- seq(0, 6, length.out = 500)

ln_params <- function(mean, sd) {
  sdlog <- sqrt(log(1 + (sd^2 / mean^2)))
  meanlog <- log(mean) - sdlog^2 / 2
  list(meanlog = meanlog, sdlog = sdlog)
}

df_dist <- do.call(rbind, lapply(1:nrow(summary_df), function(i) {
  
  p <- ln_params(summary_df$mean[i], summary_df$sd[i])
  
  data.frame(
    x = x,
    density = dlnorm(
      x,
      meanlog = p$meanlog,
      sdlog = p$sdlog
    ),
    group = summary_df$group[i],
    type = summary_df$type[i]
  )
}))


ggplot(df_dist, aes(x = x, y = density, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_area(alpha = 0.2, position = "identity") +
  
  facet_wrap(~ type, nrow = 1,
             labeller = labeller(
               type = c(
                 "Empty"   = "Empty vector",
                 "Mouse ChREBP",
                 "Rat ChREBP"
               )
             )) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  labs(
    x = "Aktivitet",
    y = "Täthet"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12)
  ) + 
  scale_color_manual(
    name = "Glukoskoncentration",
    values = c("A" = "steelblue", "B" = "orange"),
    labels = c("A" = "5.5 mM", "B" = "27.5 mM")
  ) +
  scale_fill_manual(
    name = "Glukoskoncentration",
    values = c("A" = "steelblue", "B" = "orange"),
    labels = c("A" = "5.5 mM", "B" = "27.5 mM")
  ) + coord_cartesian(ylim = c(0, 3.2))

##############################



##############################

# Exempel 1.8

summary_df <- data.frame(
  condition = c("Glc", "Glc",
                "Glc+acetate", "Glc+acetate"),
  group = c("Empty vector", "Rat ChREBP",
            "Empty vector", "Rat ChREBP"),
  mean  = c(0.875, 3.300,
            0.315, 0.940),
  sd    = c(0.125, 0.450,
            0.125, 0.190)
)

summary_df$condition <- factor(summary_df$condition,
                               levels = c("Glc", "Glc+acetate"))

x <- seq(0, 6, length.out = 500)

df_dist <- do.call(rbind, lapply(1:nrow(summary_df), function(i) {
  data.frame(
    x = x,
    density = dnorm(x,
                    mean = summary_df$mean[i],
                    sd   = summary_df$sd[i]),
    group = summary_df$group[i],
    condition = summary_df$condition[i]
  )
}))


ggplot(df_dist, aes(x = x, y = density, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_area(alpha = 0.2, position = "identity") +
  
  facet_wrap(~ condition, nrow = 1) +
  
  labs(
    x = "Aktivitet",
    y = "Täthet"
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12)
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c("Empty vector" = "steelblue",
               "Rat ChREBP" = "orange")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Empty vector" = "steelblue",
               "Rat ChREBP" = "orange")
  ) +
  
  coord_cartesian(ylim = c(0, 4))

##############################



##############################

# Exempel 1.9

summary_df <- data.frame(
  condition = c("Glc+octanoate", "Glc+octanoate",
                "Glc+palmitate", "Glc+palmitate"),
  group = c("Empty vector", "Rat ChREBP",
            "Empty vector", "Rat ChREBP"),
  mean  = c(0.315, 0.800,
            0.300, 0.625),
  sd    = c(0.110, 0.260,
            0.100, 0.175)
)

summary_df$condition <- factor(summary_df$condition,
                               levels = c("Glc+octanoate", "Glc+palmitate"))

x <- seq(0, 6, length.out = 500)

df_dist <- do.call(rbind, lapply(1:nrow(summary_df), function(i) {
  data.frame(
    x = x,
    density = dnorm(x,
                    mean = summary_df$mean[i],
                    sd   = summary_df$sd[i]),
    group = summary_df$group[i],
    condition = summary_df$condition[i]
  )
}))


ggplot(df_dist, aes(x = x, y = density, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_area(alpha = 0.2, position = "identity") +
  
  facet_wrap(~ condition, nrow = 1) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  labs(
    x = "Aktivitet",
    y = "Täthet"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12)
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c("Empty vector" = "steelblue",
               "Rat ChREBP" = "orange")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Empty vector" = "steelblue",
               "Rat ChREBP" = "orange")
  ) +
  
  coord_cartesian(ylim = c(0, 4))

##############################



##############################

# Exempel 1.10

n <- 5

summary_df <- data.frame(
  condition = c("Glc", "Glc",
                "Glc+acetate", "Glc+acetate"),
  group = c("Empty vector", "Rat ChREBP",
            "Empty vector", "Rat ChREBP"),
  mean  = c(0.875, 3.300,
            0.315, 0.940),
  sd    = c(0.125, 0.450,
            0.125, 0.190) * sqrt(n)
)

summary_df$condition <- factor(summary_df$condition,
                               levels = c("Glc", "Glc+acetate"))

x <- seq(0, 6, length.out = 500)

ln_params <- function(mean, sd) {
  sdlog <- sqrt(log(1 + (sd^2 / mean^2)))
  meanlog <- log(mean) - sdlog^2 / 2
  list(meanlog = meanlog, sdlog = sdlog)
}

df_dist <- do.call(rbind, lapply(1:nrow(summary_df), function(i) {
  
  p <- ln_params(summary_df$mean[i], summary_df$sd[i])
  
  data.frame(
    x = x,
    density = dlnorm(
      x,
      meanlog = p$meanlog,
      sdlog = p$sdlog
    ),
    group = summary_df$group[i],
    condition = summary_df$condition[i]
  )
}))


ggplot(df_dist, aes(x = x, y = density, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_area(alpha = 0.2, position = "identity") +
  
  facet_wrap(~ condition, nrow = 1) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  labs(
    x = "Aktivitet",
    y = "Täthet"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12)
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c("Empty vector" = "steelblue",
               "Rat ChREBP" = "orange")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Empty vector" = "steelblue",
               "Rat ChREBP" = "orange")
  ) +
  
  coord_cartesian(ylim = c(0, 4))

##############################



##############################

# Exempel 1.11

n <- 5

summary_df <- data.frame(
  condition = c("Glc+octanoate", "Glc+octanoate",
                "Glc+palmitate", "Glc+palmitate"),
  group = c("Empty vector", "Rat ChREBP",
            "Empty vector", "Rat ChREBP"),
  mean  = c(0.315, 0.800,
            0.300, 0.625),
  sd    = c(0.110, 0.260,
            0.100, 0.175) * sqrt(n)
)

summary_df$condition <- factor(summary_df$condition,
                               levels = c("Glc+octanoate", "Glc+palmitate"))

x <- seq(0, 6, length.out = 500)

ln_params <- function(mean, sd) {
  sdlog <- sqrt(log(1 + (sd^2 / mean^2)))
  meanlog <- log(mean) - sdlog^2 / 2
  list(meanlog = meanlog, sdlog = sdlog)
}

df_dist <- do.call(rbind, lapply(1:nrow(summary_df), function(i) {
  
  p <- ln_params(summary_df$mean[i], summary_df$sd[i])
  
  data.frame(
    x = x,
    density = dlnorm(
      x,
      meanlog = p$meanlog,
      sdlog = p$sdlog
    ),
    group = summary_df$group[i],
    condition = summary_df$condition[i]
  )
}))


ggplot(df_dist, aes(x = x, y = density, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_area(alpha = 0.2, position = "identity") +
  
  facet_wrap(~ condition, nrow = 1) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  labs(
    x = "Aktivitet",
    y = "Täthet"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12)
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c("Empty vector" = "steelblue",
               "Rat ChREBP" = "orange")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Empty vector" = "steelblue",
               "Rat ChREBP" = "orange")
  ) +

  coord_cartesian(ylim = c(0, 4))

##############################



##############################

# Exempel 1.12

n <- 5

summary_df <- data.frame(
  group = c("A", "B", "C", "D", "A", "B", "C", "D"),
  type  = c("sample", "sample", "sample", "sample", "mean", "mean", "mean", "mean"),
  mean  = c(62.5, 120.5, 145.0, 156.3, 62.5, 120.5, 145.0, 156.3),
  sd    = c(sqrt(n)*12.5, sqrt(n)*13.0, sqrt(n)*29.0, sqrt(n)*8.7, 12.5, 13.0, 29.0, 8.7)   # SD for samples, SE for means
)

summary_df$type <- factor(summary_df$type,
                          levels = c("sample", "mean"))

x <- seq(0, 400, length.out = 100)


ln_params <- function(mean, sd) {
  sdlog <- sqrt(log(1 + (sd^2 / mean^2)))
  meanlog <- log(mean) - sdlog^2 / 2
  list(meanlog = meanlog, sdlog = sdlog)
}


df_dist <- do.call(rbind, lapply(1:nrow(summary_df), function(i) {
  
  if (summary_df$type[i] == "sample") {
    
    p <- ln_params(summary_df$mean[i], summary_df$sd[i])
    
    density <- dlnorm(
      x,
      meanlog = p$meanlog,
      sdlog = p$sdlog
    )
    
  } else {
    
    density <- dnorm(
      x,
      mean = summary_df$mean[i],
      sd = summary_df$sd[i]
    )
    
  }
  
  data.frame(
    x = x,
    density = density,
    group = summary_df$group[i],
    type = summary_df$type[i]
  )
}))


ggplot(df_dist, aes(x = x, y = density, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ type, nrow = 1,
             labeller = labeller(
               type = c(
                 "sample"   = "Stickprov (SD)",
                 "mean" = "Medelvärden (SE)"
               )
             )) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  labs(
    x = "AMPK-aktivitet, pmol/min/mg protein",
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12)
  ) + 
  scale_color_manual(
    name = NULL,
    values = c("A" = "darkgrey", "B" = "red", "C" = "blue", "D" = "green"),
    labels = c("A" = "Glc",
               "B" = "Glc+acetate",
               "C" = "Glc+octanoate",
               "D" = "Glc+palmitate")
  )

##############################



##############################

# Exempel 2.1


# Table I - Demographics
data <- data.frame(
  variable = c(
    "Age",
    "Stature",
    "Body mass",
    "BMI"
  ),
  
  n_HIT = 16,
  mean_HIT = c(
    23.00,
    175.50,
    68.58,
    22.22
  ),
  sd_HIT = c(
    3.00,
    8.16,
    9.04,
    1.97
  ),
  
  n_3ST = 14,
  mean_3ST = c(
    22.00,
    169.21,
    73.30,
    25.49
  ),
  sd_3ST = c(
    2.00,
    7.91,
    11.65,
    2.51
  )
)


# Pooled SD

data$SD_pooled <- sqrt(
  (
    (data$n_HIT - 1) * data$sd_HIT^2 +
      (data$n_3ST - 1) * data$sd_3ST^2
  ) /
    (
      data$n_HIT + data$n_3ST - 2
    )
)


# Cohen's d: abs( HIT - 3ST )
# to only indicate magnitude not direction

data$d <- abs(
  (data$mean_HIT - data$mean_3ST)
) / data$SD_pooled


# Probability of superiority
# Reported in the direction of the higher mean

data <- data |>
  mutate(
    
    P_superiority = ifelse(
      mean_HIT >= mean_3ST,
      
      pnorm(
        (mean_HIT - mean_3ST) /
          sqrt(sd_HIT^2 + sd_3ST^2)
      ),
      
      pnorm(
        (mean_3ST - mean_HIT) /
          sqrt(sd_HIT^2 + sd_3ST^2)
      )
    ),
    
    superiority_label = ifelse(
      mean_HIT >= mean_3ST,
      "P(HIT > 3ST)",
      "P(3ST > HIT)"
    )
  )


# Overlap coefficient

calculate_ovl <- function(mu1, sd1, mu2, sd2) {
  
  lower <- min(
    mu1 - 5 * sd1,
    mu2 - 5 * sd2
  )
  
  upper <- max(
    mu1 + 5 * sd1,
    mu2 + 5 * sd2
  )
  
  integrate(
    function(x) {
      pmin(
        dnorm(x, mean = mu1, sd = sd1),
        dnorm(x, mean = mu2, sd = sd2)
      )
    },
    lower = lower,
    upper = upper
  )$value
}

data$OVL <- mapply(
  calculate_ovl,
  data$mean_HIT,
  data$sd_HIT,
  data$mean_3ST,
  data$sd_3ST
)


# Annotation text

annotations <- data |>
  rowwise() |>
  mutate(
    x_min = min(
      mean_HIT - 4 * sd_HIT,
      mean_3ST - 4 * sd_3ST
    ),
    
    x_max = max(
      mean_HIT + 4 * sd_HIT,
      mean_3ST + 4 * sd_3ST
    ),
    
    x_position = x_min + 1.1 * (x_max - x_min),
    
    max_density = max(
      dnorm(
        seq(
          min(
            mean_HIT - 4 * sd_HIT,
            mean_3ST - 4 * sd_3ST
          ),
          max(
            mean_HIT + 4 * sd_HIT,
            mean_3ST + 4 * sd_3ST
          ),
          length.out = 1000
        ),
        mean = mean_HIT,
        sd = sd_HIT
      ),
      dnorm(
        seq(
          min(
            mean_HIT - 4 * sd_HIT,
            mean_3ST - 4 * sd_3ST
          ),
          max(
            mean_HIT + 4 * sd_HIT,
            mean_3ST + 4 * sd_3ST
          ),
          length.out = 1000
        ),
        mean = mean_3ST,
        sd = sd_3ST
      )
    ),
    
    y_position = max_density * 1.2,
    
    label = sprintf(
      "%s = %.2f\nCohen's d = %.2f\nOVL = %.2f",
      superiority_label,
      P_superiority,
      d,
      OVL
    )
  ) |>
  ungroup()


# Density data

plot_data <- lapply(
  seq_len(nrow(data)),
  function(i) {
    
    row <- data[i, ]
    
    x_min <- min(
      row$mean_HIT - 4 * row$sd_HIT,
      row$mean_3ST - 4 * row$sd_3ST
    )
    
    x_max <- max(
      row$mean_HIT + 4 * row$sd_HIT,
      row$mean_3ST + 4 * row$sd_3ST
    )
    
    x <- seq(
      x_min,
      x_max,
      length.out = 1000
    )
    
    hit <- dnorm(
      x,
      mean = row$mean_HIT,
      sd = row$sd_HIT
    )
    
    st3 <- dnorm(
      x,
      mean = row$mean_3ST,
      sd = row$sd_3ST
    )
    
    data.frame(
      variable = row$variable,
      x = x,
      HIT = hit,
      ST3 = st3,
      overlap = pmin(hit, st3)
    )
  }
) |>
  bind_rows()


# Convert curves to long format

plot_curves <- plot_data |>
  pivot_longer(
    cols = c(HIT, ST3),
    names_to = "group",
    values_to = "density"
  ) |>
  mutate(
    group = recode(
      group,
      "ST3" = "3ST"
    )
  )


# Mean lines

mean_lines <- bind_rows(
  
  data |>
    transmute(
      variable,
      group = "HIT",
      x = mean_HIT,
      yend = dnorm(
        mean_HIT,
        mean = mean_HIT,
        sd = sd_HIT
      )
    ),
  
  data |>
    transmute(
      variable,
      group = "3ST",
      x = mean_3ST,
      yend = dnorm(
        mean_3ST,
        mean = mean_3ST,
        sd = sd_3ST
      )
    )
)


variable_labels <- c(
  "Age" = "Ålder",
  "Stature" = "Kroppslängd",
  "Body mass" = "Kroppsmassa",
  "BMI" = "BMI"
)




#### Plotting ###

ggplot() +
  
  # OVL
  geom_ribbon(
    data = plot_data,
    aes(
      x = x,
      ymin = 0,
      ymax = overlap
    ),
    fill = "grey70",
    alpha = 0.6
  ) +
  
  # Density curves
  geom_line(
    data = plot_curves,
    aes(
      x = x,
      y = density,
      colour = group
    ),
    linewidth = 1.2
  ) +
  
  # Mean lines
  geom_segment(
    data = mean_lines,
    aes(
      x = x,
      xend = x,
      y = 0,
      yend = yend,
      colour = group
    ),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "HIT" = "blue",
      "3ST" = "red"
    )
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  geom_text(
    data = annotations,
    aes(
      x = x_position,
      y = y_position,
      label = label
    ),
    hjust = 1.05,
    vjust = 1.2,
    size = 4
  ) +
  
  facet_wrap(
    ~variable,
    scales = "free",
    ncol = 2,
    labeller = as_labeller(variable_labels)
  ) +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )



##############################



##############################

# Exempel 2.2

# Gender distribution: HIT vs 3ST

gender_data <- data.frame(
  
  group = c(
    "HIT",
    "HIT",
    "3ST",
    "3ST"
  ),
  
  gender = c(
    "Män",
    "Kvinnor",
    "Män",
    "Kvinnor"
  ),
  
  count = c(
    9,
    7,
    4,
    10
  )
)

# Set facet order
gender_data$group <- factor(
  gender_data$group,
  levels = c("HIT", "3ST")
)


ggplot(
  gender_data,
  aes(
    x = gender,
    y = count,
    fill = gender
  )
) +
  
  geom_col(
    width = 0.6,
    colour = "black",
    linewidth = 0.8
  ) +
  
  geom_text(
    aes(label = count),
    vjust = -0.4,
    size = 5
  ) +
  
  scale_fill_manual(
    values = c(
      "Män" = "blue",
      "Kvinnor" = "red"
    )
  ) +
  
  facet_wrap(
    ~group,
    ncol = 2,
    labeller = as_labeller(
      c(
        "HIT" = "HIT (n = 16)",
        "3ST" = "3ST (n = 14)"
      )
    )
  ) +
  
  scale_y_continuous(
    limits = c(0, 12),
    breaks = seq(0, 12, 2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    x = NULL,
    y = "Antal deltagare"
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.line = element_line(
      colour = "black"
    ),
    
    axis.ticks.x = element_blank(),
    
    strip.text = element_text(
      face = "bold"
    ),
    legend.position = "none"
  )

##############################



##############################

# Exempel 2.3

rm(list = ls())

# HIT: Pre vs Post

# Data from Table 2

data_HIT <- data.frame(
  
  variable = c(
    "Chest Press",
    "Heel Raise",
    "Rear Deltoid",
    "Elbow Flexion",
    "Seated Row",
    "Knee Extension",
    "Knee Flexion",
    "Abdominal Flexion",
    "Push-up"
  ),
  
  mean_pre = c(
    32.06,
    32.81,
    30.50,
    27.38,
    36.00,
    35.69,
    37.50,
    19.75,
    21.81
  ),
  
  sd_pre = c(
    15.04,
    12.53,
    11.51,   # Corrected
    6.82,
    13.82,
    13.08,
    20.48,
    10.44,
    13.15
  ),
  
  mean_post = c(
    57.69,
    60.63,
    64.25,
    48.69,
    76.31,
    53.25,
    65.19,
    35.44,
    41.00
  ),
  
  sd_post = c(
    18.81,
    22.44,
    32.94,
    11.69,
    32.25,
    19.70,
    35.80,
    12.54,
    41.04
  ),
  
  mean_change = c(
    25.63,
    27.81,
    33.75,
    21.31,
    40.31,
    17.56,
    27.69,
    15.69,
    19.19
  ),
  
  sd_change = c(
    16.09,
    16.04,
    27.16,
    12.41,
    27.37,
    18.13,
    19.09,
    11.15,
    40.21
  )
)


# Within-group effect size

data_HIT <- data_HIT |>
  mutate(
    
    # Within-group standardized change
    d = mean_change / sd_change,
    
    # Probability that a randomly selected
    # Post observation exceeds a randomly
    # selected Pre observation
    P_post_gt_pre = pnorm(
      (mean_post - mean_pre) /
        sqrt(sd_pre^2 + sd_post^2)
    ),
    
    superiority_label = "P(Post > Pre)"
  )


# OVL function

calculate_ovl <- function(mu1, sd1, mu2, sd2) {
  
  lower <- min(
    mu1 - 5 * sd1,
    mu2 - 5 * sd2
  )
  
  upper <- max(
    mu1 + 5 * sd1,
    mu2 + 5 * sd2
  )
  
  integrate(
    function(x) {
      pmin(
        dnorm(x, mean = mu1, sd = sd1),
        dnorm(x, mean = mu2, sd = sd2)
      )
    },
    lower = lower,
    upper = upper
  )$value
}


data_HIT$OVL <- mapply(
  calculate_ovl,
  data_HIT$mean_pre,
  data_HIT$sd_pre,
  data_HIT$mean_post,
  data_HIT$sd_post
)


# Annotation

data_HIT <- data_HIT |>
  mutate(
    
    label = sprintf(
      "%s = %.2f\nCohen's d = %.2f\nOVL = %.2f",
      superiority_label,
      P_post_gt_pre,
      d,
      OVL
    )
  )


# Generate normal curves

plot_data_HIT <- lapply(
  seq_len(nrow(data_HIT)),
  function(i) {
    
    row <- data_HIT[i, ]
    
    x_min <- min(
      row$mean_pre - 3.5 * row$sd_pre,
      row$mean_post - 3.5 * row$sd_post
    )
    
    x_max <- max(
      row$mean_pre + 5 * row$sd_pre,
      row$mean_post + 5 * row$sd_post
    )
    
    x <- seq(
      x_min,
      x_max,
      length.out = 1000
    )
    
    pre <- dnorm(
      x,
      mean = row$mean_pre,
      sd = row$sd_pre
    )
    
    post <- dnorm(
      x,
      mean = row$mean_post,
      sd = row$sd_post
    )
    
    data.frame(
      variable = row$variable,
      x = x,
      Pre = pre,
      Post = post,
      overlap = pmin(pre, post)
    )
  }
) |>
  bind_rows()


# Long format for curves

plot_curves_HIT <- plot_data_HIT |>
  pivot_longer(
    cols = c(Pre, Post),
    names_to = "group",
    values_to = "density"
  ) |>
  mutate(
    group = recode(
      group,
      "Pre" = "HIT-Pre",
      "Post" = "HIT-Post"
    )
  )


# Mean lines

mean_lines_HIT <- bind_rows(
  
  data_HIT |>
    transmute(
      variable,
      group = "HIT-Pre",
      x = mean_pre,
      yend = dnorm(
        mean_pre,
        mean = mean_pre,
        sd = sd_pre
      )
    ),
  
  data_HIT |>
    transmute(
      variable,
      group = "HIT-Post",
      x = mean_post,
      yend = dnorm(
        mean_post,
        mean = mean_post,
        sd = sd_post
      )
    )
)


# Annotation positions

annotations_HIT <- data_HIT |>
  rowwise() |>
  mutate(
    
    x_min = min(
      mean_pre - 4 * sd_pre,
      mean_post - 4 * sd_post
    ),
    
    x_max = max(
      mean_pre + 4 * sd_pre,
      mean_post + 4 * sd_post
    ),
    
    x_position = x_min +
      0.90 * (x_max - x_min),
    
    max_density = max(
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_pre,
        sd = sd_pre
      ),
      
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_post,
        sd = sd_post
      )
    ),
    
    y_position = max_density * 0.4
    
  ) |>
  ungroup()


# exercise labels translation
exercise_labels <- c(
  "Chest Press" = "Bröstpress",
  "Heel Raise" = "Tåhävning",
  "Rear Deltoid" = "Bakre axel",
  "Elbow Flexion" = "Armbågsflexion",
  "Seated Row" = "Sittande rodd",
  "Knee Extension" = "Knäextension",
  "Knee Flexion" = "Knäflexion",
  "Abdominal Flexion" = "Bålflexion",
  "Push-up" = "Armhävning"
)


# Plot

ggplot() +
  
  # OVL
  geom_ribbon(
    data = plot_data_HIT,
    aes(
      x = x,
      ymin = 0,
      ymax = overlap
    ),
    fill = "grey70",
    alpha = 0.6
  ) +
  
  # Normal curves
  geom_line(
    data = plot_curves_HIT,
    aes(
      x = x,
      y = density,
      colour = group
    ),
    linewidth = 1.2
  ) +
  
  # Mean lines
  geom_segment(
    data = mean_lines_HIT,
    aes(
      x = x,
      xend = x,
      y = 0,
      yend = yend,
      colour = group
    ),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  # Effect-size annotations
  geom_text(
    data = annotations_HIT,
    aes(
      x = x_position,
      y = y_position,
      label = label,
    ),
    hjust = 0.5,
    vjust = 0,
    size = 3.0
  ) +
  
  # Colours
  scale_colour_manual(
    name = NULL,
    values = c(
      "HIT-Pre" = "blue",
      "HIT-Post" = "red"
    )
  ) +
  
  # 3 x 3 panel
  facet_wrap(
    ~variable,
    scales = "free",
    ncol = 3,
    #labeller = as_labeller(exercise_labels)
  ) +
  
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.line = element_line(
      colour = "black"
    ),
    
    axis.text.y = element_blank(),
    
    axis.ticks.y = element_blank(),
    
    legend.position = "top",
    
    strip.text = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )



##############################



##############################

# Exempel 2.4

rm(list = ls())

# 3ST: Pre vs Post

# Data from Table 2

data_3ST <- data.frame(
  
  variable = c(
    "Chest Press",
    "Heel Raise",
    "Rear Deltoid",
    "Elbow Flexion",
    "Seated Row",
    "Knee Extension",
    "Knee Flexion",
    "Abdominal Flexion",
    "Push-up"
  ),
  
  mean_pre = c(
    31.07,
    24.79,
    40.93,   # Corrected
    23.86,
    31.07,
    36.00,
    44.50,
    21.43,
    21.14
  ),
  
  sd_pre = c(
    13.85,
    5.55,
    25.36,   # Corrected
    8.38,
    13.85,
    17.55,
    18.68,
    9.89,
    13.98
  ),
  
  mean_post = c(
    46.93,
    37.86,
    57.79,
    35.50,
    66.21,
    53.79,
    58.43,
    36.64,
    27.54
  ),
  
  sd_post = c(
    19.26,
    14.13,
    25.19,
    9.88,
    20.05,
    25.19,
    17.25,
    11.32,
    11.38
  ),
  
  mean_change = c(
    15.86,
    13.07,
    16.86,   # Corrected
    11.64,
    23.64,
    17.79,
    13.93,
    15.21,
    4.43     # Corrected
  ),
  
  sd_change = c(
    12.02,
    12.04,
    17.38,   # Corrected
    7.90,
    17.93,
    20.25,
    11.95,
    8.58,
    11.40
  )
)


# Within-group effect size

data_3ST <- data_3ST |>
  mutate(
    
    # Within-group standardized change
    
    d = mean_change / sd_change,
    
    # Probability that a randomly selected
    # Post observation exceeds a randomly
    # selected Pre observation
    
    P_post_gt_pre = pnorm(
      (mean_post - mean_pre) /
        sqrt(sd_pre^2 + sd_post^2)
    ),
    
    superiority_label = "P(Post > Pre)"
  )


# OVL function

calculate_ovl <- function(mu1, sd1, mu2, sd2) {
  
  lower <- min(
    mu1 - 5 * sd1,
    mu2 - 5 * sd2
  )
  
  upper <- max(
    mu1 + 5 * sd1,
    mu2 + 5 * sd2
  )
  
  integrate(
    function(x) {
      pmin(
        dnorm(x, mean = mu1, sd = sd1),
        dnorm(x, mean = mu2, sd = sd2)
      )
    },
    lower = lower,
    upper = upper
  )$value
}


data_3ST$OVL <- mapply(
  calculate_ovl,
  data_3ST$mean_pre,
  data_3ST$sd_pre,
  data_3ST$mean_post,
  data_3ST$sd_post
)


# Annotation

data_3ST <- data_3ST |>
  mutate(
    
    label = sprintf(
      "%s = %.2f\nCohen's d = %.2f\nOVL = %.2f",
      superiority_label,
      P_post_gt_pre,
      d,
      OVL
    )
  )


# Generate normal curves

plot_data_3ST <- lapply(
  seq_len(nrow(data_3ST)),
  function(i) {
    
    row <- data_3ST[i, ]
    
    x_min <- min(
      row$mean_pre - 3.5 * row$sd_pre,
      row$mean_post - 3.5 * row$sd_post
    )
    
    x_max <- max(
      row$mean_pre + 5 * row$sd_pre,
      row$mean_post + 5 * row$sd_post
    )
    
    x <- seq(
      x_min,
      x_max,
      length.out = 1000
    )
    
    pre <- dnorm(
      x,
      mean = row$mean_pre,
      sd = row$sd_pre
    )
    
    post <- dnorm(
      x,
      mean = row$mean_post,
      sd = row$sd_post
    )
    
    data.frame(
      variable = row$variable,
      x = x,
      Pre = pre,
      Post = post,
      overlap = pmin(pre, post)
    )
  }
) |>
  bind_rows()


# Long format for curves

plot_curves_3ST <- plot_data_3ST |>
  pivot_longer(
    cols = c(Pre, Post),
    names_to = "group",
    values_to = "density"
  ) |>
  mutate(
    group = recode(
      group,
      "Pre" = "3ST-Pre",
      "Post" = "3ST-Post"
    )
  )


# Mean lines

mean_lines_3ST <- bind_rows(
  
  data_3ST |>
    transmute(
      variable,
      group = "3ST-Pre",
      x = mean_pre,
      yend = dnorm(
        mean_pre,
        mean = mean_pre,
        sd = sd_pre
      )
    ),
  
  data_3ST |>
    transmute(
      variable,
      group = "3ST-Post",
      x = mean_post,
      yend = dnorm(
        mean_post,
        mean = mean_post,
        sd = sd_post
      )
    )
)


# Annotation positions

annotations_3ST <- data_3ST |>
  rowwise() |>
  mutate(
    
    x_min = min(
      mean_pre - 4 * sd_pre,
      mean_post - 4 * sd_post
    ),
    
    x_max = max(
      mean_pre + 4 * sd_pre,
      mean_post + 4 * sd_post
    ),
    
    x_position = x_min +
      0.90 * (x_max - x_min),
    
    max_density = max(
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_pre,
        sd = sd_pre
      ),
      
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_post,
        sd = sd_post
      )
    ),
    
    y_position = max_density * 0.4
  ) |>
  ungroup()


# Exercise labels translation

exercise_labels <- c(
  "Chest Press" = "Bröstpress",
  "Heel Raise" = "Tåhävning",
  "Rear Deltoid" = "Bakre axel",
  "Elbow Flexion" = "Armbågsflexion",
  "Seated Row" = "Sittande rodd",
  "Knee Extension" = "Knäextension",
  "Knee Flexion" = "Knäflexion",
  "Abdominal Flexion" = "Bålflexion",
  "Push-up" = "Armhävning"
)


# Plot

ggplot() +
  
  # OVL
  
  geom_ribbon(
    data = plot_data_3ST,
    aes(
      x = x,
      ymin = 0,
      ymax = overlap
    ),
    fill = "grey70",
    alpha = 0.6
  ) +
  
  # Normal curves
  
  geom_line(
    data = plot_curves_3ST,
    aes(
      x = x,
      y = density,
      colour = group
    ),
    linewidth = 1.2
  ) +
  
  # Mean lines
  
  geom_segment(
    data = mean_lines_3ST,
    aes(
      x = x,
      xend = x,
      y = 0,
      yend = yend,
      colour = group
    ),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  # Effect-size annotations
  
  geom_text(
    data = annotations_3ST,
    aes(
      x = x_position,
      y = y_position,
      label = label,
    ),
    hjust = 0.5,
    vjust = 0,
    size = 3.0
  ) +
  
  # Colours
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "3ST-Pre" = "blue",
      "3ST-Post" = "red"
    )
  ) +
  
  # 3 x 3 panel
  
  facet_wrap(
    ~variable,
    scales = "free",
    ncol = 3,
    #labeller = as_labeller(exercise_labels)
  ) +
  
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.line = element_line(
      colour = "black"
    ),
    
    axis.text.y = element_blank(),
    
    axis.ticks.y = element_blank(),
    
    legend.position = "top",
    
    strip.text = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )



##############################



##############################

# Exempel 2.5

# HIT vs 3ST: Pre

# Clear previous variables
rm(list = ls())

# Data from Table 2

data_HIT <- data.frame(
  
  variable = c(
    "Chest Press",
    "Heel Raise",
    "Rear Deltoid",
    "Elbow Flexion",
    "Seated Row",
    "Knee Extension",
    "Knee Flexion",
    "Abdominal Flexion",
    "Push-up"
  ),
  
  mean = c(
    32.06,
    32.81,
    30.50,
    27.38,
    36.00,
    35.69,
    37.50,
    19.75,
    21.81
  ),
  
  sd = c(
    15.04,
    12.53,
    11.51,
    6.82,
    13.82,
    13.08,
    20.48,
    10.44,
    13.15
  ),
  
  n = 16
)


data_3ST <- data.frame(
  
  variable = c(
    "Chest Press",
    "Heel Raise",
    "Rear Deltoid",
    "Elbow Flexion",
    "Seated Row",
    "Knee Extension",
    "Knee Flexion",
    "Abdominal Flexion",
    "Push-up"
  ),
  
  mean = c(
    31.07,
    24.79,
    40.93,
    23.86,
    31.07,
    36.00,
    44.50,
    21.43,
    21.14
  ),
  
  sd = c(
    13.85,
    5.55,
    25.36,
    8.38,
    13.85,
    17.55,
    18.68,
    9.89,
    13.98
  ),
  
  n = 14
)


# Combine HIT and 3ST

data <- data.frame(
  
  variable = data_HIT$variable,
  
  mean_HIT = data_HIT$mean,
  sd_HIT = data_HIT$sd,
  n_HIT = data_HIT$n,
  
  mean_3ST = data_3ST$mean,
  sd_3ST = data_3ST$sd,
  n_3ST = data_3ST$n
)


# Pooled SD

data$SD_pooled <- sqrt(
  (
    (data$n_HIT - 1) * data$sd_HIT^2 +
      (data$n_3ST - 1) * data$sd_3ST^2
  ) /
    (
      data$n_HIT +
        data$n_3ST -
        2
    )
)


# Cohen's d
# Absolute value = magnitude only

data$d <- abs(
  data$mean_HIT - data$mean_3ST
) / data$SD_pooled


# Probability of superiority
# Direction follows the group with the higher mean

data <- data |>
  mutate(
    
    P_superiority = ifelse(
      
      mean_HIT >= mean_3ST,
      
      pnorm(
        (mean_HIT - mean_3ST) /
          sqrt(sd_HIT^2 + sd_3ST^2)
      ),
      
      pnorm(
        (mean_3ST - mean_HIT) /
          sqrt(sd_HIT^2 + sd_3ST^2)
      )
    ),
    
    superiority_label = ifelse(
      mean_HIT >= mean_3ST,
      "P(HIT > 3ST)",
      "P(3ST > HIT)"
    )
  )


# OVL function

calculate_ovl <- function(mu1, sd1, mu2, sd2) {
  
  lower <- min(
    mu1 - 5 * sd1,
    mu2 - 5 * sd2
  )
  
  upper <- max(
    mu1 + 5 * sd1,
    mu2 + 5 * sd2
  )
  
  integrate(
    function(x) {
      pmin(
        dnorm(
          x,
          mean = mu1,
          sd = sd1
        ),
        dnorm(
          x,
          mean = mu2,
          sd = sd2
        )
      )
    },
    lower = lower,
    upper = upper
  )$value
}


data$OVL <- mapply(
  calculate_ovl,
  data$mean_HIT,
  data$sd_HIT,
  data$mean_3ST,
  data$sd_3ST
)


# Annotation

data <- data |>
  mutate(
    
    label = sprintf(
      "%s = %.2f\nCohen's d = %.2f\nOVL = %.2f",
      superiority_label,
      P_superiority,
      d,
      OVL
    )
  )


# Generate normal curves

plot_data <- lapply(
  seq_len(nrow(data)),
  function(i) {
    
    row <- data[i, ]
    
    x_min <- min(
      row$mean_HIT - 3.5 * row$sd_HIT,
      row$mean_3ST - 3.5 * row$sd_3ST
    )
    
    x_max <- max(
      row$mean_HIT + 5 * row$sd_HIT,
      row$mean_3ST + 5 * row$sd_3ST
    )
    
    x <- seq(
      x_min,
      x_max,
      length.out = 1000
    )
    
    hit <- dnorm(
      x,
      mean = row$mean_HIT,
      sd = row$sd_HIT
    )
    
    st3 <- dnorm(
      x,
      mean = row$mean_3ST,
      sd = row$sd_3ST
    )
    
    data.frame(
      variable = row$variable,
      x = x,
      HIT = hit,
      ST3 = st3,
      overlap = pmin(hit, st3)
    )
  }
) |>
  bind_rows()


# Long format for curves

plot_curves <- plot_data |>
  pivot_longer(
    cols = c(HIT, ST3),
    names_to = "group",
    values_to = "density"
  )


# Mean lines

mean_lines <- bind_rows(
  
  data |>
    transmute(
      variable,
      group = "HIT",
      x = mean_HIT,
      yend = dnorm(
        mean_HIT,
        mean = mean_HIT,
        sd = sd_HIT
      )
    ),
  
  data |>
    transmute(
      variable,
      group = "ST3",
      x = mean_3ST,
      yend = dnorm(
        mean_3ST,
        mean = mean_3ST,
        sd = sd_3ST
      )
    )
)


# Annotation positions

annotations <- data |>
  rowwise() |>
  mutate(
    
    x_min = min(
      mean_HIT - 4 * sd_HIT,
      mean_3ST - 4 * sd_3ST
    ),
    
    x_max = max(
      mean_HIT + 4 * sd_HIT,
      mean_3ST + 4 * sd_3ST
    ),
    
    x_position = x_min +
      0.90 * (x_max - x_min),
    
    max_density = max(
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_HIT,
        sd = sd_HIT
      ),
      
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_3ST,
        sd = sd_3ST
      )
    ),
    
    y_position = max_density * 0.4
  ) |>
  ungroup()


# Exercise labels

exercise_labels <- c(
  "Chest Press" = "Bröstpress",
  "Heel Raise" = "Tåhävning",
  "Rear Deltoid" = "Bakre axel",
  "Elbow Flexion" = "Armbågsflexion",
  "Seated Row" = "Sittande rodd",
  "Knee Extension" = "Knäextension",
  "Knee Flexion" = "Knäflexion",
  "Abdominal Flexion" = "Bålflexion",
  "Push-up" = "Armhävning"
)


# Plot

ggplot() +
  
  # OVL
  
  geom_ribbon(
    data = plot_data,
    aes(
      x = x,
      ymin = 0,
      ymax = overlap
    ),
    fill = "grey70",
    alpha = 0.6
  ) +
  
  # Normal curves
  
  geom_line(
    data = plot_curves,
    aes(
      x = x,
      y = density,
      colour = group
    ),
    linewidth = 1.2
  ) +
  
  # Mean lines
  
  geom_segment(
    data = mean_lines,
    aes(
      x = x,
      xend = x,
      y = 0,
      yend = yend,
      colour = group
    ),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  # Effect-size annotations
  
  geom_text(
    data = annotations,
    aes(
      x = x_position,
      y = y_position,
      label = label
    ),
    hjust = 0.5,
    vjust = 0,
    size = 3.0
  ) +
  
  # Colours
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "HIT" = "blue",
      "ST3" = "red"
    ),
    labels = c(
      "HIT" = "Pre-HIT",
      "ST3" = "Pre-3ST"
    )
  ) +
  
  # 3 x 3 panel
  
  facet_wrap(
    ~variable,
    scales = "free",
    ncol = 3,
    #labeller = as_labeller(exercise_labels)
  ) +
  
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.line = element_line(
      colour = "black"
    ),
    
    axis.text.y = element_blank(),
    
    axis.ticks.y = element_blank(),
    
    legend.position = "top",
    
    strip.text = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )



##############################



##############################

# Exempel 2.6

# HIT vs 3ST: Post

# Clear previous variables

rm(list = ls())

# Data from Table 2

data_HIT <- data.frame(
  
  variable = c(
    "Chest Press",
    "Heel Raise",
    "Rear Deltoid",
    "Elbow Flexion",
    "Seated Row",
    "Knee Extension",
    "Knee Flexion",
    "Abdominal Flexion",
    "Push-up"
  ),
  
  mean = c(
    57.69,
    60.63,
    64.25,
    48.69,
    76.31,
    53.25,
    65.19,
    35.44,
    41.00
  ),
  
  sd = c(
    18.81,
    22.44,
    32.94,
    11.69,
    32.25,
    19.70,
    35.80,
    12.54,
    41.04
  ),
  
  n = 16
)


data_3ST <- data.frame(
  
  variable = c(
    "Chest Press",
    "Heel Raise",
    "Rear Deltoid",
    "Elbow Flexion",
    "Seated Row",
    "Knee Extension",
    "Knee Flexion",
    "Abdominal Flexion",
    "Push-up"
  ),
  
  mean = c(
    46.93,
    37.86,
    57.79,
    35.50,
    66.21,
    53.79,
    58.43,
    36.64,
    27.54
  ),
  
  sd = c(
    19.26,
    14.13,
    25.19,
    9.88,
    20.05,
    25.19,
    17.25,
    11.32,
    11.38
  ),
  
  n = 14
)


# Combine HIT and 3ST

data <- data.frame(
  
  variable = data_HIT$variable,
  
  mean_HIT = data_HIT$mean,
  sd_HIT = data_HIT$sd,
  n_HIT = data_HIT$n,
  
  mean_3ST = data_3ST$mean,
  sd_3ST = data_3ST$sd,
  n_3ST = data_3ST$n
)


# Pooled SD

data$SD_pooled <- sqrt(
  (
    (data$n_HIT - 1) * data$sd_HIT^2 +
      (data$n_3ST - 1) * data$sd_3ST^2
  ) /
    (
      data$n_HIT +
        data$n_3ST -
        2
    )
)


# Cohen's d

# Absolute value = magnitude only

data$d <- abs(
  data$mean_HIT - data$mean_3ST
) / data$SD_pooled


# Probability of superiority

# Direction follows the group with the higher mean

data <- data |>
  mutate(
    
    P_superiority = ifelse(
      
      mean_HIT >= mean_3ST,
      
      pnorm(
        (mean_HIT - mean_3ST) /
          sqrt(sd_HIT^2 + sd_3ST^2)
      ),
      
      pnorm(
        (mean_3ST - mean_HIT) /
          sqrt(sd_HIT^2 + sd_3ST^2)
      )
    ),
    
    superiority_label = ifelse(
      mean_HIT >= mean_3ST,
      "P(HIT > 3ST)",
      "P(3ST > HIT)"
    )
  )


# OVL function

calculate_ovl <- function(mu1, sd1, mu2, sd2) {
  
  lower <- min(
    mu1 - 5 * sd1,
    mu2 - 5 * sd2
  )
  
  upper <- max(
    mu1 + 5 * sd1,
    mu2 + 5 * sd2
  )
  
  integrate(
    function(x) {
      pmin(
        dnorm(
          x,
          mean = mu1,
          sd = sd1
        ),
        dnorm(
          x,
          mean = mu2,
          sd = sd2
        )
      )
    },
    lower = lower,
    upper = upper
  )$value
}


data$OVL <- mapply(
  calculate_ovl,
  data$mean_HIT,
  data$sd_HIT,
  data$mean_3ST,
  data$sd_3ST
)


# Annotation

data <- data |>
  mutate(
    
    label = sprintf(
      "%s = %.2f\nCohen's d = %.2f\nOVL = %.2f",
      superiority_label,
      P_superiority,
      d,
      OVL
    )
  )


# Generate normal curves

plot_data <- lapply(
  seq_len(nrow(data)),
  function(i) {
    
    row <- data[i, ]
    
    x_min <- min(
      row$mean_HIT - 3.5 * row$sd_HIT,
      row$mean_3ST - 3.5 * row$sd_3ST
    )
    
    x_max <- max(
      row$mean_HIT + 5 * row$sd_HIT,
      row$mean_3ST + 5 * row$sd_3ST
    )
    
    x <- seq(
      x_min,
      x_max,
      length.out = 1000
    )
    
    hit <- dnorm(
      x,
      mean = row$mean_HIT,
      sd = row$sd_HIT
    )
    
    st3 <- dnorm(
      x,
      mean = row$mean_3ST,
      sd = row$sd_3ST
    )
    
    data.frame(
      variable = row$variable,
      x = x,
      HIT = hit,
      ST3 = st3,
      overlap = pmin(hit, st3)
    )
  }
) |>
  bind_rows()


# Long format for curves

plot_curves <- plot_data |>
  pivot_longer(
    cols = c(HIT, ST3),
    names_to = "group",
    values_to = "density"
  )


# Mean lines

mean_lines <- bind_rows(
  
  data |>
    transmute(
      variable,
      group = "HIT",
      x = mean_HIT,
      yend = dnorm(
        mean_HIT,
        mean = mean_HIT,
        sd = sd_HIT
      )
    ),
  
  data |>
    transmute(
      variable,
      group = "ST3",
      x = mean_3ST,
      yend = dnorm(
        mean_3ST,
        mean = mean_3ST,
        sd = sd_3ST
      )
    )
)


# Annotation positions

annotations <- data |>
  rowwise() |>
  mutate(
    
    x_min = min(
      mean_HIT - 4 * sd_HIT,
      mean_3ST - 4 * sd_3ST
    ),
    
    x_max = max(
      mean_HIT + 4 * sd_HIT,
      mean_3ST + 4 * sd_3ST
    ),
    
    x_position = x_min +
      0.90 * (x_max - x_min),
    
    max_density = max(
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_HIT,
        sd = sd_HIT
      ),
      
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_3ST,
        sd = sd_3ST
      )
    ),
    
    y_position = max_density * 0.4
  ) |>
  ungroup()


# Exercise labels

exercise_labels <- c(
  "Chest Press" = "Bröstpress",
  "Heel Raise" = "Tåhävning",
  "Rear Deltoid" = "Bakre axel",
  "Elbow Flexion" = "Armbågsflexion",
  "Seated Row" = "Sittande rodd",
  "Knee Extension" = "Knäextension",
  "Knee Flexion" = "Knäflexion",
  "Abdominal Flexion" = "Bålflexion",
  "Push-up" = "Armhävning"
)


# Plot

ggplot() +
  
  # OVL
  
  geom_ribbon(
    data = plot_data,
    aes(
      x = x,
      ymin = 0,
      ymax = overlap
    ),
    fill = "grey70",
    alpha = 0.6
  ) +
  
  # Normal curves
  
  geom_line(
    data = plot_curves,
    aes(
      x = x,
      y = density,
      colour = group
    ),
    linewidth = 1.2
  ) +
  
  # Mean lines
  
  geom_segment(
    data = mean_lines,
    aes(
      x = x,
      xend = x,
      y = 0,
      yend = yend,
      colour = group
    ),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  # Effect-size annotations
  
  geom_text(
    data = annotations,
    aes(
      x = x_position,
      y = y_position,
      label = label
    ),
    hjust = 0.5,
    vjust = 0,
    size = 3.0
  ) +
  
  # Colours
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "HIT" = "blue",
      "ST3" = "red"
    ),
    labels = c(
      "HIT" = "Post-HIT",
      "ST3" = "Post-3ST"
    )
  ) +
  
  # 3 x 3 panel
  
  facet_wrap(
    ~variable,
    scales = "free",
    ncol = 3,
    # labeller = as_labeller(exercise_labels)
  ) +
  
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.line = element_line(
      colour = "black"
    ),
    
    axis.text.y = element_blank(),
    
    axis.ticks.y = element_blank(),
    
    legend.position = "top",
    
    strip.text = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )



##############################



##############################

# Exempel 2.7

# HIT: Pre vs Post - Body composition

# Clear previous variables

rm(list = ls())

# Data from Table 3

data_HIT <- data.frame(
  
  variable = c(
    "Body Mass",
    "Fat Free Mass",
    "Muscle Mass",
    "Fat Mass",
    "Fat Percentage",
    "Total Body Water"
  ),
  
  mean_pre = c(
    68.58,
    55.47,
    52.69,
    13.12,
    19.31,
    40.02
  ),
  
  sd_pre = c(
    9.04,
    9.28,
    8.85,
    4.13,
    6.07,
    6.59
  ),
  
  mean_post = c(
    69.04,
    55.88,
    53.09,
    13.16,
    19.11,
    40.27
  ),
  
  sd_post = c(
    9.35,
    8.80,
    8.40,
    4.53,
    5.98,
    6.21
  ),
  
  mean_change = c(
    0.46,
    0.42,
    0.40,
    0.05,
    -0.20,
    0.27
  ),
  
  sd_change = c(
    2.27,
    1.59,
    1.50,
    1.74,
    2.03,
    1.26
  )
)

# Within-group effect size

data_HIT <- data_HIT |>
  mutate(
    
    # Within-group standardized change
    # Absolute value = magnitude only
    d = abs(mean_change) / sd_change,
    
    # Probability of superiority
    # Direction follows whichever mean is higher
    P_superiority = ifelse(
      mean_post >= mean_pre,
      
      pnorm(
        (mean_post - mean_pre) /
          sqrt(sd_pre^2 + sd_post^2)
      ),
      
      pnorm(
        (mean_pre - mean_post) /
          sqrt(sd_pre^2 + sd_post^2)
      )
    ),
    
    superiority_label = ifelse(
      mean_post >= mean_pre,
      "P(Post > Pre)",
      "P(Pre > Post)"
    )
  )

# OVL function

calculate_ovl <- function(mu1, sd1, mu2, sd2) {
  
  lower <- min(
    mu1 - 5 * sd1,
    mu2 - 5 * sd2
  )
  
  upper <- max(
    mu1 + 5 * sd1,
    mu2 + 5 * sd2
  )
  
  integrate(
    function(x) {
      pmin(
        dnorm(
          x,
          mean = mu1,
          sd = sd1
        ),
        dnorm(
          x,
          mean = mu2,
          sd = sd2
        )
      )
    },
    lower = lower,
    upper = upper
  )$value
}

data_HIT$OVL <- mapply(
  calculate_ovl,
  data_HIT$mean_pre,
  data_HIT$sd_pre,
  data_HIT$mean_post,
  data_HIT$sd_post
)

# Annotation

data_HIT <- data_HIT |>
  mutate(
    
    label = sprintf(
      "%s = %.2f\nCohen's d = %.2f\nOVL = %.2f",
      superiority_label,
      P_post_gt_pre,
      d,
      OVL
    )
  )

# Generate normal curves

plot_data_HIT <- lapply(
  seq_len(nrow(data_HIT)),
  function(i) {
    
    row <- data_HIT[i, ]
    
    x_min <- min(
      row$mean_pre - 3.5 * row$sd_pre,
      row$mean_post - 3.5 * row$sd_post
    )
    
    x_max <- max(
      row$mean_pre + 5 * row$sd_pre,
      row$mean_post + 5 * row$sd_post
    )
    
    x <- seq(
      x_min,
      x_max,
      length.out = 1000
    )
    
    pre <- dnorm(
      x,
      mean = row$mean_pre,
      sd = row$sd_pre
    )
    
    post <- dnorm(
      x,
      mean = row$mean_post,
      sd = row$sd_post
    )
    
    data.frame(
      variable = row$variable,
      x = x,
      Pre = pre,
      Post = post,
      overlap = pmin(pre, post)
    )
  }
) |>
  bind_rows()

# Long format for curves

plot_curves_HIT <- plot_data_HIT |>
  pivot_longer(
    cols = c(Pre, Post),
    names_to = "group",
    values_to = "density"
  )

# Mean lines

mean_lines_HIT <- bind_rows(
  
  data_HIT |>
    transmute(
      variable,
      group = "Pre",
      x = mean_pre,
      yend = dnorm(
        mean_pre,
        mean = mean_pre,
        sd = sd_pre
      )
    ),
  
  data_HIT |>
    transmute(
      variable,
      group = "Post",
      x = mean_post,
      yend = dnorm(
        mean_post,
        mean = mean_post,
        sd = sd_post
      )
    )
)

# Annotation positions

annotations_HIT <- data_HIT |>
  rowwise() |>
  mutate(
    
    x_min = min(
      mean_pre - 4 * sd_pre,
      mean_post - 4 * sd_post
    ),
    
    x_max = max(
      mean_pre + 4 * sd_pre,
      mean_post + 4 * sd_post
    ),
    
    x_position = x_min +
      0.90 * (x_max - x_min),
    
    max_density = max(
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_pre,
        sd = sd_pre
      ),
      
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_post,
        sd = sd_post
      )
    ),
    
    y_position = max_density * 0.4
  ) |>
  ungroup()

# Swedish labels

bodycomp_labels <- c(
  "Body Mass" = "Kroppsmassa (kg)",
  "Fat Free Mass" = "Fettfri massa (kg)",
  "Muscle Mass" = "Muskelmassa (kg)",
  "Fat Mass" = "Fettmassa (kg)",
  "Fat Percentage" = "Fettprocent (%)",
  "Total Body Water" = "Totalt kroppsvatten (kg)"
)

# Plot

ggplot() +
  
  # OVL
  
  geom_ribbon(
    data = plot_data_HIT,
    aes(
      x = x,
      ymin = 0,
      ymax = overlap
    ),
    fill = "grey70",
    alpha = 0.6
  ) +
  
  # Normal curves
  
  geom_line(
    data = plot_curves_HIT,
    aes(
      x = x,
      y = density,
      colour = group
    ),
    linewidth = 1.2
  ) +
  
  # Mean lines
  
  geom_segment(
    data = mean_lines_HIT,
    aes(
      x = x,
      xend = x,
      y = 0,
      yend = yend,
      colour = group
    ),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  # Effect-size annotations
  
  geom_text(
    data = annotations_HIT,
    aes(
      x = x_position,
      y = y_position,
      label = label
    ),
    hjust = 0.5,
    vjust = 0,
    size = 3.0
  ) +
  
  # Colours
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Pre" = "blue",
      "Post" = "red"
    ),
    labels = c(
      "Pre" = "HIT-Pre",
      "Post" = "HIT-Post"
    )
  ) +
  
  # Swedish facet labels
  
  facet_wrap(
    ~variable,
    scales = "free",
    ncol = 3,
    labeller = as_labeller(bodycomp_labels)
  ) +
  
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.line = element_line(
      colour = "black"
    ),
    
    axis.text.y = element_blank(),
    
    axis.ticks.y = element_blank(),
    
    legend.position = "top",
    
    strip.text = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )


##############################



##############################

# Exempel 2.8

# 3ST: Pre vs Post - Body composition

# Clear previous variables

rm(list = ls())

# Data from Table 3

data_3ST <- data.frame(
  
  variable = c(
    "Body Mass",
    "Fat Free Mass",
    "Muscle Mass",
    "Fat Mass",
    "Fat Percentage",
    "Total Body Water"
  ),
  
  mean_pre = c(
    73.30,
    51.05,
    48.49,
    22.25,
    30.34,
    36.85
  ),
  
  sd_pre = c(
    11.65,
    9.03,
    8.60,
    5.07,
    5.23,
    6.53
  ),
  
  mean_post = c(
    74.04,
    50.50,
    47.95,
    23.54,
    31.92,
    36.40
  ),
  
  sd_post = c(
    11.07,
    9.16,
    8.73,
    4.77,
    5.21,
    6.55
  ),
  
  mean_change = c(
    0.75,
    -0.55,
    -0.54,
    1.29,
    1.58,
    -0.45  # Paper reports -0.86; inconsistent with reported pre/post means
  ),
  
  sd_change = c(
    2.35,
    1.66,
    1.61,
    3.02,
    3.55,
    1.81
  )
)

# Within-group effect size

data_3ST <- data_3ST |>
  mutate(
    
    # Within-group standardized change
    # Absolute value = magnitude only
    d = abs(mean_change) / sd_change,
    
    # Probability of superiority
    # Direction follows whichever mean is higher
    P_superiority = ifelse(
      
      mean_post >= mean_pre,
      
      pnorm(
        (mean_post - mean_pre) /
          sqrt(sd_pre^2 + sd_post^2)
      ),
      
      pnorm(
        (mean_pre - mean_post) /
          sqrt(sd_pre^2 + sd_post^2)
      )
    ),
    
    superiority_label = ifelse(
      mean_post >= mean_pre,
      "P(Post > Pre)",
      "P(Pre > Post)"
    )
  )

# OVL function

calculate_ovl <- function(mu1, sd1, mu2, sd2) {
  
  lower <- min(
    mu1 - 5 * sd1,
    mu2 - 5 * sd2
  )
  
  upper <- max(
    mu1 + 5 * sd1,
    mu2 + 5 * sd2
  )
  
  integrate(
    function(x) {
      pmin(
        dnorm(
          x,
          mean = mu1,
          sd = sd1
        ),
        dnorm(
          x,
          mean = mu2,
          sd = sd2
        )
      )
    },
    lower = lower,
    upper = upper
  )$value
}

data_3ST$OVL <- mapply(
  calculate_ovl,
  data_3ST$mean_pre,
  data_3ST$sd_pre,
  data_3ST$mean_post,
  data_3ST$sd_post
)

# Annotation

data_3ST <- data_3ST |>
  mutate(
    
    label = sprintf(
      "%s = %.2f\nCohen's d = %.2f\nOVL = %.2f",
      superiority_label,
      P_superiority,
      d,
      OVL
    )
  )

# Generate normal curves

plot_data_3ST <- lapply(
  seq_len(nrow(data_3ST)),
  function(i) {
    
    row <- data_3ST[i, ]
    
    x_min <- min(
      row$mean_pre - 3.5 * row$sd_pre,
      row$mean_post - 3.5 * row$sd_post
    )
    
    x_max <- max(
      row$mean_pre + 5 * row$sd_pre,
      row$mean_post + 5 * row$sd_post
    )
    
    x <- seq(
      x_min,
      x_max,
      length.out = 1000
    )
    
    pre <- dnorm(
      x,
      mean = row$mean_pre,
      sd = row$sd_pre
    )
    
    post <- dnorm(
      x,
      mean = row$mean_post,
      sd = row$sd_post
    )
    
    data.frame(
      variable = row$variable,
      x = x,
      Pre = pre,
      Post = post,
      overlap = pmin(pre, post)
    )
  }
) |>
  bind_rows()

# Long format for curves

plot_curves_3ST <- plot_data_3ST |>
  pivot_longer(
    cols = c(Pre, Post),
    names_to = "group",
    values_to = "density"
  )

# Mean lines

mean_lines_3ST <- bind_rows(
  
  data_3ST |>
    transmute(
      variable,
      group = "Pre",
      x = mean_pre,
      yend = dnorm(
        mean_pre,
        mean = mean_pre,
        sd = sd_pre
      )
    ),
  
  data_3ST |>
    transmute(
      variable,
      group = "Post",
      x = mean_post,
      yend = dnorm(
        mean_post,
        mean = mean_post,
        sd = sd_post
      )
    )
)

# Annotation positions

annotations_3ST <- data_3ST |>
  rowwise() |>
  mutate(
    
    x_min = min(
      mean_pre - 4 * sd_pre,
      mean_post - 4 * sd_post
    ),
    
    x_max = max(
      mean_pre + 4 * sd_pre,
      mean_post + 4 * sd_post
    ),
    
    x_position = x_min +
      0.90 * (x_max - x_min),
    
    max_density = max(
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_pre,
        sd = sd_pre
      ),
      
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_post,
        sd = sd_post
      )
    ),
    
    y_position = max_density * 0.4
  ) |>
  ungroup()

# Swedish labels

bodycomp_labels <- c(
  "Body Mass" = "Kroppsmassa (kg)",
  "Fat Free Mass" = "Fettfri massa (kg)",
  "Muscle Mass" = "Muskelmassa (kg)",
  "Fat Mass" = "Fettmassa (kg)",
  "Fat Percentage" = "Fettprocent (%)",
  "Total Body Water" = "Totalt kroppsvatten (kg)"
)

# Plot

ggplot() +
  
  # OVL
  
  geom_ribbon(
    data = plot_data_3ST,
    aes(
      x = x,
      ymin = 0,
      ymax = overlap
    ),
    fill = "grey70",
    alpha = 0.6
  ) +
  
  # Normal curves
  
  geom_line(
    data = plot_curves_3ST,
    aes(
      x = x,
      y = density,
      colour = group
    ),
    linewidth = 1.2
  ) +
  
  # Mean lines
  
  geom_segment(
    data = mean_lines_3ST,
    aes(
      x = x,
      xend = x,
      y = 0,
      yend = yend,
      colour = group
    ),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  # Effect-size annotations
  
  geom_text(
    data = annotations_3ST,
    aes(
      x = x_position,
      y = y_position,
      label = label
    ),
    hjust = 0.5,
    vjust = 0,
    size = 3.0
  ) +
  
  # Colours
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Pre" = "blue",
      "Post" = "red"
    ),
    labels = c(
      "Pre" = "3ST-Pre",
      "Post" = "3ST-Post"
    )
  ) +
  
  # 2 x 3 panel
  
  facet_wrap(
    ~variable,
    scales = "free",
    ncol = 3,
    labeller = as_labeller(bodycomp_labels)
  ) +
  
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.line = element_line(
      colour = "black"
    ),
    
    axis.text.y = element_blank(),
    
    axis.ticks.y = element_blank(),
    
    legend.position = "top",
    
    strip.text = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )



##############################



##############################

# Exempel 2.9

# HIT vs 3ST: Pre - Body composition

# Clear previous variables

rm(list = ls())

# Data from Table 3

data_HIT <- data.frame(
  
  variable = c(
    "Body Mass",
    "Fat Free Mass",
    "Muscle Mass",
    "Fat Mass",
    "Fat Percentage",
    "Total Body Water"
  ),
  
  mean = c(
    68.58,
    55.47,
    52.69,
    13.12,
    19.31,
    40.02
  ),
  
  sd = c(
    9.04,
    9.28,
    8.85,
    4.13,
    6.07,
    6.59
  ),
  
  n = 16
)

data_3ST <- data.frame(
  
  variable = c(
    "Body Mass",
    "Fat Free Mass",
    "Muscle Mass",
    "Fat Mass",
    "Fat Percentage",
    "Total Body Water"
  ),
  
  mean = c(
    73.30,
    51.05,
    48.49,
    22.25,
    30.34,
    36.85
  ),
  
  sd = c(
    11.65,
    9.03,
    8.60,
    5.07,
    5.23,
    6.53
  ),
  
  n = 14
)

# Combine HIT and 3ST

data <- data.frame(
  
  variable = data_HIT$variable,
  
  mean_HIT = data_HIT$mean,
  sd_HIT = data_HIT$sd,
  n_HIT = data_HIT$n,
  
  mean_3ST = data_3ST$mean,
  sd_3ST = data_3ST$sd,
  n_3ST = data_3ST$n
)

# Pooled SD

data$SD_pooled <- sqrt(
  (
    (data$n_HIT - 1) * data$sd_HIT^2 +
      (data$n_3ST - 1) * data$sd_3ST^2
  ) /
    (
      data$n_HIT +
        data$n_3ST -
        2
    )
)

# Cohen's d

# Absolute value = magnitude only

data$d <- abs(
  data$mean_HIT - data$mean_3ST
) / data$SD_pooled

# Probability of superiority

# Direction follows the group with the higher mean

data <- data |>
  mutate(
    
    P_superiority = ifelse(
      
      mean_HIT >= mean_3ST,
      
      pnorm(
        (mean_HIT - mean_3ST) /
          sqrt(sd_HIT^2 + sd_3ST^2)
      ),
      
      pnorm(
        (mean_3ST - mean_HIT) /
          sqrt(sd_HIT^2 + sd_3ST^2)
      )
    ),
    
    superiority_label = ifelse(
      mean_HIT >= mean_3ST,
      "P(HIT > 3ST)",
      "P(3ST > HIT)"
    )
  )

# OVL function

calculate_ovl <- function(mu1, sd1, mu2, sd2) {
  
  lower <- min(
    mu1 - 5 * sd1,
    mu2 - 5 * sd2
  )
  
  upper <- max(
    mu1 + 5 * sd1,
    mu2 + 5 * sd2
  )
  
  integrate(
    function(x) {
      pmin(
        dnorm(
          x,
          mean = mu1,
          sd = sd1
        ),
        dnorm(
          x,
          mean = mu2,
          sd = sd2
        )
      )
    },
    lower = lower,
    upper = upper
  )$value
}

data$OVL <- mapply(
  calculate_ovl,
  data$mean_HIT,
  data$sd_HIT,
  data$mean_3ST,
  data$sd_3ST
)

# Annotation

data <- data |>
  mutate(
    
    label = sprintf(
      "%s = %.2f\nCohen's d = %.2f\nOVL = %.2f",
      superiority_label,
      P_superiority,
      d,
      OVL
    )
  )

# Generate normal curves

plot_data <- lapply(
  seq_len(nrow(data)),
  function(i) {
    
    row <- data[i, ]
    
    x_min <- min(
      row$mean_HIT - 3.5 * row$sd_HIT,
      row$mean_3ST - 3.5 * row$sd_3ST
    )
    
    x_max <- max(
      row$mean_HIT + 5 * row$sd_HIT,
      row$mean_3ST + 5 * row$sd_3ST
    )
    
    x <- seq(
      x_min,
      x_max,
      length.out = 1000
    )
    
    hit <- dnorm(
      x,
      mean = row$mean_HIT,
      sd = row$sd_HIT
    )
    
    st3 <- dnorm(
      x,
      mean = row$mean_3ST,
      sd = row$sd_3ST
    )
    
    data.frame(
      variable = row$variable,
      x = x,
      HIT = hit,
      ST3 = st3,
      overlap = pmin(hit, st3)
    )
  }
) |>
  bind_rows()

# Long format for curves

plot_curves <- plot_data |>
  pivot_longer(
    cols = c(HIT, ST3),
    names_to = "group",
    values_to = "density"
  )

# Mean lines

mean_lines <- bind_rows(
  
  data |>
    transmute(
      variable,
      group = "HIT",
      x = mean_HIT,
      yend = dnorm(
        mean_HIT,
        mean = mean_HIT,
        sd = sd_HIT
      )
    ),
  
  data |>
    transmute(
      variable,
      group = "ST3",
      x = mean_3ST,
      yend = dnorm(
        mean_3ST,
        mean = mean_3ST,
        sd = sd_3ST
      )
    )
)

# Annotation positions

annotations <- data |>
  rowwise() |>
  mutate(
    
    x_min = min(
      mean_HIT - 4 * sd_HIT,
      mean_3ST - 4 * sd_3ST
    ),
    
    x_max = max(
      mean_HIT + 4 * sd_HIT,
      mean_3ST + 4 * sd_3ST
    ),
    
    x_position = x_min +
      0.95 * (x_max - x_min),
    
    max_density = max(
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_HIT,
        sd = sd_HIT
      ),
      
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_3ST,
        sd = sd_3ST
      )
    ),
    
    y_position = max_density * 0.4
  ) |>
  ungroup()

# Swedish labels

bodycomp_labels <- c(
  "Body Mass" = "Kroppsmassa (kg)",
  "Fat Free Mass" = "Fettfri massa (kg)",
  "Muscle Mass" = "Muskelmassa (kg)",
  "Fat Mass" = "Fettmassa (kg)",
  "Fat Percentage" = "Fettprocent (%)",
  "Total Body Water" = "Totalt kroppsvatten (kg)"
)

# Plot

ggplot() +
  
  # OVL
  
  geom_ribbon(
    data = plot_data,
    aes(
      x = x,
      ymin = 0,
      ymax = overlap
    ),
    fill = "grey70",
    alpha = 0.6
  ) +
  
  # Normal curves
  
  geom_line(
    data = plot_curves,
    aes(
      x = x,
      y = density,
      colour = group
    ),
    linewidth = 1.2
  ) +
  
  # Mean lines
  
  geom_segment(
    data = mean_lines,
    aes(
      x = x,
      xend = x,
      y = 0,
      yend = yend,
      colour = group
    ),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  # Effect-size annotations
  
  geom_text(
    data = annotations,
    aes(
      x = x_position,
      y = y_position,
      label = label
    ),
    hjust = 0.5,
    vjust = 0,
    size = 2.8
  ) +
  
  # Colours
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "HIT" = "blue",
      "ST3" = "red"
    ),
    labels = c(
      "HIT" = "Pre-HIT",
      "ST3" = "Pre-3ST"
    )
  ) +
  
  # 2 x 3 panel
  
  facet_wrap(
    ~variable,
    scales = "free",
    ncol = 3,
    labeller = as_labeller(bodycomp_labels)
  ) +
  
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.line = element_line(
      colour = "black"
    ),
    
    axis.text.y = element_blank(),
    
    axis.ticks.y = element_blank(),
    
    legend.position = "top",
    
    strip.text = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )



##############################



##############################

# Exempel 2.10

# HIT vs 3ST: Post - Body composition

# Clear previous variables

rm(list = ls())

# Data from Table 3

data_HIT <- data.frame(
  
  variable = c(
    "Body Mass",
    "Fat Free Mass",
    "Muscle Mass",
    "Fat Mass",
    "Fat Percentage",
    "Total Body Water"
  ),
  
  mean = c(
    69.04,
    55.88,
    53.09,
    13.16,
    19.11,
    40.27
  ),
  
  sd = c(
    9.35,
    8.80,
    8.40,
    4.53,
    5.98,
    6.21
  ),
  
  n = 16
)

data_3ST <- data.frame(
  
  variable = c(
    "Body Mass",
    "Fat Free Mass",
    "Muscle Mass",
    "Fat Mass",
    "Fat Percentage",
    "Total Body Water"
  ),
  
  mean = c(
    74.04,
    50.50,
    47.95,
    23.54,
    31.92,
    36.40
  ),
  
  sd = c(
    11.07,
    9.16,
    8.73,
    4.77,
    5.21,
    6.55
  ),
  
  n = 14
)

# Combine HIT and 3ST

data <- data.frame(
  
  variable = data_HIT$variable,
  
  mean_HIT = data_HIT$mean,
  sd_HIT = data_HIT$sd,
  n_HIT = data_HIT$n,
  
  mean_3ST = data_3ST$mean,
  sd_3ST = data_3ST$sd,
  n_3ST = data_3ST$n
)

# Pooled SD

data$SD_pooled <- sqrt(
  (
    (data$n_HIT - 1) * data$sd_HIT^2 +
      (data$n_3ST - 1) * data$sd_3ST^2
  ) /
    (
      data$n_HIT +
        data$n_3ST -
        2
    )
)

# Cohen's d

# Absolute value = magnitude only

data$d <- abs(
  data$mean_HIT - data$mean_3ST
) / data$SD_pooled

# Probability of superiority

# Direction follows the group with the higher mean

data <- data |>
  mutate(
    
    P_superiority = ifelse(
      
      mean_HIT >= mean_3ST,
      
      pnorm(
        (mean_HIT - mean_3ST) /
          sqrt(sd_HIT^2 + sd_3ST^2)
      ),
      
      pnorm(
        (mean_3ST - mean_HIT) /
          sqrt(sd_HIT^2 + sd_3ST^2)
      )
    ),
    
    superiority_label = ifelse(
      mean_HIT >= mean_3ST,
      "P(HIT > 3ST)",
      "P(3ST > HIT)"
    )
  )

# OVL function

calculate_ovl <- function(mu1, sd1, mu2, sd2) {
  
  lower <- min(
    mu1 - 5 * sd1,
    mu2 - 5 * sd2
  )
  
  upper <- max(
    mu1 + 5 * sd1,
    mu2 + 5 * sd2
  )
  
  integrate(
    function(x) {
      pmin(
        dnorm(
          x,
          mean = mu1,
          sd = sd1
        ),
        dnorm(
          x,
          mean = mu2,
          sd = sd2
        )
      )
    },
    lower = lower,
    upper = upper
  )$value
}

data$OVL <- mapply(
  calculate_ovl,
  data$mean_HIT,
  data$sd_HIT,
  data$mean_3ST,
  data$sd_3ST
)

# Annotation

data <- data |>
  mutate(
    
    label = sprintf(
      "%s = %.2f\nCohen's d = %.2f\nOVL = %.2f",
      superiority_label,
      P_superiority,
      d,
      OVL
    )
  )

# Generate normal curves

plot_data <- lapply(
  seq_len(nrow(data)),
  function(i) {
    
    row <- data[i, ]
    
    x_min <- min(
      row$mean_HIT - 3.5 * row$sd_HIT,
      row$mean_3ST - 3.5 * row$sd_3ST
    )
    
    x_max <- max(
      row$mean_HIT + 5 * row$sd_HIT,
      row$mean_3ST + 5 * row$sd_3ST
    )
    
    x <- seq(
      x_min,
      x_max,
      length.out = 1000
    )
    
    hit <- dnorm(
      x,
      mean = row$mean_HIT,
      sd = row$sd_HIT
    )
    
    st3 <- dnorm(
      x,
      mean = row$mean_3ST,
      sd = row$sd_3ST
    )
    
    data.frame(
      variable = row$variable,
      x = x,
      HIT = hit,
      ST3 = st3,
      overlap = pmin(hit, st3)
    )
  }
) |>
  bind_rows()

# Long format for curves

plot_curves <- plot_data |>
  pivot_longer(
    cols = c(HIT, ST3),
    names_to = "group",
    values_to = "density"
  )

# Mean lines

mean_lines <- bind_rows(
  
  data |>
    transmute(
      variable,
      group = "HIT",
      x = mean_HIT,
      yend = dnorm(
        mean_HIT,
        mean = mean_HIT,
        sd = sd_HIT
      )
    ),
  
  data |>
    transmute(
      variable,
      group = "ST3",
      x = mean_3ST,
      yend = dnorm(
        mean_3ST,
        mean = mean_3ST,
        sd = sd_3ST
      )
    )
)

# Annotation positions

annotations <- data |>
  rowwise() |>
  mutate(
    
    x_min = min(
      mean_HIT - 4 * sd_HIT,
      mean_3ST - 4 * sd_3ST
    ),
    
    x_max = max(
      mean_HIT + 4 * sd_HIT,
      mean_3ST + 4 * sd_3ST
    ),
    
    x_position = x_min +
      0.95 * (x_max - x_min),
    
    max_density = max(
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_HIT,
        sd = sd_HIT
      ),
      
      dnorm(
        seq(
          x_min,
          x_max,
          length.out = 1000
        ),
        mean = mean_3ST,
        sd = sd_3ST
      )
    ),
    
    y_position = max_density * 0.4
  ) |>
  ungroup()

# Swedish labels

bodycomp_labels <- c(
  "Body Mass" = "Kroppsmassa (kg)",
  "Fat Free Mass" = "Fettfri massa (kg)",
  "Muscle Mass" = "Muskelmassa (kg)",
  "Fat Mass" = "Fettmassa (kg)",
  "Fat Percentage" = "Fettprocent (%)",
  "Total Body Water" = "Totalt kroppsvatten (kg)"
)

# Plot

ggplot() +
  
  # OVL
  
  geom_ribbon(
    data = plot_data,
    aes(
      x = x,
      ymin = 0,
      ymax = overlap
    ),
    fill = "grey70",
    alpha = 0.6
  ) +
  
  # Normal curves
  
  geom_line(
    data = plot_curves,
    aes(
      x = x,
      y = density,
      colour = group
    ),
    linewidth = 1.2
  ) +
  
  # Mean lines
  
  geom_segment(
    data = mean_lines,
    aes(
      x = x,
      xend = x,
      y = 0,
      yend = yend,
      colour = group
    ),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  # Effect-size annotations
  
  geom_text(
    data = annotations,
    aes(
      x = x_position,
      y = y_position,
      label = label
    ),
    hjust = 0.5,
    vjust = 0,
    size = 2.8
  ) +
  
  # Colours
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "HIT" = "blue",
      "ST3" = "red"
    ),
    labels = c(
      "HIT" = "Post-HIT",
      "ST3" = "Post-3ST"
    )
  ) +
  
  # 2 x 3 panel
  
  facet_wrap(
    ~variable,
    scales = "free",
    ncol = 3,
    labeller = as_labeller(bodycomp_labels)
  ) +
  
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.line = element_line(
      colour = "black"
    ),
    
    axis.text.y = element_blank(),
    
    axis.ticks.y = element_blank(),
    
    legend.position = "top",
    
    strip.text = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )

##############################