
# Skript som används för att genera grafer till till "projekt.tex" filen.

setwd("C:/R-statistics-analysis")

library(ggplot2)
library(ggpattern)
library(dplyr)
library(deSolve)
library(reshape2)

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
    strip.text = element_text(linewidth = 10, margin = margin(t = 0, b = 0)),
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

# Inferens 1

x <- seq(-3, 6, length.out = 1000)

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = 0, sd = 1),
    dnorm(x, mean = 1.645, sd = 1)
  ),
  distribution = factor(rep(c("Mean 0", "Mean 1.645"), each = length(x)))
)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(0, 1.645),
    labels = c(
      expression(mu[0] == 0),
      expression(mu[1] == 1.645)
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
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = 1.645, xend = 1.645, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1)
##############################



##############################

# Inferens 2

x <- seq(-3, 6, length.out = 1000)

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = 0, sd = 1),
    dnorm(x, mean = 1.645, sd = 1)
  ),
  distribution = factor(rep(c("Mean 0", "Mean 1.645"), each = length(x)))
)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_ribbon(
    data = subset(df, x >= 1.645 & distribution == "Mean 0"),
    aes(x = x, ymin = 0, ymax = density),
    alpha = 0.5,
    inherit.aes = FALSE,
    fill = "orange"
  ) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(0, 1.645),
    labels = c(
      expression(mu[0] == 0),
      expression(mu[1] == 1.645)
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
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = 1.645, xend = 1.645, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  annotate("text", x = 2.8, y = 0.06, label = expression(alpha == 0.05), color = "orange", size = 5)

##############################



##############################

# Inferens 3

x <- seq(-3, 6, length.out = 1000)

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = 0, sd = 1),
    dnorm(x, mean = 1.645, sd = 1)
  ),
  distribution = factor(rep(c("Mean 0", "Mean 1.645"), each = length(x)))
)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_ribbon(
    data = subset(df, x < 1.645 & distribution == "Mean 1.645"),
    aes(x = x, ymin = 0, ymax = density),
    alpha = 0.5,
    inherit.aes = FALSE,
    fill = "orange"
  ) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(0, 1.645),
    labels = c(
      expression(mu[0] == 0),
      expression(mu[1] == 1.645)
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
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = 1.645, xend = 1.645, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  annotate("text", x = -1, y = 0.06, label = expression(beta == 0.50), color = "orange", size = 5)
##############################



##############################

# Inferens 4

x <- seq(-3, 6, length.out = 1000)

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = 0, sd = 1),
    dnorm(x, mean = 1.645, sd = 1)
  ),
  distribution = factor(rep(c("Mean 0", "Mean 1.645"), each = length(x)))
)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_ribbon(
    data = subset(df, x >= 1.645 & distribution == "Mean 1.645"),
    aes(x = x, ymin = 0, ymax = density),
    alpha = 0.5,
    inherit.aes = FALSE,
    fill = "orange"
  ) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(0, 1.645),
    labels = c(
      expression(mu[0] == 0),
      expression(mu[1] == 1.645)
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
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = 1.645, xend = 1.645, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  annotate("text", x = 4.5, y = 0.06, label = expression(1 - beta == 0.50), color = "orange", size = 5)
##############################



##############################

# Inferens 5

x <- seq(-3, 7, length.out = 1000)

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = 0, sd = 1),
    dnorm(x, mean = 2.5, sd = 1)
  ),
  distribution = factor(rep(c("Mean 0", "Mean 2.5"), each = length(x)))
)


crit_val <- 1.645
shade_alpha <- subset(df, distribution == "Mean 0" & x >= crit_val)

alt_mean <- 2.5
alt_sd <- 1
alt_density_at_crit <- dnorm(crit_val, mean = alt_mean, sd = alt_sd)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_ribbon(
    data = subset(df, x >= 1.645 & distribution == "Mean 2.5"),
    aes(x = x, ymin = 0, ymax = density),
    alpha = 0.5,
    inherit.aes = FALSE,
    fill = "orange"
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "Mean 0" & x >= crit_val),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "stripe",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
  ) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(0, 2.5),
    labels = c(
      expression(mu[0] == 0),
      expression(mu[1] == 2.5)
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
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = 2.5, xend = 2.5, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  annotate("text", x = 5.5, y = 0.06, label = expression(1 - beta %~~% 0.80), color = "orange", size = 5) +
  
  geom_segment(aes(x = crit_val, xend = crit_val, y = 0, yend = alt_density_at_crit),
               color = "brown", linetype = "dashed", linewidth = 1) +
  
  # Label "alpha = 0.05" near the top of the dashed line
  annotate("text", x = crit_val + 0.2, y = 0.09, label = expression(alpha == 0.05),
           color = "brown", size = 5, hjust = 1.3, vjust = 4.0)

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

# Histogram 1

# Generate random data
x <- rnorm(n = 100, mean = 0, sd = 1)

ggplot(data.frame(x = x), aes(x = x)) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "black",
    alpha = 0.6
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

# Generate exponential data
x <- rexp(n = 500, rate = 1)   # rate = λ

ggplot(data.frame(x = x), aes(x = x)) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "black",
    alpha = 0.6
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
  scale_y_continuous(breaks = seq(0, 4, by = 2)) +
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
                 "sample"   = "Stickprov (SD)",
                 "mean" = "Medelvärden (SE)"
               )
             )) +
  
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

x <- seq(0, 1500, length.out = 1000)

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

ggplot(df, aes(x = x, y = density, color = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c(
      "Glucose" = "darkgrey",
      "Glucose + acetate" = "red",
      "Glucose + octanoate" = "blue",
      "Glucose + palmitate" = "green"
    )
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

x <- seq(0, 1500, length.out = 1000)

df <- data.frame(
  x = rep(x, 4),
  density = c(
    dnorm(x, mean = 35, sd = 2*6),
    dnorm(x, mean = 910, sd = 2*87),
    dnorm(x, mean = 942, sd = 2*106),
    dnorm(x, mean = 956, sd = 2*95)
  ),
  distribution = factor(
    rep(c("Glucose", "Glucose + acetate", "Glucose + octanoate", "Glucose + palmitate"), each = length(x))
  )
)

ggplot(df, aes(x = x, y = density, color = distribution)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c(
      "Glucose" = "darkgrey",
      "Glucose + acetate" = "red",
      "Glucose + octanoate" = "blue",
      "Glucose + palmitate" = "green"
    )
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
  facet_wrap(~ type, nrow = 1,
             labeller = labeller(
               type = c(
                 "sample"   = "Stickprov (SD)",
                 "mean" = "Medelvärden (SE)"
               )
             )) +
  
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
  facet_wrap(~ type, nrow = 1,
             labeller = labeller(
               type = c(
                 "sample"   = "Stickprov (SD)",
                 "mean" = "Medelvärden (SE)"
               )
             )) +
  
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
  y_start = -0.01,
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
    labels = expression(-2*s, -s, bar(x), +s, +2*s)
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
mu_B <- 2.5
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
    breaks = c(0, 2),
    labels = c("","")
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
  )
##############################



##############################

# Överlappning 2

x <- seq(-4, 8, length.out = 1000)

mu_A <- 0
mu_B <- 2.5
sigma <- 1

df <- data.frame(
  x = rep(x, 2),
  density = c(
    dnorm(x, mean = mu_A, sd = sigma),
    dnorm(x, mean = mu_B, sd = sigma)
  ),
  distribution = factor(rep(c("Mean 0", "Mean 2"), each = length(x)))
)

dA <- dnorm(x, mean = mu_A, sd = sigma)
dB <- dnorm(x, mean = mu_B, sd = sigma)

AB_density <- pmin(dA, dB)


# Central 95% limits
lower_A <- qnorm(0.025, mean = mu_A, sd = sigma)
upper_A <- qnorm(0.975, mean = mu_A, sd = sigma)

lower_B <- qnorm(0.025, mean = mu_B, sd = sigma)
upper_B <- qnorm(0.975, mean = mu_B, sd = sigma)

left  <- max(lower_A, lower_B)
right <- min(upper_A, upper_B)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon_pattern(
    data = subset(
      data.frame(x = x, density = AB_density),
      x >= left & x <= right
    ),
    aes(x = x, ymin = 0, ymax = density),
    inherit.aes = FALSE,
    fill = NA,                  # transparent background
    pattern = "stripe",
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_angle = 45,
    pattern_density = 0.2,
    pattern_spacing = 0.03,
    alpha = 1
  ) +
  scale_x_continuous(
    breaks = c(0, 2),
    labels = c("", "")
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
  geom_segment(
    aes(
      x = lower_A,
      xend = lower_A,
      y = 0,
      yend = dnorm(lower_A, mean = mu_A, sd = sigma)
    ),
    color = "blue",
    linetype = 1,
    linewidth = 1.5
  ) +
  geom_segment(
    aes(
      x = upper_A,
      xend = upper_A,
      y = 0,
      yend = dnorm(lower_A, mean = mu_A, sd = sigma)
    ),
    color = "blue",
    linetype = 1,
    linewidth = 1.5
  ) +
  geom_segment(
    aes(
      x = lower_B,
      xend = lower_B,
      y = 0,
      yend = dnorm(lower_B, mean = mu_B, sd = sigma)
    ),
    color = "red",
    linetype = 1,
    linewidth = 1.5
  ) +
  geom_segment(
    aes(
      x = upper_B,
      xend = upper_B,
      y = 0,
      yend = dnorm(upper_B, mean = mu_B, sd = sigma)
    ),
    color = "red",
    linetype = 1,
    linewidth = 1.5
  ) +
  geom_segment(
    aes(
      x = mu_A,
      xend = mu_A,
      y = 0,
      yend = dnorm(mu_A, mean = mu_A, sd = sigma)
    ),
    color = "blue",
    linetype = "dashed",
    linewidth = 1.2
  ) +
  geom_segment(
    aes(
      x = mu_B,
      xend = mu_B,
      y = 0,
      yend = dnorm(mu_B, mean = mu_B, sd = sigma)
    ),
    color = "red",
    linetype = "dashed",
    linewidth = 1.2
  )
##############################




##############################

# P(B>A) #1

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

# P(B>A) #2

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
n <- 20
se <- sigma / sqrt(n)

# 95% confidence limits
A_lower <- mu_A - 1.96 * se
A_upper <- mu_A + 1.96 * se

B_lower <- mu_B - 1.96 * se
B_upper <- mu_B + 1.96 * se

shade_A = x >= A_lower & x <= A_upper
shade_B = x >= B_lower & x <= B_upper


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
  
  geom_ribbon(
    data = subset(df, shade),
    aes(ymin = 0, ymax = density, fill = distribution),
    alpha = 0.3,
    colour = NA
  ) +
  scale_fill_manual(
    values = c("Grupp A" = "red",
               "Grupp B" = "blue"),
    name = expression(paste("95% CI (n = 20)"))
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

x <- seq(80, 130, length.out = 1000)


# Parameters
mu_A <- 100
mu_B <- 110
sigma <- 20
n <- 20
se <- sigma / sqrt(n)

# 95% confidence limits
A_lower <- mu_A - 1.96 * se
A_upper <- mu_A + 1.96 * se

B_lower <- mu_B - 1.96 * se
B_upper <- mu_B + 1.96 * se

shade_A = x >= A_lower & x <= A_upper
shade_B = x >= B_lower & x <= B_upper


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


crit_val <- A_upper
shade_alpha <- subset(df, distribution == "Grupp A" & x >= crit_val)

alt_density_at_crit <- dnorm(
  crit_val,
  mean = mu_B,
  sd = se
)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_ribbon(
    data = subset(df, x >= crit_val & distribution == "Grupp B"),
    aes(x = x, ymin = 0, ymax = density),
    alpha = 0.5,
    inherit.aes = FALSE,
    fill = "orange"
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "Grupp A" & x >= crit_val),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "stripe",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
  ) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(mu_A, mu_B),
    labels = c(
      expression(mu[A]==100),
      expression(mu[B]==110)
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
  geom_segment(aes(x = mu_A, xend = mu_A, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_B, xend = mu_B, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  annotate("text", x = 119, y = 0.06, label = expression(1 - beta %~~% 0.61), color = "orange", size = 5) +
  
  geom_segment(aes(x = crit_val, xend = crit_val, y = 0, yend = alt_density_at_crit),
               color = "brown", linetype = "dashed", linewidth = 1)
##############################



##############################

# SD vs SE exempel 4

x <- seq(80, 130, length.out = 1000)


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

shade_A = x >= A_lower & x <= A_upper
shade_B = x >= B_lower & x <= B_upper


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


crit_val <- A_upper
shade_alpha <- subset(df, distribution == "Grupp A" & x >= crit_val)

alt_density_at_crit <- dnorm(
  crit_val,
  mean = mu_B,
  sd = se
)

ggplot(df, aes(x = x, y = density, fill = distribution)) +
  geom_ribbon(
    data = subset(df, x >= crit_val & distribution == "Grupp B"),
    aes(x = x, ymin = 0, ymax = density),
    alpha = 0.5,
    inherit.aes = FALSE,
    fill = "orange"
  ) +
  geom_ribbon_pattern(
    data = subset(df, distribution == "Grupp A" & x >= crit_val),
    aes(x = x, ymin = 0, ymax = density),
    fill = "NA",
    pattern = "stripe",
    pattern_fill = "brown",
    pattern_angle = 45,
    pattern_density = 0.1,
    inherit.aes = FALSE
  ) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    breaks = c(mu_A, mu_B),
    labels = c(
      expression(mu[A]==100),
      expression(mu[B]==110)
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
  geom_segment(aes(x = mu_A, xend = mu_A, y = 0, yend = max(df$density)), color = "red", linetype = 1, linewidth = 1) +
  geom_segment(aes(x = mu_B, xend = mu_B, y = 0, yend = max(df$density)), color = "blue", linetype = 1, linewidth = 1) +
  annotate("text", x = 119, y = 0.06, label = expression(1 - beta %~~% 0.84), color = "orange", size = 5) +
  
  geom_segment(aes(x = crit_val, xend = crit_val, y = 0, yend = alt_density_at_crit),
               color = "brown", linetype = "dashed", linewidth = 1)
##############################



##############################

# SD vs SE exempel 5

# Parameters
mu_A <- 100
mu_B <- 110
sigma <- 20
n <- 20
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
