
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

# install.packages("deSolve")

library(deSolve)

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

# Rita kurvor för koncentrationerna
library(ggplot2)

library(reshape2)
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

library(ggpattern) # for dashed alpha region area

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
  annotate("text", x = crit_val + 0.2, y = crit_density, label = expression(alpha == 0.05),
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

library(ggplot2)

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

# Stewart pH 1


# Quartic equation solver in R

# Function to solve quartic equations
solve_quartic <- function(a, b, c, d, e) {
  # Input validation
  if (!is.numeric(c(a, b, c, d, e))) {
    stop("All coefficients must be numeric.")
  }
  if (a == 0) {
    stop("Coefficient 'a' cannot be zero for a quartic equation.")
  }
  
  # Coefficients in descending order of powers
  coeffs <- c(a, b, c, d, e)
  
  # Solve using polyroot (returns complex roots if needed)
  roots <- polyroot(coeffs)
  
  return(roots)
}

# Example usage:
# Equation: 2x^4 - 3x^3 + x^2 - 5x + 6 = 0
a <- 2
b <- -3
c <- 1
d <- -5
e <- 6

roots <- solve_quartic(a, b, c, d, e)

# Display results
cat("Roots of the quartic equation:\n")
print(roots)
