
# Data analysis of idle brick breaker token prestige farming 25.01.2026

setwd("C:/R-statistics-analysis")


# Data consists of two metrics:
# 1. time to prestige
# 2. tokens per prestige

# Let's analyse the distribution of these variables from a screen recording
# of multiple prestiges

###########################################################

# Time stamps in video of prestiges (s)
time_stamps <- c(0, 12, 22, 31, 38, 47, 56, 65, 73, 91, 100, 110, 119, 129, 138, 150)
length(time_stamps)

# Time to prestige (difference between two time stamps)
time_to_prestige <- diff(time_stamps)

time_to_prestige
length(time_to_prestige)

###########################################################

# Tokens per prestige
tokens_per_prestige <- c(33, 33, 33, 33, 33, 33, 33, 40, 56, 36, 36, 33, 33, 36, 36)
length(tokens_per_prestige)

###########################################################

# Mean and SD of time to prestige
mean_time <- mean(time_to_prestige)
sd_time <- sd(time_to_prestige)

# Hypothetical normal distribution of time to prestige
x_time <- seq(mean_time - 4*sd_time, mean_time + 4*sd_time, length.out = 100)
norm_time <- dnorm(x_time, mean = mean_time, sd = sd_time)

###########################################################

# Mean and SD of tokens per prestige
mean_tokens <- mean(tokens_per_prestige)
sd_tokens <- sd(tokens_per_prestige)

#Hypothetical normal distribution of tokens per prestige
x_tokens <- seq(mean_tokens - 4*sd_tokens, mean_tokens + 4*sd_tokens, length.out = 100)
norm_tokens <- dnorm(x_tokens, mean = mean_tokens, sd = sd_tokens)

###########################################################

# Histograms of time to prestige and tokens per prestige
par(mfrow = c(1, 2))

# Custom bin edges
hist(time_to_prestige, 
     breaks = seq(0, 30, by = 2), 
     probability = TRUE,
     main = "Time to Prestige",
     xlab = "Time (s)",
     col = "lightblue"
     )
lines(x_time, norm_time, col = "red", lwd = 4)

# Finer bins for tokens
hist(
  tokens_per_prestige, 
  breaks = seq(10, 70, by = 2), 
  probability = TRUE,
  main = "Tokens per Prestige",
  xlab = "Tokens",
  col = "lightblue"
  )
lines(x_tokens, norm_tokens, col = "red", lwd = 4)
par(mfrow = c(1, 1))

###########################################################

# 95% CI for time and tokens

# Time
n  <- length(time_to_prestige)
se <- sd_time / sqrt(n)
ci_time <- mean_time + qt(c(0.025, 0.975), df = n - 1) * se

# Tokens
n  <- length(tokens_per_prestige)
se <- sd_tokens / sqrt(n)
ci_tokens <- mean_tokens + qt(c(0.025, 0.975), df = n - 1) * se

cat(sprintf("95%% CI for time to prestige: [%.2f, %.2f] seconds\n", ci_time[1], ci_time[2]))
cat(sprintf("95%% CI for tokens per prestige: [%.2f, %.2f] tokens\n", ci_tokens[1], ci_tokens[2]))

###########################################################

# Tokens per hour

# Tokens per hour for each prestige
tokens_per_hour <- tokens_per_prestige / (time_to_prestige / 3600)  # time_to_prestige in seconds, convert to hours

# Mean and SD
mean_tph <- mean(tokens_per_hour)
sd_tph <- sd(tokens_per_hour)
n <- length(tokens_per_hour)
se_tph <- sd_tph / sqrt(n)


# X-axis values for normal curve
x_tph <- seq(mean_tph - 4*sd_tph, mean_tph + 4*sd_tph, length.out = 100)
norm_tph <- dnorm(x_tph, mean = mean_tph, sd = sd_tph)

###########################################################

###########################################################

# Tokens per day (24 hours)

# Tokens per day
tokens_per_day <- tokens_per_hour * 24

# Mean and SD
mean_tpd <- mean(tokens_per_day)
sd_tpd <- sd(tokens_per_day)

# X-axis for normal curve
x_tpd <- seq(mean_tpd - 4*sd_tpd, mean_tpd + 4*sd_tpd, length.out = 100)
norm_tpd <- dnorm(x_tpd, mean = mean_tpd, sd = sd_tpd)

###########################################################

# Histograms of tokens per hour and tokens per day

# Plot side by side
par(mfrow = c(1, 2))

hist(tokens_per_hour, 
     breaks = seq(5e3, 20e3, by = 1e3), 
     probability = TRUE,
     main = "Tokens per Hour",
     xlab = "Tokens/hour",
     col = "lightblue")
lines(x_tph, norm_tph, col = "red", lwd = 4)

hist(tokens_per_day, 
     breaks = seq(1e5, 5.5e5, by = 4e4), 
     probability = TRUE,
     main = "Tokens per Day",
     xlab = "Tokens/day",
     col = "lightblue")
lines(x_tpd, norm_tpd, col = "red", lwd = 4)

par(mfrow = c(1, 1))

###########################################################

# 95% CI for tokens per hour
ci_tph <- mean_tph + qt(c(0.025, 0.975), df = n - 1) * se_tph

cat(sprintf("95%% CI for tokens per hour: [%.2f, %.2f] tokens/hour\n", ci_tph[1], ci_tph[2]))


# 95% CI for tokens per day
ci_tpd <- ci_tph * 24

cat(sprintf("95%% CI for tokens per day: [%.2f, %.2f] tokens/day\n", ci_tpd[1], ci_tpd[2]))

###########################################################

# Data analysis of prestige tokens farming 27.01.2026

# Instant prestiges take about 2 seconds
# and earn around 37-48 tokens per prestige

# We assume a normal distribution for time and tokens per prestige



# Time per prestige (seconds) (2 s is lower 95% of range)
mean_time <- 2 / (1 - 1.96 * 0.05)
sd_time   <- 0.05 * mean_time



# Tokens per prestige
mean_tokens <- (40 + 51) / 2
sd_tokens   <- 11 / (2 * 1.96)



# X ranges
x_time <- seq(mean_time - 4*sd_time, mean_time + 4*sd_time, length.out = 1000)
x_tokens <- seq(mean_tokens - 4*sd_tokens, mean_tokens + 4*sd_tokens, length.out = 1000)



# Densities
d_time <- dnorm(x_time, mean_time, sd_time)
d_tokens <- dnorm(x_tokens, mean_tokens, sd_tokens)



plot(x_time, d_time, type = "l", lwd = 4,
     main = "Time per Prestige",
     xlab = "Seconds", ylab = "Density")



plot(x_tokens, d_tokens, type = "l", lwd = 4,
     main = "Tokens per Prestige",
     xlab = "Tokens", ylab = "Density")



# Means
mean_tph <- mean_tokens / (mean_time / 3600)
mean_tpd <- mean_tph * 24



# SDs (ratio scaling)
sd_tph <- mean_tph * sqrt(
  (sd_tokens / mean_tokens)^2 +
    (sd_time / mean_time)^2
)
sd_tpd <- sd_tph * 24



# X ranges
x_tph <- seq(mean_tph - 4*sd_tph, mean_tph + 4*sd_tph, length.out = 1000)
x_tpd <- seq(mean_tpd - 4*sd_tpd, mean_tpd + 4*sd_tpd, length.out = 1000)



# Densities
d_tph <- dnorm(x_tph, mean_tph, sd_tph)
d_tpd <- dnorm(x_tpd, mean_tpd, sd_tpd)



plot(x_tph, d_tph, type = "l", lwd = 4,
     main = "Tokens per Hour",
     xlab = "Tokens/hour", ylab = "Density")



plot(x_tpd, d_tpd, type = "l", lwd = 4,
     main = "Tokens per Day",
     xlab = "Tokens/day", ylab = "Density")



# 95% CI for tokens per hour
ci_tph <- mean_tph + c(-1, 1) * 1.96 * sd_tph



# 95% CI for tokens per day
ci_tpd <- mean_tpd + c(-1, 1) * 1.96 * sd_tpd



cat(sprintf("95%% CI for tokens per hour: [%.0f, %.0f] tokens/hour\n",
            ci_tph[1], ci_tph[2]))



cat(sprintf("95%% CI for tokens per day: [%.0f, %.0f] tokens/day\n",
            ci_tpd[1], ci_tpd[2]))

###########################################################



###########################################################

# Data analysis of prestige tokens farming 09.02.2026

# Instant prestiges take about 1.55 seconds
# and earn around 58-74 tokens per prestige

# We assume a normal distribution for time and tokens per prestige



# Time per prestige (seconds) (1.55 s is lower 95% of range)
mean_time <- 1.55 / (1 - 1.96 * 0.05)
sd_time   <- 0.05 * mean_time



# Tokens per prestige
mean_tokens <- (58 + 74) / 2
sd_tokens   <- (74 - 58) / (2 * 1.96)



# X ranges
x_time <- seq(mean_time - 4*sd_time, mean_time + 4*sd_time, length.out = 1000)
x_tokens <- seq(mean_tokens - 4*sd_tokens, mean_tokens + 4*sd_tokens, length.out = 1000)



# Densities
d_time <- dnorm(x_time, mean_time, sd_time)
d_tokens <- dnorm(x_tokens, mean_tokens, sd_tokens)



plot(x_time, d_time, type = "l", lwd = 4,
     main = "Time per Prestige",
     xlab = "Seconds", ylab = "Density")



plot(x_tokens, d_tokens, type = "l", lwd = 4,
     main = "Tokens per Prestige",
     xlab = "Tokens", ylab = "Density")



# Means
mean_tph <- mean_tokens / (mean_time / 3600)
mean_tpd <- mean_tph * 24



# SDs (ratio scaling)
sd_tph <- mean_tph * sqrt(
  (sd_tokens / mean_tokens)^2 +
    (sd_time / mean_time)^2
)
sd_tpd <- sd_tph * 24



# X ranges
x_tph <- seq(mean_tph - 4*sd_tph, mean_tph + 4*sd_tph, length.out = 1000)
x_tpd <- seq(mean_tpd - 4*sd_tpd, mean_tpd + 4*sd_tpd, length.out = 1000)



# Densities
d_tph <- dnorm(x_tph, mean_tph, sd_tph)
d_tpd <- dnorm(x_tpd, mean_tpd, sd_tpd)



plot(x_tph, d_tph, type = "l", lwd = 4,
     main = "Tokens per Hour",
     xlab = "Tokens/hour", ylab = "Density")



plot(x_tpd, d_tpd, type = "l", lwd = 4,
     main = "Tokens per Day",
     xlab = "Tokens/day", ylab = "Density")



# 95% CI for tokens per hour
ci_tph <- mean_tph + c(-1, 1) * 1.96 * sd_tph



# 95% CI for tokens per day
ci_tpd <- mean_tpd + c(-1, 1) * 1.96 * sd_tpd



cat(sprintf("95%% CI for tokens per hour: [%.0f, %.0f] tokens/hour\n",
            ci_tph[1], ci_tph[2]))



cat(sprintf("95%% CI for tokens per day: [%.0f, %.0f] tokens/day\n",
            ci_tpd[1], ci_tpd[2]))

###########################################################
