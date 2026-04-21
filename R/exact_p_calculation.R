# ─────────────────────────────────────────────────────────────────────────────
#                            #### Cheval 2015 ####
# ─────────────────────────────────────────────────────────────────────────────

# Sample size
n <- 97

# Pearson correlation coefficient
r <- 0.27

# Compute the t-statistic
t_statistic <- r * sqrt((n - 2) / (1 - r^2))

# Degrees of freedom
df <- n - 2

# Compute the p-value
p_value <- 2 * pt(abs(t_statistic), df = df, lower.tail = FALSE)

# Print the p-value
print(p_value)

# ─────────────────────────────────────────────────────────────────────────────
#                            #### Oliver 2018 ####
# ─────────────────────────────────────────────────────────────────────────────

# Sample size
n <- 103

# Pearson correlation coefficient
r <- 0.03

# Compute the t-statistic
t_statistic <- r * sqrt((n - 2) / (1 - r^2))

# Degrees of freedom
df <- n - 2

# Compute the p-value
p_value <- 2 * pt(abs(t_statistic), df = df, lower.tail = FALSE)

# Print the p-value
print(p_value)


# ─────────────────────────────────────────────────────────────────────────────
#                            #### Hannan 2019 ####
# ─────────────────────────────────────────────────────────────────────────────

# Sample size
n <- 104

# Pearson correlation coefficient
r <- 0.25

# Compute the t-statistic
t_statistic <- r * sqrt((n - 2) / (1 - r^2))

# Degrees of freedom
df <- n - 2

# Compute the p-value
p_value <- 2 * pt(abs(t_statistic), df = df, lower.tail = FALSE)

# Print the p-value
print(p_value)


# ─────────────────────────────────────────────────────────────────────────────
#                            #### Berry 2020 ####
# ─────────────────────────────────────────────────────────────────────────────

# Sample size
n <- 149

# Pearson correlation coefficient
r <- 0.187

# Compute the t-statistic
t_statistic <- r * sqrt((n - 2) / (1 - r^2))

# Degrees of freedom
df <- n - 2

# Compute the p-value
p_value <- 2 * pt(abs(t_statistic), df = df, lower.tail = FALSE)

# Print the p-value
print(p_value)


# ─────────────────────────────────────────────────────────────────────────────
#                            #### Wang 2025 ####
# ─────────────────────────────────────────────────────────────────────────────

# Sample size
n <- 34

# Pearson correlation coefficient
r <- 0.585

# Compute the t-statistic
t_statistic <- r * sqrt((n - 2) / (1 - r^2))

# Degrees of freedom
df <- n - 2

# Compute the p-value
p_value <- 2 * pt(abs(t_statistic), df = df, lower.tail = FALSE)

# Print the p-value
print(p_value)

# ─────────────────────────────────────────────────────────────────────────────
#                            #### Terashima 2025 ####
# ─────────────────────────────────────────────────────────────────────────────

# Sample size
n <- 226

# Pearson correlation coefficient
r <- 0.15

# Compute the t-statistic
t_statistic <- r * sqrt((n - 2) / (1 - r^2))

# Degrees of freedom
df <- n - 2

# Compute the p-value
p_value <- 2 * pt(abs(t_statistic), df = df, lower.tail = FALSE)

# Print the p-value
print(p_value)
