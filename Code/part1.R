#### Part 1
# Loading packages and data
source("Code/load_data.R")

# Creating models
lin = lm(betaplasma ~ bmi, data = carotene)
log = lm(log(betaplasma) ~ bmi, data = carotene)

# Plotting dependent variable against independent variable
ggplot(carotene, aes(x = bmi, y = betaplasma)) +
  geom_point() +
  xlab(expression("BMI (kg/"*m^2*")")) +
  ylab(expression("Betaplasma (ng/ml)")) +
  labs(title = "Plasma Beta-Carotene Levels vs Body Mass Index")

# Plotting transformed dependent variable against independent variable
ggplot(carotene, aes(x = bmi, y = log(betaplasma))) +
  geom_point() +
  xlab(expression("BMI (kg/"*m^2*")")) +
  ylab(expression("Log Betaplasma (ng/ml)")) +
  labs(title = "Log Plasma Beta-Carotene Levels vs Body Mass Index")

####1(a) - Comparing residuals
e_lin <- lin$residuals
lin_lim_e <- max(abs(e_lin))*c(-1, 1)

ggplot(carotene, aes(x = bmi, y = e_lin)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y = lin_lim_e) +
  xlab(expression("BMI (kg/"*m^2*")")) +
  ylab("Residual") +
  labs(title = "Residuals vs Body Mass Index")

# QQ-plot
ggplot(data = carotene, aes(sample = e_lin)) +
  geom_qq(size = 3) + geom_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  labs(title = "Normal Q-Q-plot of the Residuals")

e_log <- log$residuals
log_lim_e <- max(abs(e_log))*c(-1, 1)

ggplot(carotene, aes(x = bmi, y = e_log)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y = log_lim_e) +
  xlab(expression("BMI (kg/"*m^2*")")) +
  ylab("Residual") +
  labs(title = "Residuals vs Body Mass Index")
# log model looks better!

# QQ-plot
ggplot(data = carotene, aes(sample = e_log)) +
  geom_qq(size = 3) + geom_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  labs(title = "Normal Q-Q-plot of the Residuals")

####1(b)
# Model summary
summary(log)
confint(log)
# Fitted line with intervals
c(min(carotene$bmi), max(carotene$bmi))
betaplasma_seq <- data.frame(bmi=seq(16, 51))

betaplasma_seq |> mutate(
  fit = predict(log, newdata = betaplasma_seq),
  conf = predict(log, newdata = betaplasma_seq, interval = "confidence"),
  pred = predict(log, newdata = betaplasma_seq, interval = "prediction")) -> 
  betaplasma_ints

ggplot(betaplasma_ints, aes(x = bmi)) + 
  geom_point(data = carotene, aes(y = log(betaplasma)), size = 3) +
  geom_line(aes(y = fit), color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = conf[, "lwr"], ymax = conf[, "upr"]), alpha = 0.2) +
  geom_line(aes(y = pred[, "lwr"]), color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = pred[, "upr"]), color = "red", linetype = "dashed", linewidth = 1) +
  xlab(expression("BMI (kg/"*m^2*")")) +
  ylab("Log Betaplasma (ng/ml)") +
  labs(title = "Log Plasma Beta-Carotene vs Body Mass Index")

# Fitted line with intervals in original scale

betaplasma_seq |> mutate(
  fit = predict(log, newdata = betaplasma_seq),
  conf = predict(log, newdata = betaplasma_seq, interval = "confidence"),
  pred = predict(log, newdata = betaplasma_seq, interval = "prediction")) -> 
  betaplasma_ints

ggplot(betaplasma_ints, aes(x = bmi)) + 
  geom_point(data = carotene, aes(y = betaplasma), size = 3) +
  geom_line(aes(y = exp(fit)), color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = exp(conf[, "lwr"]), ymax = exp(conf[, "upr"])), alpha = 0.2) +
  geom_line(aes(y = exp(pred[, "lwr"])), color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = exp(pred[, "upr"])), color = "red", linetype = "dashed", linewidth = 1) +
  xlab(expression("BMI (kg/"*m^2*")")) +
  ylab("Betaplasma (ng/ml)") +
  labs(title = "Plasma Beta-Carotene vs Body Mass Index")

####1(c) - Interpreting coefficients

## BMI increase by 1 unit = decrease of betaplasma by -3.52%
(exp(log$coefficients[2] * 1) - 1) * 100

# CI for 1 unit increase
CI <- confint(log) # extracting confidence intervals from model
CI_b1 <- CI[2,] # extracting confidence interval for bmi
(exp(CI_b1 * 1) - 1) * 100 # since relationship is multiplicative, 1 unit increase is 1 * CI_b1, then exponentiate to original scale and subtract 1 to get percentage change


## BMI decrease by 1 unit = increase of betaplasma by 3.65%
(exp(log$coefficients[2] * -1) - 1) * 100

# CI for 1 unit decrease
(exp(CI_b1 * -1) - 1) * 100


## BMI decrease by 10 units = increase of betaplasma by 43.16%
(exp(log$coefficients[2] * -10) - 1) * 100

# CI for 10 unit decrease

(exp(CI_b1 * -10) - 1) * 100

# Intervals change order since we go from log scale to original scale

####1(d) - Hypothesis test
# t-test
summary(log)
# P = 3.11e-07 < 0.05, i.e H0 rejected, significant linear relationship!

# critical t-value on 5% level
qt(0.975, df = 315-2)

