#Loading required packages and data:
library(readxl)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(patchwork)
library(performance)
library(dplyr)
library(car)
library(rstatix)
carotene <- read_xlsx("data/carotene.xlsx")

#Part 1
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

##Part 2
#Clearing
keep <- c("carotene")
rm(list=setdiff(ls(),keep))

#create factor
mutate(carotene,
       smokstat = factor(smokstat,
                         levels = c(1,2,3),
                         labels = c("Never", "Former", "Current Smoker"))) -> carotene


#Table
carotene |>
  group_by(smokstat) |>
  summarise(
    count=n(),
    mean_value = mean(betaplasma),
    sd_value = sd(betaplasma)
  ) -> freq_table
print(freq_table)  #Never as reference makes most sense

#Boxplots
boxplot <- ggplot(carotene, aes(x = smokstat, y = betaplasma, color = smokstat)) +
  geom_boxplot() +
  labs(x = "Smoking status", y="Plasma β-carotene (ng/ml)",
       color = "Smoking Status")

boxplot_log <- ggplot(carotene, aes(x = smokstat, y = log(betaplasma), color = smokstat)) +
  geom_boxplot() +
  labs(x = "Smoking status", y="Log Plasma β-carotene (ng/ml)",
       color = "Smoking Status")
combinedbox <- boxplot + boxplot_log
#ggsave("combined_boxplot.png", combinedbox, width = 14, height = 10, dpi = 300)
#Fit models
#Model 2b
model2b <- lm(log(betaplasma)~smokstat,data=carotene)
summary(model2b)
#Alternative model
carotene_relev <- mutate(carotene,smokstat = relevel(smokstat,"Current Smoker"))
model2b_alt <- lm(log(betaplasma)~smokstat,data=carotene_relev)
summary(model2b_alt)

#Expected values and confidence intervals for both models
carotene_x0 <- data.frame(smokstat = c("Never", "Former", "Current Smoker"))
cbind(carotene_x0, fit=predict(model2b,newdata=carotene_x0),
      conf = predict(model2b, newdata = carotene_x0, interval = "confidence")) |>
  mutate(df = NULL,
         residual.scale=NULL,
         conf.fit=NULL) ->
  carotene_x0_pred
carotene_x0_pred
cbind(smokstat=carotene_x0_pred[, 1],exp(carotene_x0_pred[, 2:ncol(carotene_x0_pred)]))

cbind(carotene_x0, fit=predict(model2b_alt,newdata=carotene_x0),
      conf = predict(model2b_alt, newdata = carotene_x0, interval = "confidence")) |>
  mutate(df = NULL,
         residual.scale=NULL,
         conf.fit=NULL) ->
  carotene_x0_pred_alt
carotene_x0_pred_alt
cbind(smokstat=carotene_x0_pred_alt[, 1],exp(carotene_x0_pred_alt[, 2:ncol(carotene_x0_pred_alt)]))

#F-test
summary(model2b)
anova(model2b)

##Part 3
#Clearing
keep <- c("carotene")
rm(list=setdiff(ls(),keep))
data <- carotene

data$sex %<>%
  factor %>%
  fct_recode(male = "1", 
             female = "2") %>%
  relevel(ref = "female")

# Frequency of Sex in Sample
data$sex %>% fct_count(prop=TRUE) %>%
  print()

data$vituse %<>%
  factor %>%
  fct_recode(often = "1", 
             sometimes = "2",
             never = "3") %>%
  relevel(ref = "never")


# Frequency of Vitamin Use in Sample
data$vituse %>% fct_count(prop=TRUE) %>%
  print()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cor_matrix <- data %>% select(-sex, -smokstat, -vituse, -betaplasma) %>%
  cor

indices <- which((abs(cor_matrix) > 0.6 & abs(cor_matrix) < 1) == TRUE, arr.ind = TRUE)

# Get names of covariates with |corr| > 0.6 and remove symetric duplicates
result <- data.frame(
  Variable1 = rownames(cor_matrix)[indices[, 1]],
  Variable2 = colnames(cor_matrix)[indices[, 2]],
  Correlation = cor_matrix[indices]
)
result <- result[!duplicated(t(apply(result, 1, sort))), ]

print(result)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



# Assuming 'data' is your data frame
p1 <- data %>% ggplot(aes(x = fat, y = calories)) +
  geom_point() +
  geom_point(data = filter(data, alcohol>200), color = "magenta") +
  ggtitle("Fat vs Calories")

p2 <- data %>% ggplot(aes(x = cholesterol, y = calories)) +
  geom_point() +
  geom_point(data = filter(data, alcohol>200), color = "magenta") +
  ggtitle("Cholesterol vs Calories")

p3 <- data %>% ggplot(aes(x = cholesterol, y = fat)) +
  geom_point() +
  geom_point(data = filter(data, alcohol>200), color = "magenta") +
  ggtitle("Cholesterol vs Fat")

# Combine using patchwork
combined_plot <- p1 |  (p2 / p3)  

# Display the combined plot
print(combined_plot)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data %>% ggplot(aes(x = alcohol, y = calories)) +
  geom_point()

data %>% filter(alcohol > 200)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


model_bad <- lm(log(betaplasma) ~ ., data = data)
summary(model_bad)

performance::check_collinearity(model_bad)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model <- lm(log(betaplasma) ~ . - calories, data = data)
summary(model)
performance::check_collinearity(model)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
vif_bad <- performance::check_collinearity(model_bad) %>% 
  tibble %>%
  select(Term, VIF) %>%
  mutate(VIF = round(VIF, 2))

vif <- performance::check_collinearity(model) %>% 
  tibble %>%
  select(Term, VIF) %>%
  mutate(VIF = round(VIF, 2))

bind_rows(vif_bad, vif) %>% write.csv('~/Downloads/temp.csv')


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Log-parameters
results <- as.data.frame(cbind(
  model$coefficients,
  confint(model)
))
colnames(results) <- c("beta_Estimate", "beta_2.5 %", "beta_97.5 %")

exp_results <- exp(results)
colnames(exp_results) <- c("exp_Estimate", "exp_2.5 %", "exp_97.5 %")

#cbind(results, exp_results) %>% 
 # round(3) %>%
  #write.csv('~/Downloads/temp.csv')



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(model)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
red_model = lm(log(betaplasma) ~ bmi, data = data)
summary(red_model)

anova(red_model, model)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
red_model = lm(log(betaplasma) ~ smokstat, data = data)
summary(red_model)

anova(red_model, model)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred <- data %>%
  mutate(yhat = predict(model),
         r = rstudent(model))

highlightcolors <- c("|r*|>3" = "red")

p1 <- ggplot(pred, aes(x = yhat, y = r)) +
  geom_point() +
  geom_hline(yintercept = c(-2, 0, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = 2) +
  geom_point(data = filter(pred, abs(r) > 3), 
             aes(color = "|r*|>3"), size = 3) +
  labs(title = "Studentized residuals vs  predictor",
       color = "Highlight") +
  scale_color_manual(values = highlightcolors) +
  theme(legend.position = "bottom")

p2 <- ggplot(pred, aes(x = yhat, y = sqrt(abs(r)))) +
  geom_point() +
  geom_hline(yintercept = c(0, sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = 2) +
  geom_point(data = filter(pred, abs(r) > 3), 
             aes(color = "|r*|>3"), size = 3) +
  labs(title = "Sqrt absolute studentized residuals vs predictor",
       color = "Highlight") +
  scale_color_manual(values = highlightcolors) +
  theme(legend.position = "bottom")

print(p1 | p2)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred <- pred %>%
  mutate(v = hatvalues(model))

# with 1/n and 2(p+1)/n horizontal lines:
# p+1 = 
pplus1 <- length(model$coefficients)
n <- nobs(model)

ggplot(cbind(pred), aes(x = yhat, y = v)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  labs(title = "leverage vs log prediction",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = highlightcolors)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred %>% filter(v > 0.8)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred <- pred %>%
  mutate(D = cooks.distance(model))

f1 <- pplus1
f2 <- model$df.residual
cook.limit <- qf(0.5, f1, f2)

ggplot(pred, aes(yhat, D)) + 
  geom_point(size = 3) +
  geom_point(data = filter(pred, alcohol > 200),
             aes(color = "alcohol>200"), size = 3) +
  geom_hline(yintercept = cook.limit, color = "red") +
  geom_hline(yintercept = 4/n, linetype = 2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Cook's D",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  scale_color_manual(values = highlightcolors)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred %>% filter(D > 0.1)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Define highlight aesthetics
highlightshapes <- c("Cook's D>0.1" = 24)
highlightcolors <- c("alcohol>200" = "magenta", "|r*|>3" = "red", "cholesterol>900" = "blue")

# Extract DFBETAS and rename columns
dfbetas_data <- as.data.frame(dfbetas(model))
colnames(dfbetas_data) <- paste0("dfbeta_", colnames(dfbetas_data))

# Combine model predictions and DFBETAS
plot_data <- cbind(pred, dfbetas_data)

# Calculate thresholds
cook.limit.excl <- 4 / nrow(plot_data)
n_excl <- nrow(plot_data)

# Generate a plot for each parameter automatically
params <- colnames(dfbetas_data)
plots <- lapply(params, function(param) {
  
  # Create a temporary column for the current parameter's DFBETAS
  plot_data$df_val <- plot_data[[param]]
  
  ggplot(plot_data, aes(x = yhat, y = df_val)) +
    geom_point(size = 2) +
    geom_point(data = filter(plot_data, abs(r) > 3),
               aes(color = "|r*|>3"), size = 3) +
    geom_point(data = filter(plot_data, alcohol > 200),
               aes(color = "alcohol>200"), size = 3) +
    geom_point(data = filter(plot_data, cholesterol > 850),
               aes(color = "cholesterol>900"), size = 3) +
    
    geom_point(data = filter(plot_data, D > 0.1),
               aes(shape = "Cook's D>0.1"), size = 3) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = sqrt(cook.limit.excl) * c(-1, 1),
               color = "red") +
    geom_hline(yintercept = 2 / sqrt(n_excl) * c(-1, 1),
               color = "red", linetype = "dashed") +
    ylab(paste0("DFBETAS for ", param)) +
    xlab("Fitted values") +
    labs(title = paste("DFBETAS:", param),
         caption = "Thresholds: y = sqrt(F_0.5) and 2/sqrt(n)") +
    theme(legend.position = "bottom") +
    scale_color_manual(values = highlightcolors) +
    scale_shape_manual(values = highlightshapes)
})

# Print each plot
for (p in plots) {
  print(p)
}

##Part4

#4.a
#remove influential observation
newdata <- filter(data, cholesterol<900)
#reestimate model
model_excl <- update(model, data = newdata)
newpred <- mutate(newdata,
                  yhat = predict(model_excl),
                  D = cooks.distance(model_excl)) 
pplus1 <- length(model_excl$coefficients)
n <- nobs(model_excl)
#Use these limits to begin with
f1 <- pplus1
f2 <- model_excl$df.residual
cook.limit <- qf(0.5,f1,f2)

cook2 <- ggplot(newpred, aes(yhat, D)) +
  geom_point(size =3) +
  geom_hline(yintercept = 4/n,linetype =2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Cook's Distance",
       caption = "y=4/n (dashed)",
       color = "Highlight") +
  scale_color_manual(highlightcolors) #don't need higher limit
cook1 <- cook2 + geom_hline(yintercept = cook.limit, color = "red")
cook1
cook2 #don't need higher limit
#ggsave("p4_cook.png", plot = cook2, width = 8, height = 6, units = "in", dpi = 300)
#Turn factoprs into dummies to calculate correlation
dummy_data <- model.matrix(~ . -1, data = newdata)  # -1 removes the intercept column
dummy_df <- as.data.frame(dummy_data)       
dummy_df %>%
  select(where(~length(unique(na.omit(.))) > 1)) %>%  # remove constant columns
  cor_test() %>%
  filter(var1 < var2) %>%                             # avoid duplicate pairs
  arrange(desc(cor))                                  # sort by correlation (descending)
#calculate VIF
vif(model_excl)
#4.b
##stepwise selection 
#full dataset
null_model <- lm(log(betaplasma)~1,data=data)
full_model <- lm(log(betaplasma)~.,data=data)
stepmodel_full <- step(null_model, 
                       scope = list(lower = null_model, upper = full_model),
                       direction = "both",
                       k = log(nobs(full_model)))

#reduced dataset
null_model <- update(null_model, data=newdata)
full_model <- update(full_model, data=newdata)
stepmodel_reduced <- step(null_model, 
                          scope = list(lower = null_model, upper = full_model),
                          direction = "both",
                          k = log(nobs(full_model)))
##Estimates and confidence intervals
summary(stepmodel_full)
confint(stepmodel_full)
summary(stepmodel_reduced)
confint(stepmodel_reduced)

##Part5
#Clearing
model4b <- stepmodel_reduced
keep <- c("model4b","newdata")
rm(list=setdiff(ls(),keep))

###5(a)

# Testing if we can combine "sometimes" and "often" categories into "user" in categorical x-variable "vituse" in model4b

# Adding new column for new vituse variable
newdata <- newdata |> mutate(
  vituse2 = ifelse(vituse == "never", "never", "user"),
  vituse2 = factor(vituse2, levels = c("never", "user"))  # set reference level
)

# Fitting reduced model
model5a <- lm(log(betaplasma) ~ bmi + fiber + calories + vituse2, data = newdata)

# Testing if reduced model is sufficient
anova(model_5a, model4b)

# Presenting new reduced model
model5a_sum <- summary(model5a)
model5a_sum
confint(model5a)

###5(b)

# Fitting all old models on reduced data
model1b <- lm(log(betaplasma) ~ bmi, data = newdata)
model2b <- lm(log(betaplasma) ~ smokstat, data = newdata)
model3c <- lm(log(betaplasma) ~ . - calories, data = newdata)

# Saving summaries
model1b_sum <- summary(model1b)
model2b_sum <- summary(model2b)
model3c_sum <- summary(model3c)
model4b_sum <- summary(model4b)
model5a_sum <- summary(model5a)

# Number of beta parameters
length(coef(model1b))
length(coef(model2b))
length(coef(model3c))
length(coef(model4b))
length(coef(model5a))

# Residual standard deviation
model1b_sum$sigma
model2b_sum$sigma
model3c_sum$sigma
model4b_sum$sigma
model5a_sum$sigma

# R2
model1b_sum$r.squared
model2b_sum$r.squared
model3c_sum$r.squared
model4b_sum$r.squared
model5a_sum$r.squared

# adjusted R2
model1b_sum$adj.r.squared
model2b_sum$adj.r.squared
model3c_sum$adj.r.squared
model4b_sum$adj.r.squared
model5a_sum$adj.r.squared

# AIC
AIC(model1b)
AIC(model2b)
AIC(model3c)
AIC(model4b)
AIC(model5a)

# BIC
BIC(model1b)
BIC(model2b)
BIC(model3c)
BIC(model4b)
BIC(model5a)

