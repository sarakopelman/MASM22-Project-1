## ------------------------------------------------------
library(readxl)
library(tidyverse)
library(magrittr)

data <- read_excel("../Data/carotene.xlsx")
head(data)
summary(data)


## ------------------------------------------------------
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


## ------------------------------------------------------
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


## ------------------------------------------------------
library(ggplot2)
library(patchwork)

# Assuming 'data' is your data frame
p1 <- data %>% ggplot(aes(x = fat, y = calories)) +
  geom_point() +
  ggtitle("Fat vs Calories")

p2 <- data %>% ggplot(aes(x = cholesterol, y = calories)) +
  geom_point() +
  ggtitle("Cholesterol vs Calories")

p3 <- data %>% ggplot(aes(x = cholesterol, y = fat)) +
  geom_point() +
  ggtitle("Cholesterol vs Fat")

# Combine using patchwork
combined_plot <- p1 |  (p2 / p3)  

# Display the combined plot
print(combined_plot)


## ------------------------------------------------------
data %>% ggplot(aes(x = alcohol, y = calories)) +
  geom_point()


## ------------------------------------------------------
library(performance)

model <- lm(log(betaplasma) ~ ., data = data)
summary(model)

performance::check_collinearity(model)


## ------------------------------------------------------
model <- lm(log(betaplasma) ~ . - calories, data = data)
summary(model)
performance::check_collinearity(model)


## ------------------------------------------------------
# Log-parameters
results <- as.data.frame(cbind(
  model$coefficients,
  confint(model)
))
colnames(results) <- c("Estimate", "2.5 %", "97.5 %")

print(results)
print(exp(results))


## ------------------------------------------------------
summary(model)


## ------------------------------------------------------
red_model = lm(log(betaplasma) ~ bmi, data = data)
summary(red_model)

anova(red_model, model)


## ------------------------------------------------------
data$smokstat %<>%
  factor %>%
  fct_recode(never = "1", 
             former = "2",
             current = "3") %>%
  relevel(ref = "never")

red_model = lm(log(betaplasma) ~ smokstat, data = data)
summary(red_model)

anova(red_model, model)


## ------------------------------------------------------
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


## ------------------------------------------------------
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


## ------------------------------------------------------
pred %>% filter(v > 0.8)


## ------------------------------------------------------
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


## ------------------------------------------------------
pred %>% filter(D > 0.1)


## ------------------------------------------------------
library(ggplot2)
library(dplyr)

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

