source("Code/part4.R")
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
