source("Code/part3.R")
library(car)
library(rstatix)
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
