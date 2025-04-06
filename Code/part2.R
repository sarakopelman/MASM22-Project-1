source("Code/load_data.R")
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
ggplot(carotene, aes(x = smokstat, y = betaplasma, color = smokstat)) +
  geom_boxplot() +
  labs(x = "Smoking status", y="Plasma β-carotene (ng/ml)",
       color = "Smoking Status")

ggplot(carotene, aes(x = smokstat, y = log(betaplasma), color = smokstat)) +
  geom_boxplot() +
  labs(x = "Smoking status", y="Plasma β-carotene (ng/ml)",
       color = "Smoking Status")
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
summary(model_2b)
