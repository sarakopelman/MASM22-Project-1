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
  
