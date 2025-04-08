source("Code/part4.R")
model4b <- stepmodel_reduced
keep <- c("model4b","newdata")
rm(list=setdiff(ls(),keep))
