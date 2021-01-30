#### -- LLSR data Update ####
library(LLSR)
path <- "E:/Projetos/GitHub/LLSR/dev/data.xlsx"
llsr_data <- AQSysDB(path)
setwd("data")
save(llsr_data, file = "llsr_data.rda")
#### -- End LLSR data Update ####
