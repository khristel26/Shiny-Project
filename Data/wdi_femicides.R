# World Bank femicides data

#install.packages("WDI")
library(WDI)

setwd("D:\\DAP 2\\final-project-diego_khristel")

femicides = WDI(indicator='VC.IHR.PSRC.FE.P5', country=c('MX','PE'), start=2011, end=2020)

write.csv(femicides, "femicides_mexicoperu.csv")
