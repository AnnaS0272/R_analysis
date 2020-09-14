library(tidyverse)
MechaCar <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F) #read in dataset
MechaCarMR <- lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar) #generate multiple linear regression model
summary(MechaCarMR)

#perform summary analysis on suspension coil
CoilAnalysis <- read.csv('Suspension_Coil.csv', stringsAsFactors = F) #read in dataset

CoilSummaryByLot <- CoilAnalysis %>% group_by(Manufacturing_Lot) %>% 
  summarise(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI)) # statistical summary grouped by lot

#Total Summary without grouping
CoilSummaryAll <- CoilAnalysis %>% summarise(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

#Create a sample population using subset function
t.test(subset(CoilAnalysis, Manufacturing_Lot=='Lot1') $PSI, mu=1500)
t.test(subset(CoilAnalysis, Manufacturing_Lot=='Lot2') $PSI, mu=1500)
t.test(subset(CoilAnalysis, Manufacturing_Lot=='Lot3') $PSI, mu=1500)

#Create a sample population for all lots using subset function
t.test(CoilAnalysis $PSI, mu=1500)
