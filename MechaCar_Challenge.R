#DELIVERABLE1
library(tidyverse)
library(dplyr)
df <- read.csv('MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)
head(df)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = df)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = df))

#DELIVERABLE2
table_sc <- read.csv('Suspension_Coil.csv', check.names = F, stringsAsFactors = F)
head(table_sc)
total_summary <- table_sc %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
lot_summary <- table_sc %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups='keep')

#DELIVERABLE3
?t.test() 
head(table_sc)
t.test(table_sc$PSI, mu = 1499)

lot1 <- subset(table_sc, Manufacturing_Lot=='Lot1')
t.test(lot1$PSI, mu = 1499)

lot2 <- subset(table_sc, Manufacturing_Lot=='Lot2')
t.test(lot2$PSI, mu = 1499)

lot3 <- subset(table_sc, Manufacturing_Lot=='Lot3')
t.test(lot3$PSI, mu = 1499)
