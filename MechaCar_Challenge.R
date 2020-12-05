library(tidyverse)
library(dplyr)
df <- read.csv('MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)
head(df)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = df)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = df))

