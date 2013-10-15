# Rimuovi tutto (clear-clc in matlab)
rm(list=ls()) 

# Load library
library(foreign)
library(lmtest)
library(sandwich)

# Source script
source("rob.R")

# Set the directory
setwd("/Users/federicaromei/Dropbox/ase/New_Practice_2013_R/Practice_03_Dummy_Variables_and_Multiple_Regression/R_file")
getwd()

# Upload the dataset
openness <- read.dta('openness.dta')
# clear from missing data
openness  <- na.omit(openness)

# Works with the variables
attach(openness)

Delta_inf= (mean(inf[oil==1] )-mean(inf[oil==0])) /
  sqrt(sd(inf[oil==1])^2/length(inf[oil==1]) +sd(inf[oil==0])^2/length(inf[oil==0]))

# Dummy Regression
dum <- lm(inf~oil)
rob(dum,0.05)

# Rimuovi tutto (clear-clc in matlab)
rm(list=ls()) 

source("rob.R")

# Upload the dataset
crime <- read.dta('Crime.dta')
# clear from missing data
crime <- na.omit(crime)

crim <- lm(narr86~pcnv+ptime86 +qemp86, data = crime)
rob(crim,0.05)
crim2 <- lm(narr86~pcnv+ptime86 +qemp86+avgsen, data = crime)
rob(crim2,0.05)
