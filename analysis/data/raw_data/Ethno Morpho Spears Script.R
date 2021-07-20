###################
###Load packages###
###################

library(dplyr)
library(tidyverse)
library(moments)
library(Hmisc)
library(GGally)


#####################
###Import dataset###
#####################

ds<-Ethno_Spears_3

#######################
###Editing Variables###
#######################

## change variables "Delivery" and "PoB_Group" to factor variables

ds$Delivery <- as.factor(ds$Delivery)
ds$PoB_Group <- as.factor(ds$PoB_Group)

## 

#######################
###Descriptive stats###
#######################

# Select variables for descriptive statistics

ds_numeric <- ds %>% select(8:13, 15:24)

# Conduct summary statistics

summary(ds, na.rm = TRUE)

# Calculate Standard Deviation for the following variables: Length, Mass, DIA 10, DIA 100, DIA 150, DIA 200, DIA 250, MAX DIA, DIA MID, LMB, PoB

sd(ds$Length, na.rm = TRUE)
sd(ds$Mass, na.rm = TRUE)
sd(ds$DIA_10, na.rm = TRUE)
sd(ds$DIA_100, na.rm = TRUE)
sd(ds$DIA_150, na.rm = TRUE)
sd(ds$DIA_200, na.rm = TRUE)
sd(ds$DIA_250, na.rm = TRUE)
sd(ds$MaxDIA, na.rm = TRUE)
sd(ds$DIA_Mid, na.rm = TRUE)
sd(ds$LMD, na.rm = TRUE)
sd(ds$PoB, na.rm = TRUE)

# Calculate Coefficient of Variation for the following variables: Length, Mass, DIA 10, DIA 100, DIA 150, DIA 200, DIA 250, MAX DIA, DIA MID, LMB, PoB

cv_Length <- sd(ds$Length) / mean(ds$Length) * 100
cv_Mass <- sd(ds$Mass) / mean(ds$Mass) * 100
cv_DIA_10 <- sd(ds$DIA_10,na.rm = TRUE) / mean(ds$DIA_10, na.rm = TRUE) * 100_
cv_DIA_100 <- sd(ds$DIA_100,na.rm = TRUE) / mean(ds$DIA_100, na.rm = TRUE) * 100
cv_DIA_150 <- sd(ds$DIA_150,na.rm = TRUE) / mean(ds$DIA_150, na.rm = TRUE) * 100
cv_DIA_200 <- sd(ds$DIA_200) / mean(ds$DIA_200) * 100
cv_DIA_250 <- sd(ds$DIA_250) / mean(ds$DIA_250) * 100
cv_MaxDIA <- sd(ds$MaxDIA) / mean(ds$MaxDIA) * 100
cv_DIA_Mid <- sd(ds$DIA_Mid) / mean(ds$DIA_Mid) * 100
cv_LMD <- sd(ds$LMD) / mean(ds$LMD) * 100
cv_PoB <- sd(ds$PoB) / mean(ds$PoB) * 100

#######################
####Normality Stats####
#######################

# Calculate Shapiro-Wilk for Length, MaxDIA, Percent_LMD, Percent_PoB, DIA_10, DIA_50, DIA_100, DIA_150, DIA_200, DIA_250, DIA_Mid, DIA_Mass
shapiro.test(ds$Length)
shapiro.test(ds$MaxDIA)
shapiro.test(ds$Percent_LMD)
shapiro.test(ds$Percent_PoB)
shapiro.test(ds$DIA_10)
shapiro.test(ds$DIA_50)
shapiro.test(ds$DIA_100)
shapiro.test(ds$DIA_150)
shapiro.test(ds$DIA_200)
shapiro.test(ds$DIA_250)
shapiro.test(ds$DIA_Mid)
shapiro.test(ds$Mass)

# Make Histograms for Length, Mass, Max_DIA and PoB(%)

ggplot(ds, aes(x=Length)) + geom_histogram(binwidth = 500, colour = "black", fill = "darkmagenta") + theme_classic()+labs(x="Length in mm",y="count")
ggplot(ds, aes(x=Mass)) + geom_histogram(binwidth = 500, colour = "black", fill = "darkmagenta") + theme_classic()+labs(x="Mass in grams",y="count")
ggplot(ds, aes(x=MaxDIA)) + geom_histogram(binwidth = 4, colour = "black", fill = "darkmagenta") + theme_classic()+labs(x="Maximum Diameter in mm",y="count")
ggplot(ds, aes(x=Percent_PoB)) + geom_histogram(binwidth = 3, colour = "black", fill = "darkmagenta") + theme_classic()+labs(x="Point of Balance as % of total length",y="count")

#######################
###CORRELATION STATS###
#######################

# This is to make the Spearman's correlation matrix, selected data presented in Table 4
rcorr(as.matrix(ds[8:23], method = "spearman", use = "pairwise"))

#Fig X. Correlation plot for selected measurements

corr_plot <- ds %>% select(8,9,11,13,15,17,19,23,24)

#Fig. X. Scatterplot of PoB (%) and LMD (%)

ggplot(ds, aes(x=Percent_PoB, y=Percent_LMD)) +geom_point(mapping = aes(x = PoB, y = LMD, color = Delivery)) + theme_dark()+labs(x="PoB %",y="LMD %")

ggplot(ds, aes(x=Mass, y=MaxDIA)) + geom_point(mapping = aes(x = PoB, y = LMD, color = Delivery)) + theme_dark()+labs(x="Mass in grams",y="Maximum Diameter in mm")

## Christian I'm struggling w/ the above scatterplots, I cannot understand why this keeps giving the wrong values here, so weird :/ I also think the colours could be better, and I can't figure out how to add labels to the Delivery (1=thrown, 2=thrust, 3=unknown)






