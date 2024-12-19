################################
# Lab05_Section4_MarshallRei.R
#
# version: final
################################

# close all, clear all
rm(list=ls())
graphics.off()

# set the working directory
setwd("/Users/reisplace/Desktop/YaleBiostats/Datasets")

# read-in data from file
noaa = read.csv("NOAA_LongFormat.csv")
# 12000 rows, 4 columns
noaa = na.omit(noaa)
# 11771 rows, 4 columns

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Normality 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
shapiro.test(noaa$Temperature)
# yields error due to sample size
s = moments::skewness(noaa$Temperature)
# yields -0.74 (beyond |s|=0.3)
hist(noaa$Temperature,20)
# yields a left-skew distribution
qqnorm(noaa$Temperature)
# yields a non-linear plot

print(paste("skewness of temperature = ",round(s,3),", i.e. Non-normal"))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. ANOVA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# check normality via skewness
db = doBy::summaryBy(Temperature ~ City, data=noaa,FUN=moments::skewness)
n_normal = sum(abs(db[,2])<0.3)
print(paste(n_normal,"normally-distributed cities out of ",length(db[,2])))
# yields 9 out of 10 normally-distrbuted city datasets

# check normality via shapiro-wilk
db = doBy::summaryBy(Temperature ~ City, data=noaa,FUN=shapiro.test)
n_normal = sum(db[,3]>0.05)
print(paste(n_normal,"normally-distributed cities out of ",length(db[,3])))
# yields 0 out of 10 normally-distrbuted city datasets

# anova
anova_test = aov(Temperature ~ City, data=noaa)
p_val = summary(anova_test)[[1]][1,5]
print(paste("There is a statistically significant difference between cities; P=",p_val))
# p-value computer zero. statistically significant 

#post-hoc review
#TukeyHSD(anova_test)
#boxplot(Temperature~City,data=noaa)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Chi-Square
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# subset on city
noaa = subset(noaa, subset=City %in% c("LAX","LAS"))
# 2300 rows, 4 columns

# subset to autumn
noaa = subset(noaa,subset=Month%in%c(9,10,11))

# dichotomize on temperature
noaa$Feeling = noaa$Temperature>65

# create a two-way table
cont_tbl = table(noaa$City,noaa$Feeling)
colnames(cont_tbl) = c("cold","warm")

# show table
cont_tbl

# chi-square test
chisq.test(cont_tbl,correct=TRUE)
cstp = chisq.test(cont_tbl,correct=TRUE)$p.value
print(paste("chi-square test p-value =",cstp))
# yields p=2.5E-5

# via epitools
epitools::oddsratio(cont_tbl,correct=TRUE)
orp = epitools::oddsratio(cont_tbl,correct=TRUE)$p.value[2,3]
print(paste("odds ratio test p-value =",orp))
# yields p=2.5E-5



# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Stratification
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# stratify Year into Eras
noaa$Era = cut(noaa$Year,
									breaks=c(0,1949,1999,3000),
									labels=c("Early","Mid","Late"))

# create a three-way table
cont_tbl = table(noaa$City,noaa$Feeling,noaa$Era)
colnames(cont_tbl) = c("cold","warm")

# show table
cont_tbl

# cochrane-mantel-haenszel test
mantelhaen.test(cont_tbl)
cmhp = mantelhaen.test(cont_tbl,correct=TRUE)$p.value
print(paste("chi-square test p-value =",cmhp))
# yields p = 2.5E-5



# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. Logistic Regression
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# logistic regression
log_mod = glm(Feeling~City,data=noaa,family="binomial")
summary(log_mod)

### log-reg
or = round(exp(coef(log_mod)[2]),3)
print(paste("odds ratio = ", or))
print("more likely to be warm in LAS than LAX")
# yields OR = 2.16


