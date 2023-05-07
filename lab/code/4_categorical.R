# categorical data analysis 

# _______________ ----
# exercise 1: lung function ----

# 1a) ----
# create variable 'highpef' for pefmean >=500

lung_data <- read.csv('./lab/data/PEFH98-english.csv')



# examine variable pefmean
# visualise the distribution
hist(lung_data$pefmean)
abline(v = 500, col = 'red', lwd = 2)

# option 1
# code new variable: highpef with cutoff = 500
# this is a binary variable, 1 means yes (higher than 500), 0 means no
highpef <- ifelse(lung_data$pefmean > 500, '1', '0')

# check if it makes sense
head(lung_data$pefmean)
head(highpef)

# option 2 
# create a vector with all '0's
# then set the elements above 500 in 'pefmean' as '1'
# (replacement with index)

n <- length(lung_data$pefmean) # 106
highpef_alt <- rep('0', n) # repeat '0' 106 times
highpef_alt[lung_data$pefmean > 500] <- '1'
head(highpef_alt)




# 1b ----
# make a table for pefmean>500 vs gender
# take out gender variable
gender <- lung_data$gender

# we can count how many subjects are in each category for the two variables
# put in two tables 
table(gender)
table(highpef)

# now we count how many subjects are in the combinations 

table_gender_highpef <- table(gender, highpef)
table_gender_highpef


# 1c ----
# calculate risk ratio and odds ratio (point, no CI)
# outcome: high pef (1); reference: 0
# exposure: gender = male; reference: female 

# first use formula (only point estimates)
# risk ratio (relative risk)
# risk in exposed / risk in unexposed

risk_exposed <- 46/(46+6)  # 0.885
risk_unexposed <- 5/(5+48) # 0.094
rr <- risk_exposed/risk_unexposed
rr  # 9.37

# odds ratio
# odds of event in exposed group / odds of event in non-exposed group
odds_exposed <- 46/6
odds_unexposed <- 5/48
or <- odds_exposed/odds_unexposed
or # 73.6


# 1d ----
# association between highpef and gender
# ?chisq.test

chisq.test(table_gender_highpef)

# note: 
# we have not distinguished between exposure and outcome
# by default, 'table' function puts the first input in rows (gender)
# the order does not affect the result of chi-square test

# reverse
table_highpef_gender <- table(highpef, gender)
chisq.test(table_highpef_gender)





# optional ----
# we can verify using packages

# install.packages('epitools')

# match the expected data format
# col: outcome -, outcome +
# row: exposure -, exposure +
tb1 <- matrix(c(48, 5, 6, 46), byrow = T, ncol = 2)
tb1
epitools::epitab(tb, method = 'riskratio')
epitools::epitab(tb, method = 'oddsratio')



# install.packages('epiR')

# match the expected data format
# col: outcome +, outcome -
# row: exposure +, exposure -
tb2 <- matrix(c(46, 6, 5, 48), byrow = T, ncol = 2)
tb2
epiR::epi.2by2(tb2, method = 'cohort.count')


# ADVANCED
# verify with logistic regression for odds ratio
# glmmmm <- glm(as.factor(highpef) ~ gender, family = 'binomial')
# summary(glmmmm)
# exp(glmmmm$coefficients)




# ____________ ----
# exercise 2: birth ----

# 2a) ----
# bwt (continuous) vs smk (categorical)

# load data
birth <- read.csv('./lab/data/birth.csv')
head(birth)

# take the variables
birthweight <- birth$bwt
smoker <- birth$smk

# get to know how many smoker and non-smoker
table(smoker)

# histogram for all (both categories together)
hist(birthweight)

# separate birthweight based on smoking
bw_smoker <- birthweight[smoker == 'smoker']
bw_nonsmoker <- birthweight[smoker == 'nonsmoker']

# histogram for smokers
hist(bw_smoker)
hist(bw_nonsmoker)

# boxplot 
boxplot(bw_smoker)
boxplot(bw_nonsmoker)

# can produce summary statistics too 
summary(bw_smoker)
summary(bw_nonsmoker)


# 2b) ----
# check assumption of normality

qqnorm(bw_smoker)
qqline(bw_smoker)

qqnorm(bw_nonsmoker)
qqline(bw_nonsmoker)

# two samples t-test 
t.test(bw_smoker, bw_nonsmoker, paired = F) # welch
t.test(bw_smoker, bw_nonsmoker, paired = F, var.equal = T)



# 2c) ----
# hypertension vs low birth weight

# take out the variables
head(birth)
hypertension <- birth$ht


# separate birthweight based on hypertension
bw_hypertension <- birthweight[hypertension == 'yes']
bw_nohypertension <- birthweight[hypertension == 'no']

hist(bw_hypertension)
hist(bw_nohypertension)

# boxplot 
boxplot(bw_hypertension)
boxplot(bw_nohypertension)

# can produce summary statistics too 
summary(bw_hypertension)
summary(bw_nohypertension)


# check normality
qqnorm(bw_hypertension)
qqline(bw_hypertension)

qqnorm(bw_nohypertension)
qqline(bw_nohypertension)

# two samples t-test 
t.test(bw_hypertension, bw_nohypertension, paired = F) # welch
t.test(bw_hypertension, bw_nohypertension, paired = F, var.equal = T)

sd(bw_hypertension)
sd(bw_nohypertension)



# 2d) ----
# smoking on birth weight
# distinguish case/non case, exposed/unexposed!
low_bw <- birth$low
table(low_bw)

tb_bw_smk <- table(low_bw, smoker)
chisq.test(tb_bw_smk)

# hypertension

tb_bw_ht <- table(low_bw, hypertension)
chisq.test(tb_bw_ht)

# can specify the computation: 
chisq.test(tb_bw_ht, correct = F) # remove the continuity correction
chisq.test(tb_bw_ht, simulate.p.value = T) # simulate p values




