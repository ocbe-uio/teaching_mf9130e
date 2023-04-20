# categorical data analysis 

# _______________ ----
# exercise 1: lung function ----

# 1a) ----
# create variable 'highpef' for pefmean >=500

lung_data <- haven::read_dta('./lab/data/PEFH98-english.dta')

# recode gender: female, male
lung_data$gender <- factor(lung_data$gender, 
                           levels = c('1','2'),
                           labels = c('female','male')) 
head(lung_data)


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
# association between highpef and gender
# ?chisq.test

# take out gender variable
gender <- lung_data$gender

# we can count how many subjects are in each category for the two variables
# put in two tables 
table(gender)
table(highpef)

# now we count how many subjects are in the combinations 

table_gender_highpef <- table(gender, highpef)
table_gender_highpef

chisq.test(table_gender_highpef)

# note: 
# we have not distinguished between exposure and outcome
# by default, 'table' function puts the first input in rows (gender)
# the order does not affect the result of chi-square test

# reverse
table_highpef_gender <- table(highpef, gender)
chisq.test(table_highpef_gender)



# extension: compute RR, OR ----
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



