# EXERCISE 1 ----
# birth data
birth_data <- read.csv('./lab/data/birth.csv')
head(birth_data)

# 1a) ----
# low ~ age 
# dependent: low birth weight; 
# independent: age
# be careful with the levels: bwt<= 2500 is the cases (code as 1)

table(birth_data$low)

# recode low variable (numeric 1 and 0)
birth_data$is_low_bwt <- ifelse(birth_data$low == 'bwt <= 2500', 
                                1, 0)

# check each category
table(birth_data$is_low_bwt)

# fit lr
lr_low_age <- glm(is_low_bwt ~ age, 
                  data = birth_data, 
                  family = 'binomial')
summary(lr_low_age)

# double check whether 0/1 categories are correct
table(lr_low_age$y)

# the result regresssion coefficient is not odds ratio
lr_low_age$coefficients

# equivalently,
# coefficients(lr_low_age)
# coef(lr_low_age)
# confidence interval for beta:
# confint(lr_low_age)


# to get odds ratio, exponentiate 
exp(coef(lr_low_age)) # OR
exp(confint(lr_low_age)) # CI for OR


# 1b) -----
# low ~ smk 

lr_low_smk <- glm(is_low_bwt ~ smk, 
                  data = birth_data, 
                  family = 'binomial')
summary(lr_low_smk)

# odds ratio 
exp(coef(lr_low_smk))
exp(confint(lr_low_smk))


# 1c) ----
# low ~ eth 
# ethnicity needs to be factorised
# baseline: white
birth_data$eth <- eth <- factor(birth_data$eth, 
              levels = c('white', 'black', 'other'), 
              labels = c('white', 'black', 'other'))
head(birth_data$eth)

lr_low_eth <- glm(is_low_bwt ~ eth, 
                  data = birth_data, 
                  family = 'binomial')
summary(lr_low_eth)

# odds ratio 
exp(coef(lr_low_eth))
exp(confint(lr_low_eth))




# 1d) -----

lr_low_all <- glm(is_low_bwt ~ age + smk + eth, 
                  data = birth_data, 
                  family = 'binomial')
summary(lr_low_all)

# odds ratio 
exp(coef(lr_low_all))
exp(confint(lr_lr_low_all))






# ___________ ----
# EXERCISE 2 ----

# need to transform firstchild, smoking into categorical

framingham <- read.csv('./lab/data/framingham.csv')
head(framingham)

table(framingham$firstchd)
# evidence is 1, no-evidence is 0

framingham$firstchd_coded <- ifelse(framingham$firstchd == 'evidence', 
                                  1, 0)

# check if correct
table(framingham$firstchd_coded)

# 2a) ----
# firstchd, smoke 

lr_chd_smk <- glm(firstchd_coded ~ smoke, 
                  data = framingham, 
                  family = 'binomial')
# double check if category is correct
table(lr_chd_smk$y)  

summary(lr_chd_smk)

# odds ratio 
exp(coef(lr_chd_smk))
exp(confint(lr_chd_smk))


# 2b) ----
# firstchd, meansbp

lr_chd_bp <- glm(firstchd_coded ~ meansbp, 
                 data = framingham, 
                 family = 'binomial')

summary(lr_chd_bp)

# odds ratio 
exp(coef(lr_chd_bp))
exp(confint(lr_chd_bp))


# 2c) -----
# include the others as well 
lr_chd_all <- glm(firstchd_coded ~ meansbp + smoke + cholesterol + age, 
                  data = framingham, 
                  family = 'binomial')

summary(lr_chd_all)


# odds ratio 
round(exp(coef(lr_chd_all)), digits = 3)
round(exp(confint(lr_chd_all)), digits = 3)




