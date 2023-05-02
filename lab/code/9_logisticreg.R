
# LOGISTIC REGRESSION ----

# ____ birth ____ ----
birth_data <- haven::read_dta('./lab/data/birth.dta')
birth_data

# low ~ age ----
# 1. dependent: low; independent: age

lr_low_age <- glm(low ~ age, data = birth_data, family = 'binomial')
summary(lr_low_age)

# the result regresssion coefficient is not odds ratio
lr_low_age$coefficients

coefficients(lr_low_age)
coef(lr_low_age)
confint(lr_low_age)


# to get odds ratio, exponentiate 

exp(coef(lr_low_age))
exp(confint(lr_low_age))



# low ~ smk ----

lr_low_smk <- glm(low ~ smk, data = birth_data, family = 'binomial')
summary(lr_low_smk)

# odds ratio 
exp(coef(lr_low_smk))
exp(confint(lr_low_smk))


# low ~ eth ----
# ethnicity needs to be factorised
birth_data$eth

# lr_low_eth <- glm(low ~ eth, data = birth_data, family = 'binomial')
# summary(lr_low_eth)


# multivariate ----

# ______ framingham _____ ----

# need to transform firstchild, smoking into categorical

framingham <- haven::read_dta('./lab/data/framingham.dta')
framingham

summary(framingham)










