# ------------------------------
# Code for regression analysis I
# ------------------------------

# __________
# EXERCISE 1
# lung data

#| label: linear-1a-1
#| warning: false
#| echo: true

lung_data <- read.csv('data/PEFH98-english.csv')
# head(lung_data)

# assign variables (not strictly necessary)
pefsit1 <- lung_data$pefsit1
pefsit2 <- lung_data$pefsit2
weight <- lung_data$weight

# 1a) -----

par(mfrow = c(1, 2)) # make plots in 1 row 2 col

# scatter plot: pefsit2 vs pefsit1
plot(x = pefsit1, y = pefsit2, 
     main = 'PEF sit1 vs PEF sit2', 
     xlab = 'pefsit1', ylab = 'pefsit2', 
     pch = 20)
abline(lm(pefsit2 ~ pefsit1, data = lung_data), 
       col = 'blue', lwd = 3)

# scatter plot: pefsit1 vs weight
plot(x = weight, y = pefsit1, 
     main = 'Weight vs pefsit1', 
     xlab = 'Weight', ylab = 'pefsit1', 
     pch = 20)
abline(lm(pefsit1 ~ weight, data = lung_data), 
       col = 'blue', lwd = 3)

# 1b) -----

#| label: linear-1b-1
#| warning: false
#| echo: true

cor(pefsit2, pefsit1) # need to remove NA here

which(is.na(pefsit1)) # no missing
which(is.na(pefsit2)) # 66th missing

# option 1: cor() removes NA for you 
# specify use complete observations
cor(pefsit2, pefsit1, use = 'pairwise.complete.obs')

 
# option 2: you process (remove) the row of missing 
# from both variables (remove element 66)
pefsit2_narm <- pefsit2[!is.na(pefsit2)]
pefsit1_narm <- pefsit1[!is.na(pefsit2)]

# use pefsit2_narm instead of pefsit2 to compute cor
# should be the same
cor(pefsit2_narm, pefsit1_narm)

#| label: linear-1b-2
#| warning: false
#| echo: true

# pefsit1, weight
cor(pefsit1, weight)

#| label: linear-1b-3
#| warning: false
#| echo: true

# pairwise for multiple pairs 
# use age, height, weight, pefsit1, pefsit2, pefsit3, pefmean
# select a smaller dataset 
lungdata2 <- lung_data[, c('age', 'height', 'weight', 'pefsit1', 
                           'pefsit2', 'pefsit3', 'pefmean')]

head(lungdata2, 3)

# produce correlation matrix for all the variables here
# round(1.2345, digits = 2) gives 1.23
round(cor(lungdata2, use = 'pairwise.complete.obs'), digits = 2)

# 1c) -----

#| label: linear-1c-1
#| warning: false
#| echo: true

# pef2 vs pef 1
lm_pef2_pef1 <- lm(pefsit2 ~ pefsit1, data = lung_data)
summary(lm_pef2_pef1)

#| label: linear-1c-2
#| warning: false
#| echo: true

# pef1 vs weight 
lm_pef1_weight <- lm(pefsit1 ~ weight, data = lung_data)
summary(lm_pef1_weight)

# 1d) -----

#| label: linear-2d-1
#| warning: false
#| echo: true

par(mfrow = c(2, 2)) # plot 2 by 2
plot(lm_pef2_pef1)

plot(lm_pef1_weight)


# __________
# EXERCISE 2
# blood pressure

#| label: linear-2a-1
#| warning: false
#| echo: true

# load data
bp <- read.csv('data/bp.csv')
head(bp)

# 2a) -----

#| label: linear-2a-2
#| warning: false
#| echo: true

# correlation age vs bp
cor(bp$Age, bp$bloodpressure)

# or,
cor(bp)

# 95% CI, p-value
cor.test(bp$Age, bp$bloodpressure) 

# 2b) -----

#| label: linear-2b-1
#| warning: false
#| echo: true

# fit a linear regression model
model_age_bp <- lm(bloodpressure ~ Age, data = bp)
summary(model_age_bp)

# to predict (insert x), you need to put data in a data frame
predict(model_age_bp, 
        newdata = data.frame(Age = c(40, 75)), 
        interval = 'prediction')

#| label: linear-2b-2
#| warning: false
#| echo: true

plot(x = bp$Age, y = bp$bloodpressure, 
     main = 'Age versus Blood Pressure', 
     xlab = 'Age', ylab = 'Blood pressure', 
     pch = 20, 
     xlim = c(15, 80))
# add the regression line on top
abline(lm(bloodpressure ~ Age, data = bp), 
       col = 'blue', lwd = 3)

# residual plots
plot(lm(bloodpressure ~ Age, data = bp))

