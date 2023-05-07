# code for regression analysis I, II

# EXERCISE 1 ----
# blood pressure 

bp <- read.csv('./lab/data/bp.csv')
head(bp)

# 1a) ----

# correlation age vs bp
cor(bp$Age, bp$bloodpressure)

# or,
cor(bp)

# 95% CI, p-value
cor.test(bp$Age, bp$bloodpressure) 


# 1b) ----

# fit a linear regression model
model_age_bp <- lm(bloodpressure ~ Age, data = bp)
summary(model_age_bp)

# to predict (insert x), you need to put data in a data frame
predict(model_age_bp, 
        newdata = data.frame(Age = c(40, 75)), 
        interval = 'prediction')



# visually, 
plot(x = bp$Age, y = bp$bloodpressure, 
     main = 'Age versus Blood Pressure', 
     xlab = 'Age', ylab = 'Blood pressure', 
     pch = 20, 
     xlim = c(15, 80))
abline(lm(bloodpressure ~ Age, data = bp), 
       col = 'blue', lwd = 3)




# __________ ----
# EXERCISE 2 ----
# lung data 

lung_data <- read.csv('./lab/data/PEFH98-english.csv')
head(lung_data)


# assign variables 
pefsit1 <- lung_data$pefsit1
pefsit2 <- lung_data$pefsit2
weight <- lung_data$weight



# 2a) -----

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




# 2b) ----

cor(pefsit2, pefsit1) # need to remove NA here

which(is.na(pefsit1)) # no missing
which(is.na(pefsit2)) # 66th missing

# option 1: cor() removes NA for you 
# specify use complete observations
cor(pefsit2, pefsit1, use = 'complete.obs')

 
# option 2: you process (remove) the row of missing 
# from both variables (remove element 66)
pefsit2_narm <- pefsit2[!is.na(pefsit2)]
pefsit1_narm <- pefsit1[!is.na(pefsit2)]

# use pefsit2_narm instead of pefsit2 to compute cor
# should be the same
cor(pefsit2_narm, pefsit1_narm)



# pefsit1, weight
cor(pefsit1, weight)


# pairwise for multiple pairs 
# use age, height, weight, pefsit1, pefsit2, pefsit3, pefmean
# select a smaller dataset 
lungdata2 <- lung_data[, c('age', 'height', 'weight', 'pefsit1', 
                           'pefsit2', 'pefsit3', 'pefmean')]

head(lungdata2, 3)

# produce correlation matrix for all the variables here
round(cor(lungdata2, use = 'complete.obs'), digits = 2)
# round(1.2345, digits = 2) gives 1.23


# 2c) ----

# pef2 vs pef 1
lm_pef2_pef1 <- lm(pefsit2 ~ pefsit1, data = lung_data)
summary(lm_pef2_pef1)



# pef1 vs weight 
lm_pef1_weight <- lm(pefsit1 ~ weight, data = lung_data)
summary(lm_pef1_weight)


# 2d) ----
# residual analysis
par(mfrow = c(2, 2)) # plot 2 by 2
plot(lm_pef2_pef1)

plot(lm_pef1_weight)


# 2e) ----
# multivariate 

# pefsit1 vs (weight, gender)
# note that we converted gender into categorical
lm_pef1_weight_gender <- lm(pefsit1 ~ weight + gender, 
                            data = lung_data)
# lm_pef1_weight_gender
summary(lm_pef1_weight_gender)


# 2f) ----
# we can try two sets
# 1. height weight gender
lm_pefm_height_weight_gen <- lm(pefmean ~ height + weight + gender, 
                                data = lung_data)

summary(lm_pefm_height_weight_gen)

# 2. weight gender
lm_pefm_weight_gen <- lm(pefmean ~ weight + gender, 
                                data = lung_data)

summary(lm_pefm_weight_gen)



# __________ ----
# EXERCISE 3 ----

# liggetid data

liggetid <- read.csv('./lab/data/liggetid.csv')
head(liggetid)

# 3a) ----
# boxplot for men and women: length of stay (liggetid)

boxplot(liggetid ~ kjoenn, data = liggetid)

# 3b) ----
# response (dep): liggetid
# predictor (indep): kjoenn, slag

lm_ligge <- lm(liggetid ~ slag + kjoenn, 
               data = liggetid)
summary(lm_ligge)

# visualize the residual
par(mfrow = c(2, 2))
plot(lm_ligge)

# 3c) ----
# response (dep): log transformed (lnliggti)
# predictor (indep): kjoenn, slag

lm_logligge <- lm(lnliggti ~ slag + kjoenn, 
                  data = liggetid)
summary(lm_logligge)

par(mfrow = c(2, 2))
plot(lm_logligge)









