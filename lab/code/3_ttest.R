# t-test

# exercise 1 (heart) ----

heart <- c(11.5, 14.75, 13.75, 10.5, 14.75,
           13.5, 10.75, 9.5, 11.75, 12, 
           10.5, 11.75, 10, 14.5, 12, 
           11, 14, 15, 11.5, 10.25)

# 1a) ----
# compute the mean
sum_heart <- 11.5 + 14.75 + 13.75 + 10.5 + 14.75 + 
  13.5 + 10.75 + 9.5 + 11.75 + 12 + 
  10.5 + 11.75 + 10 + 14.5 + 12 + 
  11 + 14 + 15 + 11.5 + 10.25

n <- 20 # length(heart)

sum_heart
sum_heart/20

# formula: sum(heart)/length(heart)
mean(heart)  # 12.162


# 1b) ----
# standard deviation, 95% confidence interval

# formula (sd): sqrt(1/(n-1) * (sum(xi - xbar)^2))
# translate into format that R can understand:

n <- 20  # length(heart)
sqrt(1/(n-1) * (sum((heart - mean(heart))^2))) # be careful with brackets

sd(heart) # 1.779


# 95% CI (quantile from t-distribution)
# formula: CI = xbar +- t(n-1, alpha/2) * sd/sqrt(n)
# translate into format R can understand:

t025 <- qt(p = 0.025, df = n-1) # -2.09
t975 <- qt(p = 0.975, df = n-1) # 2.09
c(t025, t975) # symmetric around 0

ci_lower <- mean(heart) - t975 * sd(heart)/sqrt(20)
ci_upper <- mean(heart) + t975 * sd(heart)/sqrt(20)
c(ci_lower, ci_upper)


# verify by doing a one-sample t-test
# by default, it tests whether mean is 0
t.test(heart)


# 1c) ----
# visually check
# histogram
hist(heart, breaks = 10, main = 'Histogram of heart data')
abline(v = mean(heart), col = 'red', lwd = 2)
abline(v = c(ci_lower, ci_upper), col = 'red', lwd = 2,
       lty = 'dashed')

# qqplot 
qqnorm(heart, pch = 20)
qqline(heart, lwd = 2)



# boxplot 
par(mfrow = c(1, 2))
# compare with 11
boxplot(heart, horizontal = T, main = 'Compare with mean = 11')
abline(v = mean(heart), col = 'red', lwd = 3)
abline(v = c(ci_lower, ci_upper), col = 'red', lwd = 2,
       lty = 'dashed')
abline(v = 11, col = 'forestgreen', lwd = 3)


# compare with 11.5
boxplot(heart, horizontal = T, main = 'Compare with mean = 11.5')
abline(v = mean(heart), col = 'red', lwd = 3)
abline(v = c(ci_lower, ci_upper), col = 'red', lwd = 2,
       lty = 'dashed')
abline(v = 11.5, col = 'forestgreen', lwd = 3)



# 1d) ----
# H0: mu = 11; H1: mu != 11
t.test(heart, mu = 11, conf.level = 0.95)


# H0: mu = 11.5; H1: mu != 11.5
t.test(heart, mu = 11.5, conf.level = 0.95)




# ___________ ----
# exercise 2 (lung function) ----

lung_data <- read.csv('./lab/data/PEFH98-english.csv')

head(lung_data)


# 2a) ----
# mean height (standard deviation, 95% ci) for male, female
head(lung_data[, c('height', 'gender')], 10)

gender <- lung_data$gender

# height for female
height_f <- lung_data$height[gender == 'female']
# number of females
nf <- length(height_f)  # 54

mean(height_f) # 169.57
sd(height_f) # 5.69
# se_f <- sd(height_f)/sqrt(54) 0.774


# histogram for height_f
hist(height_f, main = 'Histogram of height (female)', xlab = 'Height (cm)')
abline(v = mean(height_f), lwd = 4, col = 'red')
abline(v = 167, lwd = 4, col = 'forestgreen')

# quantile for t distribution 
t975 <- qt(p = 0.975, df = nf-1) # 2.005

# 95% CI 
ci_upper_f <- mean(height_f) + t975 * sd(height_f)/sqrt(nf) # 171.1277
ci_lower_f <- mean(height_f) - t975 * sd(height_f)/sqrt(nf) # 168.0204
c(ci_lower_f, ci_upper_f)

# add CI on top of the plot  
abline(v = ci_lower_f, lwd = 2, col = 'red', lty = 'dashed')
abline(v = ci_upper_f, lwd = 2, col = 'red', lty = 'dashed')

# if my xbar is normally distributed,  
# why is t.test returning a CI based on t distribution?



# height for male 
height_m <- lung_data$height[gender == 'male']
# number of males
nm <- length(height_m)  # 52


mean(height_m) # 181.87
sd(height_m) # 5.67
# se_m <- sd(height_m)/sqrt(52) # 0.786

# find quantile for males (pay attention to df)
t975 <- qt(p = 0.975, df = nm-1) # 2.007
# 95% CI
ci_upper_m <- mean(height_m) + t975 * sd(height_m)/sqrt(nm) # 183.44
ci_lower_m <- mean(height_m) - t975 * sd(height_m)/sqrt(nm)  # 180.28
c(ci_lower_m, ci_upper_m)

# verify by t.test
t.test(height_m)

# 2b) ----

t.test(height_f, mu = 167)

# calculate by hand
t_stat <- (mean(height_f) - 167)/(sd(height_f)/sqrt(nf))
t_stat  # 3.323

# compare this with t distribution with nf-1 degrees of freedom
pval_twosided <- pt(q = t_stat, df = nf-1, lower.tail = F)*2
pval_twosided




# 2c) ----
# compare pefsit1, pefsit2 (paired t-test)

pefsit1 <- lung_data$pefsit1
pefsit2 <- lung_data$pefsit2
# hist(pefsit1)
# hist(pefsit2)

t.test(pefsit1, pefsit2, paired = T)

# alternatively, you can test whether the difference is equal to 0
diff_sit1_sit2 <- pefsit1 - pefsit2
t.test(diff_sit1_sit2, mu = 0)



# 2d) ----
# compare pefsitm, pefstam (paired t-test)
pefsitm <- lung_data$pefsitm
pefstam <- lung_data$pefstam


t.test(pefsitm, pefstam, paired = T)




# 2e) -----
# check assumption 

diff_sitm_stam <- pefsitm - pefstam
qqnorm(diff_sitm_stam, pch = 20)
qqline(diff_sitm_stam, col = 'red', lwd = 2)


# 2f) ----
# compare pefmean for two genders
# this is independent two samples t-test
# we need two variables: pefmean for men, pefmean for women

pefmean_f <- lung_data$pefmean[gender == 'female']
pefmean_m <- lung_data$pefmean[gender == 'male']

# you can visually spot whether there is a difference
# par(mfrow = c(1, 2))
hist(pefmean_f, main = 'pefmean (F)')
abline(v = mean(pefmean_f, na.rm = T), col = 'red', lwd = 2)
hist(pefmean_m, main = 'pefmean (M)')
abline(v = mean(pefmean_m), col = 'red', lwd = 2)

# two sample t-test 
t.test(pefmean_f, pefmean_m, paired = F)


# 2g) ----

# par(mfrow = c(1, 2))
qqnorm(pefmean_f, pch = 20, main = 'Q-Q plot: pefmean (F)')
qqline(pefmean_f, col = 'red', lwd = 2)

qqnorm(pefmean_m, pch = 20, main = 'Q-Q plot: pefmean (M)')
qqline(pefmean_m, col = 'red', lwd = 2)






