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

ci_lower <- 12.162 - 2.09 * 1.779/sqrt(20)
ci_upper <- 12.162 + 2.09 * 1.779/sqrt(20)
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














