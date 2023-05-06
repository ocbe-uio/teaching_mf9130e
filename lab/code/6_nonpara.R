# non-parametric tests

# EXERCISE 1 ----

lung_data <- read.csv('./lab/data/PEFH98-english.csv')
head(lung_data)

# 1a) ----
# compute difference betweenn pefstam and pefsitm
# the order does not matter

diff_pef <- lung_data$pefstam - lung_data$pefsitm

# 1b) ----
# histogram

hist(diff_pef)

# qqplot 
qqnorm(diff_pef)
qqline(diff_pef)


# 1c) ----
# one sample (signed rank test)
wilcox.test(diff_pef)

# alternatively
pefstam <- lung_data$pefstam
pefsitm <- lung_data$pefsitm

wilcox.test(pefstam, pefsitm, paired = T)

# the data is not severly skewed, can also use t-test 
t.test(pefstam, pefsitm, paired = T)



# __________ -----
# EXERCISE 2 ----

liggetid <- read.csv('./lab/data/liggetid.csv')
head(liggetid)



# 2a) -----

# liggetid (length of stay: los)
los <- liggetid$liggetid
stroke <- liggetid$slag

summary(los)

table(stroke)
summary(stroke) # this shows 85 missing
sum(is.na(stroke)) # 85


# visualize length of hospital stay (los)

hist(los, breaks = 25)
qqnorm(los, pch = 20, main = 'Q-Q plot for length of stay')
qqline(los, col = 'red', lwd = 2)


# 2b) ----
# separate los 


ligt_s1 <- los[which(!is.na(stroke) & stroke == 1)]
ligt_s0 <- los[which(!is.na(stroke) & stroke == 0)]

# mean, median 
c(mean(ligt_s1), median(ligt_s1))
c(mean(ligt_s0), median(ligt_s0))


# visualize
par(mfrow = c(2,2))

# s1
hist(ligt_s1, main = 'Length of hospital stay, stroke yes', 
     xlab = 'mean: 145; median: 58', 
     xlim = c(0, 1300))
abline(v = mean(ligt_s1), col = 'blue', lwd = 2)
abline(v = median(ligt_s1), col = 'blue', lwd = 2, lty = 'dashed')
# qqplot
qqnorm(ligt_s1, pch = 20, main = 'Q-Q plot for length of stay, stroke yes')
qqline(ligt_s1, col = 'red', lwd = 2)

# s0
hist(ligt_s0, main = 'Length of hospital stay, stroke no', 
     xlab = 'mean: 116; median: 40', 
     xlim = c(0, 1300))
abline(v = mean(ligt_s0), col = 'blue', lwd = 2)
abline(v = median(ligt_s0), col = 'blue', lwd = 2, lty = 'dashed')
# qqplot
qqnorm(ligt_s0, pch = 20, main = 'Q-Q plot for length of stay, stroke no')
qqline(ligt_s0, col = 'red', lwd = 2)


# non parametric test
# not matched; independent samples
# wilcoxon rank sum 
wilcox.test(ligt_s1, ligt_s0)



# 2c) -----

# if you do a t-test, you can get different results
# t.test(ligt_s1, ligt_s0)

# use log transformed 
loglos <- liggetid$lnliggti
hist(loglos, main = 'Log transformed length of stay')
qqnorm(loglos, pch = 20, main = 'Q-Q plot for log(length of stay)')
qqline(loglos, col = 'red', lwd = 2)

# separate data
logligt_s1 <- loglos[which(!is.na(stroke) & stroke == 1)]
logligt_s0 <- loglos[which(!is.na(stroke) & stroke == 0)]

t.test(logligt_s1, logligt_s0)



# __________ -----
# EXERCISE 3 ----

antibody <- read.csv('./lab/data/antibody.csv')
head(antibody)

# 3a) ----
# name variables 

before <- antibody$before
after <- antibody$after

# some summary stat of the data
summary(before)
summary(after)


# first reproduce the result from t-test
t.test(antibody$before, antibody$after, paired = T)


# optional
# long format data (requires tidyr package)
antibody_longdata <- tidyr::pivot_longer(antibody, 
                                         cols = tidyr::everything(), 
                                         names_to = 'group', 
                                         values_to = 'measurements')
head(antibody_longdata)
boxplot(measurements ~ group, 
        data = antibody_longdata, 
        horizontal = T, 
        main = 'Box plot for before and after measurements')

# limit the y axis (horizontal x axis)
boxplot(measurements ~ group, 
        data = antibody_longdata, 
        horizontal = T, 
        ylim = c(0, 4),
        main = 'Limit the measurement range for plotting')


# histogram 
hist(before, main = 'Measurements: before')
qqnorm(before, pch = 20, main = 'Q-Q plot for measurements: before')
qqline(before, col = 'red', lwd = 2)

hist(after, main = 'Measurements: after')
qqnorm(after, pch = 20, main = 'Q-Q plot for measurements: after')
qqline(after, col = 'red', lwd = 2)

# histogram for the difference s
hist(after - before, main = 'Difference (after - before)')



# 3b) ----
# matched data (paired)
# one-sample test (signed rank)

wilcox.test(before, after, paired = T)
# the error means there are ties (no change)

# what happens if you use independent two-samples test?
wilcox.test(before, after, paired = F) # p = 0.14




# __________ -----
# EXERCISE 4 ----

nausea <- read.csv('./lab/data/nausea.csv')
head(nausea)

treatment <- nausea$value[nausea$group == 1]
placebo <- nausea$value[nausea$group == 2]

summary(treatment)
summary(placebo)


# 4a) -----
# these samples are not matched: two sample independent test

# boxplot
boxplot(value ~ group, data = nausea, 
        main = 'Nausea: treatment vs placebo')

wilcox.test(treatment, placebo, paired = F)










