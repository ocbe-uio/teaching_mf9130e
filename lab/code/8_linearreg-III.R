# --------------------------------
# Code for regression analysis III
# --------------------------------

# __________
# EXERCISE 1
# crime data

#| label: linear-1a-1
#| warning: false
#| echo: true

crime_data <- read.csv('data/crime_mod.csv')
# head(crime_data)

# 1a) -----

par(mfrow = c(1, 2)) # make plots in 1 row 2 col

# scatter plot: crime vs pctmetro
plot(crime ~ pctmetro, pch='.', data = crime_data)
text(crime ~ pctmetro, labels = state, data = crime_data)

abline(lm(crime ~ pctmetro, data = crime_data), col = 'blue')

# scatter plot: crime vs single
plot(crime ~ single, pch='.', data = crime_data)
text(crime ~ single, labels = state, data = crime_data)

abline(lm(crime ~ single, data = crime_data), col = 'blue')

# 1b) -----

#| label: linear-1b-1
#| warning: false
#| echo: true

# regression: crime vs pctmetro
summary(lm(crime ~ pctmetro, data = crime_data))

# residual analysis:
par(mfrow=c(2,2))
plot(lm(crime ~ pctmetro, data = crime_data))

# 1c) -----

#| label: linear-1c-1
#| warning: false
#| echo: true

# regression: crime vs single
summary(lm(crime ~ single, data = crime_data))

# residual analysis:
par(mfrow=c(2,2))
plot(lm(crime ~ single, data = crime_data))

# 1d) -----

#| label: linear-1d-1
#| warning: false
#| echo: true

# regression: crime vs pctmetro
summary(lm(crime ~ pctmetro, 
           data = crime_data[which(crime_data$state != 'dc'),]))

# residual analysis:
par(mfrow=c(2,2))
plot(lm(crime ~ pctmetro, 
        data = crime_data[which(crime_data$state != 'dc'),]))

#| label: linear-1d-2
#| warning: false
#| echo: true

# regression: crime vs single
summary(lm(crime ~ single, 
           data = crime_data[which(crime_data$state != 'dc'),]))

# residual analysis:
par(mfrow=c(2,2))
plot(lm(crime ~ single, 
        data = crime_data[which(crime_data$state != 'dc'),]))


# __________
# EXERCISE 2
# birth data

#| label: linear-2a-1
#| warning: false
#| echo: true

birth <- read.csv('data/birth.csv')
# head(birth)

# 2a) -----

par(mfrow = c(1, 2)) # make plots in 1 row 2 col

# scatter plots:
plot(bwt ~ age, data = birth)
abline(lm(bwt ~ age, data = birth), col = 'blue')

plot(bwt ~ lwt, data = birth)
abline(lm(bwt ~ lwt, data = birth), col = 'blue')

#| label: linear-2a-2
#| warning: false
#| echo: true

par(mfrow = c(2, 2)) # make plots in 2 row 2 col

# scatter plots separate for smokers and non-smokers:
plot(bwt ~ age, data = birth[which(birth$smk == 'smoker'),], 
     main="Smokers")
abline(lm(bwt ~ age, data = birth[which(birth$smk == 'smoker'),]), 
          col = 'blue')

plot(bwt ~ age, data = birth[which(birth$smk == 'nonsmoker'),], 
     main="Non-smokers")
abline(lm(bwt ~ age, data = birth[which(birth$smk == 'nonsmoker'),]), 
       col = 'blue')

plot(bwt ~ lwt, data = birth[which(birth$smk == 'smoker'),], 
     main="Smokers")
abline(lm(bwt ~ lwt, data = birth[which(birth$smk == 'smoker'),]), 
          col = 'blue')

plot(bwt ~ lwt, data = birth[which(birth$smk == 'nonsmoker'),], 
     main="Non-smokers")
abline(lm(bwt ~ lwt, data = birth[which(birth$smk == 'nonsmoker'),]), 
       col = 'blue')

# 2b) -----

#| label: linear-2b-1
#| warning: false
#| echo: true

cor.test(~ bwt + lwt, data = birth)

# 2c) -----

#| label: linear-2c-1
#| warning: false
#| echo: true

boxplot(bwt ~ smk, data = birth)

# 2d) -----

#| label: linear-2d-1
#| warning: false
#| echo: true

# regression
summary(lm(bwt ~ smk, data = birth))

#| label: linear-2d-2
#| warning: false
#| echo: true

# scatter plots:
summary(lm(bwt ~ smk + lwt, data = birth))

#| label: linear-2d-3
#| warning: false
#| echo: true

# scatter plots:
summary(lm(bwt ~ smk + lwt + eth, data = birth))

# residual analysis:
par(mfrow=c(2,2))
plot(lm(bwt ~ smk + lwt + eth, data = birth))

