# material for regression 


# ____ lung funnction PEF ____ ----

lung_data <- read.csv('./lab/data/PEFH98-english.csv')

head(lung_data)

# note: gender is continuous, need to convert to factor

lung_data$gender <- factor(lung_data$gender, 
                              levels = c('1','2'),
                              labels = c('female', 'male'))



# assign variables 
pefsit1 <- lung_data$pefsit1
pefsit2 <- lung_data$pefsit2
weight <- lung_data$weight



# correlation -----
# scatter plot: pefsit2 vs pefsit1

plot(pefsit2, pefsit1)


# scatter plot: pefsit1 vs weight

plot(pefsit1, weight)

# TO DO: INSERT REG LINE

# correlation
?cor
is.na(pefsit1)
is.na(pefsit2)
cor(pefsit2, pefsit1). # need to remove NA here


cor(pefsit1, weight) # ok


# univariate reg ----

# pef2 vs pef 1
lm_pef2_pef1 <- lm(pefsit2 ~ pefsit1, data = lung_data)
summary(lm_pef2_pef1)



# pef1 vs weight 
lm_pef1_weight <- lm(pefsit1 ~ weight, data = lung_data)
summary(lm_pef1_weight)


# TODO: residual analysis here ----




# multivar reg ----

# pefsit1 vs (weight, gender)
# note that we converted gender into categorical
lm_pef1_weight_gender <- lm(pefsit1 ~ weight + gender, data = lung_data)
lm_pef1_weight_gender
summary(lm_pef1_weight_gender)




