# Exploratory data analysis (part 1, part2)
# some content



# _______________ ----
# exercise 1: weight ----

# 1a) ----
# create a new variable named 'weight'

weight <- c(50, 75, 70, 74, 95, 
            83, 65, 94, 66, 65, 
            65, 75, 84, 55, 73, 
            68, 72, 67, 53, 65)

# 1b) ----
# descriptive analysis of variable weight

mean(weight)
median(weight)
max(weight)
min(weight)

# alternatively, 
summary(weight)


# 1c) ----
# make a hihstogram
hist(weight)


# 1d) ----
# make a boxplot
boxplot(weight)



# _______________ ----
# exercise 2: lung function ----

# 2a) ----
# load data 
# you should set the path to where you saved it!

lung_data <- haven::read_dta('./lab/data/PEFH98-english.dta')
head(lung_data)

# alternatively, use csv
# lung_data <- read.csv('./lab/data/PEFH98-english.csv', sep = ',')
# note the difference in how gender is coded 

# it would be easier to read by you if you call it differently
lung_data$gender <- factor(lung_data$gender, 
                           levels = c('1','2'),
                           labels = c('female','male')) 
head(lung_data)



# 2b) ----
# number of observations
nrow(lung_data)

# list of variable names, it is asking for columnn names
colnames(lung_data)


# histogram
hist(lung_data$height)
hist(lung_data$weight)
hist(lung_data$age)
hist(lung_data$pefsitm)
hist(lung_data$pefstam)


# 2c) ----

# histogram 
# with base R, it is hard to make grouped histogram
# we filter the data based on gender, plot separately

height_f <- lung_data$height[lung_data$gender == 'female']
height_m <- lung_data$height[lung_data$gender == 'male']

par(mfrow = c(1,2)) # plot in parallel
hist(height_f)
hist(height_m)

# we can make it more customized
# add axis limit, title and xaxis name
par(mfrow = c(1,2)) # plot in parallel
hist(height_f, main = 'Height: female', xlab = 'Height (cm)',
     xlim = c(150, 200))
hist(height_m, main = 'Height: male', xlab = 'Height (cm)',
     xlim = c(150, 200))

# pefmean

pefmean_f <- lung_data$pefmean[lung_data$gender == 'female']
pefmean_m <- lung_data$pefmean[lung_data$gender == 'male']

par(mfrow = c(1,2)) # plot in parallel
hist(pefmean_f)
hist(pefmean_m)


# boxplot
boxplot(height ~ gender, data = lung_data)

# it is also possible to remove the frame
boxplot(pefmean ~ gender, data = lung_data, frame = F)



# 2d) -----

# pefmean height
plot(lung_data$pefmean, lung_data$height)

# it is possible to customize 
plot(lung_data$pefmean, lung_data$height, 
     main = 'PEF mean vs height', 
     xlab = 'PEF mean', ylab = 'Height',
     pch = 20)
# pch: plotting symbols
# pch = 20 is small solid dots
# you can try to set this value between 0 to 25 
# http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r

# pefmean weight
plot(lung_data$pefmean, lung_data$weight)

# pefmean age
plot(lung_data$pefmean, lung_data$age)






