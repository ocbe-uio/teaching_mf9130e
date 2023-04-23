# Course material for "MF9130E V23, Introduction to Statistics" 
# 2023.04.26


# 1: create a variable ----

a <- 3
b <- 4
c <- 5

# compute the average
(a+b+c)/3


# create a numeric variable number_1
number_1 <- 1.2

# a character variable student
student <- 'hadley'

# a logical variable true_or_false
true_or_false <- T


# return variable 
number_1
print(number_1)




# 2. data types ----

# check variable types
class(number_1)
class(student)
class(true_or_false)



# 3. data structure ----
# vector ----

# create a vector, use c()
# numeric
num_vector <- c(1, 2, 3, 4, 5)
num_vector

# character
char_vector <- c('student_a', 'student_b', 'student_c')
char_vector

# logical 
logical_vector <- c(T, F, T, F)
logical_vector



# shortcut for creating a vector 
# numeric
# num_vector <- c(1, 2, 3, 4, 5)
num_vector <- 1:5 # from 1 to 5
seq(from = 1, to = 11, by = 2) # from 1 to 11, with 2 between each
rep(1, 5) # repeat 1 for 5 times

# character
# char_vector <- c('student_a', 'student_b', 'student_c')
char_vector <- paste0('student_', c('a', 'b', 'c'))


# combinne vectors
vec1 <- c(1, 3, 5)
vec2 <- c(100, 101)
c(vec1, vec2)

# you can also save it into a new variable, 
# so that you can access it in the future
vec_combined <- c(vec1, vec2)
vec_combined


# matrix ----

# create a matrix directly
matrix_1 <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = T)
matrix_1


# combine two vectors
vec1 <- c(1, 2)
vec2 <- c(3, 4)

# bind by columnn
matrix_c <- cbind(vec1, vec2)
matrix_c
# bind by row
matrix_r <- rbind(vec1, vec2)
matrix_r


# data.frame ----

# create a data.frame
mini_data <- data.frame(
  age = c(20, 50, 32), 
  sex = c('male', 'female', 'male'), 
  has_covid = c(T, T, F)
)
mini_data


# 4. basic data manipulation ----

# get the dimension of your data

vec1 <- c(1, 2)
length(vec1)

# matrix
mat <- matrix(data = c(1, 2, 3, 4), nrow = 2, byrow = 2)
dim(mat)

# dataframe
dim(mini_data)

# number of rows and columns
nrow(mini_data)
ncol(mini_data)



# accessing elements of your data

# vectors
letters <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')

# 3rd letter
letters[3]

# 3rd, and 5th
letters[c(3, 5)]

# letters beyond 4
letters[5:8] # or, letters[5:length(letters)]


# matrix
mat_3by3 <- matrix(data = 1:9, nrow = 3, byrow = T)
mat_3by3
# element (2,3)
mat_3by3[2, 3]

# first row
mat_3by3[1,]




# data.frame 
# via index
# first row 
mini_data[1, ]

# second col
mini_data[, 2]

# via column name 
mini_data$age
mini_data['age']


# filter a vector based on a second vector 
age <- c(55, 60, 65)
sex <- c('Male', 'Female', 'Male')

# select age for sex == Female
# first create a variable indicating sex == Female is true
sex_indicator <- sex == 'Female'

# next combine age with sex_indicator, this only selects the 2nd element
age[sex_indicator] 

# you can skip the middle step:
age[sex == 'Female']



# 5: data import ----

# read a csv file
penguins_mini <- read.csv('data/penguins_mini.csv', sep = ',')

penguins_mini





