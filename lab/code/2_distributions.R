# lab code for normal and binomial distribution


# randomness ----
# simulate 
sample(1:6, size = 2, replace = T)

sample(1:3, size = 3, replace = F)
sample(1:3, size = 3, replace = T) # replacement is allowed


# if set seed, the result is the same
# you need to run the line set.seed(yourseed) right before
set.seed(1)
sample(1:6, size = 2, replace = T)



# binomial distribution -----

# for this simulation, we do not need to set seed
# because the reproducibility is not too important for the demo

x <- rbinom(n = 500, size = 1, prob = 0.5)
hist(x)

# test different prob
x <- rbinom(n = 500, size = 1, prob = 0.15)
hist(x)

# test out different size
x <- rbinom(n = 500, size = 8, prob = 0.5)
hist(x)


# increase n and p (size and prob)
x <- rbinom(n = 500, size = 30, prob = 0.5)
hist(x, breaks = 20)

# more data points
x <- rbinom(n = 2000, size = 30, prob = 0.5)
hist(x, breaks = 20)





# normal distribution -----

# two parameters: mean and sd
x <- rnorm(1000, mean = 0, sd = 1)

x <- rnorm(1000, mean = 10, sd = 2)

# visualize
hist(x)


# check the summary
summary(x)

# mean
mean(x)

# variance and sd
var(x)
sd(x)

# (sd(x))^2 # these two are equivalent

# can plot the mean on the histogram to indicate the center
hist(x)

# v means verticle line, lwd means line width
abline(v = mean(x), col = 'red', lwd = 2)



# quantiles and probability
p1 <- pnorm(1.96, mean = 0, sd = 1)
p2 <- pnorm(-1.96, mean = 0, sd = 1)

1-p1 # equal to p2



# bar plot ----
# make bar plot given counts

dd <- data.frame(prob = c(0.03, 0.14, 0.13, 0.38, 0.33),
                 n = c(3,16,15,44,38))
rownames(dd) <- c('0-17', '18-24', '25-34', '35-64', '65+')


# bar plot for counts
barplot(dd$n, names.arg = rownames(dd),
        main = 'Number of killed for road accidents')


# bar plot for probability
barplot(dd$prob, names.arg = rownames(dd),
        main = 'Proportion for killed road accidents')




# normal distribution: birth -----

# load birth data first
# if you forgot, check notes from day 2 (descriptive stat)
birth

# we use the variable bwt

birth_weight <- birth$bwt
hist(birth_weight)

# find the probability above 4000
# solution 1: from empirical data
# first find how many are above 400 (counting)
birth_weight>4000

# we see 9 TRUE
# which finds the indices
# length counts the number of elements
which(birth_weight >4000)
length(which(birth_weight >4000)==T)

# alternatively, since R codes T as 1 and F as 0
# we can use sum() command
sum(birth_weight>4000)


# probability 
9/189

# similarly, we can find weight between 2800 and 3000 (including)
which(birth_weight <= 3000 & birth_weight >= 2800)
length(which(birth_weight <= 3000 & birth_weight >= 2800))

# probability
20/189



# solution 2
# use normal approximation 
# first get the parameters mean and sd (or sqrt variance)
m <- mean(birth_weight)
s <- sd(birth_weight)
# s <- sqrt(var(birth_weight))

# probability of birthweight above 4000

# if you want 
pnorm(4000, mean = m, sd =s, lower.tail = F)


# you can also use the standard normal dist
# whose mean and sd (var) are 0 and 1
# can be translated as a second variable Y above 1.45 
# see lecture notes for why this is the case!
pnorm(1.45, lower.tail = F)




# binomial vs normal ----
# the approximation is N(np, npq) if n is large, p close to 0.5
# note: the small n (first argument) is how many random samples you want
# it is different from the 'size' argument in binomial distribution


N <- 50
P <- 0.5

# binom(50, 0.5)
x_binom <- rbinom(n = 1000, size = N, prob = P)
hist(x_binom)

# normal
x_norm <- rnorm(n = 1000, mean = N*P, sd = sqrt(50*0.5*(1-0.5)))
hist(x_norm)


# take summary statistics 
summary(x_binom)
summary(x_norm)



