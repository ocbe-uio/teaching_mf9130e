### 

require(pwr)

# _____ EXERCISE _____ -----
# ex 1 ----
# a) 
# STATA: power two means 3, diff(2.5) sd(5) power(0.9)
# power: 0.9
# level of significance: 0.05
# delta: 2.5
# sd: 5
# standardized difference (effect size): 2.5/5

pwr.t.test(n = NULL, 
           d = 2.5/5, 
           type = 'two.sample',
           alternative = 'two.sided',
           power = 0.9,
           sig.level = 0.05)

# b)
# STATA: power two means 3, diff(3) sd(5) power(0.9)
# delta: from 2.5 to 3

pwr.t.test(n = NULL, 
           d = 3/5, 
           type = 'two.sample',
           alternative = 'two.sided',
           power = 0.9,
           sig.level = 0.05)


# c) 
# STATA: power two means 3, diff(3) sd(6) power(0.9)
# delta: from 2.5 to 3
# sd: from 5 to 6
pwr.t.test(n = NULL, 
           d = 3/6, # same effect size as in point a)
           type = 'two.sample',
           alternative = 'two.sided',
           power = 0.9,
           sig.level = 0.05)





# ex 2 ----
# prop in control group: p1 = 0.15
# prop in treatment group: p2 = 0.08
# power: 0.8
# significance level: 0.05
# STATA: power twoproportions 0.15 0.08, test(chi2)
# (stata gives 325 in each group, here it is 318: 
# effect size slightly different)
# if you use h = 0.2194, the result is closer to 325
effect.size <- ES.h(p1 = 0.15, p2 = 0.08)
effect.size
pwr.2p.test(h = effect.size, 
            n = NULL, 
            sig.level = 0.05, 
            power = 0.8, 
            alternative = 'two.sided')



# ex 3 ----

# delta: 0.5
# sd: 1
# power: 0.8
# significance level: 0.05
# effect size: 0.5/1 = 0.5

pwr.t.test(n = NULL, 
           d = 0.5/1, 
           type = 'two.sample',
           alternative = 'two.sided',
           power = 0.8,
           sig.level = 0.05)




# ex 4 ----

# a)
# using the notation from lecture:
a <- 0.1
p <- 0.5

n <- 1.96^2 * p*(1-p)/a^2
n


# b)
a <- 0.03
p <- 0.5

n <- 1.96^2 * p*(1-p)/a^2
n

# c)
a <- 0.03
p <- 0.2

n <- 1.96^2 * p*(1-p)/a^2
n


# ex 5 ----

# a)
# compute effect size:
p1 <- 0.3
p2 <- 0.2
pbar <- (p1+p2)/2
es <- (p1-p2)/sqrt(pbar * (1-pbar)) # 0.23
es

# or use ES.h
effect.size <- ES.h(p1 = 0.3, p2 = 0.2)
effect.size # more precise calculation with R

pwr.2p.test(h = effect.size, 
            n = NULL, 
            sig.level = 0.05, 
            power = 0.8, 
            alternative = 'two.sided')


# b)
# n = 65, find power
# note that n is for per group, so divide by 2
effect.size <- ES.h(p1 = 0.3, p2 = 0.2)
pwr.2p.test(h = effect.size, 
            n = 65/2, 
            sig.level = 0.05, 
            power = NULL, 
            alternative = 'two.sided')
# c)
p1 <- 4/34
p0 <- 11/31
RR <- p1/p0
RR

SElogRR <- sqrt(1/4-1/34+1/11-1/31)
CI <- c(exp(log(RR) - 1.96 * SElogRR),
        exp(log(RR) + 1.96 * SElogRR))
CI
