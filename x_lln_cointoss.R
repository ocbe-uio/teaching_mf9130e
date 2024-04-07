# law of large number 

# coin tosses
n <- 100
p <- 0.5
s <- 1

coin <- rbinom(n = n, size = s, prob = p)
coin

n_heads <- cumsum(coin)
n_tosses <- 1:n

p_cumulative <- n_heads/n_tosses
p_cumulative <- round(p_cumulative, digits = 2)
plot(p_cumulative, ylim = c(0,1), pch = 20,
     xlab = 'Toss', 
     ylab = 'Frequency', 
     main = 'Coin tosses cumulative probability of H (1)')

# plot the 'true' probability
abline(h = p, col = 'red')


