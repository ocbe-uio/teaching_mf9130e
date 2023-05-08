# install package pwr
install.packages('pwr')
# load package pwr
require(pwr)

# _____ LECTURE _____ -----

# mean: independent samples ----

pwr.t.test(n = NULL, 
           d = 0.5, 
           sig.level = 0.05,
           type = 'two.sample',
           alternative = 'two.sided',
           power = 0.8)

# mean: paired samples ----

pwr.t.test(n = NULL, 
           d = 0.5, 
           sig.level = 0.05,
           type = 'paired',  
           alternative = 'two.sided',
           power = 0.8)

# proportion ----

effect.size <- ES.h(p1 = 0.1, p2 = 0.2)
pwr.2p.test(h = effect.size, 
            n = NULL, 
            sig.level = 0.05, 
            power = 0.8, 
            alternative = 'two.sided')

# proportion ---- finding power

effect.size <- ES.h(p1 = 0.15, p2 = 0.3)
effect.size
pwr.2p.test(h = effect.size, 
            n = 200 / 2, # sample size PER GROUP! 
            sig.level = 0.05, 
            alternative = 'less')

