# -------------------------------
# Code for regression analysis II
# -------------------------------

# __________
# EXERCISE 1
# lung data

#| label: linear-1a-1
#| warning: false
#| echo: true

lung_data <- read.csv('data/PEFH98-english.csv')

# 1a) -----

# pefsit1 vs (weight, gender)
# note that we converted gender into categorical
lm_pef1_weight_gender <- lm(pefsit1 ~ weight + gender, 
                            data = lung_data)
# lm_pef1_weight_gender
summary(lm_pef1_weight_gender)

#| label: linear-1a-2
#| warning: false
#| echo: true

#residual plots
par(mfrow=c(2,2))
plot(lm_pef1_weight_gender)

# 1b) -----

#| label: linear-1b-1
#| warning: false
#| echo: true

# 1. height weight gender
lm_pefm_height_weight_gen <- lm(pefmean ~ height + weight + gender, data = lung_data)

summary(lm_pefm_height_weight_gen)

#| label: linear-1b-2
#| warning: false
#| echo: true

# 2. weight gender
lm_pefm_weight_gen <- lm(pefmean ~ weight + gender, data = lung_data)

summary(lm_pefm_weight_gen)

#| label: linear-1b-3
#| warning: false
#| echo: true

#residual plots
par(mfrow=c(2,2))
plot(lm_pefm_weight_gen)


# __________
# EXERCISE 2
# length of hospital stay

#| label: linear-2a-1
#| warning: false
#| echo: true

liggetid <- read.csv('data/liggetid.csv')
head(liggetid, 3)

# 2a) -----

# boxplot
boxplot(liggetid ~ kjoenn, data = liggetid)

# 2b) -----

#| label: linear-2b-1
#| warning: false
#| echo: true

# response (dep): liggetid
# predictor (indep): kjoenn, slag

lm_ligge <- lm(liggetid ~ slag + kjoenn, data = liggetid)
summary(lm_ligge)

#| label: linear-2b-2
#| warning: false
#| echo: true

# visualize the residuals
par(mfrow = c(2, 2))
plot(lm_ligge)

# 2c) -----

#| label: linear-2c-1
#| warning: false
#| echo: true

# response (dep): log transformed (lnliggti)
# predictor (indep): kjoenn, slag

lm_logligge <- lm(lnliggti ~ slag + kjoenn, 
                  data = liggetid)
summary(lm_logligge)

#| label: linear-2c-2
#| warning: false
#| echo: true

par(mfrow = c(2, 2))
plot(lm_logligge)

