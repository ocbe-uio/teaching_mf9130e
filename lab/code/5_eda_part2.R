# eda II
# use liggetid data as example 

liggetid <- read.csv('./lab/data/liggetid.csv')
head(liggetid)



# BASE R ----
# basic descriptive 
dim(liggetid)
colnames(liggetid)
summary(liggetid)

# you can see quite some NA in the data (e.g. slag/stroke)
str(liggetid) # also tells you data type 


# subsetting, rename ----
# take a few columns

# year of admission, age, sex, admission from, stroke, length of stay
data_los <- liggetid[, c('innaar', 'alder', 'kjoenn', 
                         'kom_fra', 'slag', 'liggetid')]
head(data_los)

# rename in english
names(data_los)

names(data_los) <- c('adm_year', 'age', 'sex', 'adm_from', 'stroke', 'los')
head(data_los, 3)


# filtering ----
# base R filtering needs you to use data$variable inside the square bracket
# filter based on sex == kvinne
dsex_kvinne <- data_los[data_los$sex == 'kvinne', ]
head(dsex_kvinne)


# based on length of stay over 1000
data_los[data_los$los > 1000, ]

# stroke has some NA, let us examine those
dstroke_na <- data_los[is.na(data_los$stroke), ]
head(dstroke_na)


# combine multiple conditions with &
data_los[data_los$adm_year == 1986 & 
           data_los$age > 80 & 
           data_los$sex == 'mann', ]







# _________ ----
# TIDYVERSE/data.table ----
# tidyverse is a collection of R packages that help with data manipulation
# install.packages('tidyverse')
library(data.table)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# understand the dataset
glimpse(liggetid) # str


# subsetting, rename ----
# take a few columns

data_los2 <- select(liggetid, c('innaar', 'alder',  'kjoenn', 
                                'kom_fra', 'slag', 'liggetid'))
head(data_los2)

# rename
# note: you can also use the base R way here
setnames(data_los2, old = 'innaar', new = 'adm_year')
setnames(data_los2, old = 'alder', new = 'age')
setnames(data_los2, old = 'kjoenn', new = 'sex')
setnames(data_los2, old = 'kom_fra', new = 'adm_from')
setnames(data_los2, old = 'slag', new = 'stroke')
setnames(data_los2, old = 'liggetid', new = 'los')


# filtering ----
# filter based on sex == kvinne
dsex_kvinne2 <- filter(data_los2, sex == 'kvinne')


# based on length of stay over 1000
filter(data_los2, los > 1000)


# combine multiple conditions with &
# admission year 1986, age greater than 80, sex mann
filter(data_los2, adm_year == 1986 & age > 80 & sex == 'mann')



# _________ ------
# VISUALIZATION ----
# visualisation usually require some additional data manipulation
# rename in english
data_los <- liggetid[, c('innaar', 'alder', 'kjoenn', 
                         'kom_fra', 'slag', 'liggetid')]
names(data_los) <- c('adm_year', 'age', 'sex', 'adm_from', 'stroke', 'los')

# remove NA by hand
data_los <- filter(data_los, !is.na(sex) & !is.na(stroke) & !is.na(adm_from))

# table(data_los$adm_year)

data_los$adm_from <- as.character(data_los$adm_from)
data_los$adm_from <- factor(data_los$adm_from, 
                            levels = c('1', '2', '3', '4', '5', '6'), 
                            labels = c('Home', 'Div.Medicine', 'Div.Surgery', 
                                       'Div.Other', 'Other hospital', 'Nursing home'))

data_los$adm_year <- factor(data_los$adm_year,
                            levels = c(1981:1987),
                            labels = as.character(1981:1987))

data_los$stroke <- factor(data_los$stroke, 
                          levels = c(0, 1), 
                          labels = c('no','yes'))


# scatterplot ----
# age vs length of stay

# baseR:
plot(data_los$age, data_los$los)

# ggplot 
plt_scat <- ggplot(data = data_los, 
                   mapping = aes(x = age, y = los))

plt_scat <- plt_scat + geom_point() # add point
plt_scat

# can make some customizations: change titles, bigger fonts
plt_scat <- plt_scat + labs(
  x = 'Age', 
  y = 'Length of hosptial stay (days)', 
  title = 'Length of stay versus age'
)
plt_scat <- plt_scat + theme_bw() # make white background
plt_scat <- plt_scat + theme(
  axis.text = element_text(size = 15),
  axis.title = element_text(size = 20), 
  plot.title = element_text(size = 20)
)
plt_scat


# can add on more information: color

plt_scat2 <- ggplot(data = data_los, 
                    mapping = aes(x = age, y = los, shape = sex, color = sex))
plt_scat2 <- plt_scat2 + geom_point(size = 2, alpha = 0.7)
# customize
plt_scat2 <- plt_scat2 + labs(
  x = 'Age', 
  y = 'Length of hosptial stay (days)', 
  title = 'Length of stay versus age'
)
plt_scat2 <- plt_scat2 + theme_bw() # make white background
# change text size
plt_scat2 <- plt_scat2 + theme(
  axis.text = element_text(size = 12),
  axis.title = element_text(size = 12), 
  plot.title = element_text(size = 15)
)
# change color
plt_scat2 <- plt_scat2 + scale_color_brewer(palette = 'Set1')

plt_scat2


# can add histogram on top
library(ggExtra)

ggMarginal(plt_scat, type = 'histogram')




# histogram ----



plt_hist <- ggplot(data = data_los, mapping = aes(x = los, fill = sex))
plt_hist <- plt_hist + geom_histogram() 
plt_hist <- plt_hist + facet_wrap( ~ sex, ncol = 1)

# some customization
plt_hist <- plt_hist + theme_minimal() # make minimal background
# change axis
plt_hist <- plt_hist + labs(
  x = 'Length of hospital stay (days)', 
  y = 'Count', 
  title = 'Histograms for length of hospital stay'
)
# change text size
plt_hist <- plt_hist + theme(
  axis.text = element_text(size = 12),
  axis.title = element_text(size = 12), 
  plot.title = element_text(size = 15)
)
# change color
plt_hist <- plt_hist + scale_fill_brewer(palette = 'Set1')
plt_hist




# ridge (histograms)
library(ggridges)
plt_ridge <- ggplot(data = data_los, 
                    mapping = aes(x = los, y = adm_year, fill = sex))
plt_ridge <- plt_ridge + geom_density_ridges(alpha = 0.6) 
plt_ridge <- plt_ridge + theme_ridges()
plt_ridge <- plt_ridge + labs(
  x = 'Length of hosptial stay (days)', 
  y = 'Admission year', 
  title = 'Length of stay in each year, for each gender'
)
# change color
plt_ridge <- plt_ridge + scale_fill_brewer(palette = 'Set1')
plt_ridge





# boxplot ----
# we try to plot length of stay versus year, and potentially sex and adm type
# base R is limited in this regard
boxplot(los ~ adm_year, data = data_los, main = 'Length of stay vs year')
boxplot(los ~ sex, data = data_los,  horizontal = T, main = 'Length of stay vs sex')
boxplot(los ~ adm_from, data = data_los, horizontal = T,
        main = 'Length of stay vs admission type')




# ggplot can make more flexible plots
# add color 
plt_box <- ggplot(data = data_los, 
                  mapping = aes(x = adm_year, y = los, fill = sex))
plt_box <- plt_box + geom_boxplot(outlier.size = 1)
# plt_box <- plt_box + facet_wrap( ~ sex)
plt_box <- plt_box + coord_flip()

# customize
plt_box <- plt_box + theme_bw() # make white background
plt_box <- plt_box + labs(
  x = 'Admission year', 
  y = 'Length of hosptial stay (days)', 
  title = 'Length of stay in each year, both men and women'
)
plt_box <- plt_box + theme(
  axis.text = element_text(size = 12),
  axis.title = element_text(size = 12), 
  plot.title = element_text(size = 15), 
  strip.text = element_text(size = 12)
)

plt_box <- plt_box + scale_fill_brewer(palette = 'Set1')
plt_box 




# add even more information 
plt_box2 <- ggplot(data = data_los, 
                  mapping = aes(x = adm_year, y = los, fill = sex))
plt_box2 <- plt_box2 + geom_boxplot(outlier.size = 0.8)
plt_box2 <- plt_box2 + facet_wrap( ~ adm_from)


# customize
plt_box2 <- plt_box2 + theme_bw() # make white background
plt_box2 <- plt_box2 + labs(
  x = 'Admission year', 
  y = 'Length of hosptial stay (days)', 
  title = 'Length of stay in each year, each type of admission'
)
plt_box2 <- plt_box2 + theme(
  axis.text = element_text(size = 11),
  axis.title = element_text(size = 12), 
  plot.title = element_text(size = 15), 
  strip.text = element_text(size = 12), 
  axis.text.x = element_text(angle = 45) # more readable
)

plt_box2 <- plt_box2 + scale_fill_brewer(palette = 'Set1')
plt_box2 


