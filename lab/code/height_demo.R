
#read in data after downloading the csv file 
#NB edit directory to match file location on your computer
ppnLR <- read.csv("~/tempi/BT2012/ppnLR.csv", sep=";")

#define variables and put into a data frame
sex<-as.factor(ppnLR$sex)
height<-ppnLR$height
ppn<-as.factor(ppnLR$ppn)

ppnData = data.frame(ppn,height,sex)

#call graphics library (install first if not already present)
library(ggplot2)
#investigate distribution of height using histogram with 20 bins
ggplot(ppnData,aes(x=height))+geom_histogram(bins=20)
#split the histogram by sex
ggplot(ppnData,aes(x=height))+geom_histogram(aes(color = sex, fill = sex), position = "identity",bins=15, alpha = 0.4)
#investigate frequency of ppn for females and males
#create first a frequency table by combining the two variables sex and ppn
sex_ppn <- table(ppnData$sex,ppnData$ppn)
sex_ppn
prop.table(sex_ppn,1)
prop.table(sex_ppn,2)

#fit logistic regression model to explain ppn risk as a function of height
ppnLR <- glm(ppn ~ height, family = "binomial")
#examine model fitting results
summary(ppnLR)

#fit another logistic regression model to compare ppn risk between females and males
ppnLR2 <- glm(ppn ~ sex, family = "binomial")
#examine model fitting results (no significant difference associated with sex)
summary(ppnLR2)

#include now both height and sex as an explanatory variables in an additive model
ppnLR3 <- glm(ppn ~ height+sex, family = "binomial")
summary(ppnLR3)

#examine last what happens if an interaction effect between sex and height is included in the model
ppnLR4 <- glm(ppn ~ height+sex+height:sex, family = "binomial")
summary(ppnLR4)
#note how inclusion of the additional term changes the estimate of height effect and the baseline (Intercept)

#use boxplots to compare height distributions between diagnosed vs non-diagnosed  and females/males 
ggplot(ppnData,aes(x=ppn,y=height,group=ppn))+geom_boxplot(aes(fill=sex))+facet_grid(. ~ sex)
