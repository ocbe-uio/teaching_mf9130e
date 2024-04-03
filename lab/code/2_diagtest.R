# intro to probability, diagnostic testing 


# example 1: mammography ----

tp <- 22
tn <- 331
fp <- 16
fn <- 3


positives <- tp + fn
positives
negatives <- tn + fp
negatives

# sensitivity: tp / positives
tp/positives

# specificity: tn / negatives
tn/negatives

# positive predictive value ppv
# tp / positive test
tp / (tp+fp)


# example 2: HIV -----

# prevalence 0.1%
# other values
# prevalence <- 0.01
# prevalence <- 0.1
# prevalence <- 0.0001
prevalence <- 0.001


# false positive rate 0.2%
# fpr is 1-specificity
specificity <- 1-0.002

# false negative rate 2%
# fnr is 1-sensitivity
sensitivity <- 1-0.02

# find ppv 
# use formula 

a <- sensitivity * prevalence
b <- sensitivity * prevalence + (1-specificity) * (1-prevalence)
a/b

# you can skip the step where you name a and b
(sensitivity * prevalence) / (sensitivity * prevalence + (1-specificity) * (1-prevalence))



# what if prevalence is different? 
# assign a new value to prevalence
# run the code again



