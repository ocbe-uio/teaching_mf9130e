# Course material for "Machine Learning in Medical Bioinformatics", 
# May 03, 2022.
# Computer Lab (with R): A Cancer Modeling Example.
# See 3_StatPrinciples_RLab.pdf for some background info.

# Exercise on analysis of miRNA, mRNA and protein data from the paper 
# Aure et al, Integrated analysis reveals microRNA networks coordinately 
# expressed with key proteins in breast cancer, Genome Medicine, 2015.

# Please run the code provided to replicate some of the analyses in Aure et al. (2015).
# Make sure you can explain what all the analysis steps do and that you 
# understand all the results.
# In addition, there are three extra tasks (TASK 1, TASK 2, TASK 3), where no R code
# is provided. Please do these tasks when you have time available at the end of the lab.

#setwd("<your project directory where the data files are>")
# getwd()
proj_root <- rprojroot::find_rstudio_root_file()
setwd(paste0(proj_root, '/lab/'))


# PART 1: EDA ----
# Part 1: Exploratory analysis to understand the data.


# Read the data

mir <- read.table("data/miRNA-421x282.txt", header=T, sep="\t", dec=".")
rna <- read.table("data/mRNA-100x282.txt", header=T, sep="\t", dec=".")
prt <- read.table("data/prot-100x282.txt", header=T, sep="\t", dec=".")

# Convert to matrix format

mir <- as.matrix(mir)
rna <- as.matrix(rna)
prt <- as.matrix(prt)

# Have a look at the data to see if it looks ok

mir[1:4, 1:4]
rna[1:4, 1:4]
prt[1:4, 1:4]

# Look at the overall distribution of expression values

par(mfrow=c(3,1))
hist(mir, nclass=40, xlim=c(-5,5), col="lightblue")
hist(rna, nclass=40, xlim=c(-5,5), col="lightblue")
hist(prt, nclass=40, xlim=c(-5,5), col="lightblue")

# Look at mRNA-protein associations (only first nine genes)

par(mfrow=c(3,3))
par(mar=c(3,3,3,3))
for (i in 1:9) {
  plot(rna[i,], prt[i,], pch=19)
}

# Let us add some annotation to the above plot

par(mfrow=c(3,3))
par(mar=c(3,3,3,3))
for (i in 1:9) {
  plot(rna[i,], prt[i,], pch=19)
  title(rownames(rna)[i])
  lines(smooth.spline(rna[i,], prt[i,], df=4), col="red")
}

## TASK 1 ----
# Extend the above analysis to cover all genes.

# Compute and plot mRNA-protein correlations

rho = rep(NA, nrow(rna))  
for (i in 1:nrow(rna)) {
  rho[i] = cor(rna[i,], prt[i,])
}
par(mfrow=c(1,1))
hist(rho, col="lightblue")
#plot(density(rho))

# Calculate the correlation of each miRNA to each protein

RHO = matrix(NA, nrow(mir), nrow(prt))
for (i in 1:nrow(mir)) {
  for (j in 1:nrow(prt)) {
     RHO[i,j] = cor(mir[i,], prt[j,]) 
  }
}
par(mfrow=c(1,1))
hist(RHO, col="lightblue")

# Visualize as heatmap

source("code/clustermap_beta.R")

plot.init(tree=c(2,3))
hcluster(RHO, clust="row", distance="euclidean", linkage="complete")
hcluster(RHO, clust="col", distance="euclidean", linkage="complete")
plot.hmap(RHO)
plot.tree(side=2)
plot.tree(side=3)
plot.hmap.key()

## TASK 2 ----
# Compare this heatmap with Figure 3 in Aure et al. (2015). 
# Are these two figures showing the same results?




# PART 2: MODEL ----
# Part 2: Model (on the log-scale) the association of miRNA espression on 
#         protein expression adjusting for the corresponding mRNA


# Example: Investigate miR-107 and B-RAF (see Aure et al, 2015, Figure 2H):

prt.BRAF = prt[12,]
rna.BRAF = rna[12,]
mir.107 = mir[16,] 

# (a) Linear regression model (on the log-scale) (Aure et al. 2015, equation (3)):
fitA <- lm(prt.BRAF ~ mir.107 + rna.BRAF)
summary(fitA)

#Add smooth non-linear cures to the scatterplots: use existing panel.smooth() function
#Add linear regression lines to the scatterplots:
panel.linear <- function (x, y, col.regres = "blue", ...) 
{ 
  points(x, y, pch=19) 
  ok <- is.finite(x) & is.finite(y) 
  if (any(ok)) 
    abline(stats::lm(y[ok] ~ x[ok]), col = col.regres, ...) 
} 
pairs(data.frame(mir.107, prt.BRAF, rna.BRAF), 
      lower.panel = panel.smooth,
      upper.panel = panel.linear)

# (b) Lasso-penalised linear model with all miRNAs (Aure et al. 2015, equation (4)):

library(glmnet)

# 10-fold CV to determine the optimal lambda:
# Note: rna.BRAF is penalised together with all the mir variables. 
# You can use the penalty.factor option to avoid this.
set.seed(1234)
cvfit <- cv.glmnet(y=prt.BRAF, x=t(rbind(mir, rna.BRAF)),
                   alpha=1, nfolds=10, standardize=TRUE)

par(mfrow=c(1,1))
plot(cvfit)
lambda.opt <- cvfit$lambda.min

# Coefficient path plot and coefficients for optimal lambda:
fitB <- cvfit$glmnet.fit

plot(fitB, xvar="lambda")
abline(v=log(lambda.opt))

coef(fitB, s=lambda.opt)
predict(fitB, type="nonzero", s=lambda.opt)

# Compare the regression coefficient of mir.107 from the models in (a) and (b):
coef(fitA)["mir.107"]
as.matrix(coef(fitB, s=cvfit$lambda.min))["hsa-miR-107",]

# TASK 3 ----
### Repeat the lasso analysis, but this time do not penalise the rna.BRAF variable
### together with the mir variables. 
### Check out the information on the penalty.factor option in ?glmnet to understand how.

# 10-fold CV to determine the optimal lambda:
# Note: rna.BRAF is penalised together with all the mir variables. 
# You can use the penalty.factor option to avoid this.
set.seed(1234)
cvfit <- cv.glmnet(y=prt.BRAF, x=t(rbind(mir, rna.BRAF)),
                   alpha=1, nfolds=10, standardize=TRUE,
                  penalty.factor=c(rep(1,nrow(mir)),0))

par(mfrow=c(1,1))
plot(cvfit)
lambda.opt <- cvfit$lambda.min

# Coefficient path plot and coefficients for optimal lambda:
fitB <- cvfit$glmnet.fit

plot(fitB, xvar="lambda")
abline(v=log(lambda.opt))

coef(fitB, s=lambda.opt)
predict(fitB, type="nonzero", s=lambda.opt)
