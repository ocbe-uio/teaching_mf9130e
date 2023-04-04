# Course material for "MF9130E V23, Introduction to Statistics" 
# 2023.04.26

#setwd("<your project directory where the data files are>")
# getwd()
proj_root <- rprojroot::find_rstudio_root_file()
setwd(paste0(proj_root, '/lab/'))



# PART 1: basic commands ----




# PART 2: data import, export ----
# read and save datafile 



mir <- read.table("data/miRNA-421x282.txt", header=T, sep="\t", dec=".")
rna <- read.table("data/mRNA-100x282.txt", header=T, sep="\t", dec=".")
prt <- read.table("data/prot-100x282.txt", header=T, sep="\t", dec=".")

# PART 3: data frame manipulation ----



# PART 4: visualisation (base r) ----
