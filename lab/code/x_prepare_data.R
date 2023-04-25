# script I use to prepare dataset used in R labs
# NOT USED FOR DEMONSTRATION


library(magrittr)
getwd()


# palmer penguin ----
# suitable for: 
# introducing data types
# small data in and out
# NA

install.packages('palmerpenguins')

penguins <- palmerpenguins::penguins
penguins <- as.data.frame(penguins)
penguins

# save penguins data in csv 
write.csv(penguins, file = './lab/data/penguins_complete.csv', 
          row.names = F)




# select a smaller dataset for exploration
sample(1:344, size = 9)
penguins_mini <- penguins[c(3, 4, 34, 78, 151, 171, 277, 341, 201),]
penguins_mini
rownames(penguins_mini) <- NULL
penguins_mini

summary(penguins_mini)

# csv (recommended)
write.csv(penguins_mini, file = './lab/data/penguins_mini.csv', 
          row.names = F)

tt <- read.csv('./lab/data/penguins_mini.csv', sep = ',')
tt


# save penguins_mini into 3 formats: rda, csv, xlsx
# rda 
save(penguins_mini, file = './lab/data/penguins_mini.rda')

load('./lab/data/penguins_mini.rda') # when using load, don't name the object



# xlsx 

install.packages('xlsx')

xlsx::write.xlsx(penguins_mini, file = './lab/data/penguis_mini.xlsx', 
                col.names = T)
# this option might ask for sheet name
ttt <- xlsx::read.xlsx('./lab/data/penguis_mini.xlsx', sheetName = 'Sheet1')
ttt


# covid19 sysvak -----
# suitable for: 
# more advanced data manip

sysvak <- read.csv('./lab/data/covid19_sysvak_by_location_sex_age.csv')
sysvak <- data.table::setDT(sysvak)
colnames(sysvak)
summary(sysvak)

sysvak$location_code %>% unique
# only select a few 
sysvak[granularity_geo %in% c('nation', 'county'), 
       .(location_code, 
         location_name,
         age, 
         sex, 
         n_dose_1, 
         n_dose_2, 
         n_dose_3, 
         n_dose_4, 
         pr100_dose_1, 
         pr100_dose_2, 
         pr100_dose_3, 
         pr100_dose_4, 
         pop)]
head(sysvak)
tail(sysvak)





# covid19 hospitalisation ----
# suitable for: 
# univariate exploration (EDA)
# time series 


hosp <- read.csv('./lab/data/covid19_hospital_by_time.csv')
hosp <- data.table::setDT(hosp)
hosp_narrow <- hosp[, .(date, year, week, n_icu, n_hospital_main_cause)]

plot(hosp_narrow$n_hospital_main_cause)

plot(as.Date(hosp_narrow$date), hosp_narrow$n_hospital_main_cause)
summary(hosp_narrow$n_hospital_main_cause)
# hist(hosp_narrow$n_hospital_main_cause)



# lung data ----
# prepare a csv version
lung_data <- haven::read_dta('./lab/data/PEFH98-english.dta')
lung_data

save(lung_data, file = './lab/data/PEFH98-english.rda')

# 1 is female, 2 is male  
# here just to make it consistent with original dataset
lung_data$gender <- factor(lung_data$gender, 
                           levels = c('1','2'),
                           labels = c('1', '2')) 


write.csv(lung_data, file = './lab/data/PEFH98-english.csv', 
          row.names = F)

tt <- read.csv('./lab/data/PEFH98-english.csv', sep = ',')
tt

str(tt)
# looks like it'll need some recoding on gender anyway


# liggetid ----
liggetid <- haven::read_dta('./lab/data/liggetid.dta')
liggetid

write.csv(liggetid, file = './lab/data/liggetid.csv', 
          row.names = F)

tt <- read.csv('./lab/data/liggetid.csv', sep = ',')
t
tt




