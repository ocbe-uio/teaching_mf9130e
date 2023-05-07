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



# _______ week 1 _____ ----
# PEFH98 ----
# prepare a csv version
lung_data <- haven::read_dta('./lab/data/PEFH98-english.dta')
lung_data


# 1 is female, 2 is male  
# here just to make it consistent with original dataset
lung_data$gender <- factor(lung_data$gender, 
                           levels = c('1','2'),
                           labels = c('female', 'male')) 

lung_data <- as.data.frame(lung_data)
lung_data
save(lung_data, file = './lab/data/PEFH98-english.rda')

load('./lab/data/PEFH98-english.rda')

write.csv(lung_data, file = './lab/data/PEFH98-english.csv', row.names = F)

tt <- read.csv('./lab/data/PEFH98-english.csv', sep = ',')
tt

str(tt)


# birth ----

birth <- haven::read_dta('./lab/data/birth.dta')
head(birth)
tail(birth)


# some processing 
birth$low <- factor(birth$low, 
                    levels = c('0','1'),
                    labels = c('bwt > 2500', 'bwt <= 2500')) 

birth$eth <- factor(birth$eth, 
                    levels = c('1','2','3'),
                    labels = c('white', 'black', 'other')) 


birth$smk <- factor(birth$smk, 
                    levels = c(0, 1), 
                    labels = c('nonsmoker', 'smoker'))


birth$ht <- factor(birth$ht, 
                   levels = c(0, 1), 
                   labels = c('no', 'yes'))


birth$ui <- factor(birth$ui, 
                   levels = c(0, 1), 
                   labels = c('no', 'yes'))

birth <- as.data.frame(birth)
birth


# save two copies
save(birth, file = './lab/data/birth.rda')
write.csv(birth, file = './lab/data/birth.csv', row.names = F)



# _______ week 2 ______ -----

# antibody ----

antibody

# remove diff
antibody <- antibody[, 1:2]
head(antibody)

# save
save(antibody, file = './lab/data/antibody.rda')
write.csv(antibody, file = './lab/data/antibody.csv', row.names = F)


# nausea ----

head(nausea)

# save 
save(nausea, file = './lab/data/nausea.rda')
write.csv(nausea, file = './lab/data/nausea.csv', row.names = F)




# bp ----

bp

# save two copies
save(bp, file = './lab/data/bp.rda')
write.csv(bp, file = './lab/data/bp.csv', row.names = F)





# melanoma ----

melanoma <- haven::read_dta('./lab/data/melanoma.dta')
# melanoma <- melanoma[, 1:8]

head(melanoma)
colnames(melanoma)

melanoma$status <- factor(melanoma$status, 
                    levels = c('1','2','4'),
                    labels = c('1', '2', '4')
                    # labels = c('Death from disease',
                     #          'Censored', 
                    #           'Deatth from other causes')
                    ) 

melanoma$ulceration <- factor(melanoma$ulceration, 
                              levels = c('1', '2'), 
                              labels = c('Yes', 'No'))


melanoma$gender <- factor(melanoma$gender, 
                          levels = c('1', '2'), 
                          labels = c('f', 'm'))

melanoma$grouped_tumor_thickness <- factor(melanoma$grouped_tumor_thickness, 
                                           levels = c('1', '2', '3'),
                                           labels = c('0-2 mm', '2-5 mm', '5+ mm'))

head(melanoma)
save(melanoma, file = './lab/data/melanoma.rda')
write.csv(melanoma, file = './lab/data/melanoma.csv', row.names = F)



# haven::read_dta('./lab/data/framingham.dta')
# liggetid ----
liggetid <- haven::read_dta('./lab/data/liggetid.dta')
liggetid


liggetid$kjoenn <- factor(liggetid$kjoenn, 
                          levels = c('0', '1'), 
                          labels = c('kvinne', 'mann'))


head(liggetid)
write.csv(liggetid, file = './lab/data/liggetid.csv', 
          row.names = F)


save(liggetid, file = './lab/data/liggetid.rda')

tt <- read.csv('./lab/data/liggetid.csv', sep = ',')
head(tt)



# framingham ----
head(framingham)
framingham$firstchd

framingham$smoke <- factor(framingham$smoke, 
                          levels = c('0', '1'), 
                          labels = c('non-smoking', 'smoking'))


framingham$firstchd <- factor(framingham$firstchd, 
                           levels = c('0', '1'), 
                           labels = c('no-evidence', 'evidence'))


write.csv(framingham, file = './lab/data/framingham.csv', 
          row.names = F)
save(framingham, file = './lab/data/framingham.rda')

