setwd("<your working folder>")

library('tidyverse')
library('readr')
library('skimr')
library('janitor')
library('mice')
library('lattice')
library("lme4")
library('broom.mixed')
library('sf')
library('mapview')

#Load data from file. Please download the data from CSIS site (https://reconasia.csis.org/reconnecting-asia-map/) and save to your working folder for example. 
rapd_fn <- "<your working folder>/Reconnecting Asia Project Database - Project Database.csv"
rapd <- readr::read_csv(rapd_fn, skip = 4)
rapd <- janitor::clean_names(rapd)

#Missing values in full data
nasum<-function(x) {sum(is.na(x))}; napct<-function(x) { round(nasum(x)/dim(rapd)[1],2) }
apply(rapd, 2, napct) # %

#Filter only china funded or those that belong to the corridors
rapd <- rapd %>% 
  dplyr::filter(grepl('china', funding_sources, ignore.case = TRUE) | 
                  grepl("belt and road|CPEC|BCIMEC|CCWAEC|CICPEC|CMREC|NELB", initiatives, ignore.case = TRUE))

#Drop out some columns that are not needed
all <- names(rapd)
drop <- c(1:2, 5, 7:19, 21:22, 24:25, 27:33)
keep <- !(1:length(all) %in% drop)
data <- rapd[, keep]

#Drop some rows based on status-variable value
data <- data[data$status != "NULL",]
data <- data[data$status != "Cancelled",]
data <- data[data$status != "Decommissioned",]

#Tidy countries-variable values so that it contains only one country
#Create a new row for each such project that is attached to multiple countries
data$countries <- stringr::str_remove_all(data$countries,"[']")
data <- tidyr::separate_rows(data, countries, convert = TRUE, sep = ",")
data$countries <- stringr::str_trim(data$countries)
data <- data[data$countries != "North",] #North-Korea was separated with ','. Remove duplicates.
colnames(data)[2] <- "country"

#Corridors
CPEC <- c("Pakistan") #China-Pakistan Economic Corridor (CPEC)
BCIMEC <- c("Bangladesh", #Bangladesh-China- India-Myanmar Economic Corridor (BCIMEC)
            "India",
            "Myanmar")
CCWAEC <- c("Iran", #China-Central Asia- West Asia Economic Corridor (CCWAEC)
            "Kazakhstan",
            "Kyrgyzstan",
            "Tajikistan",
            "Turkey",
            "Turkmenistan",
            "Uzbekistan")
CICPEC <- c("Cambodia", #China-Indochina Peninsula Economic Corridor (CICPEC)
            "Laos",
            "Malaysia",
            "Myanmar",
            "Thailand",
            "Vietnam")
CMREC <- c("Mongolia", #China-Mongolia-Russia Economic Corridor (CMREC)
           "Russia")
NELB <- c("Belarus", #New Eurasia Land Bridge Economic Corridor (NELB)
          "Czech Republic",
          "Kazakhstan",
          "Poland",
          "Russia",
          "Germany")
corridors <- c("OUT", "CPEC", "BCIMEC", "CCWAEC", "CICPEC", "CMREC", "NELB")
data <- dplyr::mutate(data,
                      corridor = dplyr::case_when(
                        country %in% CPEC ~ "CPEC",
                        country %in% BCIMEC ~ "BCIMEC",
                        country %in% CCWAEC ~ "CCWAEC",
                        country %in% CICPEC ~ "CICPEC",
                        country %in% CMREC ~ "CMREC",
                        country %in% NELB ~ "NELB",
                        TRUE ~ "OUT")
)
data$corridor <- factor(data$corridor, levels = corridors)

#Set categorical variables
factor_variables<-c("infrastructure_type", "country", "status")
data[factor_variables] <- lapply(data[factor_variables], as.factor)

inlist <- names(data)[-c(7)] # Do not include corridor-variable in imputation
pred <- mice::quickpred(data, minpuc = 0.5, include = inlist, exclude = c("corridor"))

#Impute
imp <- mice::mice(data, pred = pred, seed = 12345, m = 10)

#Some plots
mice::densityplot(imp)

mice::stripplot(imp, pch = 20, cex = 1.2)

vnames <- c("start_year", "completion_year")
cd1 <- mice::complete(imp)[, vnames]
mis <- (is.na(imp$data$start_year) | is.na(imp$data$completion_year))
cd <- data.frame(mis = mis, cd1)
lattice::xyplot(completion_year ~ start_year,
                data = cd,
                groups = mis,
                xlab = "start_year",
                ylab = "completion_year",
                type = c("g","p"), 
                pch = c(1, 4),
                scales = list(alternating = 1, tck = c(1, 0)))

#Analyse complete data
fit <- with(imp, {
  data <- data.frame(as.list(environment()))
  data <- data[data$start_year > 2012 & data$start_year < 2022, c("country", "start_year", "corridor")]
  data <- mutate(data,
                 time = case_when(
                   start_year == 2013 ~ 1,
                   start_year == 2014 ~ 2,
                   start_year == 2015 ~ 3,
                   start_year == 2016 ~ 4,
                   start_year == 2017 ~ 5,
                   start_year == 2018 ~ 6,
                   start_year == 2019 ~ 7,
                   start_year == 2020 ~ 8,
                   start_year == 2021 ~ 9,
                   TRUE ~ 10)
  )
  data <- data %>%
    group_by(corridor, country, time) %>% 
    summarise(project_count = n())
  
  glmer.nb(project_count ~ corridor + (1 + time | country),
           control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
           data = data)
  
})

summary(pool(fit))

#Some warnings from glmer.nb. Random effects are small.
#There is not much variation in time/country.
#Difficult to estimate random effects.
data_comp_1<-complete(imp) #Data from 1. imputation
data_comp_1 <- data_comp_1[data_comp_1$start_year > 2012 & 
                             data_comp_1$start_year < 2022, c("country", "start_year", "corridor")]
data_comp_1 <- dplyr::mutate(data_comp_1,
                             time = case_when(
                               start_year == 2013 ~ 1,
                               start_year == 2014 ~ 2,
                               start_year == 2015 ~ 3,
                               start_year == 2016 ~ 4,
                               start_year == 2017 ~ 5,
                               start_year == 2018 ~ 6,
                               start_year == 2019 ~ 7,
                               start_year == 2020 ~ 8,
                               start_year == 2021 ~ 9,
                               TRUE ~ 10)
)

data_comp_1 <- data_comp_1 %>%
  dplyr::group_by(corridor, country, time) %>% 
  dplyr::summarise(project_count = n()) 

fit.nb.1 <- glmer.nb(project_count ~ corridor + (1 + time | country),
                     data = data_comp_1,
                     control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                     verbose = TRUE)
ranef(fit.nb.1)
ggplot(data_comp_1,aes(x = time, y = project_count, col = country)) + geom_jitter() + geom_boxplot(alpha = 0.2) + facet_wrap(~country)


#Map
drop_map <- c(1:5, 7:31)
keep_map <- !(1:length(all) %in% drop_map)
data_map <- rapd[, keep_map]
data_map <- data_map[data_map$status != "NULL",]
data_map <- data_map[data_map$status != "Cancelled",]
data_map <- data_map[data_map$status != "Decommissioned",]
data_map <- cc(data_map)
project_locations <- st_as_sf(data_map, coords = c("longitude", "latitude"), crs = 4326)
mapview(project_locations)
