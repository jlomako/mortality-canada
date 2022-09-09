######################################################################
#
# Weekly Death Counts Canada : create csv file
#
######################################################################

# data downloaded from Statistics Canada on 9-9-2022:

# Table 13-10-0768-01 Weekly death counts, by age group and sex
# DOI: https://doi.org/10.25318/1310076801-eng
# https://www150.statcan.gc.ca/n1/tbl/csv/13100768-eng.zip

# population data incl age, sex, geo, year
# Table: 17-10-0005-01 (formerly CANSIM 051-0001)
# Table 17-10-0005-01  Population estimates on July 1st, by age and sex
# DOI: https://doi.org/10.25318/1710000501-eng


library(dplyr)
library(stringr)

# get mortality data
data <- vroom::vroom("13100768-eng/13100768.csv", show_col_types = FALSE)
data <- data.frame(data)

# select and rename columns: geo, date, age, deaths (=number of deaths)
data <- data %>% 
  select(geo = GEO, date = REF_DATE, age = Age.at.time.of.death, sex = Sex, deaths = VALUE)

# clean up and convert variables
data$geo <- str_replace(data$geo, ", place of occurrence", "")
data$geo <- as.factor(data$geo)
levels(data$geo)

data$age <- str_replace(data$age, "Age at time of death, ", "")
data$age <- as.factor(data$age)
levels(data$age) # 0-44  45-64  65-84  85+  all ages

data$sex <- as.factor(data$sex)
levels(data$sex) # Both sexes  Females  Males

# convert date
data$date <- as.Date(data$date)

# extract years and weeks
data$year <- format(data$date, "%Y")
data$week <- format(data$date, "%U")


####
# get population data and transform
population <- vroom::vroom("13100768-eng/17100005.csv", show_col_types = FALSE)
population <- data.frame(population)

population <- population %>% 
  select(geo = GEO, year = REF_DATE, sex = Sex, ages = Age.group, pop = VALUE) %>%
  filter(year >= 2010) %>%
  filter(str_detect(ages,"(to|over|Median|Average)", T)) %>% # omit rows containing..
  mutate(ages = as.numeric(str_replace(ages, "( years| year)", ""))) %>% # delete string
  # create age groups
  mutate(age = ifelse(ages > 84, "85 years and over", 
                      ifelse(ages > 64, "65 to 84 years",
                             ifelse(ages > 44, "45 to 64 years",
                                    ifelse(ages <= 44, "0 to 44 years", "all ages"))))) %>% 
  # group by and summarize population
  group_by(geo, year, sex, age) %>% 
  summarise(pop = sum(pop))

# replace NA with "all ages"
population$age <- population$age %>% tidyr::replace_na("all ages")

# join data and population
population$year <- as.character(population$year)
data <- data %>% 
  left_join(population, by = c("geo", "year", "sex", "age")) 

# write file
write.csv(data, file = "data.csv", row.names = FALSE)


