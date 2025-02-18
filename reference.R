library(tidyverse)


# Loading Data
population <- read_csv("population/population.csv")
ppp <- read_csv("ppp.csv")
lifexp <- read_csv("life expectancy/lifexp.csv")
fertility <-read_csv("fertility/fertility.csv")

# Summary
summary(ppp)
head(population)
colnames(ppp)
nrow(ppp)
ncol(ppp)

# Selecting features
ppp_country_year <- select(ppp, ref_area.label, time, obs_value)
fertility_2020 <- select(fertility, "Country Name", "2020")
ppp_country_year$ref_area.label
ppp_country_year['time']
# Selecting all except x,y,z
select(ppp_country_year, -time)
select(ppp_country_year, -c(time, ref_area.label))

# Filtering
pppBra2023 <- filter(ppp, ref_area.label == "Brazil", time == 2023)
pppBra200x <- filter(ppp, time %in% seq(2000,2023,1))
pppFra2020_2022 <- filter(ppp, ref_area.label == "France", time %in% c(2020,2022)) 

# NAs
colSums(is.na(ppp))

# Replacing NAs with a value
ppp_rm_na <- ppp %>% replace_na(list(note_source.label = "", y = "empty"))

# Replacing specific values
razil <- ppp %>% 
  mutate(ref_area.label = ifelse(ref_area.label == "Brazil", "Razil", ref_area.label))

# Counting distinct values
count(ppp_country_year, time)

# Ordering
arrange(ppp, desc(time))

# Adding new features
mutate(ppp_country_year, obs_value_x2 = obs_value * 2) #Keep everything
transmute(ppp_country_year, obs_value_x2 = obs_value * 2) #Keep only new features

# Grouping
ppp_group <- select(ppp, ref_area.label, time, "obs_value")
summarise(group_by(ppp_group, ref_area.label, time), mean_obs_value = mean(obs_value, na.rm = TRUE), n = n()) # n() returns the number of elements of current group

# Pipe operator
countriesB <- ppp %>%
  select(ref_area.label, time, obs_value) %>%
  filter(str_detect(ref_area.label, "^B.*")) %>%  #Using regex
  arrange(obs_value)


# Slicing
grades <- data.frame(student=c('s1', 's2', 's3', 's4'),
                     grade=c(4, 2, 10, 7),
                     situation=c("Fail", "Fail", "Pass", "Pass"))
students <- data.frame(student = c("s1", "s2", "s3", "s4"),
                       origin = c("Canada", "Mexico", "Brazil", "France"))

grades["student"] #Specific columns
grades[, 3] #All rows
grades[2,] #All columns
grades[1:2, 2:3] # Range of columns/rows
grades[grades$situation == "Pass", c("student", "situation")] #With a condition
subset(grades, grade >= 7, select=c(student, situation)) #Same as above but with subset


# Merging Datasets
school <- merge(grades, students, id = "student")

