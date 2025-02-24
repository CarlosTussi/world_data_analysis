library(tidyverse)
library(forcats)

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


# String
start_with_numbers = "^\\d"
grepl(start_with_numbers, "423432aosdjfdsifj")


#####################################
# Factors (- categorical variables) #
#####################################

months_lvl <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Ago","Sep" ,"Oct", "Nov", "Dec")
unord_months <- c("Oct", "May", "Sep", "Dec", "Feb", "Apr","Jan")
months_factor <- factor(unord_months, levels = months_lvl)
sort(months_factor)


rain <- c(12,45,65,23,43,76,12, 54,98,12,32,4)
months <- factor(months_lvl, months_lvl)
rain_df <- data.frame(months, rain) #Creating toy dataset with a dummy factor (!)
# Without reordering
ggplot(rain_df, aes(x = rain, y = months_lvl)) +
  geom_point()

# Reorder (fct_reorder)
ggplot(rain_df, aes(x = rain, y = fct_reorder(months_lvl,rain))) +
  geom_point()

# Relevel (fct_relevel) - putting elements "Dec", "Jan", "Jun", "Jul" in the front
ggplot(rain_df, aes(x = rain, y = fct_relevel(months_lvl, c("Dec", "Jan", "Jun", "Jul")))) + 
  geom_point()


# Recoding - Changing levels
rain_df_v2 <- rain_df %>% mutate(months = fct_recode(
  months_lvl,
  "January" = "Jan",      # Changing Jan and Dec only
  "December" = "Dec",
  "Summer" = "Jul",       # Also grouping them
  "Summer" = "Ago", 
  "Summer" = "Sep"
))

# Colapsing - Simillar as recoding, but useful in combining a lot of levels
rain_df_v2 <- rain_df_v2 %>% mutate(season = fct_collapse(
  months,
  winter = c("January", "Feb", "Mar"),
  spring = c("Apr", "May", "Jun"),
  summer = c("Summer"),
  autumn = c("Oct", "Nov","December")
))


# fct_lump(feat, n) - Create n groups. The last one is a combination of the smallest groups. "Other" 

