library(tidyverse)
library("ggplot2")
library("sf")
library(RColorBrewer)

# Loading Data
df_popsize <- read_csv("data/population/population.csv")
df_ppp <- read_csv("data/GDP PPP/gdp_ppp.csv")
df_lifexp <- read_csv("data/life expectancy/lifexp.csv")
df_fertility <-read_csv("data/fertility/fertility.csv")

# Filtering


##############
# Population #
##############
# Population size from 2015 until 2022 per country
df_popsize_f <- df_popsize[,c("Country Name", "2015":"2022", "Country Code")] %>%
  pivot_longer(cols = !c("Country Name","Country Code"),
               names_to = "year",
               values_to = "pop_size")

###########
# GDP PPP #
###########
# from 2015 until 2022 per country
ppp_col_names = colnames(df_ppp) # Get df column names
year_features_index = grep('^\\d', ppp_col_names) #Get only indexes that start with numbers

for (index in year_features_index){   #Remove the text part extra of the year and convert to numerical
  old_col_name <- ppp_col_names[index]
  new_col_name <-  substring(ppp_col_names[index], 1, 4) # Keep only the number and convert to numerical
  ppp_col_names[index] <- new_col_name  # Update the list with the column names
}
colnames(df_ppp) <- ppp_col_names # Update dataset column names



df_ppp_f <- df_ppp %>% 
  filter(df_ppp$`Series Name` == "GDP per person employed (constant 2021 PPP $)") %>%
  select(c("Country Name","2015":"2020")) %>%
  pivot_longer(cols = !"Country Name",
               names_to = "year",
               values_to = "gdp_ppp")

# Converting gdp_ppp column from character to numerical

df_ppp_f$gdp_ppp[df_ppp_f$gdp_ppp == ".."] = NA #removing ".." character and replacing with NA
df_ppp_f <- transform(df_ppp_f, gdp_ppp = as.numeric(gdp_ppp))

###################
# Life expectancy #
###################
df_lifexp_f <- df_lifexp %>% 
  select(c("Country Name", "2015":"2022")) %>%
  pivot_longer(cols = !"Country Name",
               names_to = "year",
               values_to = "life_exp"
  )

#############
# Fertility #
#############
df_fertility_f <- df_fertility %>% 
  pivot_longer(
              cols = c("2015":"2022"),
              names_to = "year",
              values_to = "fert_rate"
            ) %>%
  select(`Country Name`, year, fert_rate)



####################
# Combine Datasets #
####################
df <- merge(df_lifexp_f, df_popsize_f, id = c("Country Name", "year"))
df <- merge(x = df, 
            y = df_ppp_f, 
            by.x = c("Country Name", "year"), 
            by.y = c("Country.Name","year"),
            all.x = TRUE, all.y = TRUE)
df <- merge(x = df, y = df_fertility_f, id = c("Country Name","year"), all.x = TRUE, all.y = TRUE)

############################
# Exporting Merged Dataset #
############################

write_csv(df,"data/merged_df.csv")


###########################################
###########################################
###########################################
###########################################
###########################################


#[1]
# Life expectancy in Brazil from 2018 until 2022
ggplot(data = df[df$`Country Name`=="Brazil", c("year", "life_exp")],
       aes(x = year, y = life_exp, label = life_exp, fill = life_exp))  +
  geom_col() +
  geom_text(vjust = 2, color = "white")

#[2]
# Population increase 2010 - 2020 for Brazil and Italy
ggplot(data = df[df$`Country Name` %in% c("Brazil", "Italy"),  c("year", "pop_size", "Country Name")],
       aes(x = year, y= pop_size, fill = `Country Name`)) +
  # Adding bars - dodge (will separate the 2 countries)
  geom_col(position = "dodge") + 
  #Adding lines - group will create one line per country. *0.8 change the data values and force the line to go down a bit
  geom_line(aes(y = pop_size*0.8, color = `Country Name`, group = `Country Name`), linewidth = 1) +
  # Manually change the pre-assigned colors
  scale_fill_manual(values = c("Brazil" = "salmon", "Italy" = "#008080")) +
  scale_color_manual(values = c("Brazil" = "#D03000", "Italy" = "#13D4D4"))



#[3]
# Top 3 life expectancy per year (2015 - 2022)
df_top_lifexp <- data.frame(year = c(2015:2022),
                            top1 = "tbd",
                            val1 = 0,
                            top2 = "tbd",
                            val2 = 0,
                            top3 = "tbd",
                            val3 = 0)
row_index = 1
for (yr in seq(2015,2022, 1)){
  print(yr)
  df_f <- df %>% 
    # Filter year
    filter(df$year == yr) %>%
    # Order highest lifexp
    arrange(desc(life_exp))
  # Add to dataset
  #top1 - val1
  df_top_lifexp[row_index,2] <- df_f[1, "Country Name"] 
  df_top_lifexp[row_index,3] <- df_f[1, "life_exp"] 

  #top2 - val2
  df_top_lifexp[row_index,4] = df_f[2, "Country Name"] 
  df_top_lifexp[row_index,5] = df_f[2, "life_exp"]
  #top3- val3
  df_top_lifexp[row_index,6] = df_f[3, "Country Name"] 
  df_top_lifexp[row_index,7] = df_f[3, "life_exp"]
  
  row_index = row_index + 1
    
}

# Prepare the dataset to pllot the graph
df_top_lifexp <- df_top_lifexp %>% 
  pivot_longer( cols = c(val1, val2, val3), # Pivoting value's columns
                               values_to = "val",
                               names_to = "val_cat") %>%
  mutate(country = case_when( #Adding the name of the top country in the right position 
    val_cat == "val1" ~ top1,
    val_cat == "val2" ~ top2,
    val_cat == "val3" ~ top3
  )) %>%
  select(year, val_cat, country, val) 

# Plot the bar chart
ggplot(df_top_lifexp, aes(x = year, y = val, fill = country)) +
  geom_col(position = "dodge", width = 0.7) + 
  labs(x = "Year", y = "Life Exp", fill = "Country") +
  scale_x_continuous(breaks=seq(2015,2022,1)) + # Making sure all the years are displayed
  coord_cartesian(ylim = c(75, 90)) #Limiting the range of y axis



# [4] Population growth of original BRICS
ggplot(data = df[df$`Country Name` %in% c("Brazil",
                                 "Russia", 
                                 "India", 
                                 "China", 
                                 "South Africa"),], 
       aes(x = year, y = pop_size, color = `Country Name`)) + 
  geom_line(aes(group = `Country Name`), size = 1)

# [5] Top 5 pop countries in 2020 with their fertility rate and average global fertility rate

# Retrieve global fertility rate average
global_avg_fertility <- mean(
                              df %>% 
                              filter(year == 2020) %>%
                              pull(fert_rate), #extract the columns as a vector
                        na.rm = TRUE)
global_avg_ppp <- mean(df[df$year == 2020,]$gdp_ppp, na.rm = TRUE)

# Filtering top5 fertility rate in 2020
df_top5 <- df %>%
  filter(df$year == 2020, df$gdp_ppp >0) %>%
  select("Country Name", "pop_size", "fert_rate", "gdp_ppp") %>%
  arrange(fert_rate)

# Lower fertility rate
ggplot(data = df_top5  %>% head(5), 
       aes(x = `Country Name`, fill = fert_rate, y = gdp_ppp, label = fert_rate)) +
  geom_col(position = "dodge") +
  geom_line(aes(y = global_avg_ppp, group = global_avg_ppp, color = "PPP Global AVG"), size = 1, linetype = "dashed") +
  labs(y = "GDP PPP" , color = "", fill = "Fertility rate")

# Highest fertility rate
ggplot(data = df_top5 %>% arrange(desc(fert_rate)) %>% head(5), 
       aes(x = `Country Name`, fill = fert_rate, y = gdp_ppp, label = fert_rate)) +
  geom_col(position = "dodge") +
  geom_line(aes(y = global_avg_ppp, group = global_avg_ppp, color = "PPP Global AVG"), size = 1, linetype = "dashed") +
  labs(y = "GDP PPP" , color = "", fill = "Fertility rate")

# [6] Plotting map
# Geometry info got from basemap dataset from ggplot2
world <- ne_countries(scale = "medium", returnclass = "sf")
# Customize with needed info
df_world <- merge(x = df, y=select(world, c("iso_a3", "geometry")), by.x = "Country Code", by.y = "iso_a3")

# Plot the map
ggplot(df_world) +
  geom_sf(aes(geometry = geometry, fill = fert_rate)) +
  scale_fill_viridis_c(option = "rocket", direction = -1) + #Custom color pallet
  ggtitle("World fertility rate") +
  theme(plot.title = element_text(hjust = 0.5)) #To centralize the title
