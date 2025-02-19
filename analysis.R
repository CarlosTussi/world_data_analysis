library(tidyverse)



# Loading Data
df_popsize <- read_csv("data/population/population.csv")
df_ppp <- read_csv("data/GDP PPP/gdp_ppp.csv")
df_lifexp <- read_csv("data/life expectancy/lifexp.csv")
df_fertility <-read_csv("data/fertility/fertility.csv")

# Filtering

# Population size from 2015 until 2022 per country
df_popsize_f <- df_popsize[,c("Country Name", "2015":"2022")] %>%
  pivot_longer(cols = !"Country Name",
               names_to = "year",
               values_to = "pop_size")

###########
# GDP PPP #
###########
# from 2015 until 2022 per country
ppp_col_names = colnames(df_ppp) # Get df column names
year_features_index = grep('^\\d', ppp_col_names) #Get oonly indexes that start with numbers

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
  


###################
# Life expectancy #
###################
df_lifexp_f <- df_lifexp %>% 
  select(c("Country Name", "2015":"2022")) %>%
  pivot_longer(cols = !"Country Name",
               names_to = "year",
               values_to = "life_exp"
  )

# Combine Datasets
df <- merge(df_lifexp_f, df_popsize_f, id = c("Country Name", "year"))
df <- merge(x = df, 
            y = df_ppp_f, 
            by.x = c("Country Name", "year"), 
            by.y = c("Country Name","year"),
            all.x = TRUE, all.y = TRUE) %>% 
  rename("ppp$" = "gdp_ppp")




###########################################
###########################################
###########################################
###########################################
###########################################

# Life expectancy in Brazil from 2018 until 2022
ggplot(data = df[df$`Country Name`=="Brazil", c("year", "life_exp")],
       aes(x = year, y = life_exp, label = life_exp))  +
  geom_col() +
  geom_text(vjust = 2, color = "white")

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

