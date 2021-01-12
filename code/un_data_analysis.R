# use dplyr for data cleaning

#load tidyverse library
library(tidyverse)
gapminder_data <- read_csv("data/gapminder_data.csv")
View(gapminder_data)

#get summary statistics
summarize(gapminder_data, averageLifeExp = mean(lifeExp))

#piping function: %>% to link lines of code
gapminder_data %>% summarize(averageLifeExp = mean(lifeExp)) 
gapminder_data %>% summarize(averagePopSize = mean(pop), 
                             recent_year = max(year))

# use filter function to select specific rows from dataset
gapminder_data %>%  filter(year == 2007) %>% summarize(averageLifeExp = mean(lifeExp))
gapminder_data %>% summarize(first_year = min(year)) #1952
gapminder_data %>% filter(year == 1952) %>% 
  summarize(avgGDPperCapita = mean(gdpPercap)) #3725

#use group_by function to find summary stats by group
gapminder_data %>% 
  group_by(year) %>%
  summarize(averageLifeExp = mean(lifeExp))

#find mean life expectancy for each continent
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(averageLifeExp = mean(lifeExp))

#mutate- add more columns to the dataset
gapminder_data %>% 
  mutate(gdp = gdpPercap*pop)
# make new column using mutate that is population in millions
gapminder_data %>% 
  mutate(pop_in_millions = pop/1000000)

#select() - specify which column we want to keep
gapminder_data %>% 
  select(year,pop)
#drop continent column and save
no_continent <- gapminder_data %>% 
  select(-continent)

View(gapminder_data) #adds back in continent column to whole dataset

#country, continent, year, life expectancy columns
gapminder_data %>% 
  select(country,year,continent,lifeExp)

#arrange() function can sort by by factor, ie year

# long (ie the UN data) vs. wide (one row and many data observations in the row) dataframes
#pivot_longer and pivot_wider functions in dplyr
gapminder_data %>% 
  select(country,year,continent,lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

#rename() - rename columns

#create a new dataset with only data from the Americas and 2007 then remove continent and year columns
gapminder_Americas_2007 <- gapminder_data %>% 
  filter(year == 2007, continent == "Americas") %>% 
  select(-continent, -year)

gapminder_data <- read_csv("data/gapminder_data.csv") %>% 
  filter(year == 2007, continent == "Americas") %>% 
  select(-continent, -year)

View(gapminder_data)


#open and clean co2 data
#goals: data from a year that is close to 2007 (we have 2005); column for country; columns for different types of CO2 emissions
#exercise: select on ly the country, year, series, value
co2_emissions <- read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region", "country", "year", "series",
                       "value", "footnotes", "source")) %>% 
  select(-region, -footnotes, -source) %>% 
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)"="total emissions",
                         "Emissions per capita (metric tons of carbon dioxide)"="per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year) %>% 
  mutate(country = recode(country,
                          "Bolivia (Plurin. State of)" = "Bolivia",
         "United States of America" = "United States",
         "Venezuela (Boliv. Rep. of)" = "Venezuela"))

View(co2_emissions)
#use inner_join to join datasets
inner_join(gapminder_data, co2_emissions, by = "country")
gapminder_data %>% inner_join(co2_emissions, by = "country")

#anti join tells what rows are in first data set but not second
anti_join(gapminder_data, co2_emissions) #some differences exist, including how countries are names

#change puerto rico to be a part of the US
gapminder_data <- read_csv("data/gapminder_data.csv") %>% 
  filter(year == 2007, continent == "Americas") %>% 
  select(-continent, -year) %>% 
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>% 
  group_by(country) %>% 
  summarize(lifeExp = sum(lifeExp*pop)/sum(pop),
            gdpPercap = sum(gdpPercap*pop)/sum(pop),
            pop = sum(pop))
View(gapminder_data)
#check changes
anti_join(gapminder_data, co2_emissions) #everything matches

gapminder_co2 <- inner_join(gapminder_data, co2_emissions, by = "country")

#mutate and the if_else
#if_else(condition, true, false) 

gap_co2_region <- gapminder_co2 %>% 
  mutate(region = if_else(country == "Canada" | 
                            country == "United States" | 
                            country == "Mexico", "north", "south"))

# | = or  
#&& = and
#! = not

#is there a relationship between gdp and co2?
# create a scatter plot of gdp vs. co2 emissions colored by region

ggplot(data = gap_co2_region) +
  aes(x=gdpPercap, y=per_capita_emissions, color=region) +
  labs( x="GDP Per Capita", y="Emissions") +
  geom_point()


?ggplot 


