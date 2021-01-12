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
