library(tidyverse)
#load tidyverse packages
#help tab to access cheatsheets

#read in data
gapminder_1997 <- read_csv("data/gapminder_1997.csv")
# name of data  assign  read function("file")

#learn more about a function
?read_csv

read_csv(file = "data/gapminder_1997.csv")

#make a plot using ggplot and 1997 data
ggplot(data=gapminder_1997)
#need more information (Aesthetics)
ggplot(data=gapminder_1997,aes(x=gdpPercap, y=lifeExp, color = continent, size=pop/1000000)) +
  labs(x="GDP Per Capita", y="Life Expectancy", title = "Do people in wealthy countries live longer?", size="Population(in millions)") +
  geom_point() +
  scale_color_brewer(palette="Set1") 
#identify variables based on data and rename for graph, add title, add points, assign color based on continent, change color palette, show size of populations, change shape per continent
#make more concise
RColorBrewer::display.brewer.all()
#no quotes in reference to dataset columns in ggplot

gapminder_data <- read_csv("data/gapminder_data.csv")
#assign full dataset to the name "gapminder_data"
dim(gapminder_data) #tells dimensions of data
#use ggplot to plot data with year and life expectancy
ggplot(data=gapminder_data) + 
  aes(x=year, y=lifeExp, color=continent, group=country)+
  geom_line() #add in group to coordinate color by country

#plotting categorical variables
gapminder_1997 <- read_csv("gapminder_1997.csv")
View(gapminder_data)
#use gapminder_1997 data with geom_boxplot() to make boxplot where continent is the x axis and life expectancy is the y axis
ggplot(data=gapminder_1997, aes(x=continent, y=lifeExp, fill=continent)) +
  labs(x = "Continent", y = "Life Expectancy (Years)", title = "Life Expectancy by continent (1997)") +
  geom_jitter(aes(size=pop)) +
  geom_violin(alpha=0.5) #adjust transparency and color #use 'sample(colors(), size = 10)' to look for colors
#aes() is used in relation to data  

#univariate plots
ggplot(gapminder_1997) +
  aes(x=lifeExp) +
  geom_histogram(bins=20) +
  theme_classic() +
  theme(axis.text = element_text(angle=90, hjust=1, vjust=0.5))
  #adjust # of bins and theme as needed #change angle of x label

#saving plots (to un-report2 folder)
ggsave("figures/practice_plot.jpg", width=6, height=4)

#saving a different plot by storing plot as name
violin_plot <- ggplot(data=gapminder_1997, aes(x=continent, y=lifeExp, fill=continent)) +
  labs(x = "Continent", y = "Life Expectancy (Years)", title = "Life Expectancy by continent (1997)") +
  geom_jitter(aes(size=pop)) +
  geom_violin(alpha=0.5)

#updating name to hold multiple components
violin_plot + theme_bw()
violin_plot <- violin_plot + theme_bw()
violin_plot
ggsave("figures/practice_violin_plot.jpg", plot = violin_plot, width=6, height=4)

#faceting plots
ggplot(gapminder_1997) +
  aes(x=gdpPercap, y=lifeExp) +
  geom_point() +
  facet_wrap(vars(continent))
#vars() tells ggplot to use the column data

ggplot(gapminder_1997) +
  aes(x=gdpPercap, y=lifeExp) +
  geom_point() +
  facet_wrap(vars(continent))
my_awesome_plot <- ggplot(gapminder_1997) +
  aes(x=gdpPercap, y=lifeExp) +
  geom_point() +
  facet_wrap(vars(continent))
my_awesome_plot
ggsave("figures/my_awesome_plot.jpeg", plot=my_awesome_plot, width=6, height=4)
