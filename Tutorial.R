library(gapminder) # loading the data set

str(gapminder) 
class(gapminder) # checking if the data set is a data-frame
gapminder # since the data is a tibble we only print 5 rows
summary(gapminder) # summarise the data  
names(gapminder) # names of the columns
ncol(gapminder) # number of columns
length(gapminder) # number of columns
dim(gapminder) # gives the dimensions of the tibble
nrow(gapminder) # number of rows
plot(lifeExp ~ year, gapminder) # plots a scatter plot [years on x axis and life expectancy on the y axis]
plot(lifeExp ~ gdpPercap, gapminder) # plots a scatter plot [gross domestic product on x axis and life expectancy on y axis]
plot(lifeExp ~ log(gdpPercap), gapminder) # plots a scatter plot [log of gross domestic product on x axis and life expectancy on y axis]
head(gapminder$lifeExp) # prints the first 5 rows of the life expectancy column of gapminder data set
summary(gapminder$lifeExp) # summarises the life expectancy column of the gapminder data set
hist(gapminder$lifeExp) # plots a histogram of life expectancy column of the gapminder data set
summary(gapminder$year) # summarises the year variable
table(gapminder$year) # year is an integer variable but since it has only few values, it is treated as a categorical variable
class(gapminder$year) # gives the output as integer hence we can see it is not a categorical variable
class(gapminder$continent) # gives the output as factor
summary(gapminder$continent) #does not give a maximum and minimum value
table(gapminder$continent) #same output as summary as it is a categorical variable
barplot(table(gapminder$continent)) # plots a bar plot of the number of countries in each continent
levels(gapminder$continent) # a categorical variable has levels which gives us the categories
nlevels(gapminder$continent) # gives us the number of categories
levels(gapminder$year) # since it is not a categorical variable the return is NULL
nlevels(gapminder$year) # since it is not a categorical variable the return is 0 - this means no levels are present for the year variable

library(dplyr) # loading the dplyr library

filter(gapminder, lifeExp > 80) # filters to show only the observations where life expectancy is greater than 80
filter(gapminder, country == "Rwanda", year > 1979) # filters to show only the observations where the year is greater than 1979 and country is Rwanda
filter(gapminder, country == "India") # filters to show only the observations were the country is India
filter(gapminder, country %in% c("Rwanda", "Afghanistan")) # filters to show the observations where country is either Afghanistan or country is Rwanda
# Showing how we filter in base R
gapminder[gapminder$lifeExp > 80,] # this is a way to filter the observations without using the dplyr library to show the observations with life expectancy greater than 80
subset(gapminder, lifeExp > 80) # this is a way to filter the observations without using the dplyr library to show the observations with life expectancy greater than 80
subset(gapminder, country == "Rwanda") # this is way to filter the observations without using the dplyr library to show the observations with country being Rwanda
# should not subset excerpt <- gapminder[241:252,]
# not self documenting - does not tell us why we are sub-setting rows 241 to 252
# this code gives different results when the data is not sorted by country

gapminder %>% head() # head(gapminder) - takes the object on the left and applies the function head()
gapminder %>% head(3) # head(gapminder,3)
select(gapminder, year, lifeExp) #subsets the data on variables, filter subsets the data on observations
gapminder %>% select(year, lifeExp) # selects the columns year and lifeExp from the data set
gapminder %>% select(year, lifeExp) %>% head(4) # selects the columns year and lifeExp and shows the first 4 rows
head(select(gapminder, year, lifeExp), 4) # this selects the columns year and lifeExp and shows the first 4 rows
gapminder %>% filter(country == "Cambodia") %>% select(year, lifeExp) # this selects the columns year and lifeExp for Cambodia
# how to select the columns year and lifeExp for a Cambodia using base R
gapminder[gapminder$country == "Cambodia", c("year", "lifeExp")] # this selects the columns year and lifeExp for Cambodia

my_gap <- gapminder # keeping the original data untouched
my_gap %>% filter(country == "Canada")
country_canada = my_gap %>% filter(country == "Canada")

my_gap %>% mutate(gdp = pop*gdpPercap) # creating a column gdp from the column pop and gdpPercap but not making a permanent change
gdp_canada <- my_gap %>% filter(country == "Canada")
my_gap <- my_gap %>% mutate(tmp = rep(gdp_canada$gdpPercap, nlevels(country)), gdpPercap_canada = gdpPercap/tmp, tmp = NULL) # relative gdp of each country compared to the gdp of Canada
my_gap %>% filter(country == "Canada") %>% select(country, year, gdpPercap, gdpPercap_canada) # checking if the relative gdp of Canada is 1         
summary(my_gap$gdpPercap_canada) # checking the parameters of the relative gross domestic product

my_gap %>% arrange(year) # sorting the data set by year
my_gap %>% arrange(year, continent)  # sorting the data set by year and then by continent
my_gap %>% filter(year == 2007) %>% arrange(lifeExp)
my_gap %>% filter(year == 2007) %>% arrange(desc(lifeExp))
my_gap %>% rename(life_exp = lifeExp, gdp_per_capita = gdpPercap, gdp_per_capita_rel_Canada = gdpPercap_canada)
my_gap

my_gap %>% filter(country == "Burundi", year>1996) %>% select(yr = year, lifeExp, gdpPercap)
my_gap %>% filter(country == "Burundi", year>1996) %>% select(yr = year, lifeExp, gdpPercap) %>% select(gdpPercap, everything())
my_gap %>% group_by(continent) %>% summarise(n = n())
table(gapminder$continent)
str(table(gapminder$continent))
str(my_gap %>% group_by(continent) %>% summarise(n = n())) # can be used for further computation
my_gap %>% group_by(continent) %>% tally() # equivalent my_gap %>% group_by(continent) %>% summarise(n = n())
my_gap %>% count(continent) # count() function groups and tallies
my_gap %>% group_by(continent) %>% summarise(n = n(), n_contries = n_distinct(country))      
my_gap %>% group_by(continent) %>% summarise(n_countries = n_distinct(country))

my_gap %>% filter(year %in% c(1952, 2007)) %>% group_by(continent, year) %>% summarise_at(vars(lifeExp, gdpPercap), list(~mean(.),~median(.))) #summarises life expectancy and gdp per capita by continent and year
my_gap %>% filter(year %in% c(1952, 2007)) %>% group_by(continent) %>% summarise_at(vars(lifeExp, gdpPercap), list(~mean(.),~median(.))) # summarises life expectancy and gdp capita by continent
my_gap %>% group_by(continent) %>% summarise_at(vars(lifeExp, gdpPercap), list(~mean(.),~median(.)))
my_gap %>% group_by(continent, year) %>% summarise_at(vars(lifeExp, gdpPercap), list(~mean(.),~median(.)))

my_gap %>% filter(continent == "Asia") %>% group_by(year) %>% summarise(max_life_exp = max(lifeExp), min_life_exp = min(lifeExp))
# group_by() makes the functions operate within the groups that are mentioned 
# Example - group_by(country) makes the operations operate within the country group
my_gap %>% group_by(country) %>% select(country, year, lifeExp) %>% mutate(life_exp_gain = lifeExp - first(lifeExp))
asia <- my_gap %>% filter(continent == "Asia") %>% group_by(year)
asia %>% mutate(min_life_rank = min_rank(lifeExp), max_life_rank = min_rank(desc(lifeExp))) %>% filter(min_life_rank < 2 | max_life_rank < 2) %>% print(n = Inf)
my_gap %>% group_by(continent) %>% mutate(delta = lifeExp - lag(lifeExp)) %>% filter(continent == "Asia") %>% print(n = 20)
my_gap %>% group_by(continent, country) %>% mutate(delta = lifeExp - lag(lifeExp)) %>% filter(continent == "Asia") %>% print(n = 20)

my_gap %>% group_by(continent, country) %>% mutate(delta = lifeExp - lag(lifeExp)) %>% summarise(lowest_life_exp_change = min(delta, na.rm = TRUE)) %>% top_n(-1, wt = lowest_life_exp_change)
my_gap %>% group_by(continent, country) %>% mutate(delta = lifeExp - lag(lifeExp)) %>% summarise(lowest_life_exp_change = min(delta, na.rm = TRUE)) %>% mutate(lowest_life_exp = min_rank(lowest_life_exp_change)) %>% filter(lowest_life_exp == 1)
