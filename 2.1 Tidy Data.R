#Tidy data starts at line 8
#Reshaping data starts at line 36
#Separate and unite starts at line 81

library(tidyverse)
library(dslabs)

#Tidy data is defined by each row represeting one observation and each column representing a variable
#Gapminder is not considered "tidy" since it is in wide format
data(gapminder)

# create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
head(wide_data)
#We see each country gets one row, and each column is a year (not tidy)

#Can subset to see the first nine columns only
select(wide_data, country, `1960`:`1967`)

#Understand that long format is required for ggplot and other tidyverse functions!
#Wide data is helpful when you need descriptive statistics about time-invariant attributes

#This module uses "gather" instead of "pivot_longer" which is too bad!!

# tidy data from dslabs
library(dslabs)
data("gapminder")
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)

#We see this is our final target (each row contains one observation)
str(tidy_data)

# gather wide data to make new tidy data
new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)

#Recall unfavorable format of wide data
head(wide_data)

#Can reformat using gather
# gather all columns except country
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)

#There is one flaw in the gather function
# gather treats column names as characters by default
class(tidy_data$year)
class(new_tidy_data$year)

#convert = TRUE helps with this problem
# convert gathered column names to numeric
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

# ggplot works on new tidy data
new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# spread tidy data to generate wide data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)

#Sometimes, things can be a bit more complicated. We need the separate and unite functions

#This example shows unfavorable data
# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

#See how each column specifies year and two different metrics

# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

#We can use the separate() function to fix this problem
# separate on underscores
dat %>% separate(key, c("year", "variable_name"), "_")
#This shows a warning, since "life_expectancy" has an underscore
dat %>% separate(key, c("year", "variable_name"))

#We can add an additional variable that fills NA if there is no second word
# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                 fill = "right")

#We can also merge if there are extra pieces (this is probably the most desirable)
# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")

#In this case, we don't want this in "long" format with two variables separated in rows
#We need to spread so that we can have the variables separated

# separate then spread
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value) 

#EXAMPLE BELOW IS LESS EFFICIENT BUT USES THE unite() function to accomplish the same goal

# separate then unite
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_")

# full code for tidying data
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

str(co2)
head(co2)
plot(co2)

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- gather(co2_wide, month, co2, -year)
str(co2_tidy)

co2_tidy%>%
  ggplot(aes(as.numeric(month), co2, color = year))+
  geom_line()

data(admissions)
dat <- admissions %>% select(-applicants)

str(dat)

data_tidy <- spread(dat, gender, admitted)
str(data_tidy)
