#Web scraping starts on line 4
#Assessment starts on line

# import a webpage into R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
#We see here that the webpage is written as html code
h

#A node in HTML (<>) contains information about the code contained
#We can use the html_nodes() function to extract certain types of nodes
tab <- h %>% html_nodes("table")
#Can see there are three tables on the webpage
tab
#Extracting the second table
tab <- tab[[2]]
tab

#This next line of code uses the html_table() function to extract the table
tab <- tab %>% html_table
class(tab)
tab

#Finally, we set names to get the correct data
tab <- tab %>% setNames(c("state", "population", "total", "murders",
                          "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

#When basic HTML is not used, selectors are needed, and certain tools like SelectorGadget can help
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

guacamole <- list(recipe, prep_time, ingredients)
guacamole

get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 

get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")

url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"

h <- read_html(url)

tab <- h %>% html_nodes("table")

html_text(tab[[8]])
sapply(tab[1:4], html_table)
data.frame(html_table(tab[[1]]))
data.frame(html_table(tab[[2]]))
data.frame(html_table(tab[[3]]))
data.frame(html_table(tab[[4]]))

tab_1 <- html_table(tab[9])
tab_1.1 <- select(data.frame(tab_1), X2, X3, X4)
names(tab_1.1) = c("Team", "Payroll", "Average")
tab_1.1
tab_2 <- html_table(tab[18])
tab_2.1 <- select(data.frame(tab_2), X1, X2, X3)
names(tab_2.1) = c("Team", "Payroll", "Average")
tab_2.1

full <- full_join(tab_1.1, tab_2.1, "Team")
str(full)
full

url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

h <- read_html(url)

tab <- h %>% html_nodes("table")

html_table(tab, fill = TRUE)
