library(tidyverse)
library(rvest)

#Downloading the Cincinnati Reds 2021 season data

url <- "https://www.baseball-reference.com/teams/CIN/2021.shtml"

tab <- h <- read_html(url)

tab <- h %>% html_nodes("table")

html_table(tab)

batting <- data.frame(html_table(tab[[1]]))
pitching <- data.frame(html_table(tab[[2]]))

str(batting)
str(pitching)

batting <- filter(batting, Rk != "Rk")
batting <- type_convert(batting)
str(batting)

pitching <- filter(pitching, Rk != "Rk")
pitching <- type_convert(pitching)
str(pitching)

batting%>%
  ggplot(aes(x = Age, y = SB))+
  geom_point()
