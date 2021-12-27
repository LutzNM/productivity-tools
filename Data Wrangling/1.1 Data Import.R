#Check working directory
getwd()

#Change working directory (can't do this with github)
#setwd()

#View external data using this command
path <- system.file("extdata", package = "dslabs")
path
list.files(path)

#Can move these into our folder
filename <- "murders.csv"
filename2 <- "2010_bigfive_regents.xls"
fullpath <- file.path(path, filename)
fullpath2 <- file.path(path, filename2)
fullpath
fullpath2

file.copy(fullpath, getwd())
file.copy(fullpath2, getwd())
#Murders is now in our folder

#Can see this is in our folder now
file.exists(filename)

#Readr and Readxl help when we have different file types
#R can read txt, csv, and tsv files easily
#Readxl helps with xls, xlsx, etc.

library(readr)
library(readxl)

#The function excel_sheets tells us the names of sheets in an excel file
excel_sheets("2010_bigfive_regents.xls")

#The function read_lines shows us the first few lines of a file
read_lines("murders.csv", n_max = 3)

#Note the subtle difference between read.csv (Base R) and read_csv (tidyverse)
df <- read_csv("murders.csv")
df2 <- read.csv("murders.csv")

str(df) #Reads as tibble
str(df2) #Reads as dataframe

#EdX warns that strings are sometimes automatically converted to factors (I didn't experience this)
#The command "stringsAsFactors = FALSE" fixes this

#Can also read information directly from the internet
#Can use read.csv with a url
#Can use download.file

#tempdir and tempfile are helpful for temporarily pulling data from online

#Link from github
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"

#Can read using url
dat <- read_csv(url)

#Can download as well
download.file(url, "murders.csv")

#Make a temporary file name
tempfile()
tmp_filename <- tempfile()

#Download again but with temporary name
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)

#Delete file once it is in R as "dat"
file.remove(tmp_filename)

#From assessment
url2 <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"

download.file(url2, "assessment.csv")

df <- read_csv("assessment.csv", col_names = FALSE)
head(df)
str(df)
