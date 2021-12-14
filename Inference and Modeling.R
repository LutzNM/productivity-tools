#Inference and Modeling Section 1
#Video Example
library(tidyverse)
library(dslabs)
ds_theme_set()
take_poll(25)

#Inference and Modeling Section 2
#Choose mean value
X_hat <- 0.48
#Calculate SE of mean
se <- sqrt(X_hat*(1-X_hat)/25)
#Calculate probability of being within .01 of estimate
pnorm(0.01/se) - pnorm(-0.01/se)

p <- 0.45    # unknown p to estimate
N <- 1000

# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)

# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})
hist(x_hat)
min(x_hat)
max(x_hat)
mean(x_hat)
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)
#Remember that spread is 2p-1
#Why not just run a large poll?
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()

#Inference  and Modeling Section 3
#Code: Monte Carlo simulation of confidence intervals
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975)*SE_hat instead of 2*SE_hat.
p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean

#Code: Solving for 𝑧 with qnorm
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for i

#Code: Monte Carlo simulation
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975)*SE_hat instead of 2*SE_hat.
p<-.45
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

#Code: Confidence interval for the spread with sample size of 25
#Note that to compute the exact 95% confidence interval, we would use c(-qnorm(.975), qnorm(.975)) instead of 1.96.

N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)

#Code: Computing a p-value for observed spread of 0.02
N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))

#Assessment 3.1
# Load the data
data(polls_us_election_2016)

# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls<-filter(polls_us_election_2016, enddate >= "2016-10-31", state=="U.S.")

# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.

N<-polls_us_election_2016$samplesize[1]
N

# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat<-(polls_us_election_2016$rawpoll_clinton[1])/100
X_hat

# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat<-sqrt(X_hat*(1-X_hat)/N)
se_hat

# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- c(-qnorm(.975)*se_hat+X_hat, qnorm(.975)*se_hat+X_hat)

#Assignment 3.2
# The `polls` object that filtered all the data by date and nation has already been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.
pollster_results<-mutate(polls, X_hat=rawpoll_clinton/100, se_hat=sqrt(X_hat*(1-X_hat)/samplesize), lower=-qnorm(.975)*se_hat+X_hat, upper=qnorm(.975)*se_hat+X_hat)
pollster_results<-select(pollster_results, pollster, enddate, X_hat:upper)
head(pollster_results)

#Assignment 3.3
# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- mutate(pollster_results, hit=if_else(lower<=0.482 & upper>=0.482, 1, 0))
avg_hit<-summarize(avg_hit, average=mean(hit))

#Assignment 3.5
# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 


# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N<-polls$samplesize[1]
N

# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
polls<-mutate(polls, d_hat=((polls$rawpoll_clinton[1]-polls$rawpoll_trump[1]))/100)
polls$d_hat[1]

# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat<-(polls$d_hat[1]+1)/2
X_hat

# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat<-2*sqrt(X_hat*(1-X_hat)/N[1])
se_hat


# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- c(-qnorm(.975)*se_hat+d_hat[1], qnorm(.975)*se_hat+d_hat[1])
ci

#Exercise 3.6
# The subset `polls` data with 'd_hat' already calculated has been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.
polls<-mutate(polls, X_hat=(d_hat+1)/2, se_hat=2*sqrt(X_hat*(1-X_hat)/samplesize), lower=-qnorm(.975)*se_hat+d_hat, upper=qnorm(.975)*se_hat+d_hat)

pollster_results<-select(polls, pollster, enddate, d_hat, lower, upper)

#Exercise 3.7
# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value (0.021) exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- mutate(pollster_results, hit=if_else(lower<=0.021 & upper>=0.021, 1, 0))
avg_hit<-summarize(avg_hit, average=mean(hit))

#Exercise 3.8
# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)

# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster.
polls<-mutate(polls, errors=0.021-d_hat)
polls %>% ggplot(aes(x = pollster , y = errors)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Exercise 3.8
# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)

# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster, but only for pollsters who took 5 or more polls.
polls<-mutate(polls, errors=0.021-d_hat)

polls<-polls %>%
  group_by(pollster) %>%
  filter(n() >= 5)

polls %>% ggplot(aes(x = pollster , y = errors)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Section 4.1-Statistical Models
#Code: Simulating polls
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975)*SE_hat instead of 2*SE_hat.

d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

#Code: Calculating the spread of combined polls
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975) instead of 1.96.

d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)

#Section 4.3-Statistical Models
#Code: Generating simulated poll data
library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)

# keep only national polls from week before election with a grade considered reliable
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat

# compute margin of error
p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))

# histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

#Code: Investigating poll data and pollster bias
# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())

# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

#Section 4.5 Statistical Models
#Code 
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975) instead of 1.96.

# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)

#Exercise 4.1
# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Calculate the population average. Print this value to the console.
mean(x)

# Calculate the population standard deviation. Print this value to the console.
sd(x)

#Exercise 4.2
# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X<-sample(x, size=N, replace=TRUE)

# Calculate the sample average. Print this value to the console.
mean(X)

# Calculate the sample standard deviation. Print this value to the console.
sd(X)

#Exercise 4.3
# Define `se` as the standard error of the estimate. Print this value to the console.
se<-sd(X)/sqrt(N)
se

# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<-c(-qnorm(.975)*se+mean(X), qnorm(.975)*se+mean(X))

#Exercise 4,5
# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu

res<-replicate(B, {
  X<-sample(x, N, replace=TRUE)
  se<-sd(X)/sqrt(N)
  interval<-c(-qnorm(.975)*se+mean(X), qnorm(.975)*se+mean(X))
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)

#Exercise 4.6
# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster
polls%>%
  ggplot(aes(x=pollster, y=spread))+
  geom_boxplot()+
  geom_point()

#Exercise 4.13
# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma<- polls%>%
  group_by(pollster)%>%
  summarize(s=sd(spread))

# Print the contents of sigma to the console
sigma

#Exercise 4.15
# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res<-   polls%>%
  group_by(pollster)%>%
  summarize(avg=mean(spread), sd=sd(spread), num=length(spread))

# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate<-max(res$avg)-min(res$avg)

# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat<-sqrt(((res$sd[1]^2)/res$num[1])+((res$sd[2]^2)/res$num[2]))
se_hat

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<-c(-qnorm(.975)*se_hat+estimate, qnorm(.975)*se_hat+estimate)
ci

#Exercise 4.16
# Calculate the p-value
(1-pnorm(estimate, mean=0, sd=se_hat))*2

#Exercise 4.17
# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. Print the contents of this object to the console.

var<- polls%>%
  group_by(pollster)%>%
  summarize(avg=mean(spread), s=sd(spread))
var

#Section 5-Bayes
#Probability of A given B is Pr(B|A)*Pr(A)/Pr(B)

#Section 5.2
#Code: Monte Carlo simulation
prev <- 0.00025    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))

N_D <- sum(outcome == "Disease")    # number with disease
N_H <- sum(outcome == "Healthy")    # number healthy

# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))

table(outcome, test)

#Exercise 5.2
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Calculate the probability of both sons dying of SIDS. Print this value to the console.
Pr_B<-Pr_1 * Pr_2

#Exercise 5.4
# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000

# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50

# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
Pr_AB<-(Pr_BA*Pr_A)/Pr_B
Pr_AB

#Exercise 5.6
# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.

avg<-mean(polls$spread)
se<-sd(polls$spread)/sqrt(polls$samplesize)

results<-summarize(polls, avg=mean(spread), se=sd(spread)/sqrt(n()))

#Exercise 5.8
# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results`
sigma<-results$se

# Define a variable called `Y` that contains the average in the object `results`
Y<-results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B<-sigma^2/(sigma^2+tau^2)

# Calculate the expected value of the posterior distribution
B*mu+(1-B)*Y

#Exercise 5.9
# Compute the standard error of the posterior distribution. Print this value to the console.
sqrt(1/((1/sigma^2)+1/(tau^2)))

#Exercise 5.10
# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
E<-B*mu+(1-B)*Y
ci<-c(-(qnorm(.975)*se)+E, (qnorm(.975)*se)+E)
ci

#Exercise 5.11
# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0, exp_value, se)

#Exercise 5.12
# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0

p_calc<-function(tau){
  B<-sigma^2/(sigma^2+tau^2)
  se<-sqrt(1/((1/sigma^2)+1/(tau^2)))
  E<-B*mu+(1-B)*Y
  pnorm(0, E, se)
}

# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps<-sapply(taus, p_calc)

# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus, ps)

#Section 6.1
#Code: Definition of results object
#This code from previous videos defines the results object used for empirical Bayes election forecasting.

library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
#Code: Computing the posterior mean, standard error, credible interval and probability
#Note that to compute an exact 95% credible interval, we would use qnorm(.975) instead of 1.96.

mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

# 95% credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# probability of d > 0
1 - pnorm(0, posterior_mean, posterior_se)

#Section 6.2
#Code: Simulated data with 𝑋𝑗=𝑑+𝜖𝑗
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
#Code: Simulated data with 𝑋𝑖,𝑗=𝑑+𝜖𝑖,𝑗
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})
#Code: Simulated data with 𝑋𝑖,𝑗=𝑑+ℎ𝑖+𝜖𝑖,𝑗
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})
#Code: Calculating probability of 𝑑>0 with general bias
#Note that sigma now includes an estimate of the variability due to general bias 𝜎𝑏=.025.

mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)

#Section 6.3
#Code: Top 5 states ranked by electoral votes
#The results_us_election_2016 object is defined in the dslabs package:
  
  library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)

results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)
#Code: Computing the average and standard deviation for each state
results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

# 10 closest races = battleground states
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))
#Code: Calculating the posterior mean and posterior standard error
#Note there is a small error in the video code: B should be defined as sigma^2/(sigma^2 + tau^2).

mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))
#Code: Monte Carlo simulation of Election Night results (no general bias)
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clintonEV) %>%
  ggplot(aes(clintonEV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)
#Code: Monte Carlo simulation including general bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

#Section 6.4
#Code: Variability across one pollster
# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")
#Code: Trend across time for several pollsters
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)
#Code: Plotting raw percentages across time
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))

#Exercise 6.1
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions
cis<-polls%>%
  mutate(X_hat=(spread+1)/2, se=2*sqrt(X_hat*(1-X_hat)/samplesize), lower=-qnorm(.975)*se+spread, upper=qnorm(.975)*se+spread)

cis<-select(cis, state:grade, spread, lower, upper)
head(cis)

#Exercise 6.2
# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.


p_hits<-mutate(ci_data, hit = if_else(lower<=actual_spread & upper >= actual_spread, 1, 0))
p_hits<-summarize(p_hits, mean=mean(hit))
p_hits

#Exercise 6.3
# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.
p_hits<-mutate(ci_data, hit = if_else(lower<=actual_spread & upper >= actual_spread, 1, 0))
p_hits<-p_hits%>%
  group_by(pollster)%>%
  filter(n()>=5)%>%
  summarize(proportion_hits=mean(hit), n=n(), grade=grade[1])
p_hits<-arrange(p_hits, desc(proportion_hits))
p_hits

#Exercise 6.4
# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.
p_hits<-mutate(ci_data, hit = if_else(lower<=actual_spread & upper >= actual_spread, 1, 0))
p_hits<-p_hits%>%
  group_by(state)%>%
  filter(n()>=5)%>%
  summarize(proportion_hits=mean(hit), n=n())
p_hits<-arrange(p_hits, desc(proportion_hits))
p_hits

#Exercise 6.5
# Make a barplot of the proportion of hits for each state
p_hits%>%
  ggplot(aes(x=state, y=proportion_hits))+
  geom_bar(stat="identity")+
  coord_flip()

#Exercise 6.6
# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
cis<-mutate(cis, error=spread-actual_spread, hit = sign(spread)==sign(actual_spread))
errors<-cis
# Examine the last 6 rows of `errors`
tail(errors)

#Exercise 6.7
# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has 5 or more polls

p_hits<-errors%>%
  group_by(state)%>%
  filter(n()>=5)%>%
  summarize(proportion_hits=mean(hit), n=n())

# Make a barplot of the proportion of hits for each state
p_hits<-arrange(p_hits, desc(proportion_hits))
p_hits%>%
  ggplot(aes(x=state, y=proportion_hits))+
  geom_bar(stat="identity")+
  coord_flip()

#Exercise 6.8
# Generate a histogram of the error
hist(errors$error)

# Calculate the median of the errors. Print this value to the console.
median(errors$error)

#Exercise 6.9
# Create a boxplot showing the errors by state for polls with grades B+ or higher
grades<-c("A+", "A", "A-", "B+")
errors%>%
  filter(grade %in% grades)%>%
  ggplot(aes(x=state, y=error))+
  geom_boxplot()+
  geom_point()

#Exercise 6.10
# Create a boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher
grades<-c("A+", "A", "A-", "B+")
errors<-errors%>%
  filter(grade %in% grades)%>%
  group_by(state)%>%
  filter(n()>=5)%>%
  ungroup()

errors%>%
  ggplot(aes(x=state, y=error))+
  geom_boxplot()+
  geom_point()

#Section on t distribution
#Code: Calculating 95% confidence intervals with the t-distribution
z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)

#Exercise 6.11
# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.

1-pt(2, 3)+pt(-2, 3)

#Exercise 6.12
# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df<-seq(3, 50)

# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func<-function(x){
  1-pt(2, x)+pt(-2, x)
}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs<-sapply(df, pt_func)

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df, probs)

#Exercise 6.13
# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res<-replicate(B, {
  a<-sample(x, N, replace=TRUE)
  se_hat<-sd(a)/sqrt(N)
  interval<-c(-qnorm(.975)*se_hat+mean(a), qnorm(.975)*se_hat+mean(a))
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)

#Exercise 6.14
# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution
res<-replicate(B, {
  a<-sample(x, N, replace=TRUE)
  se_hat<-sd(a)/sqrt(N)
  interval<-c(-qt(.975, N-1)*se_hat+mean(a), qt(.975, N-1)*se_hat+mean(a))
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)

#Section 7.1
#Code: Research funding rates example
# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))
#Code: Two-by-two table and p-value for the Lady Tasting Tea problem
tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")

#Section 7.2
#Code: Chi-squared test
# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
  nbsp;   chisq.test()
chisq_test$p.value
#Code: Odds ratio
# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women
#Code: p-value and odds ratio responses to increasing sample size
# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()

#Exercise 7.1
# The 'errors' data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
grades<-c("A-", "C-")
totals<-filter(errors, grade %in% grades)
totals<-totals%>%
  group_by(grade, hit)%>%
  summarize(hits=n())

totals<-spread(totals, grade, hits)

# Print the proportion of hits for grade A- polls to the console
sum(totals[2, 3]/(totals[1, 3]+totals[2, 3]))

# Print the proportion of hits for grade C- polls to the console
sum(totals[2, 2]/(totals[1, 2]+totals[2, 2]))

#Exercise 7.2
# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test<-
  totals%>%
  select(-hit)%>%
  chisq.test()

# Print the p-value of the chi-squared test to the console
chisq_test$p.value

#Exercise 7.3
# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
C_right<-sum(totals[2, 2]/(totals[1, 2]+totals[2, 2]))
C_wrong<-sum(totals[1, 2]/(totals[1, 2]+totals[2, 2]))
odds_C<-sum(C_right/C_wrong)

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
A_right<-sum(totals[2, 3]/(totals[1, 3]+totals[2, 3]))
A_wrong<-sum(totals[1, 3]/(totals[1, 3]+totals[2, 3]))
odds_A<-sum(A_right/A_wrong)

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls

odds_A/odds_C

#Assessment
# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

total<-p*1500

se1<-sqrt(abs(total*(1-total)/1500))
se1

se <- sqrt(p*(1-p)/1500)
se

se*1500

se*2

d

d*(1-d)

se_hat<-2*sqrt(abs(d*(1-d)/1500))
se_hat

mean(brexit_polls$spread)
sd(brexit_polls$spread)

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)

mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

brexit_polls<-brexit_polls%>%
  mutate(lower=-qnorm(.975)*se+x_hat, upper=qnorm(.975)*se+x_hat)

june_polls<-brexit_polls%>%
  filter(enddate>="2016-06-01")

june_polls<-mutate(june_polls, se_x_hat=sqrt(x_hat*(1-x_hat)/samplesize))
june_polls<-mutate(june_polls, se_spread=se_x_hat*2)
june_polls<-mutate(june_polls, lower=-qnorm(.975)*se_spread+spread)
june_polls<-mutate(june_polls, upper=qnorm(.975)*se_spread+spread)

june_polls<-
  june_polls%>%
  mutate(contain0=if_else(june_polls$lower<=0 & june_polls$upper>=0, 1, 0), hit=if_else(june_polls$lower<=-.038 & june_polls$upper>=-.038, 1, 0))

june_polls%>%
  group_by(pollster)%>%
  summarize(sum=sum(hit), n=n(), mean=mean(hit))

june_polls%>%
  ggplot(aes(x=poll_type, y=spread))+
  geom_boxplot()

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)

combined_by_type<-
  combined_by_type%>%
  mutate(x_hat=(spread+1)/2, se_x_hat=2*sqrt(x_hat*(1-x_hat)/N))

combined_by_type<-
  combined_by_type%>%
  mutate(lower=-qnorm(.975)*se_x_hat+spread, upper=qnorm(.975)*se_x_hat+spread)

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

brexit_hit<-
  brexit_hit%>%
  group_by(poll_type, hit)%>%
  summarize(hits=n())

totals<-spread(brexit_hit, poll_type, hits)

chisq_test<-chisq.test(totals)
chisq_test$p.value

O_right<-sum(totals[2, 2]/(totals[1, 2]+totals[2, 2]))
O_wrong<-sum(totals[1, 2]/(totals[1, 2]+totals[2, 2]))
odds_O<-sum(O_right/O_wrong)

T_right<-sum(totals[2, 3]/(totals[1, 3]+totals[2, 3]))
T_wrong<-sum(totals[1, 3]/(totals[1, 3]+totals[2, 3]))
odds_T<-sum(T_right/T_wrong)

odds_O
odds_T
odds_O/odds_T

brexit_polls%>%
  ggplot(aes(x=enddate, y=spread, color=poll_type))+
  geom_smooth(method="loess")+
  geom_point()

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long%>%
  ggplot(aes(x=enddate, y=proportion, color=vote))+
  geom_smooth(method="loess", span=.3)
