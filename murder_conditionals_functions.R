library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000
sort(murder_rate)

#which states, if any, have a murder rate lower than 0.5 per 100,000.
ind_2 <- which.min(murder_rate)

if(murder_rate[ind_2] < 0.5) {
  print(murders$state[ind_2])
} else{
  print("No state has murder rate that low")
}

#Examine better approaches to the above code

#got this code wrong, trying use for loop to generate names of states that have murder_rate lower than 0.5
for (i in murder_rate) {
  if(murder_rate < 0.5) {
    print(murders$state)
  } else{
    print("No state has murder rate that low")
  }
}

search()

#The tidyverse
#Using mutate, filter
install.packages("tidyverse")
library(tidyverse)
library(dslabs,)
data("murders")
#mutate
murders <- mutate(murders, rate = round(total / population * 100000, 2))
murders <- mutate(murders, pop_in_mil = population / 10^6)
#filter
filter(murders, rate<= 0.71)
#select
new_table <- select(murders, abb, region, rate, pop_in_mil)
filter(new_table, rate <= 0.71)
murders <- mutate(murders, rank = rank(-rate))

select(murders, state, population) %>% head()
select(murders, state, population)

filter(murders, state == "New York")

mutate(murders, rank = rank(-rate))
rank <- murders$rank
#Trying to filter the first 5 highest rank - Error
filter(murders, rank = rank(-rate, 1, 5))

#using %in% to filter with dplyr
filter(murders, state %in% c("New York", "Texas"))


#pipe operator
murders %>% select(state, region, rate) 
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)
murders %>% select(state, region, rate, rank) %>% filter(rank <= 5)


#Let's use summarize
library(dplyr)
library(dslabs)
data(heights)

s <- heights %>%
  filter(sex == "Female") %>%
  summarize(average = mean(height), standard_deviation = sd(height))
s

murders <- murders %>% mutate(rate = total/population*100000)
summarize(murders, mean(rate))

us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population) * 100000)
us_murder_rate

#Multiple summaries
heights %>% 
  filter(sex == "Female") %>%
  summarize(median_min_max = quantile(height, c(0.5, 0, 1)))

median_min_max <- function(x) {
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
}
heights %>%
  filter(sex == "Female") %>%
  summarize(median_min_max(height))








