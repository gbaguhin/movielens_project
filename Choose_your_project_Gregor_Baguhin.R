#install.packages("tidyverse")
library(tidyverse)
#install.packages("tidyr")
library(tidyr)
#install.packages("caret")
library(caret)


#Data Preparation:
# Loading the csv file into RStudio
url <- "D:/Desktop/Harvard Online/Data Science/Capstone/archive/cloth_size.csv"
clothsize_data <- read_csv(url)

# View first 6 rows
head(clothsize_data)

# inspect data properties
str(clothsize_data)

# inspect data for statistics
summary(clothsize_data)

# Find out how many rows with NAs
sum(is.na(clothsize_data))

# converting size to a factor
clothsize_data <- clothsize_data %>% mutate(size=as_factor(size))

# define the outcome and predictors
y <- clothsize_data$size
a <- clothsize_data$age
b <- clothsize_data$weight
c <- clothsize_data$height


# generate training and test sets
set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- clothsize_data[test_index, ]
train_set <- clothsize_data[-test_index, ]

# we look at the weight averages for each size
clothsize_data %>% group_by(size) %>%
  summarise(avg_wt = mean(weight), avg_ht = mean(height), avg_age = mean(age))



# Plotting probability of a size "M" vs weight
clothsize_data %>%
  mutate(x = round(weight)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(size == "M")) %>%
  ggplot(aes(x, prop)) +
  geom_point()

# Linear regression
# Let's test the probability that you wear size "M" if your weight is 58kgs
train_set %>%
  filter(round(weight)==58) %>%
  summarize(y_hat = mean(size=="M"))
# We see that a person who is 58kgs has a 50% probability of wearing a size "M"


# fit logistic regression model using weight as a predictor for size M, the size with
# the highest frequency in the dataset.

glm_fit <- train_set %>% 
  mutate(y = as.numeric(weight==58)) %>%
  glm(y ~ size, data=., family = "binomial")

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")








