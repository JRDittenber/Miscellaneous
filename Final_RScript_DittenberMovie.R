########################################################
# Dittenber MovieLens R_Script                         #  
# Harvard EdX Data Science Professional Certification  #
# Capstone                                             #
########################################################

##########################################################
# Import Libraries 
##########################################################

# Note: this process could take a couple of minutes
options(repos = list(CRAN="http://cran.rstudio.com/"))
install.packages('plyr', repos ="http://cran.us.r-project.org" )
library(plyr)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(rafalib)) install.packages("rafalib", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(formatR)) install.packages("formatR", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")



library(tidyr)
library(dslabs)
library(tidyverse)
library(caret)
library(data.table)
library(psych)
library(rafalib)
library(tinytex)
library(formatR)
library(rpart)
library(ggplot2)
library(nnet)
library(randomForest)
library(dplyr)
library(glmnet)



##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#######################################################
#Exploratory Data Analysis & Preprocessing
#######################################################

#examine variables 
vars <- tibble(name = names(edx), class = class(edx))
vars


#deal with timestamp 
new_edx <- edx %>%  mutate(date_rated = as.POSIXct(timestamp, origin = "1970-01-01", tz="UTC"),
                           year_rated = format(date_rated, "%Y"))

#Verify that dates are converted correctly 

head(new_edx)

#check the data types of the mutated data set

vars <- tibble(name = names(new_edx), class = class(new_edx))
vars


#check the columns for date_rated and year_rated 

min(new_edx$date_rate); min(new_edx$year_rate); max(new_edx$date_rate); max(new_edx$year_rate)

#don't need timestamp now 

new_edx$timestamp <- NULL

#check for NA/Nulls by comparing lengths after applying na.omit()

#create new dataframe after applying na.omit -this will remove observations with NA
check_df <- na.omit(new_edx)
length(check_df$rating); length(new_edx$rating)

rm(check_df)

################################################################################
#Variable Analysis
###############################################################################


#userId 

#length of the variables 

length(new_edx$userId)

#unique users 

n_distinct(new_edx$userId)


#users sorted by rating count 

new_edx %>%  group_by(userId) %>% count() %>% arrange(desc(n))

#check for any ratings of 0

new_edx %>%  group_by(userId) %>% count() %>%  arrange(n)

#examine the distribution of movies rated per user 

new_edx %>% 
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = 'black') + 
  scale_x_log10() +
  ggtitle("Distribution of Users") +
  xlab("Number of Movies Rated") +
  ylab("Number of Users")  +
  theme_minimal()


#Ratings 


#count  number of ratings for each rating
rating_counts <- new_edx %>%
  group_by(rating) %>%
  summarise(count = n())

#bar chart
ggplot(rating_counts, aes(x = rating, y = count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.2, size = 3.5) +
  ggtitle("Distribution of Ratings") +
  xlab("Rating Scale: 0 to 5 with increments of 0.5") +
  ylab("Number of Ratings") +
  theme_minimal()


#count of each rating level 
rating_counts %>% knitr::kable()


#boxplot for ratings per year

ratings_by_year <- new_edx %>%
  group_by(year_rated, movieId) %>%
  summarise(num_ratings = n()) %>%
  mutate(sqrt_num_ratings = sqrt(num_ratings))

#create the plot
ggplot(ratings_by_year, aes(x = year_rated, y = sqrt_num_ratings)) +
  geom_boxplot() +
  ggtitle("Number of Ratings for Each Movie by Year") +
  xlab("Year") +
  ylab("Number of Ratings (sqrt transformed)") +
  theme_minimal()


#over time per week 
ratings_by_week <- new_edx %>%
  mutate(week = lubridate::round_date(date_rated, "week")) %>%
  group_by(week) %>%
  summarise(mean_rating = mean(rating))

#create the plot
ggplot(ratings_by_week, aes(x = week, y = mean_rating)) +
  geom_line() +
  geom_smooth(method = 'loess')+
  ggtitle("Average Rating by Week") +
  xlab("Week") +
  ylab("Average Rating") +
  theme_minimal()

#Genres 

#examine genres as grouped 

#total length
length(new_edx$genres)

#distinct genres combinations
n_distinct(new_edx$genres)

#sort by rating count 
new_edx %>% 
  group_by(genres) %>%
  count()          %>%
  arrange(desc(n))

#sort by highest average rating (note that rating is categorical, yet we can still apply this)
new_edx %>% group_by(genres) %>% summarize(mean_rating = mean(rating),                                      sd_rating= sd(rating)) %>%       
  arrange(desc(mean_rating)) 

#sort by lowest average rating (note that rating is categorical, yet we can still apply this)
new_edx %>%  group_by(genres) %>% summarize(mean_rating = mean(rating),                                       sd_rating= sd(rating)) %>%                                                      arrange(mean_rating)


#separate the combinations of genres
genres_edx <- new_edx %>% separate_rows(genres, sep = "\\|")
head(genres_edx)

str(genres_edx)

#determine how many observations 
length(genres_edx$genres)


#distinct genres when split 
n_distinct(genres_edx$genres)


#sort by highest average rating (note that rating is categorical, yet we can still apply this)
genres_edx %>% group_by(genres) %>% summarize(mean_rating = mean(rating),                                       sd_rating= sd(rating),                                                         count = n()) %>% arrange(desc(mean_rating)) 


#sort by highest count
genres_edx %>% group_by(genres) %>% summarize(mean_rating = mean(rating),                                       sd_rating= sd(rating),                                                          count = n())%>% arrange(desc(count)) 

#sort by lowest average rating (note that rating is categorical, yet we can still apply this)
genres_edx %>% group_by(genres) %>% summarize(mean_rating = mean(rating), sd_rating= sd(rating)) %>%   arrange(mean_rating) 


#filter the movies with at least 50 ratings
movies_50 <- new_edx %>%
  group_by(movieId) %>%
  filter(n() >= 50) %>%
  ungroup()


#count the number of ratings for each genre
genre_counts <- movies_50 %>%
  group_by(genres) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


#calculate the average rating for each genre
genre_ratings <- new_edx %>%
  group_by(genres) %>%
  summarise(average_rating = mean(rating)) %>%
  arrange(desc(average_rating)) %>%
  head(3)

#bar chart
ggplot(genre_ratings, aes(x = genres, y = average_rating)) +
  geom_col() +
  geom_text(aes(label = round(average_rating, 2)), vjust = -0.2, size = 3.5) +
  ggtitle("Top 3 Genres by Average Rating") +
  xlab("Genres") +
  ylab("Average Rating") +
  theme_minimal()


#Separate the Genres 

#calculate the average rating for each genre
genre_ratings <- genres_edx %>%
  group_by(genres) %>%
  summarise(mean_rating = mean(rating), sd_rating= sd(rating), count = n()) %>%
  arrange(desc(mean_rating)) %>%
  head(3)

#create bar chart
ggplot(genre_ratings, aes(x = genres, y = mean_rating)) +
  geom_col() +
  geom_text(aes(label = round(mean_rating, 2)), vjust = -0.2, size = 3.5) +
  ggtitle("Top 3 Indiviudal Genres by Average Rating") +
  xlab("Genre") +
  ylab("Average Rating") +
  theme_minimal()

#########################################################
#Initial Analysis Through Sampling 
#########################################################

#Train and test 
set.seed(42)

test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Multiple Linear Regression 


#sampling for baseline RMSE 
rmse_calc <- function(df, formula) {
  model <- lm(formula, data = df)
  sqrt(mean((predict(model) - df$rating)^2))
}

# sample data 1000 times and calculate RMSE for each model
rmse_samples <- data.frame(method = c("User Id Only", "User Id and Movie Id ", "User Id, Movie Id, Genres ", "Userd Id, Movie Id, Genres, Title"), 
                           RMSE = sapply(1:4, function(i) {
                             formulas <- c("rating ~ userId", "rating ~ userId + movieId", "rating ~ userId + movieId + genres", "rating ~ userId + movieId + genres + title")
                             mean(replicate(1000, rmse_calc(sample_n(train_set,77, replace = TRUE), formulas[i])))
                           }))

#print RMSE values
rmse_samples %>%  knitr::kable()


#run on the test set 


#sample data 1000 times and calculate RMSE for each model
rmse_samples <- data.frame(method = c("User Id Only", "User Id and Movie Id ", "User Id, Movie Id, Genres ", "Userd Id, Movie Id, Genres, Title"), 
                           RMSE = sapply(1:4, function(i) {
                             formulas <- c("rating ~ userId", "rating ~ userId + movieId", "rating ~ userId + movieId + genres", "rating ~ userId + movieId + genres + title")
                             # Sample the data 1000 times, calculate the RMSE for each sample, and return the mean RMSE value
                             mean(replicate(1000, rmse_calc(sample_n(test_set,77, replace =TRUE), formulas[i])))
                           }))

#print RMSE values
knitr::kable(rmse_samples)

#Decision Tree Model 


#sample data 1000 times and calculate RMSE for each model
rmse_samples <- data.frame(method = c("User Id Only", "User Id and Movie Id ", "User Id, Movie Id, Genres ", "Userd Id, Movie Id, Genres, Title"), 
                           RMSE = sapply(1:4, function(i) {
                             formulas <- c("rating ~ userId", "rating ~ userId + movieId", "rating ~ userId + movieId + genres", "rating ~ userId + movieId + genres + title")
                             mean(replicate(1000, rmse_calc(sample_n(train_set, 77, replace = TRUE), formulas[i])))
                           }))

#print RMSE values
rmse_samples |> knitr::kable()

#run on test set 

#sample data 1000 times and calculate RMSE for each model
rmse_samples <- data.frame(method = c("User Id Only", "User Id and Movie Id ", "User Id, Movie Id, Genres ", "Userd Id, Movie Id, Genres, Title"), 
                           RMSE = sapply(1:4, function(i) {
                             formulas <- c("rating ~ userId", "rating ~ userId + movieId", "rating ~ userId + movieId + genres", "rating ~ userId + movieId + genres + title")
                             mean(replicate(1000, rmse_calc(sample_n(test_set, 77, replace = TRUE), formulas[i])))
                           }))

#print RMSE values
rmse_samples %>% knitr::kable()

#Random Forest 

#sample data 1000 times and calculate RMSE for each model
rmse_samples <- data.frame(method = c("User Id Only", "User Id and Movie Id ", "User Id, Movie Id, Genres ", "Userd Id, Movie Id, Genres, Title"), 
                           RMSE = sapply(1:4, function(i) {
                             formulas <- c("rating ~ userId", "rating ~ userId + movieId",  "rating ~ userId + movieId + genres", "rating ~ userId + movieId + genres + title")
                             mean(replicate(1000, rmse_calc(sample_n(train_set, 77,  replace=TRUE), as.formula(formulas[i]))))
                           }))

#print RMSE values
rmse_samples |> knitr::kable()


#run on the test set 

#sample data 1000 times and calculate RMSE for each model
rmse_samples <- data.frame(method = c("User Id Only", "User Id and Movie Id ", "User Id, Movie Id, Genres ", "Userd Id, Movie Id, Genres, Title"), 
                           RMSE = sapply(1:4, function(i) {
                             formulas <- c("rating ~ userId", "rating ~ userId + movieId",  "rating ~ userId + movieId + genres", "rating ~ userId + movieId + genres + title")
                             mean(replicate(1000, rmse_calc(sample_n(test_set, 77,  replace=TRUE), as.formula(formulas[i]))))
                           }))

#print RMSE values
rmse_samples |> knitr::kable()

###################################################################
#Expand on the model from the course
###################################################################

#clear the environment to free up memory
rm(list = setdiff(ls(), c('edx', 'final_holdout_test')))


#create new test/train set
set.seed(42)

test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Create a model and add predictors one by one 

mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)



# fit <- lm(rating ~ as.factor(userId), data = movielens)

mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))


predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i



model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

rmse_results %>% knitr::kable()



# lm(rating ~ as.factor(movieId) + as.factor(userId))

user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()



# lm(rating ~ as.factor(movieId) + as.factor(userId)) + genres



genre_avgs <- test_set %>% 
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_u)) 


predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by = 'genres') %>%
  mutate(pred = mu + b_u + b_g) %>%
  .$pred


model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model + Genres",  
                                     RMSE = model_3_rmse ))


rmse_results %>% knitr::kable() 

######################################################################
#Demonstrate Relationship Between Sample Size and Accuracy 
######################################################################


mse <- function(predictions, actual) {
  # Calculate the squared error for each prediction
  squared_error <- (predictions - actual)^2
  
  # Calculate the mean squared error
  mean(squared_error)
}




new_edx <- edx %>%  mutate(date_rated = as.POSIXct(timestamp, origin = "1970-01-01", tz="UTC"),
                           year_rated = format(date_rated, "%Y"))


data <- new_edx


data <- data[, -c(6, 7)]


set.seed(123) 
train_indices <- createDataPartition(data[, 3], p = 0.8, list = FALSE)
train <- data[train_indices, ]
test <- data[-train_indices, ]


iterations <- 10


accuracy <- numeric(iterations)

#loop over iterations
for (i in 1:iterations) {
  n <- 100
  
  sample_indices <- sample(1:nrow(train), size = n)
  sample_train <- train[sample_indices, ]
  
  #train over a model
  model <- randomForest(x = sample_train[, -3], y = sample_train[, 3])
  
  
  predictions <- predict(model, test)
  
  
  accuracy[i] <- mse(predictions, test[, 3])
  
  
  n <- n + 100
}


sample_sizes <- seq(100, 100*iterations, by = 100)


plot(accuracy ~ sample_sizes, type = "l")

#################################################
#Run on the Final Hold Out Data Set
################################################
rm(list = setdiff(ls(), c('final_holdout_test')))

set.seed(42)

mu_hat <- mean(final_holdout_test$rating)
mu_hat

naive_rmse <- RMSE(final_holdout_test$rating, mu_hat)
naive_rmse

predictions <- rep(2.5, nrow(final_holdout_test))
RMSE(final_holdout_test$rating, predictions)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)



# fit <- lm(rating ~ as.factor(userId), data = movielens)

mu <- mean(final_holdout_test$rating) 
movie_avgs <- final_holdout_test %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))


predicted_ratings <- mu + final_holdout_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i



model_1_rmse <- RMSE(predicted_ratings, final_holdout_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))





# lm(rating ~ as.factor(movieId) + as.factor(userId))

user_avgs <- final_holdout_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


predicted_ratings <- final_holdout_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, final_holdout_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))





# lm(rating ~ as.factor(movieId) + as.factor(userId)) + genres



genre_avgs <- final_holdout_test %>% 
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_u)) 


predicted_ratings <- final_holdout_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by = 'genres') %>%
  mutate(pred = mu + b_u + b_g) %>%
  .$pred


model_3_rmse <- RMSE(predicted_ratings, final_holdout_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model + Genres",  
                                     RMSE = model_3_rmse ))


rmse_results %>% knitr::kable()





