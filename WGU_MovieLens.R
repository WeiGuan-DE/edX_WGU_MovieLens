#######################################################################
# Data Initiation - Initial Code provided by edX
#######################################################################

# Create edx and final_holdout_test sets

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

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
ratings <- ratings %>% mutate(userId = as.integer(userId),
                              movieId = as.integer(movieId),
                              rating = as.numeric(rating),
                              timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>% mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
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




#######################################################################
# Data Cleaning, Exploration and Visualization - My MovieLens Project
#######################################################################

# Install more packages if not available already
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

# Load more libraries
library(lubridate)
library(ggplot2)

# Numbers of rows and columns in the data set edx
dim(edx)

# Numbers of rows and columns in the data set final_holdout_test
dim(final_holdout_test)

# First 6 rows of edx data set
head(edx, 6)

# Number of different users
n_distinct(edx$userId)

# Number of different movies
n_distinct(edx$movieId)

# Number of different genres
n_distinct(edx$genres)

# Different values of ratings
unique(edx$rating) %>% sort()

# Five most given ratings in order from most to least
edx %>% group_by(rating) %>% 
  summarize(count = n()) %>% 
  top_n(5) %>% arrange(desc(count))

# First 6 movies with highest number of user ratings
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head()

# First 6 movies with lowest number of user ratings
edx %>% group_by(movieId, title) %>%
  summarize(n = n(), genres = genres) %>%
  arrange(n) %>%
  head()

# Rating Distribution
edx %>% ggplot(aes(rating)) +
  geom_histogram(bins = 20) +
  ggtitle("Rating Distribution")

# Distribution of movies versus their number of ratings
edx %>%
  group_by(movieId) %>%
  summarize(n = n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of Ratings") +
  ylab("Number of Movies") +
  ggtitle("Distribution of Movies versus Number of Ratings")

# Distribution of users versus their number of ratings
edx %>%
  group_by(userId) %>%
  summarize(n = n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of Ratings") +
  ylab("Number of Users") +
  ggtitle("Users versus Their Number of Ratings")

# Distribution of genres versus their average ratings
edx %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating)) %>%
  ggplot(aes(b_g)) +
  geom_histogram(bins = 30, color = "black") +
  xlab("Genre Average Rating") +
  ylab("Number of Genres") +
  ggtitle("Distribution of Genres versus Rating")

#Check if there are any NAs in the edx data set:
print(sapply(edx, function(x) sum(is.na(x))))

# Create a new feature rateDate from timestamp for the data set edx
edxNew <- edx %>% mutate(rateDate = round_date(as_datetime(timestamp), unit = "day"))

# First 6 rows of edxNew data set
head(edxNew, 6)

# Numbers of rows and columns in the data set edxNew
dim(edxNew)

# Create a new feature rateDate from timestamp for the data set final_holdout_test
final_holdout_test <- final_holdout_test %>% mutate(rateDate = round_date(as_datetime(timestamp), unit = "day"))

# First 6 rows of final_holdout_test data set
head(final_holdout_test, 6)

# Numbers of rows and columns in the data set final_holdout_test
dim(final_holdout_test)

# Distribution of rating average versus rate date
edxNew %>% 
  group_by(rateDate) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(rateDate, rating)) +
  geom_point() +
  geom_smooth() +
  xlab("Rate Date") +
  ylab("Rating Average") +
  ggtitle("Distribution of Rating Average versus Rate Date")

# Reduce the data set by selecting users with at least 15 ratings
edxNew <- edxNew %>% group_by(userId) %>% filter(n() >= 15) %>% ungroup()

# Numbers of rows and columns in the data set edxNew
dim(edxNew)

# Split edxNew into training set (80%) and test set (20%)
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
testIndex <- createDataPartition(y = edxNew$rating, times = 1, p = 0.2, list = FALSE)
trainSet <- edxNew[-testIndex, ]
testSet <- edxNew[testIndex, ]

# Make sure UserId and MovieId are existing in training set and test set.
testSet <- testSet %>% 
  semi_join(trainSet, by = "movieId") %>%
  semi_join(trainSet, by = "userId")




#######################################################################
# Model Design - My MovieLens Project
#######################################################################

# Narrow down the optimal area for lambda
lambdas <- seq(4.0, 5.0, 0.1)

# The model is evaluated using the Root Mean Squared Error (RMSE)
RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2, na.rm = TRUE))
}

# Train the model
rmses <- sapply(lambdas, function(l){
  avg <- mean(trainSet$rating)
  
  movie_avg <- trainSet %>% 
    group_by(movieId) %>%
    summarize(movie_avg = mean(rating)) 
  
  b_m <- trainSet %>% 
    group_by(movieId) %>%
    summarize(b_m = sum(rating - avg)/(n()+l))
  
  b_u <- trainSet %>% 
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - avg - b_m)/(n()+l))

  b_g <- trainSet %>%
    left_join(b_m, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - avg - b_m - b_u)/(n()+l))
  
  b_rd <- trainSet %>% 
    left_join(b_m, by='movieId') %>%
    left_join(b_u, by='userId') %>% 
    left_join(b_g, by='genres') %>%
    group_by(rateDate) %>% 
    summarize(b_rd = sum(rating - avg - b_m - b_u - b_g)/(n()+l))
  
  predicted_ratings <- testSet %>% 
    left_join(movie_avg, by = "movieId") %>% 
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_rd, by = "rateDate") %>%
    mutate(pred = avg + b_m + b_u + b_g + b_rd) %>% 
    mutate(pred = ifelse(is.na(pred), movie_avg, pred)) %>% # Replace NAs movie average
    mutate(pred = ifelse(is.na(pred), avg, pred)) %>% # Replace remaining NAs with overall average, if movie average is not available
    mutate(pred = ifelse(pred < 0, 0.5, ifelse(pred > 5, 5, pred))) %>% 
    .$pred
  
  return(RMSE(predicted_ratings, testSet$rating))
})

# Plot the lambdas and their rmses
trainResults <- data.frame(lambda = lambdas, rmse = rmses)
print(trainResults %>% ggplot(aes(lambda, rmse)) +
        ggtitle("RMSE versus lambda value") +
        geom_point() + 
        geom_smooth() +
        geom_vline(xintercept = lambdas[which.min(rmses)], color = "red"))

# Find optimal penalty parameter lambda with the minimal RMSE.
lambda <- lambdas[which.min(rmses)]

# Print optimal lambda
print(lambda)




#######################################################################
# Model Evaluation - My MovieLens Project
#######################################################################

# Perform evaluation on test & train set with the optimal lambda.
training_Rmse <- sapply(lambda, function(l){
  avg <- mean(trainSet$rating)
  
  movie_avg <- trainSet %>% 
    group_by(movieId) %>%
    summarize(movie_avg = mean(rating)) 
  
  b_m <- trainSet %>% 
    group_by(movieId) %>%
    summarize(b_m = sum(rating - avg)/(n()+l))
  
  b_u <- trainSet %>% 
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - avg - b_m)/(n()+l))
  
  b_g <- trainSet %>%
    left_join(b_m, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - avg - b_m - b_u)/(n()+l))
  
  b_rd <- trainSet %>% 
    left_join(b_m, by='movieId') %>%
    left_join(b_u, by='userId') %>% 
    left_join(b_g, by='genres') %>%
    group_by(rateDate) %>% 
    summarize(b_rd = sum(rating - avg - b_m - b_u - b_g)/(n()+l))
  
  predicted_ratings <- testSet %>% 
    left_join(movie_avg, by = "movieId") %>% 
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_rd, by = "rateDate") %>%
    mutate(pred = avg + b_m + b_u + b_g + b_rd) %>% 
    mutate(pred = ifelse(is.na(pred), movie_avg, pred)) %>% # Replace NAs movie average
    mutate(pred = ifelse(is.na(pred), avg, pred)) %>% # Replace remaining NAs with overall average, if movie average is not available
    mutate(pred = ifelse(pred < 0, 0.5, ifelse(pred > 5, 5, pred))) %>% 
    .$pred
  
  return(RMSE(predicted_ratings, testSet$rating))
})

# Print RMSE from train & test set.
print(training_Rmse)




#######################################################################
# Results - My MovieLens Project
#######################################################################

# Perform evaluation on test & train set with the optimal lambda.
final_Rmse <- sapply(lambda, function(l){
  avg <- mean(trainSet$rating)
  
  movie_avg <- trainSet %>% 
    group_by(movieId) %>%
    summarize(movie_avg = mean(rating)) 
  
  b_m <- trainSet %>% 
    group_by(movieId) %>%
    summarize(b_m = sum(rating - avg)/(n()+l))
  
  b_u <- trainSet %>% 
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - avg - b_m)/(n()+l))
  
  b_g <- trainSet %>%
    left_join(b_m, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - avg - b_m - b_u)/(n()+l))
  
  b_rd <- trainSet %>% 
    left_join(b_m, by='movieId') %>%
    left_join(b_u, by='userId') %>% 
    left_join(b_g, by='genres') %>%
    group_by(rateDate) %>% 
    summarize(b_rd = sum(rating - avg - b_m - b_u - b_g)/(n()+l))
  
  predicted_ratings <- final_holdout_test %>% 
    left_join(movie_avg, by = "movieId") %>% 
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_rd, by = "rateDate") %>%
    mutate(pred = avg + b_m + b_u + b_g + b_rd) %>% 
    mutate(pred = ifelse(is.na(pred), movie_avg, pred)) %>% # Replace NAs movie average
    mutate(pred = ifelse(is.na(pred), avg, pred)) %>% # Replace remaining NAs with overall average, if movie average is not available
    mutate(pred = ifelse(pred < 0, 0.5, ifelse(pred > 5, 5, pred))) %>% 
    .$pred
  
  return(RMSE(predicted_ratings, final_holdout_test$rating))
})

# Print RMSE from train & test set.
print(final_Rmse)

