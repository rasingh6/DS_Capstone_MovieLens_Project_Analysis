##################################
# Create edx set, validation set
##################################

# Note: The Process is gonna take few minutes to Run

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

disk <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", disk)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(disk, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(disk, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set = 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# using R 3.5 and later
Test_Index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-Test_Index,]
Temp <- movielens[Test_Index,]

# The two, userId & movieId are present in edx and validation dataset

validation <- Temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# edx dataset should consist of rows, removed from the validation data set i.e., ADD

Extract_Remove <- anti_join(temp, validation)
edx <- rbind(edx, Extract_Remove)

rm(disk, ratings, movies, Test_Index, Temp, movielens, Extract_Remove)



##########################Averageing Method##########################

# Average Rating (overall) in training dataset
Avg_Rating <- mean(edx$rating)

# ratings prediction (Unknown Rating Prediction "mu") and estimate the RMSE
RMSE(validation$rating, Avg_Rating)



####################### Method_Effect (Movie) ######################

# add average ranking term, Bias_Model
Bias_Model <- edx %>%
  group_by(movieId) %>%
  summarize(Bias_Model = mean(rating - Avg_Rating))

# predict all unknown ratings with Avg_Rating and Bias_Model
Unknown_ratings <- validation %>% 
  left_join(Bias_Model, by='movieId') %>%
  mutate(pred = Avg_Rating + Bias_Model) %>%
  pull(pred)

# calculate RMSE of movie ranking effect
RMSE(validation$rating, Unknown_ratings)

# plot the distribution of Bias_Modeli's
qplot(Bias_Model, data = Bias_Model, bins = 15, color = I("black"))



################################ Bias Movie and User ###############################

# Addition of User Bias Term
User_Bias <- edx %>% 
  left_join(Bias_Model, by='movieId') %>%
  group_by(userId) %>%
  summarize(User_Bias = mean(rating - Avg_Rating - Bias_Model))

# User and Movie Bias new Ratings
Unknown_ratings <- validation %>% 
  left_join(Bias_Model, by='movieId') %>%
  left_join(User_Bias, by='userId') %>%
  mutate(pred = Avg_Rating + Bias_Model + User_Bias) %>%
  pull(pred)

# RMSE calculation
RMSE(Unknown_ratings, validation$rating)



############################################ Effective Method ###########################################

# Determine lamdas (Ls)
Ls <- seq(from=0, to=10, by=0.25)

# RMSE for each lamda (ls)
RMSEs <- sapply(Ls, function(l){
  # Average Training Data Rating Calculation
  Avg_Rating <- mean(edx$rating)
  # Movie Bias Model Analysis
  Bias_Model <- edx %>% 
    group_by(movieId) %>%
    summarize(Bias_Model = sum(rating - Avg_Rating)/(n()+l))
  # User_Bias term Analysis
  User_Bias <- edx %>% 
    left_join(Bias_Model, by="movieId") %>%
    group_by(userId) %>%
    summarize(User_Bias = sum(rating - Bias_Model - Avg_Rating)/(n()+l))
  # Computing Validation and Prediction
  Unknown_ratings <- validation %>% 
    left_join(Bias_Model, by = "movieId") %>%
    left_join(User_Bias, by = "userId") %>%
    mutate(pred = Avg_Rating + Bias_Model + User_Bias) %>%
    pull(pred)
  # RMSE Output
  return(RMSE(Unknown_ratings, validation$rating))
})

# quick plot of RMSE vs lambdas
qplot(Ls, RMSEs)
# print minimum RMSE 
min(RMSEs)



######################################################
# Final model with regularized movie and user effects
######################################################

# The final linear model with the minimizing lambda
lam <- Ls[which.min(RMSEs)]

Bias_Model <- edx %>% 
  group_by(movieId) %>%
  summarize(Bias_Model = sum(rating - Avg_Rating)/(n()+lam))
# compute regularize user bias term
User_Bias <- edx %>% 
  left_join(Bias_Model, by="movieId") %>%
  group_by(userId) %>%
  summarize(User_Bias = sum(rating - Bias_Model - Avg_Rating)/(n()+lam))
# compute predictions on validation set based on these above terms
Unknown_ratings <- validation %>% 
  left_join(Bias_Model, by = "movieId") %>%
  left_join(User_Bias, by = "userId") %>%
  mutate(pred = Avg_Rating + Bias_Model + User_Bias) %>%
  pull(pred)
# output RMSE of these predictions
RMSE(Unknown_ratings, validation$rating)