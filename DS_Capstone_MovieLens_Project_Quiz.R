library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

############################# QUESTION 1 ###############################
#How many rows and columns are there in the edx dataset?
paste('edx dataset consists of',nrow(edx),'number of rows and',ncol(edx),'number of columns.')
# Answer [1] "edx dataset consists of 9000055 number of rows and 6 number of columns."

############################# QUESTION 2 ###############################
#How many zeros were given as ratings in the edx dataset?
#How many threes were given as ratings in the edx dataset?
paste(sum(edx$rating == 0), 'number of ratings with Zero were given and',
      sum(edx$rating == 3),'number of ratings with Three were given in edx dataset')
# Answer [1] "0 number of ratings with Zero were given and 2121240 number of ratings with Three were given in edx dataset"

############################# QUESTION 3 ###############################
#How many different movies are in the edx dataset?
edx %>% summarize(n_movies = n_distinct(movieId))
# Answer  n_movies
# 1    10677 

############################# QUESTION 4 ###############################
# How many different users are in the edx dataset?
edx %>% summarize(n_users = n_distinct(userId))
# Answer   n_users
# 1   69878

############################# QUESTION 5 ###############################
# How many movie ratings are in each of the following genres in the edx dataset?
drama <- edx %>% filter(str_detect(genres,"Drama"))
comedy <- edx %>% filter(str_detect(genres,"Comedy"))
thriller <- edx %>% filter(str_detect(genres,"Thriller"))
romance <- edx %>% filter(str_detect(genres,"Romance"))

paste('Drama has',nrow(drama),'movie ratings')
paste('Comedy has',nrow(comedy),'movie ratings')
paste('Thriller has',nrow(thriller),'movie ratings')
paste('Romance has',nrow(romance),'movie ratings')
# Answer [1] "Drama has 3910127 movie ratings"
# [1] "Comedy has 3540930 movie ratings"
# [1] "Thriller has 2325899 movie ratings"
# [1] "Romance has 1712100 movie ratings"

############################# QUESTION 6 ###############################
# Which movie has the greatest number of ratings?
edx %>% group_by(title) %>% summarise(number = n()) %>%
  arrange(desc(number))
# Answer 1 Pulp Fiction (1994)                                           31362

############################# QUESTION 7 ###############################
# What are the five most given ratings in order from most to least?
head(sort(-table(edx$rating)),5)
# Answer  4        3        5      3.5        2 
# -2588430 -2121240 -1390114  -791624  -711422 

############################# QUESTION 8 ###############################
# True or False: In general, half star ratings are less common than whole star ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).
table(edx$rating)
#  Answer  0.5       1     1.5       2     2.5       3     3.5       4     4.5       5
#       85374  345679  106426  711422  333010 2121240  791624 2588430  526736  1390114 


