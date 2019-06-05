library(tidyverse)
library(caret)
library(data.table)
library(kableExtra)
library(lubridate)
library(Matrix.utils)
library(DT)
library(wordcloud) 
library(RColorBrewer) 
library(ggthemes) 
library(irlba)
library(recommenderlab)
library(recosystem)
library(h2o)
library("mgcv")
library("nlme")
library("nnet")
library("spatial")
library("survival")
library(lattice)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lattice)
library(plotly)
library(latticeExtra)
library(dplyr) 
library(magrittr)
library(knitr)


edx <- readRDS("~/Desktop/edx/edx.rds")

str(edx)
start <- edx$rating
genres <-(edx$genres) 


# Create a dataframe to sort half and full ratings
group <-  ifelse((start == 1 |start == 2 | start == 3 | 
                    start == 4 | start == 5) ,
                 "whole_star", 
                 "half_star") 

explore_ratings <- data.frame(start, group)

ggplot(explore_ratings, aes(start, fill = group)) +
  geom_histogram( binwidth = 0.2) +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  scale_fill_manual(values = c("half_star"="#66CCFF", "whole_star"="#003366")) +
  labs(x="rating", y="number of ratings", caption = "source data: edx set") +
  ggtitle("Distribution of the ratings")

top_genr <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
top_genr
knitr::kable(head(top_genr, 10))

top_title <- edx %>%
  
  group_by(title) %>%
  summarize(count=n()) %>%
  top_n(20,count) %>%
  arrange(desc(count))

# with the head function i output the top 5 

kable(head(edx %>%
             group_by(title,genres) %>%
             summarize(count=n()) %>%
             top_n(20,count) %>%
             arrange(desc(count)) ,
           5)) %>%
  kable_styling(bootstrap_options = "bordered", full_width = F , position ="center") %>%
  column_spec(1,bold = T ) %>%
  column_spec(2,bold =T) %>%
  column_spec(3,bold=T)
top_title %>% 
  ggplot(aes(x=reorder(title, count), y=count)) +
  geom_bar(stat='identity', fill="#003366") + coord_flip(y=c(0, 40000)) +
  labs(x="", y="Number of ratings") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  labs(title="Top 20 movies title based \n on number of ratings" , caption = "source data: edx set")

edx %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))


# i calculate the average of all ratings of the edx set
mu <- mean(edx$rating)
movieId <- (edx$movieId)
# i calculate b_i on the training set
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# predicted ratings
predicted_ratings_bi <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i


#b.movie + user effect

#i calculate b_u using the training set 
user_avgs <- edx %>%  
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#predicted ratings
predicted_ratings_bu <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred


#c.movie + user + time effect

#i create a copy of validation set , valid, and create the date feature which is the timestamp converted to a datetime object  and  rounded by week.

valid <- validation
valid <- valid %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) 

# i calculate time effects ( b_t) using the training set
temp_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(b_t = mean(rating - mu - b_i - b_u))

# predicted ratings
predicted_ratings_bt <- valid %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(temp_avgs, by='date') %>%
  mutate(pred = mu + b_i + b_u + b_t) %>%
  .$pred

#d.  i calculate the RMSE for movies, users and time effects 

rmse_model1 <- RMSE(validation$rating,predicted_ratings_bi)  
rmse_model1


rmse_model2 <- RMSE(validation$rating,predicted_ratings_bu)
rmse_model2


rmse_model3 <- RMSE(valid$rating,predicted_ratings_bt)
rmse_model3

```
