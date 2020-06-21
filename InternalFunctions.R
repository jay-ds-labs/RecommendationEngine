###################################################################################
# PART 1 - Data Import, Audit & Cleaning
# 1. Importing 10Mn Dataset & Creating Train & Test
###################################################################################

data.load <- function(data.size){
  if(data.size == '10mn'){
    dl <- file.path(getwd(),'Movielens10Mn.zip')
    download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

    ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                     col.names = c("userId", "movieId", "rating", "timestamp"))
    
    movies <- as.data.frame(str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3))
    colnames(movies) <- c("movieId", "title", "genres")
    movies <- movies %>% mutate(movieId = as.numeric(movieId),
                                               title = as.character(title),
                                               genres = as.character(genres))
  }else if(data.size == '100K'){
    dl <- file.path(getwd(),'Movielens100K.zip')
    download.file("http://files.grouplens.org/datasets/movielens/ml-latest-small.zip", dl)
    
    ratings <- read.csv(unzip(dl, "ml-latest-small/ratings.csv"))
    
    movies <- read.csv(unzip(dl, "ml-latest-small/movies.csv"), stringsAsFactors = F)
  }else{
    print('Use input 100K for smaller dataset and 10mn for larger dataset')
    return()
  }
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Checking for R Version and using set.seed function appropriately
  if(as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor) >5) set.seed(1, sample.kind="Rounding") else set.seed(1)
  
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
  return(list(edx=edx,validation=validation))
}

# Function for performing basic data audit after creating test & train datasets
data.audit <- function(ds){
  options(scipen = 20)
  invisible(readline(prompt="Increase the width of your console. Press [enter] to continue"))
  cat('Top 10 rows of the dataset -')
  pander(head(ds,10), style='simple', split.table = 160)
  cat(rep('=',65),'\n\n\n')
  invisible(readline(prompt="Press [enter] to continue"))
  cat('Dataset structure -\n')
  glimpse(ds)
  cat(rep('=',65),'\n\n\n')
  invisible(readline(prompt="Press [enter] to continue"))
  cat('Dataset columnwise summary -\n')
  Hmisc::describe(ds)
}


###################################################################################
# PART 1 - Data Import, Audit & Cleaning
# 3. Data cleaning- Getting all the genres in separate columns
###################################################################################

genre.detect <- function(ds){
  genre.count <- nrow(genre.summary)
  pb <- txtProgressBar(min = 0, max = genre.count, style = 3)
  for(i in 1:nrow(genre.summary)){
    ds <- cbind(ds,str_detect(ds$genres,genre.summary$value[i]))
    names(ds)[ncol(ds)] <- genre.summary$value[i]
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(ds)
}


###################################################################################
# PART 2 - Data Exploration
# Data exploration is performed on smaller dataset (100K ratings), so that we
# can get the insights quickly on the local machine with limited memory

# 2. Exploring independent variables
###################################################################################

cumm.plot.with.labels.users <- function(p){
  x10 <- layer_data(p) %>% slice(which.min(abs(y-0.10))) %>% pull(x) %>% max()
  x25 <- layer_data(p) %>% slice(which.min(abs(y-0.25))) %>% pull(x) %>% max()
  x50 <- layer_data(p) %>% slice(which.min(abs(y-0.50))) %>% pull(x) %>% max()
  x75 <- layer_data(p) %>% slice(which.min(abs(y-0.75))) %>% pull(x) %>% max()
  x90 <- layer_data(p) %>% slice(which.min(abs(y-0.90))) %>% pull(x) %>% max()
  x99 <- layer_data(p) %>% slice(which.min(abs(y-0.99))) %>% pull(x) %>% max()
  y = c(0.1,0.25,0.5,0.75,0.9,0.99)
  x = c(x10, x25, x50, x75, x90, x99)
  d <- data.frame(x,y)
  p1 <- p + geom_text(data = d, aes(x=x,y=y, label = paste0(round(y*100),'% - ',x, ' ratings')), hjust = -0.5)
  p1 <- p1 + ggtitle(label = 'Cummulative % of users across count of ratings', subtitle = paste('Avg count of ratings per user =',round(mean(p$data$count_ratings),0),'|', 'Min count of ratings per user = ', round(min(p$data$count_ratings),0)))
  plot(p1)
}

cumm.plot.with.labels.movies <- function(p){
  x50 <- layer_data(p) %>% slice(which.min(abs(y-0.50))) %>% pull(x) %>% max()
  x75 <- layer_data(p) %>% slice(which.min(abs(y-0.75))) %>% pull(x) %>% max()
  x90 <- layer_data(p) %>% slice(which.min(abs(y-0.90))) %>% pull(x) %>% max()
  x99 <- layer_data(p) %>% slice(which.min(abs(y-0.99))) %>% pull(x) %>% max()
  y = c(0.5,0.75,0.9,0.99)
  x = c(x50, x75, x90, x99)
  d <- data.frame(x,y)
  p1 <- p + geom_text(data = d, aes(x=x,y=y, label = paste0(round(y*100),'% - ',x, ' ratings')), hjust = -0.5)
  p1 <- p1 + ggtitle(label = 'Cummulative % of movies across count of ratings', subtitle = paste('Avg count of ratings per movie =',round(mean(p$data$count_ratings),0),'|', 'Min count of ratings per movie = ', round(min(p$data$count_ratings),0)))
  plot(p1)
}

cumm.plot.with.labels.movies.genres <- function(p){
  x50 <- layer_data(p) %>% slice(which.min(abs(y-0.50))) %>% pull(x) %>% max()
  x75 <- layer_data(p) %>% slice(which.min(abs(y-0.75))) %>% pull(x) %>% max()
  x90 <- layer_data(p) %>% slice(which.min(abs(y-0.90))) %>% pull(x) %>% max()
  x99 <- layer_data(p) %>% slice(which.min(abs(y-0.99))) %>% pull(x) %>% max()
  y = c(0.5,0.75,0.9,0.99)
  x = c(x50, x75, x90, x99)
  d <- data.frame(x,y)
  p1 <- p + geom_text(data = d, aes(x=x,y=y, label = paste0(round(y*100),'% - ',x, ' genres')), hjust = -0.5)
  p1 <- p1 + ggtitle(label = 'Cummulative % of movies across count of genres', subtitle = paste('Avg count of genres per movie =',round(mean(p$data$genre.count),0)))
  plot(p1)
}

cumm.plot.with.labels.users.genres <- function(p){
  x10 <- layer_data(p) %>% slice(which.min(abs(y-0.10))) %>% pull(x) %>% max()
  x25 <- layer_data(p) %>% slice(which.min(abs(y-0.25))) %>% pull(x) %>% max()
  x50 <- layer_data(p) %>% slice(which.min(abs(y-0.50))) %>% pull(x) %>% max()
  x75 <- layer_data(p) %>% slice(which.min(abs(y-0.75))) %>% pull(x) %>% max()
  x90 <- layer_data(p) %>% slice(which.min(abs(y-0.90))) %>% pull(x) %>% max()
  x99 <- layer_data(p) %>% slice(which.min(abs(y-0.99))) %>% pull(x) %>% max()
  y = c(0.1,0.25,0.5,0.75,0.9,0.99)
  x = c(x10, x25, x50, x75, x90, x99)
  d <- data.frame(x,y)
  p1 <- p + geom_text(data = d, aes(x=x,y=y, label = paste0(round(y*100),'% - ',x, ' genres')), hjust = -0.5)
  p1 <- p1 + ggtitle(label = 'Cummulative % of users across count of genres', subtitle = paste('Avg count of genres per user =',round(mean(p$data$Cnt.Genres.by.Users),0),'|', 'Min count of genres per user = ', round(min(p$data$Cnt.Genres.by.Users),0)))
  plot(p1)
}
###################################################################################
# PART 3 - Data Preparation for Model Building
# 1. Creating train & test datasets from edx dataset for model parameter estimation
#    so that Validation set is used only for final model validation      
###################################################################################

create.test.and.train.from.edx <- function(ds){
  # Checking for R Version and using set.seed function appropriately
  if(as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor) >5) set.seed(1, sample.kind="Rounding") else set.seed(1)
  
  test_index <- createDataPartition(y = ds$rating, times = 1, p = 0.1, list = FALSE)
  train_set <- ds[-test_index,]
  test_set <- ds[test_index,]
  
  test_set <- test_set %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  removed <- anti_join(ds[test_index,], test_set)
  train_set <- rbind(train_set, removed)
  
  return(list(train_set=train_set,test_set=test_set))
}

# Function for calculating RMSE
rmse <- function(true_ratings, predicted_ratings){
  round(sqrt(mean((true_ratings - predicted_ratings)^2)),5)
}

# Function for viewing RMSE across models
view.rmse <- function(){
  pander(rmse.results, style='simple', split.table = 400)
}



###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 2 - Linear Models
# Model 6 - Movie & User Effect with Regularization
###################################################################################

model.regularization <- function(l,train,test){
  
  user.effect <- edxS %>% left_join(movie.effect,by='movieId') %>% 
    group_by(userId) %>% summarize(u_i = mean(rating - mean.rating- b_i))
  
  predicted.rating <- validationS %>% left_join(movie.effect,by='movieId') %>% 
    left_join(user.effect,by='userId') %>% 
    mutate(pred = mean.rating + b_i + u_i) %>% 
    pull(pred) 
  model.rmse <- rmse(validationS$rating,predicted.rating)
  
  mean.rating <- mean(train$rating) 
  
  movie.effect <- train %>% group_by(movieId) %>% summarize(b_i = sum(rating - mean.rating)/(n()+l[1]))
  
  user.effect <- train %>% left_join(movie.effect,by='movieId') %>% 
    group_by(userId) %>% summarize(u_i = sum(rating - mean.rating- b_i)/(n()+l[2]))
  temp <- train %>% left_join(movie.effect,by='movieId') %>% 
    left_join(user.effect, by='userId')
  
  predicted.rating <- test %>% left_join(movie.effect,by='movieId') %>% 
    left_join(user.effect,by='userId') %>%  
    mutate(pred = mean.rating + b_i + u_i) %>% 
    pull(pred)
  
  return(rmse(test$rating, predicted.rating))
}


###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 2 - Linear Models
# Model 7 - Movie, User & Genre Effect with Regularization
###################################################################################

model.regularization.with.genre <- function(l,train,test){
  
  mean.rating <- mean(train$rating) 
  
  movie.effect <- train %>% group_by(movieId) %>% summarize(b_i = sum(rating - mean.rating)/(n()+l[1]))
  
  user.effect.Drama <- train %>% left_join(movie.effect,by='movieId') %>% 
    group_by(userId,Drama) %>% summarize(u_i_Drama = sum(rating - mean.rating- b_i)/(n()+l[2]))
  temp <- train %>% left_join(movie.effect,by='movieId') %>% 
    left_join(user.effect.Drama,by=c('userId','Drama'))
  
  user.effect.Comedy <- temp %>% group_by(userId,Comedy) %>% 
    summarize(u_i_Comedy = sum(rating - mean.rating- b_i-u_i_Drama)/(n()+l[2]))
  temp <- temp %>% left_join(user.effect.Comedy,by=c('userId','Comedy'))
  
  user.effect.Action <- temp %>% group_by(userId,Action) %>% 
    summarize(u_i_Action = sum(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy)/(n()+l[2]))
  temp <- temp %>% left_join(user.effect.Action,by=c('userId','Action'))
  
  user.effect.Thriller <- temp %>% group_by(userId,Thriller) %>% 
    summarize(u_i_Thriller = sum(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action)/(n()+l[2]))
  temp <- temp %>% left_join(user.effect.Thriller,by=c('userId','Thriller'))
  
  user.effect.Adventure <- temp %>% group_by(userId,Adventure) %>% 
    summarize(u_i_Adventure = sum(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action-u_i_Thriller)/(n()+l[2]))
  temp <- temp %>% left_join(user.effect.Adventure,by=c('userId','Adventure'))
  
  user.effect.Romance <- temp %>% group_by(userId,Romance) %>% 
    summarize(u_i_Romance = sum(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action-u_i_Thriller-u_i_Adventure)/(n()+l[2]))
  temp <- temp %>% left_join(user.effect.Romance,by=c('userId','Romance'))
  
  user.effect.OtherGenre <- temp %>% group_by(userId) %>% 
    summarize(u_i_OtherGenre = sum(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action-u_i_Thriller-u_i_Adventure-u_i_Romance)/(n()+l[2]))
  temp <- temp %>% left_join(user.effect.OtherGenre,by='userId')
  
  predicted.rating <- test %>% left_join(movie.effect,by='movieId') %>% 
    left_join(user.effect.Drama,by=c('userId','Drama')) %>% 
    left_join(user.effect.Comedy,by=c('userId','Comedy')) %>% 
    left_join(user.effect.Action,by=c('userId','Action')) %>% 
    left_join(user.effect.Thriller,by=c('userId','Thriller')) %>% 
    left_join(user.effect.Adventure,by=c('userId','Adventure')) %>% 
    left_join(user.effect.Romance,by=c('userId','Romance')) %>% 
    left_join(user.effect.OtherGenre,by='userId') %>% 
    mutate(pred = mean.rating + b_i + coalesce(u_i_Drama,0) + coalesce(u_i_Comedy,0) + coalesce(u_i_Action,0) + coalesce(u_i_Thriller,0) + coalesce(u_i_Adventure,0) + coalesce(u_i_Romance,0) + coalesce(u_i_OtherGenre,0)) %>% 
    pull(pred)
  
  return(rmse(test$rating, predicted.rating))
}


###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 3 - Memory-Based Collaborative Filtering
# Model 9 - User Based Collaborative Filtering
###################################################################################

create.sequenced.data <- function(df){
  userId <- unique(df$userId)
  movieId <- unique(df$movieId)
  userId.seq <- c(1:length(userId))
  movieId.seq <- c(1:length(movieId))
  users <- data.frame(userId.seq, userId)
  movies <- data.frame(movieId.seq, movieId)
  all.data <- expand.grid(userId.seq,movieId.seq)
  colnames(all.data) <- c('userId.seq', 'movieId.seq')
  all.data <- all.data %>% left_join(users, by = 'userId.seq') %>% left_join(movies, by = 'movieId.seq') 
  all.data <- all.data %>% left_join(df, by=c('userId','movieId'))
  colnames(all.data)[5] <- 'rating'
  all.data <- all.data[!is.na(all.data$rating),]
  return(all.data)
}


convert.all.data.to.realRatingMatrix <- function(df){
  df <- create.sequenced.data(df)
  sm <- sparseMatrix(i = df$userId.seq, j = df$movieId.seq,x = df$rating,
                     dims = c(max(df$userId.seq), max(df$movieId.seq)),
                     dimnames = list(paste0("u", unique(df$userId)),paste0("m", unique(df$movieId))))
  return(new("realRatingMatrix", data = sm))
  
}

create.known.n.unknown <- function(df, n){
  head(df)  
  df2 <- df %>% group_by(by=userId) %>% filter(n()>(n+1)) %>% ungroup() %>% select(-by)
  df2 <- df2 %>% mutate(random.numbers = rnorm(nrow(df2))) %>% arrange(userId, random.numbers) %>%
    group_by(userId) %>% mutate(rank.random.numbers = rank(random.numbers, ties.method = "first")) %>% 
    ungroup() %>% mutate(selection=ifelse(rank.random.numbers>n, 'known','unknown'))
  
  df2.known <- df2 %>% filter(selection=='known')
  df2.unknown <- df2 %>% filter(selection=='unknown')
  
  df2.known.sequenced <- create.sequenced.data(df2.known)
  df2.known.realRatingMatrix <- convert.all.data.to.realRatingMatrix(df2.known.sequenced)
  
  df2.unknown.sequenced <- create.sequenced.data(df2.unknown)
  df2.unknown.realRatingMatrix <- convert.all.data.to.realRatingMatrix(df2.unknown.sequenced)
  
  return(list(known=df2.known.realRatingMatrix, unknown=df2.unknown.realRatingMatrix))
}


###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 4 - Model-Based Collaborative Filtering
# Model 11 - Slope One
###################################################################################
create.train.n.test <- function(movielens){
  # Checking for R Version and using set.seed function appropriately
  if(as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor) >5) set.seed(1, sample.kind="Rounding") else set.seed(1)
  
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  edx <- movielens[-test_index,] # 34969
  temp <- movielens[test_index,] # 3887
  
  # Make sure userId and movieId in validation set are also in edx set
  validation <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")  # 3887
  
  edx <- edx %>% 
    semi_join(validation, by = "movieId") %>%
    semi_join(validation, by = "userId")
  
  return(list(test=edx, train=validation))
}

data.prepare.for.slope.one <- function(ratings){
  names(ratings) <- c("user_id", "item_id", "rating")
  ratings <- data.table(ratings)
  
  ratings[, user_id := as.character(user_id)]
  ratings[, item_id := as.character(item_id)]
  
  setkey(ratings, user_id, item_id)
  
  return(ratings)
}










