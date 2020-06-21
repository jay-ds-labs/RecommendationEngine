# TARGET - RMSE < 0.86490
# q(save="no")
# .rs.restartR()
###################################################################################
# PART 0 - Setting up R environment
# PART 1 - Data Import, Audit & Cleaning
# PART 2 - Data Exploration
# PART 3 - Data Preparation for Model Building
# PART 4 - Model Selection on 100K Movie Dataset
# PART 5 - Model Training & Validation on 1Mn Movie Dataset using selected model
###################################################################################

###################################################################################
# PART 0 - Setting up R environment
# 1. Installing packages if required
###################################################################################
# Clean global environment
rm(list=ls())
# Set working directory
# Please change this as per your choice
setwd(getwd())

# Required package list
required.packages.data.manipulation <- c('Hmisc','data.table','plyr','tidyverse','pander','lubridate')
required.packages.visualization <- c('RColorBrewer','ggplot2','gridExtra')
required.packages.model <- c('caret','recommenderlab','recosystem','h2o')
required.packages.authoring <- c('rmarkdown','binb')
required.packages <- c(required.packages.data.manipulation,
                       required.packages.visualization,
                       required.packages.model,
                       required.packages.authoring)

# Installing required packages if needed
packages.to.install <- required.packages[which(!required.packages %in% installed.packages()[,1])]
if(length(packages.to.install)>0) {
  cat('Following packages will be installed:\n', packages.to.install)
  install.packages(packages.to.install)
  packages.to.install <- required.packages[which(!required.packages %in% installed.packages()[,1])]
}
if(length(packages.to.install)>0) cat('Failed to install:\n', packages.to.install) else print('All required packages are installed.')


###################################################################################
# PART 0 - Setting up R environment
# 2. Loading required packages & functions in memory
###################################################################################

sapply(required.packages, require, character.only = TRUE)

# SlopeOne & SVDApproximation Packages loaded through source code as they generally have trouble installing
source('ExternalFunctions.R')

# Functions are created to make this code modular and easy to understand.
# Key tasks are done using functions kept in InternalFunctions.R file.
# All functions are required to run the code.
source('InternalFunctions.R')



###################################################################################
# PART 1 - Data Import, Audit & Cleaning
# 1. Importing 10Mn Dataset & Creating Train & Test
###################################################################################

# Note: this process could take a couple of minutes
# Alternatively, you can load the datasets using load('SavedObjects/DataLargeOriginal.rdata')
train.and.test = data.load('10mn')

# Using suffix B in the names for Big (10Mn) and S for Small (100K) dataset size
edxB = train.and.test$edx
validationB = train.and.test$validation
rm(train.and.test)

# Performing basic audit on data loaded
data.audit(edxB)
data.audit(validationB)

#save(edxB, validationB, file = 'DataLargeOriginal.rdata')

###################################################################################
# PART 1 - Data Import, Audit & Cleaning
# 2. Importing 100K Dataset & Creating Train & Test
###################################################################################

# Alternatively, you can load the smaller datasets using load('SavedObjects/SmallerDatasets100K.rdata')
train.and.test = data.load('100K')

# Using suffix B in the names for Big (10Mn) and S for Small (100K) dataset size
edxS = train.and.test$edx
validationS = train.and.test$validation
rm(train.and.test)

# Performing basic audit on data loaded
data.audit(edxS)
data.audit(validationS)

#save(edxS, validationS, file ='SmallerDatasets100K.rdata')

###################################################################################
# PART 1 - Data Import, Audit & Preparation
# 3. Data Preparation- Getting all the genres in separate columns
###################################################################################

# Adding genre flags on larger dataset (10Mn)
max.count.of.genres.in.a.movie <- max(str_count(unique(edxB$genres),'\\|'))+1
genre.colnames <- paste0('genre',c(1:max.count.of.genres.in.a.movie))
genre.summary <- edxB %>% separate(col = genres, into = genre.colnames,sep = '\\|', fill = 'right') %>% 
  select(all_of(genre.colnames)) %>% gather() %>% group_by(value) %>% summarize(genre.count=n()) %>% 
  arrange(desc(genre.count)) %>% filter(value != 'NA') %>% 
  mutate(genre.perc=round(100*genre.count/nrow(edxB),0))

# Note: High run time expected. 
# Alternatively you can load('SavedObjects/DataLargeTrainWithGenreDateVars.rdata') to get edxB with all Genre & Date related variables
edxB <- genre.detect(edxB)
# Alternatively you can load('SavedObjects/DataLargeTestWithGenreDateVars.rdata') to get validationB with all Genre & Date related variables
validationB <- genre.detect(validationB)

# Adding genre flags on smaller dataset (100K). Alternatively load('SavedObjects/DataSmallWithGenreDateVars.rdata')
max.count.of.genres.in.a.movie <- max(str_count(unique(edxS$genres),'\\|'))+1
genre.colnames <- paste0('genre',c(1:max.count.of.genres.in.a.movie))
genre.summary <- edxS %>% separate(col = genres, into = genre.colnames,sep = '\\|', fill = 'right') %>% 
                select(all_of(genre.colnames)) %>% gather() %>% group_by(value) %>% summarize(genre.count=n()) %>% 
                arrange(desc(genre.count)) %>% filter(value != 'NA') %>% 
                mutate(genre.perc=round(100*genre.count/nrow(edxS),0))

edxS <- genre.detect(edxS)
validationS <- genre.detect(validationS)

# Adding date variables on larger dataset (10Mn)
edxB <- edxB %>% mutate(date = as_datetime(timestamp), yr = lubridate::year(date), mnth = lubridate::month(date), dt = lubridate::day(date), day = wday(date), hr= lubridate::hour(date))
validationB <- validationB %>% mutate(date = as_datetime(timestamp), yr = lubridate::year(date), mnth = lubridate::month(date), dt = lubridate::day(date), day = wday(date), hr= lubridate::hour(date))

# Adding date variables on smaller dataset (100K)
edxS <- edxS %>% mutate(date = as_datetime(timestamp), yr = lubridate::year(date), mnth = lubridate::month(date), dt = lubridate::day(date), day = wday(date), hr= lubridate::hour(date))
validationS <- validationS %>% mutate(date = as_datetime(timestamp), yr = lubridate::year(date), mnth = lubridate::month(date), dt = lubridate::day(date), day = wday(date), hr= lubridate::hour(date))

# save(edxB,file='DataLargeTrainWithGenreDateVars.rdata')
# save(validationB,file='DataLargeTestWithGenreDateVars.rdata')
# save(edxS,validationS, file='DataSmallWithGenreDateVars.rdata')

###################################################################################
# PART 2 - Data Exploration
# Data exploration is performed on smaller dataset (100K ratings), so that we
# can get the insights quickly on the local machine with limited memory

# 1. Exploring ratings or our dependent variable, that we wish to predict
###################################################################################
# For performing the explorations we will be using the smaller dataset with all
# genre & date variables. In case there is any issue in running previous steps you
# can load('SmallerDatasetWithGenreDateVars.rdata')
edxS %>% ggplot(aes(x=rating)) + geom_bar(aes(y = ..prop..), stat="count") + 
  geom_text(aes(y = ..prop.., label = scales::percent(..prop..,accuracy=1)), vjust = -0.5, stat="count") +
  scale_y_continuous(labels = scales::percent) + labs(y = 'Percent', x = 'Ratings') + 
  ggtitle('Distribution of ratings', subtitle = '60% of all ratings are between 3 to 4')

Hmisc::describe(edxS$rating, tabular = T)

###################################################################################
# PART 2 - Data Exploration
# Data exploration is performed on smaller dataset (100K ratings), so that we
# can get the insights quickly on the local machine with limited memory

# 2. Exploring independent variables
###################################################################################
# UserId Exploration
p <- edxS %>% group_by(userId) %>% summarize(count_ratings = n()) %>% ggplot(aes(x=count_ratings)) +
  stat_ecdf(geom = "point") + scale_y_continuous(labels = scales::percent) + labs(y = 'Cummulative Percentage of Users', x = 'Count of Ratings')
cumm.plot.with.labels.users(p)
cat(' Min UserId = ', min(edxS$userId),'\n',
    'Max UserId = ', max(edxS$userId),'\n',
    'Count of distinct UserIds = ', length(unique((edxS$userId),'\n')))

# There are 610 users who have watched 91K movies
# Meaning on an avg every one has watched 150 movies, however this is misleading
# as a few users have watched a very high number of movies.
# In fact 75% of users have actually watched less than 157 movies
# And 50% of users have watched less than 64 movies

# MovieId Exploration
p <- edxS %>% group_by(movieId) %>% summarize(count_ratings = n()) %>% ggplot(aes(x=count_ratings)) +
  stat_ecdf(geom = "point") + scale_y_continuous(labels = scales::percent) + labs(y = 'Cummulative Percentage of Movies', x = 'Count of Ratings')
cumm.plot.with.labels.movies(p)
cat(' Min UserId = ', min(edxS$movieId),'\n',
    'Max UserId = ', max(edxS$movieId),'\n',
    'Count of distinct UserIds = ', length(unique((edxS$movieId),'\n')))

# There are 9724 distinct movies and the range of movie Ids vary from 1 to 193K
# While avg count of ratings per movie is 9, it is misleading as
# 50% of movies have only upto 2 ratings and 90% of movies have only upto 24 ratings
# This is important insight as, in a larger dataset with more ratings per movie
# predictive ability of any modeling technique will improve over the smaller dataset

# Date variable Exploration
edxS %>% group_by(yr) %>% summarize(count_ratings = n()) %>% ggplot(aes(x = yr, y=count_ratings)) +
  geom_bar(stat = 'sum',show.legend = F) + labs(x = 'Years', y = 'Count of Ratings') +
  ggtitle('Count of ratings across years')
cat(' Min Year = ', min(edxS$yr),'\n',
    'Max Year = ', max(edxS$yr),'\n')

# Ratings have been done in the period between 1996 & 2018.
# However some years have higher representation than others

# Lets see if rating of the same movie varies across the years
# Lets choose the 3 most rated movies and lets look at their avg rating every year
top.3.rated.movies <- edxS %>% group_by(movieId) %>% summarize(count_ratings = n()) %>% 
  arrange(desc(count_ratings)) %>% slice(1:3) %>% pull(movieId)

edxS %>% filter(movieId %in% top.3.rated.movies) %>% group_by(movieId, yr) %>% 
  summarize(avg_ratings = mean(rating)) %>% ggplot(aes(x=yr, y = avg_ratings, color = as.factor(movieId))) +
  geom_line() + labs(x = 'Years', y = ' Average Ratings', color = 'Movie Id') + 
  ggtitle('Average yearly ratings of top 3 movies') + theme(legend.position = c(0.9, 0.85))

# It is interesting to see that, while we might feel that a movie rating should not 
# change much across the years, however, we can see that while usually among years
# avg rating of the same movie changes by 0.5 rating, there are instances when the change 
# is more than 1 rating, which is a significant change
# Time lapsed from the release of movie can be a good variable in rating prediction

# Genre Exploration
genre.summary %>% ggplot(aes(x = reorder(value,-genre.perc), y = genre.perc)) + 
  geom_bar(stat = 'sum', show.legend = F) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = 'Genres', y = 'Percentage of observations') + ggtitle('Spread of genres')
# Most popular genres are Drama & Comedy, with around 40% of observations with these genres

# Count of distinct genres present in a movie
p <- edxS %>% mutate(genre.count = rowSums(edxS[,7:26])) %>% group_by(movieId) %>% 
  summarize(genre.count = mean(genre.count)) %>% ggplot(aes(x = genre.count)) + stat_ecdf(geom='step') +
  scale_y_continuous(labels = scales::percent) + labs(y = 'Cummulative Percentage of Movies', x = 'Count of Genres')
cumm.plot.with.labels.movies.genres(p)
# Avg number of genres per movie is 2

# Count of distinct genres watched by user
x <-  aggregate(edxS[,7:26],by = list(userId=edxS$userId),function(genre) if(any(genre==T)) return(1) else return(0))
x$Cnt.Genres.by.Users <- rowSums(x[,-1])
p <- x %>% ggplot(aes(x = Cnt.Genres.by.Users))+ stat_ecdf(geom='step') +
  scale_y_continuous(labels = scales::percent) + labs(y = 'Cummulative Percentage of Users', x = 'Count of Genres')
cumm.plot.with.labels.users.genres(p)
# Avg number of genres per user is 16 and min number of genres watched by any user is 8


###################################################################################
# PART 2 - Data Exploration
# Data exploration is performed on smaller dataset (100K ratings), so that we
# can get the insights quickly on the local machine with limited memory

# 2. Exploring relationship of ratings with independent variables
###################################################################################

# Ratings & users
# Is there a bias of users as they give ratings. Are there users who generally give
# lower ratings and are there users who generally give higher ratings
edxS[,1:3] %>% group_by(userId) %>% mutate(avg.user.rating = mean(rating)) %>% ungroup() %>% 
         group_by(movieId) %>% mutate(avg.movie.rating = mean(rating)) %>% ungroup() %>% 
         mutate(diff.user.and.movie.rating = rating-avg.movie.rating) %>% group_by(userId) %>% 
         summarize(count.of.ratings = n(), avg.diff.in.ratings = mean(diff.user.and.movie.rating)) %>% filter(count.of.ratings<1000) %>% 
         ggplot(aes(x=count.of.ratings, y = avg.diff.in.ratings, color = ifelse(count.of.ratings>25 & abs(avg.diff.in.ratings)>0.25, 'High Diff', 'Normal'))) + 
         geom_point(show.legend = F) + scale_color_manual(values=c("red", "lightblue")) + labs(x = 'Count of ratings', y = 'Difference of user ratings and avg movie rating') +
         ggtitle('Users who generally give higher ratings or lower ratings as compared to others', subtitle = 'The red dots denote users with more than 25 ratings and more than .25 rating difference than others')
# There is a user bias when ratings are given. Some users generally give lower than others and some higher

# Ratings & Movies
# Is there variations within a movie ratings?
# We already know that 50% of movies have only 2 ratings. Our analysis will not make sense
# if we do not consider movies with at least a few more ratings. 
# Lets consider movies with at least 10 ratings and lets see if there are variations in ratings
edxS[,1:3] %>% group_by(movieId) %>% filter(n()>10) %>% summarize(count.of.ratings = n(), avg.rating = mean(rating), sd.rating = sd(rating)) %>% 
  ggplot(aes(x=avg.rating, y=sd.rating, color = ifelse(sd.rating>=1,'High variation','Less variation'))) + 
  scale_color_manual(values=c("red", "lightblue")) + geom_point(show.legend = F) + labs(x = 'Avg rating', y = 'Standard Deviation in ratings') +
  ggtitle('Comparison of avg rating and std dev of ratings for movies with 10+ ratings', subtitle = 'Red dots shows movies with high variations in ratings')
# There is considerable variations within the ratings. It can happen in lower rated movies as well as higher rated movies.

# Ratings & Year
edxS%>% ggplot(aes(y = rating, x = yr, group = yr)) + geom_boxplot(outlier.shape = NA) + 
  scale_x_continuous(name ="Years", breaks=seq(1996,2018,1)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = 'Years', y = 'Ratings') + ggtitle('Distribution of ratings across years')
# There are few years when ratings are higher or lower than usual

# Ratings & Months
edxS%>% ggplot(aes(y = rating, x = mnth, group = mnth)) + geom_boxplot(outlier.shape = NA) + 
  scale_x_continuous(name ="Months", breaks=seq(1,12,1)) + labs(y = 'Ratings') + ggtitle('Distribution of ratings across Months')
# There are few months when ratings are higher than usual

# Ratings & Day of week
edxS%>% ggplot(aes(y = rating, x = day, group = day)) + geom_boxplot(outlier.shape = NA) + 
  scale_x_continuous(name ="Day of Week", breaks=seq(1,7,1)) + labs(y = 'Ratings') + ggtitle('Distribution of ratings across Day of Week')
# Ratings do not change basis day of week

# Ratings & Hour
edxS%>% ggplot(aes(y = rating, x = hr, group = hr)) + geom_boxplot(outlier.shape = NA) + 
  scale_x_continuous(name ="Hour", breaks=seq(1,24,1)) + labs(y = 'Ratings') + ggtitle('Distribution of ratings across Hours')
# Ratings are higher early in the morning or during evening than usual

# Ratings & Genre
edxS[,c(3,7:25)] %>% gather(key = 'genre', value='presence',-rating) %>% filter(presence == T) %>% 
  ggplot(aes(x=genre, y = rating, group = genre)) + geom_boxplot(outlier.shape = NA) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = 'Genres', y = 'Ratings') + ggtitle('Distribution of ratings across genres')
# There is a significant impact of genres on ratings



###################################################################################
# PART 3 - Data Preparation for Model Building
# 1. Creating train & test datasets from edx dataset for model parameter estimation
#    so that Validation set is used only for final model validation      
###################################################################################

# Creating train_set & test_set on edxB (larger dataset, used for final model train & validation using selected modelling technique)
# Note: this time might take time. Alternatively you may load('SavedObjects/DataLargeTestAndTrain.rdata')
test.and.train.big <- create.test.and.train.from.edx(edxB)
train.setB <- test.and.train.big$train_set                # will be used in PART 5
test.setB <- test.and.train.big$test_set                  # will be used in PART 5
rm(test.and.train.big)
# save(train.setB, test.setB, file = 'DataLargeTestAndTrain.rdata')
# Creating train_set & test_set on edxS (smaller dataset, used for selecting modeling technique with lowest RMSE )
# Alternatively you may load('SavedObjects/DataSmallTestAndTrain.rdata')test.and.train.small <- create.test.and.train.from.edx(edxS)
train.setS <- test.and.train.small$train_set              # will be used in PART 4
test.setS <- test.and.train.small$test_set                # will be used in PART 4
rm(test.and.train.small)
#save(train.setS,test.setS, file = 'DataSmallTestAndTrain.rdata')

###################################################################################
# PART 3 - Data Preparation for Model Building
# 2. Create a blank dataset called rmse.results to store the RMSE of all  modeling 
#    techniques applied on smaller dataset for final modeling technique selection
###################################################################################

rmse.results <- data.frame(SNo = integer(), ModelType = character(), Algorithm = character(), RMSE = double(), stringsAsFactors=F)


###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 1 - Baseline Model
# Model 1 - Random Rating with equal probability
###################################################################################

# 0.1 rating is allocated to each possible 10 ratings between 0.5 to 5
model.param.rating.prob <- rep(1/length(unique(edxS$rating)),10)
predicted.rating <- sample(seq(0.5,5,0.5), size = nrow(validationS), replace = T, prob = model.param.rating.prob)
model.rmse <- rmse(validationS$rating,predicted.rating)
rmse.results[nrow(rmse.results)+1,] <- c(1, 'Baseline model', 'Random rating with 0.1 prob', model.rmse)
view.rmse()


###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 1 - Baseline Model
# Model 2 - Random Rating with existing probability in training dataset
###################################################################################

# Existing prob of ratings in training dataset
model.param.rating.prob <- edxS %>% group_by(rating) %>% summarise(perc = n()/nrow(edxS)) %>% pull(perc)
predicted.rating <- sample(seq(0.5,5,0.5), size = nrow(validationS), replace = T, prob = model.param.rating.prob)
model.rmse <- rmse(validationS$rating,predicted.rating)
rmse.results[nrow(rmse.results)+1,] <- c(2, 'Baseline model', 'Random rating with existing prob', model.rmse)
view.rmse()


###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 1 - Baseline Model
# Model 3 - Mean Rating
###################################################################################

model.param.meanRating <- mean(edxS$rating)
predicted.rating <- rep(model.param.meanRating,nrow(validationS))
model.rmse <- rmse(validationS$rating,predicted.rating)
rmse.results[nrow(rmse.results)+1,] <- c(3, 'Baseline model', 'Mean rating', model.rmse)
view.rmse()


###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 2 - Linear Models
# Model 4 - Movie Effect
###################################################################################

# Individual movie effect is considered
mean.rating <- mean(edxS$rating) 
movie.effect <- edxS %>% group_by(movieId) %>% summarize(b_i = mean(rating - mean.rating))

predicted.rating <- validationS %>% left_join(movie.effect,by='movieId') %>% mutate(pred = mean.rating + b_i) %>% pull(pred) 
model.rmse <- rmse(validationS$rating,predicted.rating)
rmse.results[nrow(rmse.results)+1,] <- c(4, 'Linear model', 'Movie effect only', model.rmse)
view.rmse()


###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 2 - Linear Models
# Model 5 - Movie & User Effect
###################################################################################

# Individual user effect is considered
mean.rating <- mean(edxS$rating) 
user.effect <- edxS %>% left_join(movie.effect,by='movieId') %>% 
  group_by(userId) %>% summarize(u_i = mean(rating - mean.rating- b_i))

predicted.rating <- validationS %>% left_join(movie.effect,by='movieId') %>% 
  left_join(user.effect,by='userId') %>% 
  mutate(pred = mean.rating + b_i + u_i) %>% 
  pull(pred) 
model.rmse <- rmse(validationS$rating,predicted.rating)
rmse.results[nrow(rmse.results)+1,] <- c(5, 'Linear model', 'Movie & user effect', model.rmse)
view.rmse()


###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 2 - Linear Models
# Model 6 - Movie, User & Genre Effect
###################################################################################

# Individual user effect is considered within a genre
# Top 6 genres in terms of popularity is considered

mean.rating <- mean(edxS$rating) 

user.effect.Drama <- edxS %>% left_join(movie.effect,by='movieId') %>% 
  group_by(userId,Drama) %>% summarize(u_i_Drama = mean(rating - mean.rating- b_i))
edxS2 <- edxS %>% left_join(movie.effect,by='movieId') %>% 
  left_join(user.effect.Drama,by=c('userId','Drama'))

user.effect.Comedy <- edxS2 %>% group_by(userId,Comedy) %>% 
              summarize(u_i_Comedy = mean(rating - mean.rating- b_i-u_i_Drama))
edxS2 <- edxS2 %>% left_join(user.effect.Comedy,by=c('userId','Comedy'))

user.effect.Action <- edxS2 %>% group_by(userId,Action) %>% 
  summarize(u_i_Action = mean(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy))
edxS2 <- edxS2 %>% left_join(user.effect.Action,by=c('userId','Action'))

user.effect.Thriller <- edxS2 %>% group_by(userId,Thriller) %>% 
  summarize(u_i_Thriller = mean(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action))
edxS2 <- edxS2 %>% left_join(user.effect.Thriller,by=c('userId','Thriller'))

user.effect.Adventure <- edxS2 %>% group_by(userId,Adventure) %>% 
  summarize(u_i_Adventure = mean(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action-u_i_Thriller))
edxS2 <- edxS2 %>% left_join(user.effect.Adventure,by=c('userId','Adventure'))

user.effect.Romance <- edxS2 %>% group_by(userId,Romance) %>% 
  summarize(u_i_Romance = mean(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action-u_i_Thriller-u_i_Adventure))
edxS2 <- edxS2 %>% left_join(user.effect.Romance,by=c('userId','Romance'))

user.effect.OtherGenre <- edxS2 %>% group_by(userId) %>% 
  summarize(u_i_OtherGenre = mean(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action-u_i_Thriller-u_i_Adventure-u_i_Romance))
edxS2 <- edxS2 %>% left_join(user.effect.OtherGenre,by='userId')

predicted.rating <- validationS %>% left_join(movie.effect,by='movieId') %>% 
  left_join(user.effect.Drama,by=c('userId','Drama')) %>% 
  left_join(user.effect.Comedy,by=c('userId','Comedy')) %>% 
  left_join(user.effect.Action,by=c('userId','Action')) %>% 
  left_join(user.effect.Thriller,by=c('userId','Thriller')) %>% 
  left_join(user.effect.Adventure,by=c('userId','Adventure')) %>% 
  left_join(user.effect.Romance,by=c('userId','Romance')) %>% 
  left_join(user.effect.OtherGenre,by='userId') %>% 
  mutate(pred = mean.rating + b_i + coalesce(u_i_Drama,0) + coalesce(u_i_Comedy,0) + coalesce(u_i_Action,0) + coalesce(u_i_Thriller,0) + coalesce(u_i_Adventure,0) + coalesce(u_i_Romance,0) + coalesce(u_i_OtherGenre,0)) %>% 
  pull(pred)

model.rmse <- rmse(validationS$rating,predicted.rating)
rmse.results[nrow(rmse.results)+1,] <- c(6, 'Linear model', 'Movie, user & genre effect', model.rmse)
view.rmse()
rm(edxS2)

###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 2 - Linear Models
# Model 7 - Movie & User Effect with Regularization
###################################################################################

# Choosing different lambda for movie & user
lambdas_m <- seq(2, 10, 0.25)
lambdas_u <- seq(2, 10, 0.25)
lambdas <- expand.grid(lambdas_m, lambdas_u)

# selecting best combination of lambdas. 
# Using train.setS & test.setS to estimate the lambdas.
# Note - this will take time to run.
rmses <- apply(lambdas, 1, model.regularization, train=train.setS, test=test.setS)
cat('Best model parameters \nMovie Lambda - ', lambdas[which.min(rmses),1],
    '\nUser Lambda - ', lambdas[which.min(rmses),2], '\nMin RMSE: ', min(rmses))

# Best model parameters 
# Movie Lambda -  3 
# User Lambda -  2 
# Min RMSE:  0.8476

model.rmse <- model.regularization(c(lambdas[which.min(rmses),1],lambdas[which.min(rmses),2]), edxS, validationS)
rmse.results[nrow(rmse.results)+1,] <- c(7, 'Linear model', 'Movie & user effect with regularization', model.rmse)
view.rmse()

###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 2 - Linear Models
# Model 8 - Movie, User & Genre Effect with Regularization
###################################################################################

# Choosing different lambda for movie & user
lambdas_m <- seq(2, 10, 0.25)
lambdas_u <- seq(2, 10, 0.25)
lambdas <- expand.grid(lambdas_m, lambdas_u)

# selecting best combination of lambdas. 
# Using train.setS & test.setS to estimate the lambdas.
# Note - this will take time to run.
rmses <- apply(lambdas, 1, model.regularization.with.genre, train=train.setS, test=test.setS)
cat('Best model parameters \nMovie Lambda - ', lambdas[which.min(rmses),1],
      '\nUser Lambda - ', lambdas[which.min(rmses),2], '\nMin RMSE: ', min(rmses))

# Best model parameters 
# Movie Lambda -  3 
# User Lambda -  10 
# Min RMSE:  0.83289

# Looks like there is further scope to improve RMSE, as user Lambda is at the boundary
# Makes sense to increase the range of user lambda 10 onwards, while keeping movie lambda 
# fixed at 3, to reduce the number of combinations
lambdas_m <- lambdas[which.min(rmses),1]
lambdas_u <- seq(10, 30, 0.25)
lambdas <- expand.grid(lambdas_m, lambdas_u)
rmses <- apply(lambdas, 1, model.regularization.with.genre, train=train.setS, test=test.setS)
cat('Best model parameters \nMovie Lambda - ', lambdas[which.min(rmses),1],
    '\nUser Lambda - ', lambdas[which.min(rmses),2], '\nMin RMSE: ', min(rmses))

tibble(lambdas_u=lambdas_u, rmses=rmses) %>% ggplot(aes(x=lambdas_u, y=rmses)) +
                                              geom_point()+ggtitle('Movie lambda fixed at 3')
# Best model parameters 
# Movie Lambda -  3 
# User Lambda -  19.25 
# Min RMSE:  0.83239

# final check with user lambda now fixed at 19.5 and moview lambdas changing
lambdas_m <- seq(2, 10, 0.25)
lambdas_u <- lambdas[which.min(rmses),2]
lambdas <- expand.grid(lambdas_m, lambdas_u)
rmses <- apply(lambdas, 1, model.regularization.with.genre, train=train.setS, test=test.setS)
cat('Best model parameters \nMovie Lambda - ', lambdas[which.min(rmses),1],
    '\nUser Lambda - ', lambdas[which.min(rmses),2], '\nMin RMSE: ', min(rmses))

tibble(lambdas_m=lambdas_m, rmses=rmses) %>% ggplot(aes(x=lambdas_m, y=rmses)) +
  geom_point()+ggtitle('User lambda fixed at 19.25')

# Best model parameters 
# Movie Lambda -  3 
# User Lambda -  19.25 
# Min RMSE:  0.83239

# Using these model parameters (movie & user lambda) on validation dataset
model.rmse = model.regularization.with.genre(l = c(lambdas[which.min(rmses),1], lambdas[which.min(rmses),2]), train = edxS, test = validationS)
rmse.results[nrow(rmse.results)+1,] <- c(8, 'Linear model', 'Movie, user & genre effect with regularization', model.rmse)
view.rmse()

###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 3 - Memory-Based Collaborative Filtering
# Model 9 - User Based Collaborative Filtering
###################################################################################

# For Collaborative Filtering technique, we can not use the existing training & validation sets
# We will need same items (or movies) for all users in training & validation
# Also for validation we will need same set of users with some ratings hidden away
# The model will predict the unknown ratings for the users in validation set basis the known ratings
# We will also need to reduce the number of users and movies further for this model to run
# otherwise R fails to allocate memory in larger matrix. This can be done by considering
# popular movies (which has been rated by more than a minimum number of users) and
# popular users (who have watched at least a certain number of movies)

# This means that the final RMSE achieved from Collaborative Techniques will not be comparable with
# the ratings achieved earlier, however they will still give us a good indication of the performance.
# In case the RMSE achieved on popular movies & popular users are still not the best RMSE among
# all other techniques than we will not worry about this method too much
# In case we see that this method gives us the best RMSE of all other methods than we will try to figure
# out how can we compare the RMSEs in a better way

# Steps involved in building Collaborative Filtering model on our 100K movie data set
# 1. Make union of edxS & validationS to make one single dataset - 'all.data.S'
# 2. Create a selection from this dataset for popular movies & users - 'popular.all.data.S'
# 3. Convert this dataset to Real Rating Matrix format (as RecommenderLab package needs this package) - real.rating.popular.all.data.S
# 4. Create 3 real rating matrices from this:
#                                             a) Training - training.all.data.S
#                                             b) Validation Known - validation.known.all.data.S
#                                             c) Validation Unknown - validation.unknown.all.data.S
# 5. Run User Based Collaborative Filtering (UBCF) model on training.all.data.S - model.UBCF
# 6. Predict unknown ratings on validation.known.all.data.S using model.UBCF
# 7. Measure RMSE by comparing the unknown ratings with ratings on validation.unknown.all.data.S

# 1. Make union of edxS & validationS to make one single dataset - 'all.data.S'
all.data.S <- rbind(edxS[,1:3], validationS[,1:3])

# 2. Create a selection from this dataset for popular movies & users - 'popular.all.data.S'
minRowCnt <- 20 # Selecting users who have rated at least 20 movies
minColCnt <- 50 # Selecting movies which has been rated by at least 50 users
popular.all.data.S <- all.data.S %>% group_by(movieId) %>% filter(n()>minColCnt) %>% ungroup() %>% 
  group_by(userId) %>% filter(n()>minRowCnt) %>% ungroup()

# 3. Convert this dataset to Real Rating Matrix format (as RecommenderLab package needs this package) - real.rating.popular.all.data.S
# I have created my own function to achieve this
real.rating.popular.all.data.S <- convert.all.data.to.realRatingMatrix(popular.all.data.S)
real.rating.popular.all.data.S # 466 users and 436 movies selected


# 4. Create 3 real rating matrices from this:
#                                             a) Training - training.all.data.S
#                                             b) Validation Known - validation.known.all.data.S
#                                             c) Validation Unknown - validation.unknown.all.data.S

# Checking for R Version and using set.seed function appropriately
if(as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor) >5) set.seed(1, sample.kind="Rounding") else set.seed(1)

n_fold <- 10  # k value for k fold cross validation
items_to_keep <- 15  # Items to consider in training set (less than min no of ratings )
rating_threshold <- 3.5  # Considering a rating of 3.5 as good rating across all movies

eval_sets <- evaluationScheme(data = real.rating.popular.all.data.S, method = "cross-validation", k = n_fold, given = items_to_keep, goodRating = rating_threshold)
training.all.data.S <- getData(eval_sets, "train")  # training set
validation.known.all.data.S <- getData(eval_sets, "known")  # known validation set
validation.unknown.all.data.S <- getData(eval_sets, "unknown")  # unknown validation set

training.all.data.S # 89% users are present in training. 414 out of 466 users.
validation.known.all.data.S # 11% users are present in testing. 52 out of 466 users. 15 ratings of each user is kept in this set.
validation.unknown.all.data.S # Same 10% users as above are kept here as well. Each user has earlier a minimum of 20 ratings. Remaining ratings post previous selection is kept here. So we will be predicting at least 5 ratings for each of the 26 users

# 5. Run User Based Collaborative Filtering (UBCF) model on training.all.data.S - model.UBCF
model.UBCF <- Recommender(data = training.all.data.S, method = "UBCF", parameter = list(method = "Cosine"))

# 6. Predict unknown ratings on validation.known.all.data.S using model.UBCF - prediction.UBCF
items_to_recommend <- 10 # upto 10 unknown ratings will be predicted
prediction.UBCF <- predict(object = model.UBCF, newdata = validation.known.all.data.S, n = items_to_recommend, type = "ratings")

# 7. Measure RMSE by comparing the unknown ratings with ratings on validation.unknown.all.data.S - rmse.UBCF
rmse.UBCF <- calcPredictionAccuracy(x = prediction.UBCF, data = validation.unknown.all.data.S, byUser = FALSE)[1]
rmse.UBCF # RMSE of 0.9081786 can be improved further if we select users with higher number of movies rated 
# RMSE drops to 0.8717059 if we select only users with minimum of 50 movie rated

rmse.results[nrow(rmse.results)+1,] <- c(9, 'Memory Based Collaborative Filtering', 'UBCF - Movies (50+ ratings) & Users (20+ ratings)', round(rmse.UBCF,5))
view.rmse()
# save.image('backup1.rdata')


###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 3 - Memory-Based Collaborative Filtering
# Model 10 - Item Based Collaborative Filtering
###################################################################################

# We will utilize the training, validation known & validation unknown datasets for IBCF technique

# 1. Run Item Based Collaborative Filtering (UBCF) model on training.all.data.S - model.IBCF
model.IBCF <- Recommender(data = training.all.data.S, method = "IBCF", parameter = list(method = "Cosine"))

# 2. Predict unknown ratings on validation.known.all.data.S using model.UBCF - prediction.UBCF
items_to_recommend <- 10 # upto 10 unknown ratings will be predicted
prediction.IBCF <- predict(object = model.IBCF, newdata = validation.known.all.data.S, n = items_to_recommend, type = "ratings")

# 3. Measure RMSE by comparing the unknown ratings with ratings on validation.unknown.all.data.S - rmse.IBCF
rmse.IBCF <- calcPredictionAccuracy(x = prediction.IBCF, data = validation.unknown.all.data.S, byUser = FALSE)[1]
rmse.IBCF # RMSE of 1.262425 

rmse.results[nrow(rmse.results)+1,] <- c(10, 'Memory Based Collaborative Filtering', 'IBCF - Movies (50+ ratings) & Users (20+ ratings)', round(rmse.IBCF,5))
view.rmse()

# We can see that Collaborative Filtering models can perform better only if we have users with a several rated movies
# In our case linear models with regularization have done better than Collaborative Filtering models
save(model.IBCF, file = 'model.IBCF.rdata')

###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 4 - Model-Based Collaborative Filtering
# Model 11 - Slope One
###################################################################################

# 1. Slope One will need the data to be prepared in a data.table format with column names as 
# user_id, item_id & rating. Also user_id & item_id should be of character type
# We will use the function - data.prepare.for.slope.one to get datasets ready for Slope One model
library(data.table)
library(plyr)
source('ExternalFunctions.R')
source('InternalFunctions.R')
load('SavedObjects/DataSmallOriginal.rdata')

edxS.SO <- data.prepare.for.slope.one(edxS[,1:3])
validationS.SO <- data.prepare.for.slope.one(validationS[,1:3])

# 2. Normalize ratings
edxS.SO.norm <- normalize_ratings(edxS.SO)

# 3. Build Slope One model
model.SO <- build_slopeone(edxS.SO.norm$ratings)

# 4. Make predictions on validation dataset (this step will take time to run)
predictions.SO <- predict_slopeone(model.SO, validationS.SO[ , c(1, 2), with = FALSE], edxS.SO.norm$ratings)
unnormalized.predictions.SO <- unnormalize_ratings(normalized = edxS.SO.norm, ratings = predictions.SO)

# 5. Calculating RMSE
rmse.SO <- rmse(validationS.SO$rating,unnormalized.predictions.SO$predicted_rating)
rmse.SO 
# RMSE of 0.87714 is not lower than the linear model with regularized user, movie & genre effects

rmse.results[nrow(rmse.results)+1,] <- c(11, 'Model Based Collaborative Filtering', 'Slope One', round(rmse.SO,5))
view.rmse()


###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 4 - Model-Based Collaborative Filtering
# Model 12 - SVD with Approximation
###################################################################################

# Even for SVD we can not use the existing datasets for training (edxS) and validation (validationS)
# We will also need to reduce the size of the dataset due to RAM limitation on local machine

# Steps to building the SVD approximation model
# 1. Make union of edxS & validationS to make one single dataset - 'all.data.S'
# 2. Create a selection from this dataset for popular movies & users - 'popular.all.data.S'
# 3. Convert this dataset to format required by SVDApproximation package - 'all.data.S'
# 4. Create matrices from this:
#                             a) Training - train_matrix
#                             b) Validation matrix for fine tuning model parameter r - valid_matrix
#                             c) Test matrix on which predictions will be matched with known ratings - test_matrix
# 5. Run SVD Approximation model - 'model.svdApprox'
# 6. Fine tune model parameter r by comparing performance with validation matrix - 'model_tunes'
# 7. Measure RMSE by comparing ratings in test matrix - 'rmse.svdApprox'

# 1. Make union of edxS & validationS to make one single dataset - 'all.data.S'
all.data.S <- rbind(edxS[,1:3], validationS[,1:3])

# 2. Create a selection from this dataset for popular movies & users - 'popular.all.data.S'
minColCnt <- 4 # Selecting movies which has been rated by at least 4 users
popular.all.data.S <- all.data.S %>% group_by(movieId) %>% filter(n()>minColCnt) %>% ungroup() 

# 3. Convert this dataset to format required by SVDApproximation package - 'all.data.S'
#       The dataset should have the complete sequence of user ids and movie ids. I have created a function create.sequenced.data() to achieve this
#       The column names should be 'user', 'item', 'rating'
#       The class of data set should be data.table

all.data.S <- create.sequenced.data(popular.all.data.S)[,c(1,2,5)]
colnames(all.data.S) <- c('user', 'item', 'rating')
all.data.S <- data.table(all.data.S)

# 4. Create matrices from this:
#                             a) Training - train_matrix
#                             b) Validation matrix for fine tuning model parameter r - valid_matrix
#                             c) Test matrix on which predictions will be matched with known ratings - test_matrix

# Checking for R Version and using set.seed function appropriately
if(as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor) >5) set.seed(1, sample.kind="Rounding") else set.seed(1)
mtx <- split_ratings(ratings_table = all.data.S, proportion = c(0.7, 0.15, 0.15))

# 5. Run SVD Approximation model - 'model.svdApprox'
model.svdApprox <- svd_build(mtx)

# 6. Fine tune model parameter r by comparing performance with validation matrix - 'model_tunes'
model_tunes <- svd_tune(model.svdApprox, r = 2:50)
model_tunes$train_vs_valid

# 7. Measure RMSE by comparing ratings in test matrix - 'rmse.svdApprox'
rmse.svdApprox <- svd_rmse(model.svdApprox, r = model_tunes$r_best, rmse_type = c("test"))
rmse.svdApprox 
# RMSE is similar to what we achieved in Slope One. 

rmse.results[nrow(rmse.results)+1,] <- c(12, 'Model Based Collaborative Filtering', 'SVD Approx. - Movies (3+ ratings)', round(rmse.svdApprox,5))
view.rmse()

###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 4 - Model-Based Collaborative Filtering
# Model 13 - Matrix Factorization with Stochastic Gradient Descent
###################################################################################
# Steps involved 
# 1. Convert the data in to the format that can be accepted by recosystem package
# 2. Build the model object
# 3. Fine tune the model for best model parameters
# 4. Train the model
# 5. Run the model on test data to predict the ratings
# 6. Calculate the RMSE on test data by comparing with actual

# 1. Convert the data in to the format that can be accepted by recosystem package
train_data <-  with(edxS, data_memory(user_index = userId, item_index = movieId, rating = rating))
test_data  <-  with(validationS,  data_memory(user_index = userId, item_index = movieId, rating = rating))

# 2. Build the model object
model.MF <-  recosystem::Reco()

# 3. Fine tune the model for best model parameters.
# This is the main step. If we can find out the right model parameters we can improve the model predictions significantly
# However there are multiple parameters to tune
# The best way is to do this iteratively -  
#                   Try each parameter with a few set of values. 
#                   Then by loking at the optimal parameters. fix a few parameters and change the others

# Checking for R Version and using set.seed function appropriately
if(as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor) >5) set.seed(1, sample.kind="Rounding") else set.seed(1)

opts <- model.MF$tune(train_data, opts = list(dim = c(300), 
                                       lrate = c(0.01),
                                       costp_l2 = c(0.01), 
                                       costq_l2 = c(0.1),
                                       nthread  = 4, niter = 10))
opts$min

# 4. Train the model
# Number of iterations plays a important role. If you increase it too much than the model will overfit on training data
model.MF$train(train_data, opts = c(opts$min, nthread = 4, niter = 100))

# 5. Run the model on test data to predict the ratings
predicted.ratings.MF <-  model.MF$predict(test_data, out_memory())
rmse.MF <-  rmse(validationS$rating, predicted.ratings.MF)
rmse.MF # 0.85368
# This is the best performance till now on the complete 100K dataset

rmse.results[nrow(rmse.results)+1,] <- c(13, 'Model Based Collaborative Filtering', 'Matrix Factorization with Stochastic Gradient Descent', round(rmse.MF,5))
view.rmse()

#save.image('backup2.rdata')
#save(edxS, validationS, file = 'CapstoneData.RData')
#rm(list=ls())
#load('CapstoneData.RData')

###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Type 5 - Ensemble Methods
# Model 14 - Ensemble Methods
###################################################################################
#clear unusued memory
invisible(gc())

# 1. Selecting variables for model building. Removing timestamp, title, genres, (no genres listed), date
edxS.selected <- edxS %>% select(-c("timestamp","title","genres", "(no genres listed)", "date" ))
validationS.selected <- validationS %>% select(-c("timestamp","title","genres", "(no genres listed)", "date" ))

# 2. Creating derived variables
# Number of movies rated by every user
edxS.selected <- edxS.selected %>% group_by(userId) %>%  mutate(n.movies_byUser = n())
validationS.selected <- validationS.selected %>% group_by(userId) %>%  mutate(n.movies_byUser = n())

# Number of users ratings by every movie
edxS.selected <- edxS.selected %>% group_by(movieId) %>% mutate(n.users_bymovie = n())
validationS.selected <- validationS.selected %>% group_by(movieId) %>% mutate(n.users_bymovie = n())

# Count of distinct genres present in a movie
edxS.selected$Cnt.Genres.in.Movie <- edxS[,7:26] %>% rowSums()
validationS.selected$Cnt.Genres.in.Movie <- validationS[,7:26] %>% rowSums()

# Count of distinct genres watched by user
x <-  aggregate(edxS[,7:26],by = list(userId=edxS$userId),function(genre) if(any(genre==T)) return(1) else return(0))
x$Cnt.Genres.by.Users <- rowSums(x[,-1])
edxS.selected <- edxS.selected %>% left_join(x[,c('userId','Cnt.Genres.by.Users')], by = 'userId')

x <-  aggregate(validationS[,7:26],by = list(userId=validationS$userId),function(genre) if(any(genre==T)) return(1) else return(0))
x$Cnt.Genres.by.Users <- rowSums(x[,-1])
validationS.selected <- validationS.selected %>% left_join(x[,c('userId','Cnt.Genres.by.Users')], by = 'userId')

# Count of movies watched by users in every genre
x <-  aggregate(edxS[,7:25],by = list(userId=edxS$userId),sum)
colnames(x)[-1] <- paste0('Cnt.',colnames(x)[-1])
edxS.selected <- edxS.selected %>% left_join(x, by = 'userId')

x <-  aggregate(validationS[,7:25],by = list(userId=validationS$userId),sum)
colnames(x)[-1] <- paste0('Cnt.',colnames(x)[-1])
validationS.selected <- validationS.selected %>% left_join(x, by = 'userId')

# 3. Converting userId, movieId & date variables to factors
edxS.selected <- edxS.selected %>% mutate(userId = as.factor(userId),
                                          movieId = as.factor(movieId),
                                          yr = as.factor(yr),
                                          mnth = as.factor(mnth),
                                          dt = as.factor(dt),
                                          day = as.factor(day),
                                          hr = as.factor(hr))

validationS.selected <- validationS.selected %>% mutate(userId = as.factor(userId),
                                                        movieId = as.factor(movieId),
                                                        yr = as.factor(yr),
                                                        mnth = as.factor(mnth),
                                                        dt = as.factor(dt),
                                                        day = as.factor(day),
                                                        hr = as.factor(hr))

# 4. Building Gradient Boosting Model
# Initializing H2O instance
h2o.init(nthreads=-1, max_mem_size = "10G")  # initializes with all available threads and 10Gb memory
h2o.removeAll() # frees up the memory

# Model iteration 1 - Number of folds for K-fold validation = 3
model.gbm1 <- h2o.gbm(y = "rating", training_frame = as.h2o(edxS.selected), nfolds = 3) # All variables except ratings are selected as independant variables
summary(model.gbm1)
predicted.ratings <- h2o.predict(model.gbm1,as.h2o(validationS.selected))
rmse.gbm1 <- rmse(as.h2o(validationS.selected$rating), predicted.ratings)
rmse.gbm1 # 0.93431. Not good RMSE. We will try to improve the model in the next iteration

# Model iteration 2
# Model run - Increasing number of trees from default 50 to 100 and depth from 5 to 10 and number of folds for K-fold validation = 5
h2o.removeAll()
model.gbm2 <- h2o.gbm(y = "rating", training_frame = as.h2o(edxS.selected), ntrees = 100, max_depth=10, nfolds = 5) 
summary(model.gbm2)
predicted.ratings <- h2o.predict(model.gbm2,as.h2o(validationS.selected))
rmse.gbm2 <- rmse(as.h2o(validationS.selected$rating), predicted.ratings)
rmse.gbm2 # 0.94129. Over fitting is happening. We will try to improve the model in the next iteration

# Model iteration 3
h2o.removeAll()
model.gbm3 <- h2o.gbm(y = "rating", training_frame = as.h2o(edxS.selected), ntrees = 50, max_depth=10, nfolds = 3, seed = 1,keep_cross_validation_predictions = TRUE, fold_assignment = "Random") 
summary(model.gbm3)
predicted.ratings <- h2o.predict(model.gbm3,as.h2o(validationS.selected))
rmse.gbm3 <- rmse(as.h2o(validationS.selected$rating), predicted.ratings)
rmse.gbm3 # 0.93777 We will stay with this RMSE for the time being.

# Model iteration 4
model.gbm4 <- h2o.gbm(y = "rating", training_frame = as.h2o(edxS.selected), ntrees = 50, max_depth=20, nfolds = 3) 
summary(model.gbm4)
predicted.ratings <- h2o.predict(model.gbm4,as.h2o(validationS.selected))
rmse.gbm4 <- rmse(as.h2o(validationS.selected$rating), predicted.ratings)
rmse.gbm4 # 0.96821 We will select the third model

rmse.results[nrow(rmse.results)+1,] <- c(14, 'Ensemble Model', 'GBM only', round(rmse.gbm3,5))
view.rmse()

# Before we move to the next model, lets see what factors impact the ratings the most.
h2o.varimp_plot(model.gbm3) # Plot of top 10 most important variables, sorted in the order of 
# relative variable importance. The most important variable is given a value 1

# While the model has not shown great performance, but it can tells us what are the key variables that 
# impact the ratings. 
# While we would have guessed that users plays the most important role in a movie rating prediction,
# it is interesting to see that date of rating, hour of the day when the rating was given, number of 
# users who have rated the movie and certain genre of movie like - drama & action plays a key role in 
# determining the ratings



# 5. Building Random Forest Model
model.rf1 <- h2o.randomForest(y = "rating", training_frame = as.h2o(edxS.selected), ntrees = 50, max_depth=20, nfolds = 3, seed = 1,keep_cross_validation_predictions = TRUE, fold_assignment = "Random")        
summary(model.rf1)
predicted.ratings <- h2o.predict(model.rf1,as.h2o(validationS.selected))
rmse.rf1 <- rmse(as.h2o(validationS.selected$rating), predicted.ratings)
rmse.rf1 # 0.95412. 

rmse.results[nrow(rmse.results)+1,] <- c(15, 'Ensemble Model', 'Random Forest only', round(rmse.rf1,5))
view.rmse()

# 6. Building the ensemble model
model.ensemble <- h2o.stackedEnsemble(y = "rating",training_frame = as.h2o(edxS.selected), 
                                      base_models = list(model.gbm3@model_id, model.rf1@model_id))

pred.ratings.ensemble <- h2o.predict(model.ensemble,as.h2o(validationS.selected))
rmse.ensemble <- RMSE(pred.ratings.ensemble, as.h2o(validationS.selected$rating))
rmse.ensemble # 0.9174705 is the final result of ensemble models

rmse.results[nrow(rmse.results)+1,] <- c(16, 'Ensemble Model', 'GBM + Random Forest', round(rmse.ensemble,5))
view.rmse()

###################################################################################
# PART 4 - Model Selection on 100K Movie Dataset
# Selection of modeling technique basis least RMSE on smaller dataset
###################################################################################

rmse.results[nrow(rmse.results)+1,] <- c(17, 'Target RMSE', 'Target RMSE', 0.86490)
rmse.results %>% arrange(RMSE)
# We can see that Model Based Collaborative Filtering techniques have given the best RMSE
# As Slope One method & SVD Approx method were run on popular movies & users only, hence we will 
# select Matrix Factorization with Stochastic Gradient Descent as the final modeling technique which 
# we will run on the larger dataset (10 Mn ratings)
# It is also interesting to see that linear model that captured the effect of movie, user & genre with regularization 
# has also performed than better than the target RMSE

###################################################################################
# PART 5 - Model Training & Validation on 10Mn Movie Dataset using selected model
###################################################################################

# 1. Convert the data in to the format that can be accepted by recosystem package
train_data <-  with(edxB, data_memory(user_index = userId, item_index = movieId, rating = rating))
test_data  <-  with(validationB,  data_memory(user_index = userId, item_index = movieId, rating = rating))

# 2. Build the model object
model.MF.10Mn <-  recosystem::Reco()

# 3. Fine tune the model for best model parameters.
# This is the main step. If we can find out the right model parameters we can improve the model predictions significantly
# However there are multiple parameters to tune
# The best way is to do this iteratively -  
#                   Try each parameter with a few set of values. 
#                   Then by loking at the optimal parameters. fix a few parameters and change the others

# Checking for R Version and using set.seed function appropriately
if(as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor) >5) set.seed(1, sample.kind="Rounding") else set.seed(1)

opts <- model.MF.10Mn$tune(train_data, opts = list(dim = c(10,20,30), lrate = c(0.1),
                                       costp_l1 = 0, costq_l1 = 0,
                                       costp_l2 = 0.01,costq_l2 = 0.1,
                                       nthread  = 4, niter = 10))
#opts$min

# 4. Train the model
# Number of iterations plays a important role. If you increase it too much than the model will overfit on training data
model.MF.10Mn$train(train_data, opts = c(opts$min, nthread = 4, niter = 100))

# 5. Run the model on test data to predict the ratings
predicted.ratings.MF.10Mn <-  model.MF.10Mn$predict(test_data, out_memory())
rmse.MF.10Mn <-  rmse(validationB$rating, predicted.ratings.MF.10Mn)
rmse.MF.10Mn # 0.78238
# The RMSE on the 10Mn dataset is better than the target

rmse.results[nrow(rmse.results)+1,] <- c(18, '10Mn dataset', 'Matrix Factorization with Stochastic Gradient Descent', round(rmse.MF.10Mn,5))
view.rmse()









