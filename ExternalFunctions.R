##################################################################################
# SlopeOne Algorithm
##################################################################################

#' Build slope one model
#'
#' @param ratings \code{data.table} of ratings. Should contain 3 columns: user_id (id of user, character), item_id (id of item, character) and rating (rating of item by user, integer or numeric)
#'
#' @return a \code{data.table} of (item_id1, item_id2, b, support) where b represents the average rating difference of 'item 2 rating' - 'item 1 rating'. support represents number of ratings used to compute b
#'
#' @examples
#' set.seed(1)
#' in_train <- rep(TRUE, nrow(MovieLense))
#' in_train[sample(1:nrow(MovieLense), size = length(unique(MovieLense$user_id)) * 5)] <- FALSE
#'
#' MovieLense_train <- MovieLense[(in_train)]
#'
#' ratings <- normalize_ratings(MovieLense_train)
#' model <- build_slopeone(ratings$ratings)
#'
#' @import data.table plyr
#'
#' @export

build_slopeone <- function(ratings, ...) {
  if (NROW(ratings) == 0) {
    return(data.table(data.frame(item_id1=c(), item_id2=c(), b=c(), support=c())))
  }
  score_diff_per_user <- dlply(ratings, .(user_id), function(rows) {
    if (NROW(rows) > 1) {
      # Get diffs for all item_id pairs.
      pair_rows_nums <- subset(
        expand.grid(rows_num1=1:NROW(rows), rows_num2=1:NROW(rows)),
        rows_num1 != rows_num2 &
          rows[rows_num1, 'item_id'] != rows[rows_num2, 'item_id'])
      data.table(
        item_id1=rows[pair_rows_nums$rows_num1, 'item_id'],
        item_id2=rows[pair_rows_nums$rows_num2, 'item_id'],
        diff=rows[pair_rows_nums$rows_num2, 'rating']
        - rows[pair_rows_nums$rows_num1, 'rating'])
    }
  }, ...)
  # ddply is slow when merging data frames within list while rbindlist is
  # much faster.
  score_diff_per_user <- rbindlist(score_diff_per_user)
  if (NROW(score_diff_per_user) == 0) {
    return(data.table(data.frame(item_id1=c(), item_id2=c(), b=c(), support=c())))
  }
  score_diff_per_user$item_id1 <- as.character(score_diff_per_user$item_id1)
  score_diff_per_user$item_id2 <- as.character(score_diff_per_user$item_id2)
  # Compute average score diff between item 1 and item 2.
  model <- score_diff_per_user[,
                               list(b=mean(diff), support=NROW(diff)),
                               by='item_id1,item_id2']
  setkey(model, item_id1, item_id2)
  return(model)
}


#' Normalize rating table
#'
#' @param ratings \code{data.table} of ratings. Should contain 3 columns: user_id (id of user, character), item_id (id of item, character) and rating (rating of item by user, integer or numeric)
#'
#' @return List of 4 objects:
#' \itemize{
#'   \item global - global rating mean
#'   \item user - \code{data.table} of users' means (id, mean)
#'   \item item - \code{data.table} of items' means (id, mean)
#'   \item ratings - \code{data.table} of normalized ratings
#' }
#'
#' @details
#' Factor rating table into global mean + user mean + item mean. This is usually a preliminary step before building a model
#'
#' @examples
#' set.seed(1)
#' in_train <- rep(TRUE, nrow(MovieLense))
#' in_train[sample(1:nrow(MovieLense), size = length(unique(MovieLense$user_id)) * 5)] <- FALSE
#'
#' MovieLense_train <- MovieLense[(in_train)]
#'
#' ratings <- normalize_ratings(MovieLense_train)
#'
#' @import data.table
#'
#' @export

normalize_ratings <- function(ratings, ...) {
  result <- list()
  result$global <- ratings[, mean(rating)]
  result$user <- ratings[, list(mean_rating=mean(rating)), by='user_id']
  result$item <- ratings[, list(mean_rating=mean(rating)), by='item_id']
  
  ratings$rating <- ratings$rating - result$global
  setkey(result$user, user_id)
  ratings$rating <- ratings$rating - result$user[J(ratings$user_id), ]$mean_rating
  setkey(result$item, item_id)
  ratings$rating <- ratings$rating - result$item[J(ratings$item_id), ]$mean_rating
  result$ratings <- ratings
  return(result)
}


#' Predict ratings for multiple users and items given known ratings
#'
#' @param model \code{data.table} of produced by \code{\link{build_slopeone}}
#' @param target \code{data.table} of (user_id, item_id) to predict ratings
#' @param ratings \code{data.table} of known ratings. Should contain 3 columns: user_id (id of user, character), item_id (id of item, character) and rating (rating of item by user, integer or numeric)
#'
#' @return a \code{data.table} containig (user_id, item_id, predicted_rating)
#'
#' @examples
#' set.seed(1)
#' in_train <- rep(TRUE, nrow(MovieLense))
#' in_train[sample(1:nrow(MovieLense), size = length(unique(MovieLense$user_id)) * 5)] <- FALSE
#'
#' MovieLense_train <- MovieLense[(in_train)]
#'
#' ratings <- normalize_ratings(MovieLense_train)
#' model <- build_slopeone(ratings$ratings)
#'
#' predictions <- predict_slopeone(model, MovieLense[(!in_train), c(1, 2), with = FALSE], ratings$ratings)
#'
#' @import data.table
#'
#' @export

predict_slopeone <-function(model, targets, ratings, ...) {
  setkey(ratings, user_id)
  adply(targets,
        1,
        function(row) {
          data.frame(
            predicted_rating=predict_slopeone_for_user(
              model, row$item_id, ratings[J(row$user_id), ]))
        }, ...)
}


#' Predict score for target_item_id given the known ratings of a single user
#'
#' @param model \code{data.table} of produced by \code{\link{build_slopeone}}
#' @param target_item_id target item id to predict rating
#' @param ratings \code{data.table} of user's known ratings. Should contain 3 columns: user_id (id of user, character), item_id (id of item, character) and rating (rating of item by user, integer or numeric)
#'
#' @return predicted rating score
#'
#' @examples
#' set.seed(1)
#' in_train <- rep(TRUE, nrow(MovieLense))
#' in_train[sample(1:nrow(MovieLense), size = length(unique(MovieLense$user_id)) * 5)] <- FALSE
#'
#' MovieLense_train <- MovieLense[(in_train)]
#'
#' ratings <- normalize_ratings(MovieLense_train)
#' model <- build_slopeone(ratings$ratings)
#' predict_slopeone_for_user(model, "Volcano (1997)", ratings$ratings[user_id == "1", c(2, 3), with = FALSE])
#'
#' @import data.table
#'
#' @export

predict_slopeone_for_user <- function(model, target_item_id, ratings) {
  # If target_id is already rated by the user, return that rating.
  already_rated <- subset(ratings, ratings$item_id == target_item_id)
  if (NROW(already_rated) == 1) {
    return(already_rated$rating)
  } else if (NROW(already_rated) > 1) {
    warning(paste(target_item_id,
                  ' is already rated by user, but there are multiple ratings.'))
    return(already_rated[1, ]$rating)
  }
  if (NROW(model) == 0) {
    return(NA)
  }
  # Compute weighted average ratings.
  ratings <- rename(ratings, c('item_id'= "item_id1"))
  ratings <- cbind(ratings, item_id2=target_item_id)
  setkey(ratings, item_id1, item_id2)
  joined <- model[ratings, ]
  joined <- joined[complete.cases(joined), ]
  if (NROW(joined) == 0) {
    return(NA)
  }
  return(sum(joined[, (b + rating) * support]) /
           sum(joined[, sum(support)]))
}


#' Un-normalize rating table
#'
#' @param normalized normalization information generated by \code{\link{normalize_ratings}}
#' @param ratings \code{data.table} of ratings. Should contain 3 columns: userI_id (id of user, character), item_id (id of item, character) and predicted_rating (predicted rating of item by user, integer or numeric)
#'
#' @return a ratings \code{data.table} after un-normalization
#'
#' @details
#' Need to apply this after prediction is made using a model built from normalized ratings
#'
#' @examples
#' set.seed(1)
#' in_train <- rep(TRUE, nrow(MovieLense))
#' in_train[sample(1:nrow(MovieLense), size = length(unique(MovieLense$user_id)) * 5)] <- FALSE
#'
#' MovieLense_train <- MovieLense[(in_train)]
#'
#' ratings <- normalize_ratings(MovieLense_train)
#' model <- build_slopeone(ratings$ratings)
#'
#' predictions <- predict_slopeone(model, MovieLense[(!in_train), c(1, 2), with = FALSE], ratings$ratings)
#' real_ratings <- unnormalize_ratings(normalized = ratings, ratings = predictions)
#'
#' rmse <- sqrt(mean((real_ratings$predicted_rating - MovieLense[(!in_train)]$rating) ^ 2))
#'
#' @import data.table
#'
#' @export


unnormalize_ratings <- function(normalized, ratings) {
  ratings$predicted_rating <- ifelse(is.na(ratings$predicted_rating), 0,
                                     ratings$predicted_rating)
  ratings$predicted_rating <- ratings$predicted_rating + normalized$global
  setkey(normalized$user, user_id)
  user_mean <- normalized$user[J(ratings$user_id), ]$mean_rating
  ratings$predicted_rating <- ratings$predicted_rating +
    ifelse(!is.na(user_mean), user_mean, 0)
  setkey(normalized$item, item_id)
  item_mean <- normalized$item[J(ratings$item_id), ]$mean_rating
  ratings$predicted_rating <- ratings$predicted_rating +
    ifelse(!is.na(item_mean), item_mean, 0)
  return(ratings)
}



##################################################################################
# SVD Approximation Algorithm
##################################################################################

#' Split ratings data into train, validation and test sets
#'
#' @param ratings_table \code{\link{data.table}} or \code{\link{data.frame}} of ratings. Should contain 3 columns: user (id of user, integer), item (id of item, integer) and rating (rating of item by user, integer or numeric)
#' @param train_test_splitted (default FALSE) is dataset is already splitted into train and test set (column train with TRUE and FALSE should be in ratings_table)
#' @param proportion (default c(0.7, 0.15, 0.15)) vector of 3 numeric values for train_test_splitted=FALSE, which sums to 1 and indicate proportion of ratings to be set to train, validation and test sets. And of length 2 for train_test_splitted=TRUE, which indicates in what proportion should be splitted train set into train and validation
#'
#' @return List of 6 objects:
#' \itemize{
#'   \item train_matrix - sparse matrix with ratings for train
#'   \item valid_matrix - sparse matrix with ratings for validation
#'   \item test_matrix - sparse matrix with ratings for test
#'   \item train_matrix_ones - train_matrix with ratings replaced with ones
#'   \item valid_matrix_ones - valid_matrix with ratings replaced with ones
#'   \item test_matrix_ones - test_matrix with ratings replaced with ones
#' }
#'
#' @details
#' Function splits data into train, validation and test sets and transforms it into sparse matrix objects
#'
#' @examples
#'
#' mtx <- split_ratings(ratings)
#'
#' @import Matrix recommenderlab data.table
#'
#' @export
#' 


split_ratings <- function(ratings_table, train_test_splitted = FALSE, proportion = c(0.7, 0.15, 0.15)) {
  
  if (train_test_splitted == FALSE) {
    in_train <- rep(TRUE, nrow(ratings_table))
    in_train[sample(1:nrow(ratings_table), size = round((1 - proportion[1]) * nrow(ratings_table), 0))] <- FALSE
    
    in_valid <- rep(FALSE, nrow(ratings_table))
    in_valid[sample(which(in_train == FALSE), size = round(proportion[2] * nrow(ratings_table), 0))] <- TRUE
    
    ratings_table$train <- in_train
    ratings_table$validation <- in_valid
  }
  
  if (train_test_splitted == TRUE) {
    in_valid <- rep(FALSE, nrow(ratings_table))
    in_valid[sample(which(ratings_table$train == TRUE), size = round(proportion[2] * nrow(ratings_table), 0))] <- TRUE
    
    ratings_table$validation <- in_valid
    ratings_table[validation == TRUE, train := FALSE]
  }
  
  
  train_matrix <- sparseMatrix(i = ratings_table[train == TRUE]$user, j = ratings_table[train == TRUE]$item, x = ratings_table[train == TRUE]$rating,
                               dims = c(length(unique(ratings_table$user)), length(unique(ratings_table$item))),
                               dimnames = list(paste("u", 1:length(unique(ratings_table$user)), sep = "") , paste("m", 1:length(unique(ratings_table$item)), sep = "")))
  
  valid_matrix <- sparseMatrix(i = ratings_table[validation == TRUE]$user, j = ratings_table[validation == TRUE]$item, x = ratings_table[validation == TRUE]$rating,
                               dims = c(length(unique(ratings_table$user)), length(unique(ratings_table$item))),
                               dimnames = list(paste("u", 1:length(unique(ratings_table$user)), sep = "") , paste("m", 1:length(unique(ratings_table$item)), sep = "")))
  
  test_matrix <- sparseMatrix(i = ratings_table[train == FALSE & validation == FALSE]$user, j = ratings_table[train == FALSE & validation == FALSE]$item, x = ratings_table[train == FALSE & validation == FALSE]$rating,
                              dims = c(length(unique(ratings_table$user)), length(unique(ratings_table$item))),
                              dimnames = list(paste("u", 1:length(unique(ratings_table$user)), sep = "") , paste("m", 1:length(unique(ratings_table$item)), sep = "")))
  
  train_matrix_ones <- as(binarize(new("realRatingMatrix", data = train_matrix), minRating=1), "dgCMatrix")
  valid_matrix_ones <- as(binarize(new("realRatingMatrix", data = valid_matrix), minRating=1), "dgCMatrix")
  test_matrix_ones <- as(binarize(new("realRatingMatrix", data = test_matrix), minRating=1), "dgCMatrix")
  
  return (list(train_matrix = train_matrix, valid_matrix = valid_matrix, test_matrix = test_matrix, train_matrix_ones = train_matrix_ones, valid_matrix_ones = valid_matrix_ones, test_matrix_ones = test_matrix_ones))
}

#' Build SVD Recommender Model
#'
#' @param mtx list of matrixes for training, validation and testing recommender, returned by \code{\link{split_ratings}} function
#'
#' @return List of 3 objects:
#' \itemize{
#'   \item decomposed_matrix - list of matrixes, returned by \code{\link{svd}} decomposition
#'   \item user_average - vector of average rating of each user
#'   \item mtx - list of train, validation and test sparse matrixes
#' }
#'
#' @details
#' Function performs SVD decomposition of user-item rating matrix
#'
#' @examples
#'
#' mtx <- split_ratings(ratings)
#' model <- svd_build(mtx)
#'
#' @import Matrix
#'
#' @export

svd_build <-  function(mtx) {
  
  train_matrix_user <- rowSums(mtx$train_matrix) / rowSums(mtx$train_matrix_ones)
  train_matrix_item <- colSums(mtx$train_matrix) / colSums(mtx$train_matrix_ones)
  train_matrix_item[which(is.na(train_matrix_item) == TRUE)] <- sum(mtx$train_matrix) / sum(mtx$train_matrix_ones)
  
  user_average <- matrix(rep(train_matrix_user, length(train_matrix_item)), ncol = length(train_matrix_item), byrow = FALSE)
  
  train <- mtx$train_matrix + matrix(rep(train_matrix_item, length(train_matrix_user)), nrow = length(train_matrix_user), byrow = TRUE) * (1 - mtx$train_matrix_ones)
  train <- train - user_average
  
  decomposed_matrix <- svd(train)
  
  d <- matrix(0, length(decomposed_matrix$d), length(decomposed_matrix$d))
  diag(d) <- decomposed_matrix$d
  
  decomposed_matrix$d <- d
  
  return(list(decomposed_matrix = decomposed_matrix, user_average = user_average, mtx = mtx))
}

#' Calculate RMSE of SVD Approximation model
#'
#' @param model model object, returned by \code{\link{svd_build}} function
#' @param r (default 10) number of latent factors to take into account to reconstruct rating matrix and make prediction
#' @param rmse_type (default c("train", "validation", "test")) vector, which indicates matrixes for which RMSE should be calculated
#'
#' @return vector of RMSE based on \code{rmse_type}. Errors are returned in next order: "train", "validation", "test"
#'
#' @details
#' Function calculate RMSE for SVD Approximation Recommender model for train, validation and test sets
#'
#' @examples
#'
#' mtx <- split_ratings(ratings)
#' model <- svd_build(mtx)
#' svd_rmse(model, r = 10, rmse_type = c("validation"))
#' svd_rmse(model, r = 5, rmse_type = c("train", "validation"))
#'
#' @import Matrix
#'
#' @export

svd_rmse <- function(model, r = 10, rmse_type = c("train", "validation", "test")) {
  
  pred <- model$decomposed_matrix$u[, 1:r] %*% model$decomposed_matrix$d[1:r, 1:r] %*% t(model$decomposed_matrix$v[,1:r]) + model$user_average
  
  rmse <- NULL
  
  if ("train" %in% rmse_type) {
    rmse <- c(rmse, sqrt(sum((pred * model$mtx$train_matrix_ones - model$mtx$train_matrix) ^ 2) / sum(model$mtx$train_matrix_ones)))
  }
  if ("validation" %in% rmse_type) {
    rmse <- c(rmse, sqrt(sum((pred * model$mtx$valid_matrix_ones - model$mtx$valid_matrix) ^ 2) / sum(model$mtx$valid_matrix_ones)))
  }
  if ("test" %in% rmse_type) {
    rmse <- c(rmse, sqrt(sum((pred * model$mtx$test_matrix_ones - model$mtx$test_matrix) ^ 2) / sum(model$mtx$test_matrix_ones)))
  }
  return(rmse)
}

#' Tune SVD Recommender Model
#'
#' @param model model object, returned by \code{\link{svd_build}} function
#' @param r (default 2:50) vector of number of latent factors to take into account while reconstructing rating matrix. From these values will be selected one with the smallest RMSE on validation set
#' @param color_pallet (default "Accent") name of palett to use to color graphs. See \code{\link{brewer.pal}}
#'
#' @return List of 3 objects:
#' \itemize{
#'   \item r_best - best value of r which was selected based on validation error
#'   \item all_rmse - \code{\link{data.frame}} with train and validation errors for each r
#'   \item train_vs_valid - ggplot graph of train and validation errors for different r
#' }
#'
#' @details
#' Function selects the best value for number of latent factors to minimize validation error
#'
#' @examples
#'
#' mtx <- split_ratings(ratings)
#' model <- svd_build(mtx)
#' svd_tune(model, r = 2:5)
#'
#' @import reshape2 ggplot2 RColorBrewer
#'
#' @export


svd_tune <- function(model, r = 2:50, color_pallet = "Accent") {
  
  rmse <- data.frame(r = integer(), train = numeric(), validation = numeric())
  
  for (k in r) {
    rmse_r <- svd_rmse(model, r = k, rmse_type = c("train", "validation"))
    rmse <- rbind(rmse, data.frame(r = k, train = rmse_r[1], validation = rmse_r[2]))
    cat("Model is evaluated for r =", k, "\n")
  }
  
  #Visualization train vs Validation errors
  
  data_plot <- melt(rmse, id.vars = "r")
  names(data_plot) <- c("r", "Type", "Value")
  
  cols <- brewer.pal(3, color_pallet)
  
  train_vs_valid <- ggplot(data = data_plot,
                           aes(x = r, y = Value, color = Type)) +
    geom_point(shape = 21, size = 6, fill="white", alpha=6/10) +
    geom_line(size = 1.1) +
    geom_vline(xintercept = rmse[which.min(rmse[,3]),1], linetype = "longdash", colour = cols[3], size = 1.5) +
    scale_color_manual(values=cols[1:2]) +
    labs(title = "Train and Validation RMSE for SVD Approximation", x = "Number of Components", y = "RMSE") +
    theme(axis.text = element_text(size=12), axis.title = element_text(size=17), title=element_text(size=15),
          legend.text = element_text(size=12))
  
  return(list(r_best = rmse[which.min(rmse[,3]),1], all_rmse = rmse, train_vs_valid = train_vs_valid))
}

#' Visualize statistics for all ratings, users' ratings and items' ratings
#'
#' @param ratings_table \code{\link{data.table}} or \code{\link{data.frame}} of ratings. Should contain 3 columns: user (id of user, integer), item (id of item, integer) and rating (rating of item by user, integer or numeric)
#' @param color_palett (default "Accent") name of palett to use to color graphs. See \code{\link{brewer.pal}}
#'
#' @return 5 graphs combined into one plot:
#' \itemize{
#'   \item count of different ratings
#'   \item histogram of users' average ratings
#'   \item histogram of items' average ratings
#'   \item histogram of number of rated items by user
#'   \item histogram of number of scores items have
#' }
#'
#' @details
#' Function combines 5 ggplot graphs into 1 plot and returns it
#'
#' @examples
#'
#' visualize_ratings(ratings_table = ratings)
#' visualize_ratings(ratings_table = ratings, color_palett = "Dark2")
#'
#' @import RColorBrewer Matrix recommenderlab ggplot2 gridExtra
#'
#' @export

visualize_ratings <- function(ratings_table, color_palett = "Accent") {
  
  #Select colors for graphs
  cols <- brewer.pal(3, color_palett)
  
  #Create sparse matrix
  ratings_sparse <- sparseMatrix(i = ratings_table$user, j = ratings_table$item, x = ratings_table$rating,
                                 dims = c(length(unique(ratings_table$user)), length(unique(ratings_table$item))),
                                 dimnames = list(paste("u", 1:length(unique(ratings_table$user)), sep = "") , paste("m", 1:length(unique(ratings_table$item)), sep = "")))
  
  #Create real rating matrix object for recommenderlab
  matrix_sparse <- new("realRatingMatrix", data = ratings_sparse)
  matrix_ones <- binarize(matrix_sparse, minRating=1)
  
  data_plot <- data.frame(table(getRatings(matrix_sparse)))
  names(data_plot) <- c("Score", "Count")
  
  all_ratings <- ggplot(data_plot,aes(x = Score, y = Count)) +
    geom_bar(stat="identity",colour="white", fill = cols[1]) +
    labs(title = "Count of different ratings") +
    theme(axis.text =element_text(size=12), axis.title=element_text(size=13), title=element_text(size=12))
  
  
  data_plot <- data.frame(rowMeans(matrix_sparse))
  names(data_plot) <- "UserAverageScore"
  users_ratings <- ggplot(data_plot,aes(x = UserAverageScore)) +
    geom_histogram(binwidth=0.1, colour = "white", fill = cols[2]) +
    geom_vline(xintercept=median(data_plot$UserAverageScore), color = "grey", size=2) +
    labs(title = "Histogram of Users' Average Ratings", x = "User Avearge Score", y = "Count") +
    theme(axis.text =element_text(size=12), axis.title=element_text(size=13), title=element_text(size=12)) +
    scale_x_continuous(breaks = c(1:5, round(median(data_plot$UserAverageScore), 2)))
  
  data_plot <- data.frame(colMeans(matrix_sparse))
  names(data_plot) <- "ItemAverageScore"
  items_ratings <- ggplot(data_plot,aes(x = ItemAverageScore)) +
    geom_histogram(binwidth=0.1, colour = "white", fill = cols[3]) +
    geom_vline(xintercept=median(data_plot$ItemAverageScore), color = "grey", size=2) +
    labs(title = "Histogram of Items' Average Ratings", x = "Item Avearge Score", y = "Count") +
    theme(axis.text =element_text(size=12), axis.title=element_text(size=13), title=element_text(size=12)) +
    scale_x_continuous(breaks = c(1:5, round(median(data_plot$ItemAverageScore), 2)))
  
  data_plot <- data.frame(rowSums(matrix_ones))
  names(data_plot) <- "UserRated"
  users_rated <- ggplot(data_plot,aes(x = UserRated)) +
    geom_histogram(binwidth=50, colour = "white", fill = cols[2]) +
    geom_vline(xintercept=median(data_plot$UserRated), color = "grey", size=2) +
    labs(title = "Histogram of Number of Rated items by user", x = "Number of Rated Items", y = "Users") +
    theme(axis.text =element_text(size=12), axis.title=element_text(size=13), title=element_text(size=12)) +
    scale_x_continuous(breaks = round(c(seq(min(data_plot$UserRated),max(data_plot$UserRated), length.out = 5), median(data_plot$UserRated)), 0))
  
  data_plot <- data.frame(colSums(matrix_ones))
  names(data_plot) <- "Rated"
  item_rated <- ggplot(data_plot,aes(x = Rated)) +
    geom_histogram(binwidth=50, colour = "white", fill = cols[3]) +
    geom_vline(xintercept=median(data_plot$Rated), color = "grey", size=2) +
    labs(title = "Histogram of Number of Scores Items have", x = "Number of Scores Item has", y = "Items") +
    theme(axis.text =element_text(size=12), axis.title=element_text(size=13), title=element_text(size=12)) +
    scale_x_continuous(breaks = round(c(seq(min(data_plot$Rated),max(data_plot$Rated), length.out = 5), median(data_plot$Rated)), 0))
  
  layout <- matrix(c(1, 1, 2, 3, 4, 5), ncol = 2, byrow = TRUE)
  return(grid.arrange(all_ratings, arrangeGrob(users_ratings, items_ratings, ncol=2), arrangeGrob(users_rated, item_rated, ncol=2), nrow = 3))
}


