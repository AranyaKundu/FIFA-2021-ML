library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(dplyr)
library(tibble)
library(ggplot2)
library(mice)
library(naniar)
library(gam)
library(glmnet)
library(forecast)
library(splitstackshape)
library(neuralnet)
library(DT)


# functions and other computations

# read data from csv
players_df <- read.csv("D:/Coursework/Mod-2/Machine Learning/Project/MLApp/players.csv",
               stringsAsFactors = TRUE, header = TRUE,
               na.strings = c("", " ", "NA"))

df <- read.csv("D:/Coursework/Mod-2/Machine Learning/Project/MLApp/fifa21.csv",
                       stringsAsFactors = TRUE, header = TRUE,
                       na.strings = c("", " ", "NA"))

# data pre-processing
colnames(df)[4] <- "OverAll_Rating" # Renaming columns
df$Weight <- gsub("lbs","", as.character(df$Weight)) |> as.numeric()
df$W.F <- gsub("★","", as.character(df$W.F)) |> as.numeric()
df$SM <- gsub("★","", as.character(df$SM)) |> as.numeric()
df$IR <- gsub("★","", as.character(df$IR)) |> as.numeric()

# data filtering
pmdf <- df[c(2, 4, 8, 16:17, 25:54, 56:65, 68:78)]


# data visualization function based on user input
viz_cols <- colnames(pmdf)
my_plot_func <- function(data, x_axis, y_axis){
  viz_dataset <- data %>% select(x_axis, y_axis)
  
  plot_1 <- plot(x = viz_dataset[, 1], y = viz_dataset[, 2], 
                 xlab = colnames(viz_dataset)[1], ylab = colnames(viz_dataset)[2], 
                 main = glue::glue("{colnames(viz_dataset)[2]} vs {colnames(viz_dataset)[1]} Graph"))
}


# Summarize feature variables
# summary(pmdf[,feat_vars])

# Visualize missing data
# vis_miss(pmdf[, feat_vars], warn_large_data = FALSE)

# Rows with missing data
nan_cols <- names(which(colSums(is.na(pmdf))>0))

#impute missing data
imputed_values <- mice(data = pmdf[, nan_cols], # Set data set
                       m = 1, # Set number of multiple imputations
                       maxit = 10, # Set maximum number of iterations
                       method = "cart", # Set method
                       seed = 7, # Set Seed
                       print = F)
pmdf[,nan_cols] <- complete(imputed_values, 1) # Extract imputed data

# data partitioning
set.seed(1234)
total_obs <- nrow(pmdf)
split_dataset <- splitstackshape::stratified(pmdf[, -1], group = "BP",
                                             size = 0.25, bothSets = T)
test_dataset <- split_dataset[[1]]
train_dataset <- split_dataset[[2]]



# model computation function based on user input
my_model <- function(data, model_type){
  if (model_type == 'Linear Model'){
    linear_model <- lm(OverAll_Rating ~ ., data = train_dataset)
    lm_pred <- predict(linear_model, test_dataset)
    lm_accr <- accuracy(lm_pred, test_dataset$OverAll_Rating)
    return (lm_accr)
  } 
  else if(model_type == 'General Additive Model'){
    # very carefully crafted
    formula = paste0("OverAll_Rating ~ foot + A.W + D.W + BP + ",
                     paste0("s(", setdiff(names(test_dataset), 
                                          c("OverAll_Rating", "foot", "A.W", "D.W", "BP")) ,
                            ", 4)", collapse = " + "
                     )) |> as.formula()
    gam_model <- gam(formula, data = train_dataset)
    gam_pred <- predict(gam_model, newdata = test_dataset)
    gam_accr <- accuracy(gam_pred, test_dataset$OverAll_Rating)
    return (gam_accr)
  }
  else if(model_type =='Lasso Regression'){
    
    # remove response variable and all categorical variables
    x_train_vars <- scale(train_dataset[, -c(1,2, 4, 47, 48)])
    
    # remove all categorical variables
    test_scale_lasso <- scale(test_dataset[, -c(2, 4, 47, 48)])
    
    #lasso Cross validation
    lambda_seq <- seq(from = 0.001, to = 1, by = 0.01)
    
    lasso_model <- cv.glmnet(x = x_train_vars, # Fit explanatory variables
                             y = train_dataset$OverAll_Rating, # Fit response variable
                             alpha = 1, # Set alpha as 1 for lasso
                             lambda = lambda_seq,
                             nfolds = 10) # Find lambda as 0.5
    
    best_lam <- lasso_model$lambda.1se # Extract best lambda
    best_lam # View best lambda
    
    # Fit best lambda lasso model
    best_lasso_model <- glmnet::glmnet(x = x_train_vars, # Set x variables
                                       y = train_dataset$OverAll_Rating, # Set response variable
                                       alpha = 1, # Set alpha as 1 for lasso
                                       lambda = best_lam) # Set lambda as best lambda
    
    # print out non-zero coeffs
    nz_coeffs <- coef(best_lasso_model)[coef(best_lasso_model)[,1]!=0, ] |> as.data.frame() |> 
      rename("NZ Coeffs" = "coef(best_lasso_model)[coef(best_lasso_model)[, 1] != 0, ]") |> 
      cbind(best_lambda = best_lam)
    dt <- cbind(Coefficients = rownames(nz_coeffs), nz_coeffs)
    return (dt)
  }
  else if(model_type == 'Neural Network based') {
    
    # generate a data frame with categorical predictors being represented as dummy variables
    # carefully crafted
    nn_formula <- paste0("~", paste0(setdiff(colnames(train_dataset), "OverAll_Rating"), 
                                     collapse = " + ")) |> as.formula()
    
    x_train_nn <- model.matrix(nn_formula, data=train_dataset)[, -1]
    # standardization
    x_mean <- apply(x_train_nn, 2, mean)
    x_sd <- apply(x_train_nn, 2, sd)
    x_train_nn <- scale(x_train_nn, center = x_mean, scale = x_sd)
    
    # combine with dependent variable Log_price
    x_train_nn <- cbind.data.frame(train_dataset$OverAll_Rating, x_train_nn)
    colnames(x_train_nn)[1] <- 'OverAll_Rating'
    
    # generate and standardize the data frame for the test data as well
    x_test_nn <- model.matrix(nn_formula, data = test_dataset)[, -1]
    # standardization
    x_test_nn <- scale(x_test_nn, center = x_mean, scale = x_sd)
    
    nn1 <- neuralnet::neuralnet(OverAll_Rating ~ ., data=x_train_nn, hidden = 2)
    plot(nn1)
    # prediction accuracy
    nn_pred <- predict(nn1, newdata = x_test_nn)[, 1]
    nn_accr <- accuracy(nn_pred, test_dataset$OverAll_Rating)
    return (c(nn1, nn_accr))
  }
  else {return (paste("Model under construction!"))}
}

# Function for Player display table

ready_player <- function(data, player_Name){
  player_details <- data %>% filter(data$Name == player_Name) %>% 
    select(c(2:6, 8:9, 13:14)) %>% t()
}

# Categorize Players and Apply ML Models (Performance Improvement by Clustering)

MID <- pmdf |> filter(BP %in% c("CAM", "CDM", "CM", "RM", "LM"))
DEF <- pmdf |> filter(BP %in% c("LWB", "LB", "RWB", "RB", "CB"))
GK <- pmdf |> filter(BP %in% "GK")
FWD <- pmdf |> filter(BP %in% c("CF", "ST", "RW", "LW"))

improved_model_analysis <- function(position, model) {
  total_data <- position
  split_dataset <- splitstackshape::stratified(total_data,
                                               group = 'BP', 
                                               size = 0.2, bothSets = TRUE)
  test_data <- split_dataset[[1]]
  train_data <- split_dataset[[2]]
  
  my_model(data = total_data, model_type = model)
}
