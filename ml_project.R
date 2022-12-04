library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
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
library(caret)
library(rpart)
library(randomForest)


# functions and other computations

# read data from csv
players_df <- read.csv("D:/Coursework/Mod-2/Machine Learning/Project/New Folder/players.csv",
               stringsAsFactors = TRUE, header = TRUE,
               na.strings = c("", " ", "NA"))

df <- read.csv("D:/Coursework/Mod-2/Machine Learning/Project/New Folder/fifa21.csv",
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

# Function for Player display table
ready_player <- function(data, player_Name){
  player_details <- data %>% filter(data$str_player_name == player_Name) %>% 
    select(c(2:6, 8:12, 14)) %>% rename(
      "Player Name" = "str_player_name",
      "Playing Positions" = "str_positions",
      "D.O.B." = "dt_date_of_birth",
      "Height" = "int_height",
      "Weight" = "int_weight",
      "Potential Rating" = "int_potential_rating",
      "Best Playing Position" = "str_best_position",
      "Best OverAll Rating" = "int_best_overall_rating",
      "Value" = "int_value",
      "Wage" = "int_wage",
      "Nationality" = "str_nationality"
    ) %>% t() %>% as.data.frame() %>% rename("Details" = "V1")
}

# Function for Player photo display
player_display <- function(player_Name){
  player_photo_1 <- players_df %>% filter(str_player_name == player_Name) %>%
    select(int_player_id)
  photo_1 <- paste(player_photo_1[1, 1], ".png", sep = "")
  return (photo_1)
}


####################
##                ##
## ML Starts here ##
##                ##
####################


# load partitioned data

load("test_dataset.rda")
load("train_dataset.rda")

# model computation function based on user input
my_model <- function(model_type){
  if (model_type == 'Linear Model'){
    linear_model <- lm(OverAll_Rating ~ ., data = train_dataset)
    lm_pred <- predict(linear_model, test_dataset)
    lm_accr <- accuracy(lm_pred, test_dataset$OverAll_Rating)
    res <- cbind(lm_accr |> as.data.frame(), linear_model$coefficients |> t()) |> t() |> as.data.frame()
    colnames(res)[1] <- "Model Details"
    return (res)
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
    res <- cbind(gam_accr |> as.data.frame(), gam_model$coefficients |> t()) |> t() |> as.data.frame()
    colnames(res)[1] <- "Model Details"
    return (res)
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
  else if (model_type == 'Bootstrap Aggregation'){
   return("None.") 
  }
  else {return (paste("Model under construction!"))}
}

# # Categorize Players and Apply ML Models (Performance Improvement by Clustering)
# 
# MID <- pmdf |> filter(BP %in% c("CAM", "CDM", "CM", "RM", "LM"))
# DEF <- pmdf |> filter(BP %in% c("LWB", "LB", "RWB", "RB", "CB"))
# GK <- pmdf |> filter(BP %in% "GK")
# FWD <- pmdf |> filter(BP %in% c("CF", "ST", "RW", "LW"))
# 
# improved_model_analysis <- function(position, model) {
#   total_data <- position
#   split_dataset <- splitstackshape::stratified(total_data,
#                                                group = 'BP', 
#                                                size = 0.2, bothSets = TRUE)
#   test_data <- split_dataset[[1]]
#   train_data <- split_dataset[[2]]
#   
#   my_model(data = total_data, model_type = model)
# }
