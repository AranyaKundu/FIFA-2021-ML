library(dplyr)
library(tibble)
library(ggplot2)
library(mice)
library(naniar)
library(gam)
library(glmnet)
library(forecast)
library(splitstackshape)
library(caret)
library(rpart)
library(randomForest)


# functions and other computations

# read data from csv
sub_df_players <- read.csv("D:/Coursework/Mod-2/Machine Learning/Project/MLApp/players.csv",
                       stringsAsFactors = TRUE, header = TRUE,
                       na.strings = c("", " ", "NA"))

main_df_players <- read.csv("D:/Coursework/Mod-2/Machine Learning/Project/MLApp/fifa21.csv",
               stringsAsFactors = TRUE, header = TRUE,
               na.strings = c("", " ", "NA"))

# data pre-processing
colnames(main_df_players)[4] <- "OverAll_Rating" # Renaming columns
main_df_players$Weight <- gsub("lbs","", as.character(main_df_players$Weight)) |> as.numeric()
main_df_players$W.F <- gsub("★","", as.character(main_df_players$W.F)) |> as.numeric()
main_df_players$SM <- gsub("★","", as.character(main_df_players$SM)) |> as.numeric()
main_df_players$IR <- gsub("★","", as.character(main_df_players$IR)) |> as.numeric()

# data filtering
modified_df <- main_df_players[c(2, 4, 8, 16:17, 25:54, 56:65, 68:78)]

# Data Imputation
nan_cols <- names(which(colSums(is.na(modified_df))>0))

#impute missing data
imputed_values <- mice(data = modified_df[, nan_cols], # Set data set
                       m = 1, # Set number of multiple imputations
                       maxit = 10, # Set maximum number of iterations
                       method = "cart", # Set method
                       seed = 7, # Set Seed
                       print = F)
modified_df[,nan_cols] <- complete(imputed_values, 1) # Extract imputed data

# Split train and test data sets: Data Partitioning

set.seed(123456)
obervations <- nrow(modified_df)
split_data <- splitstackshape::stratified(modified_df[, -1], group = "BP",
                                             size = 0.25, bothSets = T)
test_data <- split_data[[1]]
train_data <- split_data[[2]]

# Fit model to guess best playing position
# archived Models

archived_models <- function(model_type){
  if (model_type == "Single Decision Tree"){
    # Single Decision Tree model on unclustered data
    set.seed(123456)
    obervations <- nrow(modified_df)
    names(modified_df)[3] <- "Position"
    split_data <- splitstackshape::stratified(modified_df[, -1], group = "Position",
                                              size = 0.25, bothSets = T)
    test_data <- split_data[[1]]
    train_data <- split_data[[2]]
  } else if (model_type == "Clustered Single Decision Tree") {
    set.seed(123456)
    # Clustering the data set
    cmdf <- modified_df |> 
      mutate(Position = ifelse(BP %in% c("CAM", "CDM", "CM", "RM", "LM"), "MID",
                               ifelse(BP %in% c("LWB", "LB", "RWB", "RB", "CB"), "DEF", 
                                      ifelse(BP %in% c("CF", "ST", "RW", "LW"), "FWD", "GK"))
      ), .after = "BP")
    # Data partitioning on the clustered data set
    t_obs <- nrow(cmdf)
    split_cdf <- splitstackshape::stratified(cmdf[, -c(1, 3)], group = "Position",
                                             size = 0.25, bothSets = T)
    test_data <- split_cdf[[1]]
    train_data <- split_cdf[[2]]
  }
  
  # Apply DT Model to the data
  tree_model <- rpart(Position ~., data = train_data)
  tree_preds <- predict(tree_model, test_data, type = "class")
  t <- table(tree_preds, test_data$Position)
  cfm <- confusionMatrix(t, positive = "Yes")
  cfmtable <- cfm$table |> as.matrix()
  # cfmoverall <- cfm$overall |> as.data.frame()
  return (cfmtable)
}

# New feet based clustering
fmdf <- modified_df |>
  mutate(Position = ifelse(BP %in% c("CDM", "CM"), "CENTRAL MID", 
                        ifelse(BP %in% c("LWB", "LB"), "LEFT DEF", 
                            ifelse(BP %in% c("RWB", "RB", "CB"), "CENTRAL DEF",
                                ifelse(BP %in% c("CF", "CAM", "ST"), "FWD", 
                                    ifelse(BP %in% c("RM", "RW"), "RIGHT MID", 
                                        ifelse(BP %in% c("LM", "LW"), "LEFT MID", "GK")
                                            ))
                                         ))), .after = "BP")

# Data Partitioning on the clustered data set

total_ob <- nrow(fmdf)
split_fmdf <- splitstackshape::stratified(fmdf[, -c(1, 3)], group = "Position"
                                          ,size = 0.25, bothSets = T)
test_fmdf <- split_fmdf[[1]]
train_fmdf <- split_fmdf[[2]]


final_models <- function(model_choice){
  if (model_choice == "Decision Tree"){
    tree_model_3 <- rpart(Position~., data = train_fmdf)
    tree_preds_3 <- predict(tree_model_3, newdata = test_fmdf, type = "class")
    t3 <- table(tree_preds_3, test_fmdf$Position)
    cfm_1 <- confusionMatrix(t3, positive = "Yes") # Up next
    return (cfm_1)
  }
  else if(model_choice == "Extreme Gradient Boost"){
    return (paste("Model under construction!"))
  }
  else if (model_choice == "Bootstrap Aggregation"){
    return (paste("Model Under Construction!"))
  }
}

# DT Model for predicting player position
mod_split_fmdf <- splitstackshape::stratified(fmdf[, -c(3)], group = "Position"
                                                        ,size = 0.25, bothSets = T)
mod_train_fmdf <- mod_split_fmdf[[2]]
mod_test_fmdf <- mod_split_fmdf[[1]]

# Decision Tree Model
mod_dec_tree <- rpart(Position ~ ., mod_train_fmdf[, -1])
mod_preds_test <- predict(mod_dec_tree, newdata = mod_test_fmdf[, -1], type="class") |> as.data.frame()
mod_pred_train <- predict(mod_dec_tree, newdata = mod_train_fmdf[, -1], type="class") |> as.data.frame()


# Bagging Model

# set.seed(258506)
# bag_mod <- randomForest("Position" ~ ., 
#                         data = mod_train_fmdf[, -1],
#                         ntree = 150,
#                         mtry = 55
# )
# 
# bag_mod
cmb_df <- add_column(mod_train_fmdf[, c(1:3, 5)], mod_pred_train, .after = "Position")
cmb_df2 <- add_column(mod_test_fmdf[, c(1:3, 5)], mod_preds_test, .after = "Position")
names(cmb_df)[4] <- "Predicted"
names(cmb_df2)[4] <- "Predicted"
combined <- rbind(cmb_df, cmb_df2) 

# Predict player position
predict_position <- function(player_Nm){
  output <- combined %>% filter(Name == player_Nm) |> t()
  names(output)[1] <- "Details"
  return (output)
}

# Show Model Performance
# model_performance <- function(model_Name){
#   
# }
