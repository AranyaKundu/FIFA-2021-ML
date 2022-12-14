#All required libraries are loaded in ml_project.R
# read data from csv
sub_df_players <- read.csv("./players.csv",
                           stringsAsFactors = TRUE, header = TRUE,
                           na.strings = c("", " ", "NA"))

main_df_players <- read.csv("./fifa21.csv",
                            stringsAsFactors = TRUE, header = TRUE,
                            na.strings = c("", " ", "NA"))

# data pre-processing
colnames(main_df_players)[4] <- "OverAll_Rating" # Renaming columns
main_df_players$Weight <- gsub("lbs","", as.character(main_df_players$Weight)) |> as.numeric()
main_df_players$W.F <- gsub("★","", as.character(main_df_players$W.F)) |> as.numeric()
main_df_players$SM <- gsub("★","", as.character(main_df_players$SM)) |> as.numeric()
main_df_players$IR <- gsub("★","", as.character(main_df_players$IR)) |> as.numeric()

# functions and other computations

# load("test_data.rda")
# load("train_data.rda")

# Fit model to guess best playing position
# archived Models

archived_models <- function(model_type){
  if (model_type == "Single Decision Tree"){
    load("test_data.rda") # Load Train data set (imputed)
    load("train_data.rda") # Load Test data set (imputed)
    names(train_data)[2] <- "Position" # Rename column in train data
    names(test_data)[2] <- "Position" # Rename column in test data
    train_data <- train_data[, -1]
    test_data <- test_data[, -1]

  } else if (model_type == "Clustered Single Decision Tree") {
    load("test_data.rda")
    load("train_data.rda")
    set.seed(123456)
    # Clustering the data set
    cmdf <- rbind.data.frame(train_data, test_data) |> 
      mutate(Position = ifelse(BP %in% c("CAM", "CDM", "CM", "RM", "LM"), "MID",
                               ifelse(BP %in% c("LWB", "LB", "RWB", "RB", "CB"), "DEF", 
                                      ifelse(BP %in% c("CF", "ST", "RW", "LW"), "FWD", "GK"))
      ), .after = "BP")
    # Data partitioning on the clustered data set
    t_obs <- nrow(cmdf)
    split_cdf <- splitstackshape::stratified(cmdf[, -c(1, 2)], group = "Position",
                                             size = 0.25, bothSets = T)
    test_data <- split_cdf[[1]]
    train_data <- split_cdf[[2]]
  }
  
  # Apply DT Model to the data
  tree_model <- rpart(Position ~., data = train_data)
  tree_preds <- predict(tree_model, test_data, type = "class")
  t <- table(tree_preds, test_data$Position)
  cfm <- confusionMatrix(t, positive = "Yes")
  cfmtable <- cfm$table
  return (cfmtable)
}

# Load data for prediction

load("train_fmdf.rda")
load("test_fmdf.rda")

final_models <- function(model_choice){
  if (model_choice == "Decision Tree"){
    load("train_fmdf.rda")
    load("test_fmdf.rda")
    
    # Train the decision tree model
    tree_model_3 <- rpart(Position~., data = train_fmdf[, -1])
    
    # Make predictions on the test data
    tree_preds_3 <- predict(tree_model_3, newdata = test_fmdf[, -1], type = "class")
    
    # Calculate the predicted probabilities for each class
    predicted_probs <- predict(tree_model_3, newdata = test_fmdf[, -1], type = "prob")
    test_labels_numeric <- factor(test_fmdf$Position) |> as.numeric()
    
    predicted_probs_matrix <- apply(predicted_probs, 1, which.max)
    # Plot roc for GK
    gk_act <- factor(ifelse(test_fmdf$Position == "GK", "GK", "nonGK")) |> as.numeric()
    gk_pred <- factor(ifelse(tree_preds_3 == "GK", "GK", "nonGK")) |> as.numeric()

    roc_gk <- roc(gk_act, gk_pred)
    plot.roc(roc_gk, print.auc = T, col = "red", lwd = 4
             , print.auc.x = 0.2, print.auc.y = 0.35, print.auc.col = "red")
    
    # Plot roc for Central Defenders
    cdf_act <- factor(ifelse(test_fmdf$Position == "CENTRAL DEF", "CENTRAL DEF", "non CENTRAL DEF")) |> 
      as.numeric()
    cdf_pred <- factor(ifelse(tree_preds_3 == "CENTRAL DEF", "CENTRAL DEF", "non CENTRAL DEF")) |> 
      as.numeric()
    roc_cdf <- roc(cdf_act, cdf_pred)
    plot.roc(roc_cdf, print.auc = T, col ="blue", add = TRUE, lwd = 4
             , print.auc.x = 0.2, print.auc.y = 0.3, print.auc.col = "blue")
    
    # Plot roc for Central Midfielders
    cm_act <- factor(ifelse(test_fmdf$Position == "CENTRAL MID", "CENTRAL MID", "non CENTRAL MID")) |>
      as.numeric()
    cm_pred <- factor(ifelse(tree_preds_3 == "CENTRAL MID", "CENTRAL MID", "non CENTRAL MID")) |>
      as.numeric()
    roc_cm <- roc(cm_act, cm_pred)
    plot.roc(roc_cm, print.auc = T, col = "turquoise2", add = T, lwd = 4
             , print.auc.x = 0.2, print.auc.y = 0.4, print.auc.col = "turquoise2")
    
    
    # Plot roc for left midfielder
    ldf_act <- factor(ifelse(test_fmdf$Position == "LEFT DEF", "LEFT DEF", "non LEFT DEF")) |>
      as.numeric()
    ldf_pred <- factor(ifelse(tree_preds_3 == "LEFT DEF", "LEFT DEF", "non LEFT DEF")) |>
      as.numeric()
    roc_ldf <- roc(ldf_act, ldf_pred)
    plot.roc(roc_ldf, print.auc = T, col = "green", add = T, lwd = 4
             , print.auc.x = 0.2, print.auc.y = 0.45, print.auc.col = "green")
    
    # Plot roc for forwards (ST/CF/CAM)
    fwd_act <- factor(ifelse(test_fmdf$Position == "FWD", "FWD", "non FWD")) |>
      as.numeric()
    fwd_pred <- factor(ifelse(tree_preds_3 == "FWD", "FWD", "non FWD")) |>
      as.numeric()
    roc_fwd <- roc(fwd_act, fwd_pred)
    plot.roc(roc_fwd, print.auc = T, col = "purple", add = T, lwd = 4
             , print.auc.x = 0.2, print.auc.y = 0.5, print.auc.col = "purple")
    
    
    # Plot roc for Left Midfielders
    lm_act <- factor(ifelse(test_fmdf$Position == "LEFT MID", "LEFT MID", "non LEFT MID")) |>
      as.numeric()
    lm_pred <- factor(ifelse(tree_preds_3 == "LEFT MID", "LEFT MID", "non LEFT MID")) |>
      as.numeric()
    roc_lm <- roc(lm_act, lm_pred)
    plot.roc(roc_lm, print.auc = T, col = "orange", add = T, lwd = 4
             , print.auc.x = 0.2, print.auc.y = 0.55, print.auc.col = "orange")
    
    # Plot roc for Right Midfielders
    rm_act <- factor(ifelse(test_fmdf$Position == "RIGHT MID", "RIGHT MID", "non RIGHT MID")) |>
      as.numeric()
    rm_pred <- factor(ifelse(tree_preds_3 == "RIGHT MID", "RIGHT MID", "non RIGHT MID")) |>
      as.numeric()
    roc_rm <- roc(rm_act, rm_pred)
    auc_plot <- plot.roc(roc_rm, print.auc = T, col = "violetred", add = T, lwd = 4
                         , print.auc.x = 0.2, print.auc.y = 0.6, print.auc.col = "violetred")
    
    # Add a legend to the roc curve
    legend("bottomright",
      legend = c("CENTRAL DEF", "CENTRAL MID", "FWD", "GK", "LEFT DEF", 
                      "LEFT MID", "RIGHT MID"),
      col = c("blue", "turquoise2", "purple", "red", "green", "orange", "violetred"), # Add different colors
      lty = 1:1, # Set line type
      xjust = 1, yjust = 1, # Adjust x and y orientations
           )
    
    return (auc_plot)
  }
  else if (model_choice == "Bootstrap Aggregation"){
    bag_mod <- randomForest(factor(Position) ~ .,
                            data = na.omit(train_fmdf[, -1]),
                            ntree = 150,
                            mtry = 54
    )
  }
}

# DT Model for predicting player position


# Decision Tree Model
mod_dec_tree <- rpart(Position ~ ., train_fmdf[, -1])
mod_preds_test <- predict(mod_dec_tree, newdata = test_fmdf[,-1], type="class") |> as.data.frame()
mod_pred_train <- predict(mod_dec_tree, newdata = train_fmdf[, -1], type="class") |> as.data.frame()


cmb_df <- add_column(train_fmdf[, c(1:3, 5)], mod_pred_train, .after = "Position")
cmb_df2 <- add_column(test_fmdf[, c(1:3, 5)], mod_preds_test, .after = "Position")
names(cmb_df)[4] <- "Predicted"
names(cmb_df2)[4] <- "Predicted"
combined <- rbind(cmb_df, cmb_df2) 

# Predict player position
predict_position <- function(player_Nm){
  output <- combined %>% filter(Name == player_Nm) |> t()
  names(output)[2] <- "Details"
  return (output)
}


# Bagging Model

set.seed(258506)
bag_mod <- randomForest(factor(Position) ~ .,
                        data = na.omit(train_fmdf[, -1]),
                        ntree = 150,
                        mtry = 54
)

bag_mod
