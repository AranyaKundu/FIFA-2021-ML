# For the ml_project.R file

library(dplyr)
library(tibble)
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

save(test_dataset, file = "test_dataset.rda")

save(train_dataset, file = "train_dataset.rda")

load("train_dataset.rda")


# For the Potential.R file

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

save(test_data, file = "test_data.rda")
save(train_data, file = "train_data.rda")

load("test_data.rda")
load("train_data.rda")


# Feet based clustering
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
set.seed(123456)
total_ob <- nrow(fmdf)
split_fmdf <- splitstackshape::stratified(fmdf[, -c(3)], group = "Position"
                                          ,size = 0.25, bothSets = T)
test_fmdf <- split_fmdf[[1]]
train_fmdf <- split_fmdf[[2]]

save(test_fmdf, file = "test_fmdf.rda")
save(train_fmdf, file = "train_fmdf.rda")


# Data preparation for Best team 

main_players <- read.csv("D:/Coursework/Mod-2/Machine Learning/Project/MLApp/fifa21.csv",
                         stringsAsFactors = TRUE, header = TRUE,
                         na.strings = c("", " ", "NA"))


df <- main_players[, c(2:4, 8, 16:18, 21, 68, 69, 72, 80:106)]

df$Weight <- gsub("lbs","", as.character(df$Weight)) |> as.numeric()
df$W.F <- gsub("★","", as.character(df$W.F)) |> as.numeric()
df$SM <- gsub("★","", as.character(df$SM)) |> as.numeric()
df$IR <- gsub("★","", as.character(df$IR)) |> as.numeric()

for (i in 12:38){
  df[, i] <- substring(df[, i], 4) |> as.integer()
}

df_nancols <- names(which(colSums(is.na(df))>0))

df[is.na(df$GK), ]$GK <- 0

save(df, file = "total_potential.rda")


total_pot <- load("total_potential.rda")
set.seed(1234)
total_obs <- nrow(df)
split_dataset <- splitstackshape::stratified(df, group = "foot",
                                             size = 0.25, bothSets = T)
test_pot <- split_dataset[[1]]
train_pot <- split_dataset[[2]]


# apply general additive model regression

formula <- paste0("Growth ~ foot + BP + ",
                  paste0("s(", setdiff(names(df), c("Name", "foot", "BP", "Value")),
                         ", 4)", collapse = " + ")) |> as.formula()
gam2 <- gam(formula = formula, data = train_pot[, -c(1, 8)], family = "gaussian")

gam1 <- gam(Growth ~ s(Age, 4) + s(W.F, 4) + s(LB, 4) + s(LM, 4) + s(IR, 4) + s(OVA, 4) +
              s(Weight, 4) + foot + BP + s(SM, 4) + s(LS, 4) + s(LW, 4), train_pot[, -1],
            family = "gaussian")
# options(warn = -1)

gam1_pred_test <- predict(gam2, newdata = test_pot[, -c(1, 8)]) |> as.data.frame()
gam1_pred_train <- predict(gam2, newdata = train_pot[, -c(1, 8)]) |> as.data.frame()
names(gam1_pred_test) <- "growth"
names(gam1_pred_train) <- names(gam1_pred_test)
growth <- rbind(gam1_pred_test, gam1_pred_train)
df <- cbind(df, growth)

df$potential <- rowSums(cbind(df[, 3], df[, 39]))
