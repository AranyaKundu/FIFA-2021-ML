# Best Team
# Constraints 
# 1. 11 Players
# 2. GK = 1
# 3. RB = 1
# 4. CB = 2 (1 CDM + 1 CB)
# 5. LB = 1
# 6. DEF = 4
# 7. RM/RW = 0 - 1
# 8. LM/LW = 0 - 1
# 9. CM = 1 - 2
# 10. FW = 1 - 3
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

main_players <- read.csv("D:/Coursework/Mod-2/Machine Learning/Project/New Folder/players.csv",
                         stringsAsFactors = TRUE, header = TRUE,
                         na.strings = c("", " ", "NA"))

imp_df <- main_players[, c(2, 8, 9, 11)]

imp_df$dummyGK <- ifelse(imp_df$str_best_position == 'GK', 1, 0)
imp_df$dummyRB <- ifelse(imp_df$str_best_position == 'RB', 1, 0)
imp_df$dummyCB <- ifelse(imp_df$str_best_position == 'CB', 1, 0)
imp_df$dummyLB <- ifelse(imp_df$str_best_position == 'LB', 1, 0)
imp_df$dummyRM <- ifelse(imp_df$str_best_position == 'RM', 1, 0)
imp_df$dummyLM <- ifelse(imp_df$str_best_position == 'LM', 1, 0)
imp_df$dummyCM <- ifelse((imp_df$str_best_position == 'CM' | imp_df$str_best_position == 'CAM'), 1, 0)
imp_df$dummyFW <- ifelse((imp_df$str_best_position == 'CF' | imp_df$str_best_position == 'ST' | 
                            imp_df$str_best_position == 'LW' | imp_df$str_best_position == 'RW'), 1, 0)

optimal_team <- function(GK_count, RB_count, LB_count, CB_count, RM_count, CM_count, 
                         LM_count, FW_count, team_size){
  
  objective_function <- imp_df$int_potential_rating
  n = length(objective_function)
  
  model <- MIPModel() %>% 
    add_variable(x[i], i=1:n, lb = 0, type ="binary") |>
    # set_objective(sum_over(imp_df$Wage[i] * x[i], i = 1:n), sense = "min") |>
    set_objective(sum_over(imp_df$int_potential_rating[i] * x[i], i = 1:n), sense = "max") |>
    add_constraint(sum_over(x[i], i=1:n) == team_size) |>
    add_constraint(sum_over(imp_df$dummyGK[i] * x[i], i = 1:n) == GK_count) |>
    add_constraint(sum_over(imp_df$dummyRB[i] * x[i], i = 1:n) <= RB_count) |>
    add_constraint(sum_over(imp_df$dummyLB[i] * x[i], i = 1:n) <= LB_count) |>
    add_constraint(sum_over(imp_df$dummyRM[i] * x[i], i = 1:n) <= RM_count) |>
    add_constraint(sum_over(imp_df$dummyCM[i] * x[i], i = 1:n) <= CM_count) |>
    add_constraint(sum_over(imp_df$dummyFW[i] * x[i], i = 1:n) <= FW_count) |>
    add_constraint(sum_over(imp_df$dummyCB[i] * x[i], i = 1:n) >= CB_count) |>
    add_constraint(sum_over(imp_df$dummyLM[i] * x[i], i = 1:n) <= LM_count)

  solved <- solve_model(model, with_ROI("glpk"))
  dream1 <- imp_df[solved$solution>0, ]
  dream_team <- main_players[which(main_players$str_player_name %in% dream1$str_player_name), 
                             c(2, 4:6, 8:9, 11:12, 44)]
  return (dream_team)
}

teams <- read.csv(
  "D:/Coursework/Mod-2/Machine Learning/Project/New Folder/teams.csv", header = T)[, c(2:7, 10, 14)]
teams_df <- function() {
  teams %>% rename("Team Name" = "str_team_name", 
                   "League" = "str_league",
                   "OverAll" = "int_overall",
                   "Attack" = "int_attack",
                   "Midfield" = "int_midfield",
                   "Defence" = "int_defence",
                   "Trf. Budget" = "int_transfer_budget",
                   "Playing Style" = "str_offensive_style"
  )
  return (teams)
  }

