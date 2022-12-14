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

imp_df <- players_df[, c(2, 8, 9, 11)]

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
  dream_team <- players_df[which(players_df$str_player_name %in% dream1$str_player_name), 
                             c(2, 4:6, 8:9, 11:12, 44)]
  return (dream_team)
}

teams <- read.csv(
  "./teams.csv", header = T)[, c(2:7, 10, 14)]
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

# Pitch with formation 4-3-3

choiceTeam_433 <- function(){
  
  z_433 <- players_df[c(5, 7, 1, 4, 28, 68, 33, 11, 65, 31, 3), c(2, 57)]
  team_coords <- data.frame(x = c(25, 17, 25, 50, 43, 50, 75, 80, 80, 75, 95),
                            y = c(15, 50, 85, 15, 50, 85, 12, 37, 62, 87, 50),
                            photo = z_433$str_player_image_url, 
                            name = z_433$str_player_name)
  
  # dims <- list(length = 100, width = 100, penalty_box_length = 16,
  #                   penalty_box_width = 60, six_yard_box_length = 3, six_yard_box_width = 20,
  #                   penalty_spot_distance = 4, goal_width = 16, origin_x = 0, origin_y = 0)
  
  
  pitch_433 <- ggplot(team_coords) + # set data for the plot
    annotate_pitch(colour = "white", # set colour of pitch
                   fill = "springgreen4") + #set fill pitch colour
    geom_image(aes(x = 100 - x, y = y), # Set x and y axis co-ordinates
               image = team_coords$photo, asp = 4/3) + # Set images of players to be printed
    geom_text(data = team_coords, aes(x = 95-x, y = y, label = name)) + # Set player names below images 
    theme_pitch() + # Set pitch theme
    coord_flip() + #Flip the coordinates for adjusting two different formations
    theme(panel.background = element_rect(fill = "springgreen3"),
          aspect.ratio = 1.4) + # set pitch background fill
    ggtitle("Formation: 4-3-3") # Mention title for the plot
  return (pitch_433)
}

# Pitch with formation 4-4-2

choiceTeam_442 <- function(){
  z_442 <- players_df[c(2, 6, 10, 4, 68, 9, 20, 17, 34, 31, 12), c(2, 57)]
  position_coords <- data.frame(x = c(75, 75, 50, 45, 45, 50, 25, 20, 20, 25, 5),
                                y = c(33, 67, 12, 37, 62, 87, 12, 37, 62, 87, 50),
                                photo = z_442$str_player_image_url,
                                name = z_442$str_player_name)  
  pitch_442 <- ggplot(position_coords) + # set data for the plot
    annotate_pitch(colour = "white", # set colour of pitch
                   fill = "#13576c") + #set fill pitch colour
    geom_image(aes(x = x, y = y), # Set x and y axis co-ordinates
               image = position_coords$photo, asp = 4/3) + # Set images of players to be printed
    geom_text(data = position_coords, aes(x = x - 5, y = y, label = name)) + # Set player names below images
    theme_pitch() + # Set pitch theme
    coord_flip() + #Flip the coordinates for adjusting two different formations
    theme(panel.background = element_rect(fill = "#1f86a5"),
          aspect.ratio = 1.4) + # set pitch background fill
    ggtitle("Formation: 4-4-2") # Mention title for the plot
  return (pitch_442)
  
}
