# Player position data

barca_players <- tribble(
  
  ~frame, ~name, ~x, ~y,  # column names
  # Xavi Hernandez Movement
  1,  "Xavi Hernandez", 60, 70, # Passes the ball to Messi
  2,  "Xavi Hernandez", 56, 70,
  3,  "Xavi Hernandez", 54, 70,
  4,  "Xavi Hernandez", 52, 68,
  5,  "Xavi Hernandez", 50, 71,
  6,  "Xavi Hernandez", 48, 72,
  7,  "Xavi Hernandez", 48, 74,
  8,  "Xavi Hernandez", 46, 70,
  9,  "Xavi Hernandez", 44, 68,
  10, "Xavi Hernandez", 42, 64,
  11, "Xavi Hernandez", 40, 60,
  12, "Xavi Hernandez", 36, 59,
  13, "Xavi Hernandez", 33, 58, 
  14, "Xavi Hernandez", 32, 55,
  15, "Xavi Hernandez", 30, 50,
  16, "Xavi Hernandez", 28, 47,
  17, "Xavi Hernandez", 26, 46,
  18, "Xavi Hernandez", 24, 44,
  19, "Xavi Hernandez", 23, 43,
  20, "Xavi Hernandez", 21, 47,
  21, "Xavi Hernandez", 20, 50,
  22, "Xavi Hernandez", 18, 53,
  23, "Xavi Hernandez", 16, 56,
  
  # Lionel Messi Movement
  1,  "Messi", 56, 94,  # Waits for the ball
  2,  "Messi", 55, 95,  # advances from own half
  3,  "Messi", 52, 92,  # dribbles one in own half
  4,  "Messi", 51, 89,  # dribbles the second in own half
  5,  "Messi", 48, 87,  # Messi moves forward with the ball
  6,  "Messi", 45, 86,  # Messi moves forward with the ball
  7,  "Messi", 42, 84,  # Messi moves forward with the ball
  8,  "Messi", 39, 83,  # Messi moves forward with the ball
  9,  "Messi", 37, 81,  # Messi moves forward with the ball
  10, "Messi", 36, 80,  # Messi moves forward with the ball
  11, "Messi", 33, 79,  # Messi moves forward with the ball
  12, "Messi", 30, 77,  # Messi moves forward with the ball
  13, "Messi", 27, 76,  # Messi moves forward with the ball
  14, "Messi", 25, 75,  # dribbles the third in opponent half
  15, "Messi", 22, 72,  # dribbles the fourth in opponent half
  16, "Messi", 20, 65,  # Messi advances with the ball
  17, "Messi", 18, 60,  # dribbles the fifth in opponent half
  18, "Messi", 16, 59,  # Dribbles past the chasing opponents
  19, "Messi", 14, 63,  # dribbles past the goal keeper
  20, "Messi", 10, 60,  # shot
  21, "Messi", 9, 60,   # goal
  22, "Messi", 4, 65,   # Celebrates
  23, "Messi", 0, 72    # Celebrates
  
)


getafe_players <- tribble(
  ~frame, ~name, ~x, ~y, # column names
  # Getafe player 1 Movement
  1,  "Paredes", 50, 92, # Moving towards Messi
  2,  "Paredes", 50, 92, # Move towards Messi
  3,  "Paredes", 49, 94, # Obstructs Messi
  4,  "Paredes", 49, 92, # Lying down
  5,  "Paredes", 50, 91, # Rising up
  6,  "Paredes", 47, 89, # Chasing Messi
  7,  "Paredes", 44, 87, # Chasing Messi
  8,  "Paredes", 42, 85, # Chasing Messi
  9,  "Paredes", 40, 84, # Chasing Messi
  10, "Paredes", 39, 83, # Chasing Messi
  11, "Paredes", 37, 81, # Chasing Messi
  12, "Paredes", 35, 79, # Chasing Messi
  13, "Paredes", 34, 77, # Chasing Messi
  14, "Paredes", 32, 74, # Chasing Messi
  15, "Paredes", 30, 72, # Chasing Messi
  16, "Paredes", 30, 69, # Chasing Messi
  17, "Paredes", 28, 67, # Chasing Messi
  18, "Paredes", 26, 63, # Chasing Messi
  19, "Paredes", 23, 63, # Chasing Messi
  20, "Paredes", 22, 61, # Chasing Messi
  21, "Paredes", 21, 59, # Chasing Messi
  22, "Paredes", 18, 59, # Watching the ball enter goal
  23, "Paredes", 16, 59, # Watching the ball enter goal
  
  # Getafe player 2 movement
  1,  "Nacho", 51, 92,  # Moving towards Messi
  2,  "Nacho", 50, 90,  # Moving towards Messi
  3,  "Nacho", 51, 88,  # Moving towards Messi
  4,  "Nacho", 51, 88,  # Obstructs Messi
  5,  "Nacho", 52, 87,  # Got dribbled by Messi
  6,  "Nacho", 52, 88,  # Return back to Chase Messi
  7,  "Nacho", 50, 86,  # Chasing Messi
  8,  "Nacho", 48, 86,  # Chasing Messi
  9,  "Nacho", 46, 85,  # Chasing Messi
  10, "Nacho", 44, 83,  # Chasing Messi
  11, "Nacho", 43, 82,  # Chasing Messi
  12, "Nacho", 41, 82,  # Chasing Messi
  13, "Nacho", 39, 80,  # Chasing Messi
  14, "Nacho", 37, 78,  # Chasing Messi
  15, "Nacho", 35, 76,  # Chasing Messi
  16, "Nacho", 34, 74,  # Chasing Messi
  17, "Nacho", 32, 73,  # Chasing Messi
  18, "Nacho", 30, 71,  # Chasing Messi
  19, "Nacho", 28, 70,  # Chasing Messi
  20, "Nacho", 26, 69,  # Chasing Messi
  21, "Nacho", 24, 67,  # Chasing Messi
  22, "Nacho", 21, 66,  # Watching the ball enter goal
  23, "Nacho", 19, 64,  # Watching the ball enter goal
  
  # Getafe player 3 movement
  1,  "Alexis", 22, 80,  # Waiting to stop Messi
  2,  "Alexis", 23, 82,  # Waiting to stop Messi
  3,  "Alexis", 23, 79,  # Waiting to stop Messi
  4,  "Alexis", 22, 77,  # Waiting to stop Messi
  5,  "Alexis", 23, 76,  # Waiting to stop Messi
  6,  "Alexis", 22, 76,  # Waiting to stop Messi
  7,  "Alexis", 23, 75,  # Waiting to stop Messi
  8,  "Alexis", 24, 75,  # Waiting to stop Messi
  9,  "Alexis", 24, 75,  # Waiting to stop Messi
  10, "Alexis", 24, 75,  # Waiting to stop Messi
  11, "Alexis", 24, 75,  # Waiting to stop Messi
  12, "Alexis", 24, 75,  # Waiting to stop Messi
  13, "Alexis", 24, 75,  # Waiting to stop Messi
  14, "Alexis", 25, 74,  # Obstructs Messi
  15, "Alexis", 22, 76,  # Lying on the ground
  16, "Alexis", 23, 77,  # Rise up
  17, "Alexis", 21, 75,  # Start Chasing Messi
  18, "Alexis", 21, 72,  # Chasing Messi
  19, "Alexis", 20, 70,  # Chasing Messi
  20, "Alexis", 19, 68,  # Chasing Messi
  21, "Alexis", 17, 66,  # Still Chasing Messi
  22, "Alexis", 16, 64,  # Still Chasing Messi
  23, "Alexis", 15, 61,  # Still Chasing Messi
  22, "Alexis", 14, 58,  # Watching the ball enter goal
  23, "Alexis", 14, 55,  # Watching the ball enter goal
  
  # Getafe player 4 movement
  1,  "Belenguer", 22, 60,  
  2,  "Belenguer", 24, 62,
  3,  "Belenguer", 24, 63,
  4,  "Belenguer", 24, 64,
  5,  "Belenguer", 25, 66,
  6,  "Belenguer", 24, 67,
  7,  "Belenguer", 24, 69,
  8,  "Belenguer", 23, 71,
  9,  "Belenguer", 23, 73,
  10, "Belenguer", 24, 74, 
  11, "Belenguer", 25, 74,
  12, "Belenguer", 24, 73,
  13, "Belenguer", 23, 73,
  14, "Belenguer", 22, 72,
  15, "Belenguer", 22, 72, # Obstructs Messi
  16, "Belenguer", 20, 74, # Chases Messi
  17, "Belenguer", 19, 73,
  18, "Belenguer", 18, 72,
  19, "Belenguer", 17, 71, 
  20, "Belenguer", 16, 69, 
  21, "Belenguer", 15, 67,
  22, "Belenguer", 14, 65, 
  23, "Belenguer", 14, 63,
  
  # Getafe player 5 movement
  1,  "Cortes", 26, 60,  
  2,  "Cortes", 27, 61,
  3,  "Cortes", 28, 62,
  4,  "Cortes", 26, 63,
  5,  "Cortes", 23, 64,
  6,  "Cortes", 24, 65,
  7,  "Cortes", 24, 64,
  8,  "Cortes", 24, 63,
  9,  "Cortes", 24, 62,
  10, "Cortes", 23, 62,
  11, "Cortes", 22, 61,
  12, "Cortes", 23, 62,
  13, "Cortes", 23, 60,
  14, "Cortes", 22, 60,
  15, "Cortes", 22, 61,
  16, "Cortes", 22, 60,
  17, "Cortes", 21, 60, # Obstructs Messi
  18, "Cortes", 20, 60, # Chases Messi
  19, "Cortes", 18, 60,
  20, "Cortes", 16, 59,
  21, "Cortes", 15, 58,
  22, "Cortes", 14, 57,
  23, "Cortes", 13, 56,
  
  # Luis Garcia Movement
  1,  "Luis Garcia", 06, 53, # waiting on the goalpost
  2,  "Luis Garcia", 06, 54, # waiting on the goalpost
  3,  "Luis Garcia", 06, 55, # waiting on the goalpost
  4,  "Luis Garcia", 06, 56, # waiting on the goalpost
  5,  "Luis Garcia", 07, 57, # waiting on the goalpost
  6,  "Luis Garcia", 08, 58, # waiting on the goalpost
  7,  "Luis Garcia", 08, 59, # waiting on the goalpost
  8,  "Luis Garcia", 08, 60, # waiting on the goalpost
  9,  "Luis Garcia", 08, 60, # waiting on the goalpost
  10, "Luis Garcia", 08, 60, # waiting on the goalpost
  11, "Luis Garcia", 08, 60, # waiting on the goalpost
  12, "Luis Garcia", 09, 60, # waiting on the goalpost
  13, "Luis Garcia", 09, 61, # waiting on the goalpost
  14, "Luis Garcia", 10, 61, # waiting on the goalpost
  15, "Luis Garcia", 11, 61, # waiting on the goalpost
  16, "Luis Garcia", 12, 62, # waiting on the goalpost
  17, "Luis Garcia", 13, 62, # waiting on the goalpost
  18, "Luis Garcia", 13, 64, # advances to stop Messi
  19, "Luis Garcia", 13, 63, # advances to stop Messi
  20, "Luis Garcia", 12, 61, # lying down inside the box
  21, "Luis Garcia", 12, 61, # lying down inside the box
  22, "Luis Garcia", 12, 61, # lying down inside the box
  23, "Luis Garcia", 12, 61, # lying down inside the box
  
)


# Ball position data
ball <- tribble(
  
  ~frame, ~x, ~y,
  
  1,  60, 70,  # Messi receives from Xavi Hernandez
  2,  54, 96,  # Messi Possession
  3,  51, 92,  # Messi Dribbles
  4,  50, 88,  # Messi Dribbles
  5,  48, 87,  # Messi Dribbles
  6,  44, 85,
  7,  41, 84,
  8,  38, 83,
  9,  37, 81,
  10, 36, 80,  # Messi Dribbles
  11, 33, 78, 
  12, 29, 77,
  13, 27, 75,
  14, 25, 74,
  15, 22, 71, 
  16, 20, 64,
  17, 18, 61, 
  18, 15, 59,
  19, 14, 64,
  20,  9, 61,   # messi shoots the ball
  21,  0, 47,   # goal
  22, -3, 46,   # within the goal ball movement
  23, -1, 45    # within the goal ball movement
  
)

#Plot the soccer field
# Plot all the data (static plot)
plot1 <- ggplot() + 
  annotate_pitch(colour = "white", fill = "springgreen3") + 
  theme_pitch() +  # theme removes plotting elements
  theme(panel.background = element_rect(fill = "springgreen3")) +
  geom_point( # add ball data
    data = ball,
    aes(x = x, y = y),
    colour = "black", fill = "white", pch = 21, size = 2) +
  geom_point(# add player data
    data = barca_players, 
    aes(x = x, y = y), 
    colour = "black", fill = "blue", pch = 21, size = 4
  ) +
  geom_point( # Opponent player data
    data = getafe_players,
    aes(x = x, y = y),
    colour = "black", fill = "yellow", pch = 21, size = 4
  ) + 
  geom_text(       # add player labels
    data = barca_players, aes(x = x, y = y, label = name),
    nudge_x = 1, hjust = -0.2
  ) +
  geom_text( # add getafe player labels
    data = getafe_players, aes (x = x, y = y, label = name),
    nudge_x = 1, hjust = -0.2
  ) +
  ggtitle(         # add title
    label = "Barcelona vs.Getafe",
    subtitle = "GOAL Lionel Messi"
  )

plot1 + facet_wrap(~ frame) + ggtitle(NULL, NULL)

# now make the animation
# Animate the plot
animation <- plot1 +        # the plot object
  transition_states(        # Select transition states
    states = frame,         # time-step variable
    state_length = 0.01,    # duration of frame
    transition_length = 1,  # duration between frames
    wrap = T
  )

anim_save("outfile.gif", animation)
animate(animation)
