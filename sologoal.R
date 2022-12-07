# Plot the soccer field

messi_vs_getafe <- ggplot(players) + # set data for the plot
  annotate_pitch(colour = "white", # set colour of pitch
                 fill = "springgreen4") +
  theme_pitch() + # Set pitch theme
  coord_flip() + #Flip the coordinates for adjusting two different formations
  theme(panel.background = element_rect(fill = "springgreen3"),
        aspect.ratio = 1.4) + # set pitch background fill

# Player position data
players <- tribble(
  
  ~frame, ~name, ~x, ~y,  # column names
  
  1, "Xavi Hernández", 40, 80, # Passes the ball to Messi
  
  1, "Messi", 45, 95,  # advances from own half
  2, "Messi", 48, 92,  # dribbles one in own half
  3, "Messi", 51, 87,  # dribbles the second in own half
  4, "Messi", 25, 75,  # dribbles the third in opponent half
  5, "Messi", 22, 72,  # dribbles the fourth in opponent half
  6, "Messi", 18, 60,  # dribbles the fifth in opponent half
  7, "Messi", 14, 63,  # dribbles past the goal keeper
  8, "Messi", 10, 60,  # shot
  9, "Messi", 9, 60, # goal
  
  1, "Getafe 1", 49, 94, # Obstructs Messi
  2, "Getafe 1", 49, 92, # Lying down
  
  
  1, "Getafe 2", 51, 88, # Obsttruct Messi
  2, "Getafe 2", 52, 87, # Obstructs Messi
  3, "Getafe 3", 75, 60, # receives pass from De Jong
  4, "Agüero", 76, 63, # beats defender, passes to Balotelli
  5, "Agüero", 80, 50, # advances to edge of box
  6, "Agüero", 87, 38, # receives pass from Balotelli
  7, "Agüero", 93, 36, # shot
  8, "Agüero", 94, 33, # goal
  
  1, "Luis Garcia", 93, 61, # waiting on the goalpost
  2, "Balotelli", 83, 61, # advances to stop Messi
  3, "Balotelli", 83, 61, # waiting on edge of box
  4, "Balotelli", 83, 57, # waiting on edge of box
  5, "Balotelli", 83, 55, # recieves pass from Agüero
  6, "Balotelli", 83, 55, # passes to Agüero
  7, "Balotelli", 83, 54, # off the ball
  8, "Balotelli", 83, 54, # off the ball
  
)

# Ball position data
ball <- tribble(
  
  ~frame, ~x, ~y,
  
  1,  51, 50,  # De Jong possession
  2,  57, 50,  # De Jong pass
  3,  74, 60,  # receievd by Agüero
  4,  77, 63,  # Agüero pass
  5,  83, 54,  # received by Balotelli
  6,  88, 38,  # received by Agüero
  7,  94, 36,  # Agüero shot
  8, 100, 46   # goal 
  
)
