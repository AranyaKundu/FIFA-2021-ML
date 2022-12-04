# clustering by models
library(tidyr) # Load tidyr
library(cluster) # Load cluster
library(ggimage) # Load ggimage
library(ggsoccer) # Load ggsoccer for the soccer pitch


teams <- read.csv("D:/Coursework/Mod-2/Machine Learning/Project/New folder/teams.csv",
                  stringsAsFactors = T, header = T, na.strings = c("", " ", "NA"))

# compare teams in a league

comp_teams <- function(league_name, x_axis, y_axis){
  
  league_filter <- teams[teams$str_league == league_name, ]
  league_filter <- league_filter[complete.cases(league_filter), ] # remove NAs from the data
  
  plot_data <- cbind.data.frame(league_filter[,c(x_axis, y_axis, "link_logo")])
  names(plot_data)[1:2] <- c("var_1", "var_2")
  cool_plot <- ggplot(plot_data, 
                      aes(x = var_1, y = var_2)) + 
    geom_point(alpha = 0.3) + 
    geom_image(image = plot_data$link_logo, asp = 16/9) + 
    labs(x = glue::glue("Team Rating {x_axis}"), y = glue::glue("Team Rating {y_axis}"),
         title = glue::glue("{y_axis} Team Rating vs. {x_axis} Rating"),
         subtitle = glue::glue("{league_name}")) 
  
  return (cool_plot)
  
}


data <- scale(teams[, c(4:10, 12:13, 15:16)])
data <- cbind.data.frame(teams$str_team_name, teams$str_league, data, teams$int_corners, teams$int_freekicks)
names(data)[c(1:2, 14:15)] <- c("Name", "League", "Corners", "Freekicks")

clustering <- function(no_of_centers){
  set.seed(123456)
  fit_cluster <- kmeans(x = data[, 3:15], # Set data as explanatory variables 
                        centers = no_of_centers,  # Set number of clusters
                        nstart = 25, # Set number of starts
                        iter.max = 100)
  # Extract clusters
  clusters <- fit_cluster$cluster
  # Extract centers
  centers <- fit_cluster$centers
  # summary(as.factor(clusters_1))
  
  clustered_data <- data.frame(cluster = c(1:no_of_centers), centers)
  
  reshape_centers <- gather(clustered_data, features, values, int_overall:Freekicks)
  
  
  # Create plot to visualize the clusters
  
  heatmap_cluster <- ggplot(data = reshape_centers, # Set dataset for the plot
                            aes(x = features, y = cluster, fill = values)) + # Set x, y and fill values
          scale_y_continuous(breaks = seq(1, no_of_centers, by = 1)) + #set breaks for y-axis, used for tiles
    geom_tile() + # Tiles for the heatmap
  coord_equal() +# make axis size same for both axis of the map
  coord_flip() +# For better visualization
    scale_fill_gradient2(aesthetics = 'fill', # Set fill for the tiles
                        midpoint = 3,  # Set midpoint for the color bar
                        low = "blue", # set color for low values
                        mid = "white", # set color for mid values
                        high = "red", # set color for high values
                        guide = "colourbar") #set color bar
    return (heatmap_cluster) # return the ggplot graph
  
}


# cluster_table <- function(){
#   clustering(no_of_centers)
#   team_clusters <- rbind.data.frame(cluster_ = rep(0, no_of_centers))
#   for (i in 1:no_of_centers){
#     team_clusters <- merge.data.frame(team_clusters, data$Name[clusters == i])
#     # glue::glue("Teams in cluster {i}:")
#     # glue::glue("{data$Name[clusters == i]}")
#   }
# }

# Pitch

choiceTeam <- function(){
  players <- players_df[, c(2, 4, 7:9, 57)]
  z <- players_df[c(5, 7, 1, 4, 28, 68, 33, 11, 65, 31, 3), c(2, 57)]
  team_coords <- data.frame(x = c(25, 17, 25, 50, 43, 50, 75, 80, 80, 75, 95),
                            y = c(19, 52, 85, 19, 52, 85, 15, 40, 65, 90, 50),
                            photo = z$str_player_image_url)
  
  # dims <- list(length = 100, width = 100, penalty_box_length = 16,
  #                   penalty_box_width = 60, six_yard_box_length = 3, six_yard_box_width = 20,
  #                   penalty_spot_distance = 4, goal_width = 16, origin_x = 0, origin_y = 0)

  
  pitch <- ggplot(team_coords) + # set data for the plot
    annotate_pitch(colour = "white", # set colour of pitch
                   fill = "springgreen4", #set fill pitch colour
                   limits = F) + # Set pitch limits to False
    geom_image(aes(x = x, y = y), # Set x and y axis co-ordinates
      image = team_coords$photo, asp = 3/2) + # Set images of players to be printed
    theme_pitch() + # Set pitch theme
    theme(panel.background = element_rect(fill = "springgreen3")) + # ste pitch background fill
    ggtitle("Formation: 4-3-3") # Mention title for the plot
  return (pitch)
}
