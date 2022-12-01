# clustering by models
library(tidyr) # Load tidyr
library(cluster) # Load cluster
library(factoextra) # clustering algorithms & visualization
library(sparcl) # Sparse Clustering
library(teamcolors) # Load team colors
library(ggimage) # Load ggimage
library(ggdark) # Load ggdark

main_players <- read.csv("D:/Coursework/Mod-2/Machine Learning/Project/MLApp/players.csv",
                         stringsAsFactors = TRUE, header = TRUE,
                         na.strings = c("", " ", "NA"))


teams <- read.csv("D:/Coursework/Mod-2/Machine Learning/Project/New folder/teams.csv",
                  stringsAsFactors = T, header = T, na.strings = c("", " ", "NA"))

# compare teams in a league

comp_teams <- function(league_name, x_axis, y_axis){
  
  league_filter <- teams[teams$str_league == league_name, ] |> na.omit()
  
  cool_plot <- ggplot(league_filter, 
                      aes(x = x_axis, y = y_axis)) + 
    geom_point(alpha = 0.3) #+ 
    # geom_image(image = league_filter$link_logo, asp = 16/9) + 
    # labs(x = glue::glue("Team Rating {x_axis}"), y = glue::glue("Team Rating {x_axis}"),
         # title = glue::glue("{x_axis} Team Rating vs. {y_axis} Rating"),
         # subtitle = glue::glue("{league_name}")) + 
    # dark_theme_bw() + # Set theme
    # theme( # Modify plot elements
    #   axis.text = element_text(size = 10), # Change Axis text size
    #   axis.title.x = element_text(size = 12), # Change x axis title size
    #   axis.title.y = element_text(size = 12), # Change y axis title size 
    #   plot.title = element_text(size = 16), # Change plot title size
    #   plot.subtitle = element_text(size = 14), # Change plot subtitle size
    #   plot.caption = element_text(size = 10), # Change plot caption size
    #   panel.grid.major = element_blank(), # Remove grid
    #   panel.grid.minor = element_blank(), # Remove grid
    #   panel.border = element_blank(), # Remove grid
    #   panel.background = element_blank()) # Remove grid
  
  # Turn off dark mode
  # invert_geom_defaults()
  
  return (cool_plot)
  
}

















