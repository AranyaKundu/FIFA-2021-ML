source("ml_project.R", local = TRUE)
source("potential.R", local = TRUE)
source("best_team.R", local = T)
source("clustering.R", local = T)

# Building the App UI
ui <- (
  shinyUI(
    # Dash board page starts
    dashboardPage(skin="green",
      # 3 comps: header, sidebar, body
      
      # Header start here
      dashboardHeader(title = "FIFA 2021"), # Set header
      
      # Dashboard sidebar start here
      dashboardSidebar(collapsed = T,
                       sidebarMenu(
                         menuItem("Home", icon = icon("house"), tabName = "homeTab"), # Create Home Tab
                         menuItem("DataSet", # Create DataSet Tab
                                  icon = icon("table", verify_fa = F), tabName = "dataset"),
                         menuItem("Data Viz", icon = icon("database", verify_fa = F),
                                  newtab = F, tabName = "VizCraft", startExpanded = F,
                                  # Create dropdown menu using SubItem
                                  menuSubItem("Player Distribution", # Create Player Distribution
                                              # Set icon using Font-awesome
                                           icon = icon("fa-solid fa-user", verify_fa = F),
                                           newtab = F, tabName = "dataViz"),
                                  menuSubItem("Team Distribution", # Create Team distribution Tab
                                           icon = icon("fa-duotone fa-futbol", verify_fa = F), # Set icon
                                           tabName = "teams", newtab = F)
                         ),
                         menuItem("Clustered Teams", # Create Clustered Teams Tab for heat map
                                  newtab = F, tabName = "clustering",
                                   icon = icon("fa-duotone fa-map", verify_fa = F)),
                         menuItem("Compare Models", # Create Tab to compare models
                                  icon = icon("magnifying-glass", verify_fa = F),
                                  newtab = F, tabName = "Model_comparison"),
                         menuItem("Predict Position", # Create Tab to Predict position of any player
                                  icon = icon("fa-duotone fa-chart-simple", 
                                                      verify_fa = F), newtab = F, tabName = "position"),
                         menuItem("Player Details", # Create data to visualize traits of the players
                                  icon = icon("fa-solid fa-person", verify_fa = F),
                                  newtab = F, tabName = "Player"),
                         menuItem("Team Strategy", icon = icon("fa-regular fa-users", verify_fa = T),
                                  newtab = F, tabName = "team", startExpanded = F,
                                  # Create drop down using menu sub Item
                                  menuItem("Dream Team", # Create Tab for a Team based on constraints
                                           icon = icon("fa-solid fa-people-group", verify_fa = F),
                                           newtab = F, tabName = "bestTeam"),
                                  menuItem("My Team on Pitch", # Create Tab for best Teams
                                           tabName = "mychoiceTeam", newtab = F, 
                                           icon = icon("fa-solid fa-users", verify_fa = F))        
                         ),
                         menuItem("Development Archive", # Create a tab to keep old archives models
                                  icon = icon("archive", verify_fa = F),
                                  newtab = F, tabName = "archives"),
                         menuItem("Get in Touch", icon = icon("fa-solid fa-address-card", verify_fa = F),
                                  newtab = F, tabName = "contact")
                       )
      ),
      # dashboard body starts here
      dashboardBody(
        tabItems(
          # Home Page
          tabItem(tabName = "homeTab",
                  div(class="container", tags$img(src = "fifa-21.jpg"), # Display image in the home page
                      actionButton("homeButton", # Action button on the image (css associated)
                                   class = "btn", label = "Play"), 
                      p(em(HTML("&copy;"), "Developed by Aranya Kundu")) # Disclaimer at the bottom of page
                )
          ),
          
          
          # Data Set Page
          tabItem(tabName = "dataset", # Display three data sets used for the model
                  fluidRow(
                    # Create drop down to select the data set to display
                    column(3, selectInput("selectData", label = "Choose Dataset",
                                          choices = c("FIFA 2021 Dataset", "FIFA Players Table",
                                                      "FIFA Teams Table"))
                    ),
                    column(3, actionButton("display", label = "Display", class = "bttn button-1"))
                  ),
                  div(class="container", DTOutput("mydataset") %>% withSpinner())
          ),
          
          
          # Data Visualization - Players
          tabItem(tabName = "dataViz", # Plot the distribution of player's traits in the data set
                  fluidRow(
                    column(4, # Create drop down to choose X-axis variable
                           selectInput("selectX", label = "Choose X-axis Variable", 
                                       choices = viz_cols, multiple = F)
                    ),
                    column(4, # Create drop down to choose Y-axis variable
                           selectInput("selectY", label = "Choose Y-axis Variable", 
                                       choices = viz_cols, multiple = F)
                    ),
                    column(4, # Create action button to display the plot
                           actionButton("doAction", label = "Visualize", class = "bttn button-2"))
                  ),
                  fluidRow(
                    column(12, withSpinner(plotOutput("viz_Plot"))) # Add spinner while loading
                  )
          ),
          
          
          # Teams Tab
          tabItem(tabName = "teams",
                  fluidRow( # Create drop down to choose league
                    column(3, selectInput("selectLeague", label = "Choose League", 
                                          selected = "English Premier League (1)", 
                                          choices = teams$str_league, multiple = F)),
                    # Create drop down to select X-axis variable
                    column(3, selectInput("axis_x", label = "X Axis Variable", 
                                          choices = names(teams), multiple = F)),
                    # Create drop down to select Y-axis variable
                    column(3, selectInput("axis_y", label = "Y Axis Variable", 
                                          choices = names(teams), multiple = F)),
                    # Create action button to display the plot
                    column(3, actionButton("showPlot", label = "Display", class = "bttn button-1"))
                  ),
                  fluidRow(# Add spinner while loading
                    column(12, withSpinner(plotOutput("coolplot1")))
                  )
          ),
          
          
          # Clustering Heat map
          tabItem(tabName = "clustering",
                  fluidRow(# Create numeric input for the number of clusters
                    column(3, numericInput("noclusters", label = "Number of Clusters", 
                                           min = 5, max = 50, value = 10, step = 1)),
                    # Create action button to visualize the plot
                    column(3, actionButton("heatmap", label = "Visualize", class = "btn btn-info"))
                  ),
                  fluidRow(# Add spinner while loading
                    column(width = 12, withSpinner(plotOutput("heatwave")))
                  )
          ),
          
          
          # ML Models Page 
          tabItem(tabName = "Model_comparison",
                  fluidRow(
                    column(3, # Create drop down to select which model to apply
                           selectInput("ModelX", label = "Choose any model from the list", 
                                       choices = c("Select", "Linear Model", "General Additive Model",
                                                   "Lasso Regression"), selected = "select")),
                    column(3, # Create action button to compute using the model selected
                           actionButton("my_model_1", label = "Compute", class = "bttn button-1")),
                    column(3, # Create drop down to select which model to apply
                           selectInput("ModelY", label ="Choose a different Model from the list",
                                       choices = c("Select", "Linear Model", "General Additive Model",
                                                   "Lasso Regression"), selected = "select")
                    ),
                    column(3, # Create action button to compute based on selected model
                           actionButton("my_model_2", label = "Compute", class = "bttn button-1"))
                  ),
                  fluidRow(
                    column(5, # Create data table output to display output from 1st model
                           DTOutput("modelXInput")
                    ),
                    column(2), # Create data table output to display output from 2nd model
                    column(5, DTOutput("modelYInput"))
                  )
          ),
          
          
          # Position ML Page 
          tabItem(tabName = "position",
                  fluidRow(
                    column(4,
                           selectizeInput("player", label = "Choose any player from the list", 
                                       choices = NULL, multiple = F)),
                    column(2,
                           actionButton("displayPlayer", label = "Display", class = "bttn button-1")),
                    column(4, selectInput("modelinUse", label = "Choose a ML Model",
                                          choices = c("Decision Tree", "Bootsrap Aggregation"),
                                          multiple = F)),
                    column(2,
                           actionButton("showAUC", label = "Show AUC", class = "btn"))
                  ),
                  fluidRow(
                    column(6, DTOutput("predictedPosition")),
                    column(6, plotOutput("aucplot"))
                  )
          ),
          
          
          # Player details Page
          tabItem(tabName = "Player", 
                  fluidRow(
                    column(6,
                           column(8, selectizeInput("PlayerX", "Select player - 1", 
                                                    choices = NULL, multiple = F
                           )),
                           column(4, actionButton("my_player_1", label = "Display", class = "button-2"))
                    ),
                    column(6,
                           column(8, selectizeInput("PlayerY", "Select player - 2", 
                                                    choices = NULL, multiple = F
                           )),
                           column(4, actionButton("my_player_2", label = "Display", class = "button-2"))
                    )
                  ),
                  hr(),
                  fluidRow(
                    column(6, 
                           column(10, tableOutput("player1Details")),
                           column(2, imageOutput("playerXImg"))),
                    column(6,
                           column(10, tableOutput("player2Details")),
                           column(2, imageOutput("playerYImg"))
                    ),
                  ),
                  hr(),
                  fluidRow(
                    # need to adjust row height
                    column(6, DTOutput("playerXDetails")),
                    column(6, DTOutput("playerYDetails"))
                  )
          ),
          
          
          # Optimized Team Tab
          tabItem(tabName = "bestTeam", 
                  fluidRow(
                    column(width = 2, offset = 1,
                           numericInput("GK_count", "GoalKeeper", min = 1, max = 7, value = 2, step = 1)),
                    column(2, 
                           numericInput("RB_count", "Right Back", min = 1, max = 7, value = 2, step = 1)),
                    column(2, 
                           numericInput("LB_count", "Left Back", min = 1, max = 7, value = 2, step = 1)),
                    column(2, 
                           numericInput("CB_count", "Center Back", min = 1, max = 7, value = 2, step = 1)),
                    column(2, 
                           numericInput("LM_count", "Left Mid", min = 1, max = 7, value = 2, step = 1))
                  ),
                  fluidRow(
                    column(width = 2, offset = 1, 
                           numericInput("RM_count", "Right Mid", min = 1, max = 7, value = 2, step = 1)),
                    column(2, 
                           numericInput("CM_count", "Central Midfield", 
                                        min = 1, max = 7, value = 2, step = 1)),
                    column(2, 
                           numericInput("FW_count", "Forward", min = 1, max = 7, value = 2, step = 1)),
                    column(2, 
                           numericInput("size", "Team Size", min = 11, max = 40, value = 14, step = 1)),
                    column(2, actionButton("showTeam", label = "Show", class = "btn button-2"))
                  ),
                  fluidRow(withSpinner(DTOutput("my_dream_team")))
          ),
          
          
          # Soccer Pitch
          tabItem(tabName = "mychoiceTeam",
                  fluidRow(
                    column(6, plotOutput("my_choice_team_433") %>% withSpinner()),
                    column(6, plotOutput("my_choice_team_442") %>% withSpinner())
                  )
              ),
          
          
          # ML Archives Tab Code
          tabItem(tabName = "archives",
                  fluidRow(
                    column(4, selectInput("modelList", "Select Model", 
                                          choices = c("Single Decision Tree",
                                                      "Clustered Single Decision Tree"))
                           ),
                    column(2, actionButton("showModel", label = "Compute", class = "button-1"))
                  ),
                  fluidRow(
                    column(10, tableOutput("cfmtbl") %>%  withSpinner())
                  )
            ),
          
          
          # Get in Touch Page
          tabItem(tabName = "contact",
                  fluidRow(
                    column(3, tags$img("www/contact.jpg", width="200px", height="260px")),
                    column(3, p(class = "card",
                                em(HTML("fa-solid fa-phone"))))

                  )
          )
        ),
        
        
        ## Shiny BS Modal to display the data set inside a modal
        bsModal(id="defVideo", title = "EA Sports", trigger = "homeButton", size="large",
                tags$iframe(src="ea_video.mp4", 
                            frameborder="0", 
                            allow="autoplay; encrypted-media; picture-in-picture", class = "homeVideo")
        ),
        tags$head(
          tags$script(src = "sidebar.js"),
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        )
      )
    )  
  )
)


# Building the APP Server

server <- (
  shinyServer(function(input, output, session) {
    
    # Code for data set Tab
    data <- eventReactive(input$display, { # Triggers on click of the button
      if (input$selectData == 'FIFA 2021 Dataset'){
        df[, c(2:3, 5:6, 19, 21:23)]
      } else if(input$selectData == 'FIFA Players Table'){
        players_df[,c(2, 7:9, 11, 12, 14, 44)]
      } else {
        teams_df()
      }
    })
    
    output$mydataset <- renderDataTable({
      req(data())
      datatable(data())
      })
    
    
    # code for "Data Visualization" tab
    data_viz_fifa <- eventReactive(input$doAction,{
      my_plot_func(data = pmdf, x_axis = input$selectX, y_axis = input$selectY)
    })
    
    output$viz_Plot <- renderPlot(data_viz_fifa(), # Plot the data
                                  width = "auto", height = 700) # Set height and width of the plot
    
    
    # Teams Tab
    cool_plot <- eventReactive(input$showPlot, # Triggers on button click
                               {
      comp_teams(league_name = input$selectLeague, x_axis = input$axis_x, y_axis = input$axis_y)
    })
    
    output$coolplot1 <- renderPlot(cool_plot(), width = "auto", height = 700)
    
    
    # Heat map Teams
    heatmapplot <- eventReactive(input$heatmap, {
      clustering(input$noclusters)
    })
    
    output$heatwave <- renderPlot(heatmapplot(), width = "auto", height = 700)
    
    
    # Code for positions tab
    updateSelectizeInput(session, 'player', choices = df$Name, server = TRUE)
    
    playerPosition <- eventReactive(input$displayPlayer, {
      predict_position(player_Nm = input$player)
    })
    
    output$predictedPosition <- renderDT(playerPosition())
    
    showAUC <- eventReactive(input$showAUC, {
      final_models(input$modelinUse)
    })
    output$aucplot <- renderPlot(showAUC(), width = "auto", height = 600)
    
    # code for Compare models Tab 
    modelXOutput <- eventReactive(input$my_model_1, {
      my_model(model_type = input$ModelX)
    })
    
    modelYOutput <- eventReactive(input$my_model_2, {
      my_model(model_type = input$ModelY)
    })
    
    output$modelXInput <- renderDT(modelXOutput())
    output$modelYInput <- renderDT(modelYOutput())
    
    
    # Code for player display tab
    updateSelectizeInput(session, 'PlayerX', choices = players_df$str_player_name, server = TRUE)
    updateSelectizeInput(session, 'PlayerY', choices = players_df$str_player_name, 
                         selected = "Cristiano Ronaldo dos Santos Aveiro", server = TRUE)
    
    dataInput <- reactive({
      players_df[players_df$str_player_name == input$PlayerX, c(55:56)]
    })
    output$player1Details <- renderTable(dataInput())
    
    dataInput2 <- reactive({
      players_df[players_df$str_player_name == input$PlayerY, c(55:56)]
    })
    output$player2Details <- renderTable(dataInput2())
    
    playerXphoto <- eventReactive(input$my_player_1, {
      player_display(input$PlayerX)
    })
    
    output$playerXImg <- renderImage({
      list(src = file.path("www", playerXphoto()), width = "auto", height = "auto")
    }, deleteFile = F)
    
    playerYphoto <- eventReactive(input$my_player_2, {
      player_display(input$PlayerY)
    })
    
    output$playerYImg <- renderImage({
      list(src = file.path("www", playerYphoto()), width = "auto", height = "auto")
    }, deleteFile = F)
    
    output$playerXDetails <- renderDT(ready_player(data = players_df, player_Name = input$PlayerX))
    output$playerYDetails <- renderDT(ready_player(data = players_df, player_Name = input$PlayerY))
    
    
    # Code for improved Model tab 
    modelImprovedOutput <- eventReactive(input$my_model_2, {
      improved_model_analysis(position = input$positionX, model = input$ModelX)
    })
    
    output$modelAnalysis <- renderDT(modelImprovedOutput())
    
    
    # best Team Tab Code 
    showbestTeam <- eventReactive(input$showTeam, {
      optimal_team(GK_count = input$GK_count, RB_count = input$RB_count, LB_count = input$LB_count, 
                   CB_count = input$CB_count, RM_count = input$RM_count, CM_count = input$CM_count, 
                   LM_count = input$LM_count, FW_count = input$FW_count, team_size = input$size)
    })
    
    output$my_dream_team <- renderDT(showbestTeam())
    
    
    # Pitch best Team
    # Set height and width of each pitch
    output$my_choice_team_433 <- renderPlot(choiceTeam_433(), width = "auto", height = 725)
    output$my_choice_team_442 <- renderPlot(choiceTeam_442(), width = "auto", height = 725)
    
    
    # Development Archive Tab Code
    modelOut <- eventReactive(input$showModel, {
      archived_models(model_type = input$modelList)
    })
    output$cfmtbl <- renderTable(modelOut()) 
  
    
    
    # Contact Page
    #output$myPhoto <- renderImage(list(src = file.path("www/contact.jpg")))
  })
)

shinyApp(ui = ui, server = server)

