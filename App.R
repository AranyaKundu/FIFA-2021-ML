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
      #Header start here
      dashboardHeader(title = tags$img(class = "title", src='logo.png')),
      # Dashboard sidebar start here
      dashboardSidebar(collapsed = T,
                       sidebarMenu(
                         menuItem("Home", icon = icon("house"), tabName = "homeTab"),
                         menuItem("DataSet", icon = icon("table", verify_fa = F), tabName = "dataset"),
                         menuItem("Player Distribution", icon = icon("database", verify_fa = F),
                                  newtab = F, tabName = "dataViz"),
                         menuItem("Team Distribution", icon = icon("fa-duotone fa-futbol", verify_fa = F),
                                  tabName = "teams", newtab = F),
                         menuItem("Compare Models", icon = icon("magnifying-glass", verify_fa = F),
                                  newtab = F, tabName = "Model_comparison"),
                         menuItem("Predict Position", icon = icon("fa-duotone fa-chart-simple", 
                                                      verify_fa = F), newtab = F, tabName = "position"),
                         menuItem("Player Details", icon = icon("fa-solid fa-person", verify_fa = F),
                                  newtab = F, tabName = "Player"),
                         menuItem("Dream Team", icon = icon("fa-solid fa-people-group", verify_fa = F),
                                  newtab = F, tabName = "bestTeam"),
                         menuItem("Development Archive", icon = icon("archive", verify_fa = F),
                                  newtab = F, tabName = "archives")
                       )
      ),
      # dashboard body starts here
      dashboardBody(
        tabItems(
          # Home Page
          tabItem(tabName = "homeTab",
                  div(class="container", tags$img(src = "fifa-21.jpg"),
                      actionButton("homeButton", class = "btn", label = "Play"))
          ),
          # Data Set Page
          tabItem(tabName = "dataset",
                  fluidRow(
                    column(3, selectInput("selectData", label = "Choose Dataset",
                                          choices = c("FIFA 2021 Dataset", "FIFA Players Table",
                                                      "FIFA Teams Table"))
                    ),
                    column(3, actionButton("display", label = "Display", class = "bttn button-1"))
                  ),
                  div(class="container", DTOutput("mydataset"))
          ),
          # Data Visualization - Players
          tabItem(tabName = "dataViz",
                  fluidRow(
                    column(4, 
                           selectInput("selectX", label = "Choose X-axis Variable", 
                                       choices = viz_cols, multiple = F)
                    ),
                    column(4,
                           selectInput("selectY", label = "Choose Y-axis Variable", 
                                       choices = viz_cols, multiple = F)
                    ),
                    column(4, 
                           actionButton("doAction", label = "Visualize", class = "bttn button-2"))
                  ),
                  fluidRow(
                    column(12, plotOutput("viz_Plot"), div(style = "height: auto, max-height: 100%"))
                  )
          ),
          
          # Teams Tab
          tabItem(tabName = "teams",
                  fluidRow(
                    column(3, selectInput("selectLeague", label = "Choose League", 
                                          selected = "English Premier League (1)", 
                                          choices = teams$str_league, multiple = F)),
                    column(3, selectInput("axis_x", label = "X Axis Variable", 
                                          choices = names(teams), multiple = F)),
                    column(3, selectInput("axis_y", label = "Y Axis Variable", 
                                          choices = names(teams), multiple = F)),
                    column(3, actionButton("showPlot", label = "Display", class = "bttn button-1"))
                  ),
                  fluidRow(
                    column(12, plotOutput("coolplot1"), div(style = "min-height: 800px, width: auto,
                                                            max-width: 100%"))
                  )
          ),
          
          # ML Models Page //Working decently
          tabItem(tabName = "Model_comparison",
                  fluidRow(
                    column(3,
                           selectInput("ModelX", label = "Choose any model from the list", 
                                       choices = c("Select", "Linear Model", "General Additive Model",
                                                   "Lasso Regression",
                                                   "Logistic Regression", "Decision Tree Models",
                                                   "XGBoost"), selected = "select")),
                    column(3,
                           actionButton("my_model_1", label = "Compute", class = "bttn button-1")),
                    column(3,
                           selectInput("ModelY", label ="Choose a different Model from the list",
                                       choices = c("Select", "Linear Model", "General Additive Model",
                                                   "Lasso Regression",
                                                   "Logistic Regression", "Decision Tree Models",
                                                   "XGBoost"), selected = "select")
                    ),
                    column(3,
                           actionButton("my_model_2", label = "Compute", class = "bttn button-1"))
                  ),
                  fluidRow(
                    column(5,
                           DTOutput("modelXInput")
                    ),
                    column(2),
                    column(5, DTOutput("modelYInput"))
                  )
                  
                  
          ),
          # Position ML Page //cannot display confusion matrix
          tabItem(tabName = "position",
                  fluidRow(
                    column(4,
                           selectizeInput("player", label = "Choose any player from the list", 
                                       choices = NULL, multiple = F),
                    column(4,
                           actionButton("displayPlayer", label = "Display", class = "bttn button-1"))
                    )
                  ),
                  fluidRow(
                    column(6, DTOutput("predictedPosition"))
                  )
          ),
          # Player details Page 
          tabItem(tabName = "Player", 
                  fluidRow(div(style = "height: 100px, top = 0"),
                    column(4, selectizeInput("PlayerX", "Select player - 1", 
                                          choices = NULL, multiple = F
                                          )),
                    column(2, actionButton("my_player_1", label = "Display", class = "button-2")),
                    column(4, selectizeInput("PlayerY", "Select player - 2", 
                                          choices = NULL, multiple = F
                                          )),
                    column(2, actionButton("my_player_2", label = "Display", class = "button-2"))
                  ),
                  fluidRow(column(6, 
                                  column(3, uiOutput("playerDetails")),
                                  column(3,
                                  imageOutput("playerXImg"))),
                           column(6, imageOutput("playerYImg"))
                           ),
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
                           numericInput("GK_count", "GoalKeeper", min = 1, max = 7, value = 1, step = 1)),
                    column(2, 
                           numericInput("RB_count", "Right Back", min = 1, max = 7, value = 1, step = 1)),
                    column(2, 
                           numericInput("LB_count", "Left Back", min = 1, max = 7, value = 1, step = 1)),
                    column(2, 
                           numericInput("CB_count", "Center Back", min = 1, max = 7, value = 1, step = 1)),
                    column(2, 
                           numericInput("LM_count", "Left Mid", min = 1, max = 7, value = 1, step = 1))
                  ),
                  fluidRow(
                    column(width = 2, offset = 1, 
                           numericInput("RM_count", "Right Mid", min = 1, max = 7, value = 1, step = 1)),
                    column(2, 
                           numericInput("CM_count", "Central Midfield", min = 1, max = 7, value = 1, step = 1)),
                    column(2, 
                           numericInput("FW_count", "Forward", min = 1, max = 7, value = 1, step = 1)),
                    column(2, 
                           numericInput("size", "Team Size", min = 11, max = 40, value = 14, step = 1)),
                    column(2, actionButton("showTeam", label = "Show", class = "btn button-2"))
                  ),
                  fluidRow(DTOutput("my_dream_team"))
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
                    column(1),
                    column(10, tableOutput("cfmtbl")),
                    column(1)
                  )
            )
        ),
        
        ## Shiny BS Modal to display the data set inside a modal //Not working at all
        bsModal(id="defVideo", title = "EA Sports", trigger = "homeButton", size="large",
                tags$iframe(src="ea_video.mp4", 
                            frameborder="0", 
                            allow="autoplay; encrypted-media; picture-in-picture", class = "homeVideo"),
                tags$head(tags$style("#defVideo{position: absolute; width: 100%; overflow: hidden;
                                     height: 315, max-width: 400}
                                       #defVideo .modal-footer{display:none}"))
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
    
    # Code for data set Tab (need to check)
    
    data <- eventReactive(input$display, {
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
    
    # code for "Data Visualization" tab (fully working)
    data_viz_fifa <- eventReactive(input$doAction,{
      my_plot_func(data = pmdf, x_axis = input$selectX, y_axis = input$selectY)
    })
    
    output$viz_Plot <- renderPlot(data_viz_fifa())
    
    # Teams Tab // to check
    cool_plot <- eventReactive(input$showPlot, {
      # browser()
      comp_teams(league_name = input$selectLeague, x_axis = input$axis_x, y_axis = input$axis_y)
    })
    
    output$coolplot1 <- renderPlot(cool_plot())
    
    # Code for positions tab
    updateSelectizeInput(session, 'player', choices = df$Name, server = TRUE)
    
    playerPosition <- eventReactive(input$displayPlayer, {
      predict_position(player_Nm = input$player)
    })
    
    output$predictedPosition <- renderDT(playerPosition())
    
    # code for Compare models Tab //not done yet (partially working)
    modelXOutput <- eventReactive(input$my_model_1, {
      my_model(data = pmdf, model_type = input$ModelX)
    })
    
    modelYOutput <- eventReactive(input$my_model_2, {
      my_model(data = pmdf, model_type = input$ModelY)
    })
    
    output$modelXInput <- renderDT(modelXOutput())
    output$modelYInput <- renderDT(modelYOutput())
    
    # Code for player display tab
    updateSelectizeInput(session, 'PlayerX', choices = players_df$str_player_name, server = TRUE)
    updateSelectizeInput(session, 'PlayerY', choices = players_df$str_player_name, 
                         selected = "Cristiano Ronaldo dos Santos Aveiro", server = TRUE)
    playerXphoto <- eventReactive(input$my_player_1, {
      player_display(input$PlayerX)
    })
    
    output$playerXImg <- renderImage({
      list(src = file.path("www", playerXphoto()), width = 120, height = 120)
    }, deleteFile = F)
    
    playerYphoto <- eventReactive(input$my_player_2, {
      player_display(input$PlayerY) 
    })
    
    output$playerYImg <- renderImage({
      list(src = file.path("www", playerYphoto()), width = 120, height = 120)
    }, deleteFile = F)
    
    output$playerXDetails <- renderDT(ready_player(data = players_df, player_Name = input$PlayerX))
    output$playerYDetails <- renderDT(ready_player(data = players_df, player_Name = input$PlayerY))
    
    # Code for improved Model tab // Not working -confusion matrix cannot be displayed
    modelImprovedOutput <- eventReactive(input$my_model_2, {
      improved_model_analysis(position = input$positionX, model = input$ModelX)
    })
    
    output$modelAnalysis <- renderDT(modelImprovedOutput())
    
    # bestTeam Tab Code //Fully working (need to check with new dataset)
    
    showbestTeam <- eventReactive(input$showTeam, {
      optimal_team(GK_count = input$GK_count, RB_count = input$RB_count, LB_count = input$LB_count, 
                   CB_count = input$CB_count, RM_count = input$RM_count, CM_count = input$CM_count, 
                   LM_count = input$LM_count, FW_count = input$FW_count, team_size = input$size)
    })
    
    output$my_dream_team <- renderDT(showbestTeam())
    
    # Development Archive Tab Code
    
    modelOut <- eventReactive(input$showModel, {
      archived_models(model_type = input$modelList)
    })
    output$cfmtbl <- renderTable(modelOut()) # Some items of .SDcols are not column names: [BP]
    # output$cfmoverall <- renderDataTable({
    #   y <- req(modelOut())
    #   datatable(y$overall)
    # })
  })
)

shinyApp(ui = ui, server = server, display.mode = "showcase")




