source("ml_prj.R", local = TRUE)


# Building the App UI
ui <- (
  shinyUI(
    # Dash board page starts
    dashboardPage(
      # 3 comps: header, sidebar, body
      #Header start here
      dashboardHeader(title = "FIFA 2021"),
      # Dashboard sidebar start here
      dashboardSidebar(collapsed = F,
                       sidebarMenu(
                         menuItem("Home", icon = icon("house"), tabName = "homeTab"),
                         menuItem("Data visualization", icon = icon("database", verify_fa = F),
                                  newtab = F, tabName = "dataViz"),
                         menuItem("Compare Models", icon = icon("magnifying-glass", verify_fa = F),
                                  newtab = F, tabName = "Model_comparison"),
                         menuItem("Predict Position", icon = icon("fa-duotone fa-chart-simple", 
                                                      verify_fa = F), newtab = F, tabName = "position"),
                         menuItem("Player Details", icon = icon("fa-solid fa-person", verify_fa = F),
                                  newtab = F, tabName = "Player")
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
          # Data Visualization
          tabItem(tabName = "dataViz",
                  fluidRow(column(
                    4, 
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
          # ML Models Page
          tabItem(tabName = "Model_comparison",
                  fluidRow(
                    column(3,
                           selectInput("ModelX", label = "Choose any model from the list", 
                                       choices = c("Select", "Linear Model", "General Additive Model",
                                                   "Lasso Regression", "Neural Network based",
                                                   "Logistic Regression", "Decision Tree Models",
                                                   "XGBoost"), selected = "select")),
                    column(3,
                           actionButton("my_model_1", label = "Compute", class = "bttn button-1")),
                    column(3,
                           selectInput("ModelY", label ="Choose a different Model from the list",
                                       choices = c("Select", "Linear Model", "General Additive Model",
                                                   "Lasso Regression", "Neural Network based",
                                                   "Logistic Regression", "Decision Tree Models",
                                                   "XGBoost"), selected = "select")
                    ),
                    column(3,
                           actionButton("my_model_2", label = "Compute", class = "bttn button-1"))
                  ),
                  fluidRow(
                    column(6,
                           DTOutput("modelXInput")
                    ),
                    column(6, DTOutput("modelYInput"))
                  )
                  
                  
          ),
          # Position ML Page //to Edit content
          tabItem(tabName = "position",
                  fluidRow(
                    column(4,
                           selectInput("positionX", label = "Choose any position from the list", 
                                       choices = c("DEF", "MID", "FWD", "GK"), 
                                       selected = "DEF"), multiple = F),
                    column(4,
                           selectInput("ModelX", label = "Choose any model from the list", 
                                       choices = c("Select", "Linear Model", "General Additive Model",
                                                   "Lasso Regression", "Neural Network based",
                                                   "Logistic Regression", "Decision Tree Models",
                                                   "XGBoost"), selected = "select")),
                    column(4,
                           actionButton("my_model_2", label = "Compute", class = "bttn button-1"))
                  ),
                  fluidRow(DTOutput("modelAnalysis")
                  )
          ),
          # Player details Page
          tabItem(tabName = "Player", 
                  fluidRow(
                    column(4, selectInput("PlayerX", "Select player - 1", 
                                          choices = players_df$str_player_name, multiple = F,
                                          selected = "Lionel Andrés Messi Cuccittini")),
                    column(2, actionButton("my_player_1", label = "Display", class = "button-2")),
                    column(4, selectInput("PlayerY", "Select player - 2", 
                                          choices = players_df$str_player_name, multiple = F,
                                          selected = "Cristiano Ronaldo dos Santos Aveiro")),
                    column(2, actionButton("my_player_2", label = "Display", class = "button-2"))
                  ),
                  fluidRow(column(6, textOutput("playerXImg")),
                           column(6, textOutput("playerYImg"))
                           ),
                  fluidRow(
                    column(6, DTOutput("playerXDetails")),
                    column(6, DTOutput("playerYDetails"))
                  )
          )
        ),
        
        ## Shiny BS Modal to display the data set inside a modal
        ## A spinner is also added
        bsModal(id="defVideo", title = "EA Sports", trigger = "homeButton", size="large",
                tags$iframe(width = 400, height = 315,
                            src="ea_video.mp4", 
                            frameborder="0", 
                            allow="autoplay; encrypted-media; picture-in-picture", class = "homeVideo"),
                tags$head(tags$style("#defVideo{position: relative; width: 100%; overflow: hidden;}
                                       #defVideo .modal-footer{display:none}"))
        ),
        
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        )
      )
    )  
  )
)


# Building the APP Server

server <- (
  shinyServer(function(input, output, session) {
    
    # code for "Data Visualization" tab
    data_viz_fifa <- eventReactive(input$doAction,{
      my_plot_func(data = pmdf, x_axis = input$selectX, y_axis = input$selectY)
    })
    
    output$viz_Plot <- renderPlot(data_viz_fifa())
    
    # code for Compare models Tab //not done yet
    modelXOutput <- eventReactive(input$my_model_1, {
      my_model(data = pmdf, model_type = input$ModelX)
    })
    
    modelYOutput <- eventReactive(input$my_model_2, {
      my_model(data = pmdf, model_type = input$ModelY)
    })
    
    output$modelXInput <- renderDT(modelXOutput())
    output$modelYInput <- renderDT(modelYOutput())
    
    # Code for player display tab
    player_1_output <- eventReactive(input$my_player_1, {
      player_photo_1 <- players_df %>% filter(str_player_name == input$PlayerX) %>% 
        select(int_player_id)
      photo_1 <- paste(player_photo_1[1, 1], ".png", sep = "")
      output$playerXImg <- renderText({
        #c('<img src="',player_photo(),'">')
        div(tags$img(src=photo_1(), width = 140, height = 200))
      })
    })
    
    player_2_output <- eventReactive(input$my_player_2, {
      player_photo_2 <- players_df %>% filter(str_player_name == input$playerY) %>% 
        select(int_player_id)
      photo_2 <- paste(player_photo_2[1, 1], ".png", sep = "")
      output$playerYImg <- renderText({
        div(tags$img(src = photo_2(), width = 140, height = 200))
      })
    })
    
    playR_1 <- ready_player(data = players_df, player_Name = input$PlayerX)
    playR_2 <- ready_player(data = players_df, player_Name = input$PlayerY)
    output$playerXDetails <- renderDT(playR_1())
    output$playerYDetails <- renderDT(playR_2())
    
    # Code for improved Model tab
    
    modelImprovedOutput <- eventReactive(input$my_model_2, {
      improved_model_analysis(position = input$positionX, model = input$ModelX)
    })
    
    output$modelAnalysis <- renderDT(modelImprovedOutput())
    
    
  })
)

shinyApp(ui = ui, server = server)




