library(DT)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("PenPass model for Atlantic salmon"),

  # Sidebar layout with input and output definitions ----

    # Sidebar panel for inputs ----
  
    sidebarLayout( 
      
      sidebarPanel(
       fluidRow(
         p("To use default values from Stevens et al. (2019), choose 
         a year and click \"Run Model\" without changing any of the 
         default inputs."), 
         br(),
         p("To test new scenarios, enter input parameter values in the tabs 
         below from left to right and then click \"Run Model\"  to view tabular 
         or graphical results. "),
         br(),
         p("Refresh the browser between scenarios to avoid re-running the model
         each time an input value is changed."), 
         br(),
         numericInput(inputId = "n_runs", label = "How many runs per year?",
                      value = 10, step = 10, min = 0
                      ),
         # actionButton("go",label = "Run Model"),
         br(),
         selectInput(inputId = "year", label = "Select year(s) below",
                      seq(1970, 2020, 1), multiple = TRUE
                      ),
         br(),
         actionButton("go",label = "Run Model")
         ),       
       br(), 
       # br(), 
       h3("Input parameters"),
       tabsetPanel(
          tabPanel(title = "Passage rates",
                   DTOutput("passage")
                   ),
          tabPanel(title = "Life history",
                   DTOutput("lifehistory")
                   )          
          )
       ),
      
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Tabular output",
          br(),
          fluidRow(dataTableOutput('model_result')),
          br()
        ),
        tabPanel(
          title = "Plot",
          plotOutput("graph")
        )
      )
    )
  )
  
)