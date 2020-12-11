####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################
# The specific tutorial I used

# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)

#rsconnect::deployApp('https://github.com/ejimjam/SHINY/blob/main/')

# Import libraries

# Read data from original source to get the probability if you will play golf based on the weather condition
weather <- read.csv(text = getURL("https://github.com/ejimjam/SHINY/blob/main/Weather_pain.csv") )

#Read data from Edwin's modified version to predict the probability of Edwin having back and knees ache
#I did not setwd here but just did it to walk through instructions on SHINY that resonated well with me
#weather <- read.csv("Weather_pain.csv", header = TRUE)

fileinputs <- read.csv(text = getURL("https://github.com/ejimjam/SHINY/blob/main/input.csv") )

#This helped clean up the factor that were not included in the original
weather$play = factor(weather$play) 
weather$outlook = factor(weather$outlook) 
 

# Build model
model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE)

# Save model to RDS file
# saveRDS(model, "model.rds")

# Read in the RF model
#model <- readRDS("model.rds")

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("spacelab"),
  
  # Page header
  headerPanel('Will my back and knees ache today?'),
  
  # Input values
  sidebarPanel(
    HTML("<h2>Input parameters</h2>"),
    
    selectInput("outlook", label = "Outlook:", 
                choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"), 
                selected = "Rainy"),
    sliderInput("temperature", "Temperature:",
                min = 64, max = 94,
                value = 70),
    sliderInput("humidity", "Humidity:",
                min = 25, max = 98,
                value = 90),
    selectInput("windy", label = "Windy:", 
                choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                selected = "TRUE"),
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {

  # Input Data
  datasetInput <- reactive({  
    
  # outlook,temperature,humidity,windy,play
  df <- data.frame(
    Name = c("outlook",
             "temperature",
             "humidity",
             "windy"),
    Value = as.character(c(input$outlook,
                           input$temperature,
                           input$humidity,
                           input$windy)),
    stringsAsFactors = FALSE)
  
  play <- "play"
  df <- rbind(df, play)
  input <- transpose(df)
  
  write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
  test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
 # test <- read.csv(fileinputs, header = TRUE)
  
  test$outlook <- factor(test$outlook, levels = c("overcast", "rainy", "sunny"))
  
  
  Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
  print(Output)
  
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
