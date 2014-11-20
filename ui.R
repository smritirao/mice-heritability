library(shiny)
library(heatmap)

# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
  
  # Application title
  titlePanel("Mice Heritability Visualized by Covariance Matrices"),
  
    # Heatmap by Cross
    sidebarPanel(
    	selectInput("variable","Choose a Cross:", 
                  list("All"="crosses",
                       "13140x3015" = "13140x3015",
                       "15156x1566"="15156x1566",
                       "15156x3252"="15156x3252",
                       "16188x3252"="16188x3252",
                       "16188x8005" ="16188x8005",
                       "16211x13140" = "16211x13140",
                       "16441x8005"="16441x8005",
                       "16513x15156" = "16513x15156",
                       "16513x16188"="16513x16188",
                       "16912x16211"="16912x16211",
                       "18042x3032"="18042x3032",
                       "3032x16188"="3032x16188",
                       "3032x16441"="3032x16441",
                       "3154x3609"="3154x3609",
                       "3252x8002"="3252x8002",
                       "3252x8042" = "3252x8042",
                       "3609x5119"="3609x5119",
                       "3609x5489"="3609x5489",
                       "5119x8018"="5119x8018",
                       "5489x16557"="5489x16557",
                       "8002x3032"="8002x3032", 
                       "8004x8043"="8004x8043",
                       "8005x8002"="8005x8002",
                       "8010x16441"="8010x16441",
                       "8016x8034"="8016x8034",
                       "8026x5080"="8026x5080",
                       "8042x8008"="8042x8008", 
                       "8043x8008"="8043x8008", 
                       "8048x8036"="8048x8036"))),
    
    mainPanel(
    	plotOutput("legend",width=800,height=120),
      heatmapOutput('heatmap')
    )
))