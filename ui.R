
library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Motor Trend Car Road Tests Data"),
  withMathJax(),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      
      selectInput("outcome", label = h3("Select outcome"), 
                  choices = c("Select"), 
                  selected = "Select"),
      selectInput("exploratoryPred", label = h3("Select predictor for the exploratory analysis"), 
                  choices = c("Select"), 
                  selected = "Select"),
      selectInput("lmPred", label = h3("Select predictors for the linear model"), multiple = TRUE,
                  choices = NULL)
    ),
    
    # Main panel for explanation and data visualization
    mainPanel(
      h2("App Explanation"),
      HTML("This Shiny app allows some interactive exploration of the data contained in the <b>mtcars</b> dataframe.
           In the left panel, you can choose which variable to have as outcome/predictor to perform either exploratory analysis or linear regression.<br><br>
           When an outcome and a predictor for the exploratory analysis are chosen,
           an Exploratory Analysis panel will appear with a plot to show their direct relation.
           If the predictor is a factor, a violin plot will be displayed, otherwise a scatter plot.<br>
           When an outcome and a predictor for the linear model are chosen, a Linear Model panel appears,
           displaying the fitted model's formula and other information that may be valuable to correct the choice of the predictors."),
      conditionalPanel(
        condition = "input.outcome != 'Select' & input.exploratoryPred != 'Select'",
        h2("Exploratory Analysis"),
        plotOutput("violinPlot")
      ),
      uiOutput("lmSummary")
    )
  )
))
