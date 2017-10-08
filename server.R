library(shiny)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  # preprocessing on the data
  cars <- mtcars
  cars$cyl <- factor(cars$cyl)
  cars$vs <- factor(cars$vs)
  cars$gear <- factor(cars$gear)
  cars$carb <- factor(cars$carb)
  cars$am <- factor(cars$am, levels = c(0,1), labels = c("automatic", "manual"))
  
  # possible todo: have the dataset as selectable input
  dataset <- cars
  updateSelectInput(session, "outcome", choices = c("Select", names(dataset)))
  
  # initialize reactive values to dummy value to avoid errors due to missing inputs
  predictorsList <- reactiveValues(data = NULL)
  chosenOutcome <- reactiveVal("Select")
  chosenExploratoryPred <- reactiveVal("Select")
  chosenLmPred <- reactiveValues(data = character(0))
  
  observeEvent(input$outcome, {
    # update outcome reactive value
    chosenOutcome(input$outcome)
    # reset lm selection
    chosenLmPred$data <- character(0)
    
    # update list of possible predictors excluding the outcome
    if(input$outcome != "Select"){
      varNames <- names(dataset)
      predictorsList$data <- varNames[varNames != input$outcome]
    } else {
      predictorsList$data <- character(0)
    }
    updateSelectInput(session, "exploratoryPred", choices = c("Select", predictorsList$data))
    updateSelectInput(session, "lmPred", choices = predictorsList$data)
  })
  
  observeEvent(input$exploratoryPred, {
    # update exploratory predictor reactive value
    chosenExploratoryPred(input$exploratoryPred)
  })
  
  observeEvent(input$lmPred, {
    # update lm predictor reactive value
    chosenLmPred$data <- input$lmPred
  })
  
  computePlot <- reactive({
    if(chosenOutcome() == "Select" | chosenExploratoryPred() == "Select"){
      # empty plot
      ggplot()
    } else {
      outc <- chosenOutcome()
      pred <- chosenExploratoryPred()
      g <- ggplot(dataset, aes(x = get(pred), y = get(outc)))
      #  plot violin only for factors, scatter otherwise
      if(class(cars[pred][,1]) == "factor"){
        g <- g + 
          geom_violin(aes(fill = get(pred))) +
          labs(fill = "Predictor")
      } else {
        g <- g + geom_point(aes(fill = get(pred))) +
          guides(fill=FALSE)
      }
      g <- g +
        xlab(paste0(pred," (predictor)")) + 
        ylab(paste0(outc," (outcome)"))
      g
    }
  })
  
  
  output$violinPlot <-  renderPlot({
    computePlot()
  }, height = 400, width = 600)
  
  
  output$lmSummary <- renderUI({
    if(chosenOutcome() == "Select" || length(chosenLmPred$data) == 0){
      HTML("")
    } else{
      # create the formula to fit the model
      lmFormula <- paste0(chosenOutcome(), "~", paste(chosenLmPred$data, collapse = "+"))
      model <- lm(lmFormula, data = dataset)
      summ <- summary(model)
      
      # color in red the names of the variables having high P-values
      formattedNames <- sapply(names(coefficients(model)[-1]), function(name){
        # if the corresponding p-value is too big (>0.05)
        ifelse(summ$coefficients[name,4] > 0.05,
               # insert html code to make it red
               paste0("<font color='red'>", name, "</font>"),
               # otherwise just leave it as it is
               name
        )
      })
      
      # format the formula inserting the coefficients
      lmFormula <- paste0(chosenOutcome(), " ~ ", round(coefficients(model)[1],2), "", 
                          paste(sprintf(" %+.2f*%s ", 
                                        coefficients(model)[-1], 
                                        formattedNames), 
                                collapse="")
      )
      
      # display a message when p-values are above 0.05
      warningMessage <- ifelse(grepl("font", lmFormula),
                               "<br>You have predictors with high p-value (marked in red). Maybe you want to try excuding some variable.",
                               ""
      )
      # output
      tagList(
        h2("Linear Model"),
        HTML(paste0("Formula: <b>", lmFormula, "</b>")),
        HTML(warningMessage),
        HTML("<br><br>Coefficients Table:<br>"),
        renderTable(summ$coefficients, digits = 6, rownames = TRUE),
        HTML(paste0("<br>Sigma: ", sprintf(" %.2f", summ$sigma))),
        HTML(paste0("<br>This model explains about", sprintf(" %.0f%% of the outcome's variance.", summ$r.squared*100)))
      )
    }
    
  })
})