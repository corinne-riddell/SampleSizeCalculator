library(shiny)

ui <- fluidPage(
   withMathJax(),
   titlePanel("Calculate sample size"),
   
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "model", label = "Type of model", 
                     choices = c("Random-intercepts model", "Random-intercepts and random-slope model")),
         numericInput(inputId = "slope", label = paste0("Magnitude of slope we'd like to detect (", "\\(\\beta \\)", "):") , value = 1), #paste0("Magnitude of slope we wish to detect(", 
         numericInput(inputId = "var_within", label = paste0("Within-subject variance (", '\\( \\sigma^2_{residual} \\)', "):"), value = 1),
         conditionalPanel(condition = "input.model == 'Random-intercepts and random-slope model'",
                          numericInput(inputId = "var_sub", 
                                       label = paste0("Variance subject-specific slopes(", '\\( \\sigma^2_{slopes} \\)', "):"), 
                                       value = 1)),
         numericInput(inputId = "num_within", label = "Number of within-subject measurements (m):", value = 10),
         numericInput(inputId = "mean_sq_dist", label = "Mean squared distance between subject's Xs and their mean (MS_x):", value = 1),
         sliderInput(inputId = "type1error", label = "type I error rate:", value = 0.05, min = 0.001, max = 0.999),
         sliderInput(inputId = "power", label = "Power (%):", value = .80, min = 0.1, max = 0.999)
      ),
      
      mainPanel(
         uiOutput("sampleSize")
      )
   )
)

server <- function(input, output) {
  
  sample.size <- reactive({
    if(input$model == "Random-intercepts model"){
      #formula for random intercepts model
      n <- ((qnorm(p = (1-input$type1error/2)) + qnorm(input$power))^2/input$slope^2)*(input$var_within/input$num_within*input$mean_sq_dist)
    } else{
      #formula for random intercepts and slope model
      n <- ((qnorm(p = (1-input$type1error/2)) + qnorm(input$power))^2/input$slope^2)*(input$var_sub + (input$var_within/input$num_within*input$mean_sq_dist))
    }
    
  }) 
  
   output$sampleSize <- renderUI({
     HTML(paste0("The sample size required is: ", sample.size()))

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

