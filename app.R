library(shiny)
library(ggplot2)
library(plotly)


ui <- fluidPage(
   withMathJax(),
   tags$script("
MathJax.Hub.Config({
               tex2jax: {
               inlineMath: [['$','$'], ['\\(','\\)']],
               processEscapes: true
               }
               });"
),
   titlePanel("Calculate sample size"),
   
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "model", label = "First, choose your model type:", 
                     choices = c("Random-intercepts model", "Random-intercepts and random-slope model")),
         numericInput(inputId = "slope", label = paste0("Magnitude of slope we'd like to detect (", "\\beta", "):") , value = 1), #paste0("Magnitude of slope we wish to detect(", 
         numericInput(inputId = "var_within", label = paste0("Within-subject variance (", '\\sigma^2_{residual}', "):"), value = 1),
         conditionalPanel(condition = "input.model == 'Random-intercepts and random-slope model'",
                          numericInput(inputId = "var_sub", 
                                       label = paste0("Variance subject-specific slopes(", '\\sigma^2_{slopes}', "):"), 
                                       value = 1)),
         numericInput(inputId = "num_within", label = "Number of within-subject measurements (m):", value = 10),
         numericInput(inputId = "mean_sq_dist", label = "Mean squared distance between subject's $X$'s and their mean (MS_x):", value = 1),
         sliderInput(inputId = "type1error", label = "type I error rate:", value = 0.05, min = 0.001, max = 0.999),
         sliderInput(inputId = "power", label = "Power (%):", value = .80, min = 0.1, max = 0.999)
      ),
      
      mainPanel(
         uiOutput("sampleSize"),
         plotOutput("slopeGraph", width = "200px", height = "200px"),
         plotlyOutput("spreadPlot"),
         uiOutput("describe.spread")
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
     #withMathJax(helpText(glue('The formula for a {input$model} is:   $$n = \\frac{(1.96 + 0.84)^2}\\beta^2$$')))
    # HTML(glue('The formula for a {input$model} model is',
    #           ' n = (1.96 + 0.84)^2/beta^2'))
    withMathJax(helpText("The formula for a ", input$model, " is:", 
                         ifelse(input$model == "Random-intercepts model", 
                                "$$n = \\frac{(1.96 + 0.84)^2}{\\beta^2}\\cdot\\frac{\\sigma^2_{residual}}{m\\times{MS_x}}$$",
                                "$$n = \\frac{(1.96 + 0.84)^2}{\\beta^2}\\cdot\\Big({\\sigma^2_{slopes} + \\frac{\\sigma^2_{residual}}{m\\times{MS_x}}}\\Big)$$"),
                        "where $n$ is the number of subjects,", 
                        "$\\beta$ is the magnitude of the slope we wish to detect, 
                        $\\sigma^2_{residual}$ is the within-subject variance of the response measure",
                        ifelse(input$model == "Random-intercepts model", 
                               "", 
                               "$\\sigma^2_{slopes}$ is the variance of subject-specific slopes,"), 
                        "$m$ is the number of within-subject measurements, and",
                        "$MS_x$ is the mean squared distance betwen the subject's $X$'s and their mean."))
     # HTML(paste0("The sample size required is: ", sample.size()))

   })
   
   data.slope = reactive({
     data.frame(y = c(0, input$slope), x = c(0, 1))
   })
   
   output$slopeGraph <- renderPlot({
     plot <- ggplot(dat = data.slope(), aes(x = x, y = y)) +
               geom_line(col = "red") +
       geom_area(alpha = 0.5, col = "blue") +
       coord_fixed(ratio = 1) +
       theme_minimal() + ggtitle("Slope to detect")
               #scale_y_continuous(limits = c(0, max(1, input$slope))) + 
               #scale_x_continuous(limits = c(0, max(1, input$slope)))
     
     if(input$slope > 0 & input$slope <= 1){
       plot <- plot + scale_y_continuous(limits = c(0, 1)) 
     } else{
       plot <- plot + scale_x_continuous(breaks = c(0, 1), labels = c(0, 1))
     }
     
     
     return(plot)
   })

   values <- reactive({
     v1 <- rnorm(n = input$num_within, mean = 10, sd = sqrt(input$mean_sq_dist))
     return(data.frame(v1 = v1, y = rep(0, input$num_within)))
   })
     
   output$spreadPlot <- renderPlotly({
     p1 <- ggplot(data = values(), aes(x = v1, y = y)) + geom_point(alpha = 0.5, shape = 4) + geom_point(aes(x = mean(values()$v1), y = 0), col = "red")
     p2 <- ggplot(data = values(), aes(x = v1)) + geom_histogram(alpha = 0.5, col = "green", fill = "green", binwidth = input$mean_sq_dist/2)
     return(subplot(ggplotly(p1), ggplotly(p2), nrows = 2))
   })
   
   output$describe.spread <- renderUI({
     HTML(paste0("The underlying mean is 10. The empirical mean is ", round(mean(values()$v1), 1), ".<br/><br/>",
                 "The average mean distance between the Xi's and 10 is: ", round(mean((values()$v1 - 10)^2), 1), ".<br/><br/>",
                 "The average mean distance between the Xi's and the empirical mean is: ", round(mean((values()$v1 - mean(values()$v1))^2), 1)))
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

