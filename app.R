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
  
  
  selectInput(inputId = "model", label = "Choose your model type:", 
              choices = c("Random-intercepts model", "Random-intercepts and random-slope model")),
  uiOutput("formula"),
  fluidRow(
    column(6, HTML(paste0("<b/>How many measurements, (m), do you have for each unit/individual?</b>"))),
    column(2, numericInput(inputId = "num_within", label = NULL, value = 10))
  ),
  fluidRow(
    column(6, HTML(paste0("<b/>What minimal slope, (", "\\beta", "), would you like to detect?</b>"))),
    column(2, numericInput(inputId = "slope", label = NULL, value = 1))    
  ),
  plotOutput("slopeGraph", width = "200px", height = "200px"),
  fluidRow(
    column(8, HTML(paste0("<b/>What is the average squared distance between each subject's $X$'s and their mean, (MS_x)?</b>"))),
    column(2, numericInput(inputId = "mean_sq_dist", label = NULL, value = 1))  
  ),
  plotOutput("spreadPlot", width = "200px", height = "200px"),
  uiOutput("describe.spread"),
  fluidRow(
    column(8, HTML(paste0("<b/>What is the within-subject variance (", "(\\sigma^2_{residual})", ")?</b>"))),
    column(2, numericInput(inputId = "var_within", label = NULL, value = 1))  
  ),
         #numericInput(inputId = "var_within", label = paste0("Within-subject variance (", '\\sigma^2_{residual}', "):"), value = 1),
         conditionalPanel(condition = "input.model == 'Random-intercepts and random-slope model'",
                 numericInput(inputId = "var_sub", 
                              label = paste0("Variance subject-specific slopes(", '\\sigma^2_{slopes}', "):"), 
                              value = 1)),
         HTML(paste0("<b/>Try adjusting the type I error rate and statistical power to see how this affects the required sample size:</b>")),
         sliderInput(inputId = "type1error", label = "type I error rate:", value = 0.05, min = 0.001, max = 0.999),
         sliderInput(inputId = "power", label = "Power (%):", value = .80, min = 0.1, max = 0.999),
  uiOutput("sampleSize")

)

server <- function(input, output) {
  
   output$formula<- renderUI({
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
                        "$MS_x$ is the mean squared distance betwen the subject's $X$'s and their mean.",
                        "Below, we set each of the required parameters to calculate the required sample size."))
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
     } else if(input$slope > 1){#else{
       #plot <- plot + scale_x_continuous(breaks = c(0, 1), labels = c(0, 1))
       plot <- plot + scale_y_continuous(limits = c(0, input$slope)) + scale_x_continuous(limits = c(0, input$slope))
     } else if(input$slope < 0){
       plot <- plot + scale_y_continuous(limits = c(input$slope, 0)) + scale_x_continuous(limits = c(0, -input$slope))
     }
     
     
     return(plot)
   })

   values <- reactive({
     v1 <- rnorm(n = input$num_within, mean = 10, sd = sqrt(input$mean_sq_dist))
     return(data.frame(v1 = v1, y = rep(0, input$num_within)))
   })
     
   output$spreadPlot <- renderPlot({
     #p1 <- ggplot(data = values(), aes(x = v1, y = y)) + geom_point(alpha = 0.5, shape = 4) + geom_point(aes(x = mean(values()$v1), y = 0), col = "red")
     p2 <- ggplot(data = values(), aes(x = v1)) + geom_histogram(alpha = 0.5, col = "green", fill = "green", binwidth = input$mean_sq_dist/2) +
       geom_vline(aes(xintercept = mean(values()$v1)), col = "red") + theme_minimal() + geom_rug(alpha = 0.5)
     p2 + ggtitle("Histogram of subject\nmeasurements") + xlab("measurements (X's)")
     #return(subplot(ggplotly(p1), ggplotly(p2), nrows = 1))
   })
   # 
   # output$describe.spread <- renderUI({
   #   HTML(paste0("The underlying mean is 10. The empirical mean is ", round(mean(values()$v1), 1), ".<br/><br/>",
   #               "The average mean distance between the Xi's and 10 is: ", round(mean((values()$v1 - 10)^2), 1), ".<br/><br/>",
   #               "The average mean distance between the Xi's and the empirical mean is: ", round(mean((values()$v1 - mean(values()$v1))^2), 1)))
   #   
   # })
   
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

