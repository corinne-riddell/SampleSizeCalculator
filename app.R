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
  titlePanel(title=div(img(src='mcgill_logo.jpg', align = "right"), "Sample Size Calculator for Panel Studies with Repeated Measurements and Continuous Outcomes")),
  
  tabsetPanel(
    tabPanel("Overview",
             uiOutput("abstract")),
    tabPanel("Calculator",
             #fluidRow(
             #column(6,
             selectInput(inputId = "model", label = "Choose your model type:", 
                         choices = c("Random-intercepts model", "Random-intercepts and random-slope model")),
             #),
            # column(6,
             #       checkboxInput(inputId = "see_ex", label = "Show me an example from environmental science.")
            #        )
             #),
             #hr(),
             uiOutput("formula"),
             checkboxInput(inputId = "see_ex", label = "Show me an example."),
             uiOutput("conditional_example"),
             hr(),
             fluidRow(
               column(4,
                      HTML(paste0("<b/>How many measurements, (m), do you have for each unit/individual?</b>")),
                      numericInput(inputId = "num_within", 
                                   label = NULL, 
                                   value = 10, width = '80px'),
                      tableOutput("fakeData")
               ),
               
               column(4, 
                      HTML(paste0("<b/>What minimal slope, (", "\\beta", "), would you like to detect?</b>")),
                      numericInput(inputId = "slope", 
                                   label = NULL, 
                                   value = 1, width = '100px'),
                      plotOutput("slopeGraph", width = "200px", height = "200px")
               ),
               column(4,
                      HTML("<b>What is the underlying mean $X$?</b>"),
                      numericInput(inputId = "mean_X",
                                   label = NULL,
                                   value = 10, width = '80px'),
                      HTML(paste0("<b/>What is the average squared distance between each subject's $X$'s and their mean, (MS_x)?</b>")),
                      numericInput(inputId = "mean_sq_dist", 
                                   label = NULL, 
                                   value = 1, width = '80px'),
                      uiOutput("spreadPlot")#,
                      #uiOutput("describe.spread")
               )
             ),
             hr(),
             fluidRow(
               column(4, 
                      HTML(paste0("<b/>What is the within-subject variance (", "(\\sigma^2_{residual})", ")?</b>")),
                      numericInput(inputId = "var_within", label = NULL, value = 1, width = '80px')
               ),
               column(4, 
                      conditionalPanel(condition = "input.model == 'Random-intercepts and random-slope model'",
                                       HTML(paste0("<b>What is the variance of subject-specific slopes (", '(\\sigma^2_{slopes})', "):</b>")),
                                       numericInput(inputId = "var_sub",
                                                    label = NULL,
                                                    value = 1, width = '100px'))),
               column(4, 
                      HTML(paste0("<b/>Try adjusting the type I error rate and statistical power to see how this affects the required sample size:</b>")),
                      sliderInput(inputId = "type1error", label = "Type I error rate ((\\%)):", value = 0.05, min = 0.001, max = 0.999),
                      sliderInput(inputId = "power", label = "Power ((\\%)):", value = .80, min = 0.1, max = 0.999))
             ),
             hr(),
             uiOutput("sampleSize")
    )
  
  
  
  #numericInput(inputId = "var_within", label = paste0("Within-subject variance (", '\\sigma^2_{residual}', "):"), value = 1),
  
  )
  
)

server <- function(input, output, session) {
  
  observe({
    if(input$model == "Random-intercepts model"){
      if(input$see_ex == T){
        num_within_ex <- 3
        slope_ex <- -0.0025
        mean_sq_dist_ex <- 500
        var_within_ex <- 0.048
        
      } else{
        num_within_ex <- 10
        slope_ex <- 1
        mean_sq_dist_ex <- 1
        var_within_ex <- 1
      }
      
      updateNumericInput(session, "num_within", value = num_within_ex)
      updateNumericInput(session, "slope", value = slope_ex)
      updateNumericInput(session, "mean_sq_dist", value = mean_sq_dist_ex)
      updateNumericInput(session, "var_within", value = var_within_ex)
    }else{
      if(input$see_ex == T){
        num_within_ex <- 3
        slope_ex <- -0.0025
        mean_sq_dist_ex <- 500
        var_within_ex <- 0.048
        var_sub_ex <- 0.0001
        
      } else{
        num_within_ex <- 10
        slope_ex <- 1
        mean_sq_dist_ex <- 1
        var_within_ex <- 1
        var_sub_ex <- 1
      }
      
      updateNumericInput(session, "num_within", value = num_within_ex)
      updateNumericInput(session, "slope", value = slope_ex)
      updateNumericInput(session, "mean_sq_dist", value = mean_sq_dist_ex)
      updateNumericInput(session, "var_within", value = var_within_ex)    
      updateNumericInput(session, "var_sub", value = var_sub_ex) 
    }
  
    })
  
  
   output$abstract <- renderUI({
     HTML(
     paste0("</br></br><b>Motivation</b></br>",
            "Panel study designs are common in environmental epidemiology whereby repeated measurements are collected",
            " from a panel of subjects to evaluate short-term within-subject changes in response variables over time.", 
            " Examples include studies with repeated measurements of air pollution exposure and blood pressure or lung function.<br/><br/>",
            " In planning such studies, questions of how many subjects to include and how many different exposure conditions",
            " to measure are commonly asked at the design stage. In practice, these choices are constrained by budget, ",
            "logistics, and participant burden, and must be carefully balanced against statistical considerations of precision",
            " and power.",
            "</br></br><b>Sample Size Calculator</b></br>", 
            "This sample size calculator implements two formulae to provide sample size estimates for panel studies with repeated within-subject measurements.",
            " A detailed description of these formulae can be found in our published manuscript (Forthcoming in <i>Epidemiology</i>).",
           "</b></br>",
           "</b></br>",
            " To use the calculator, you will need to provide information for several inputs:<br/>",
            " <b>1)</b> The number of measurements per subject;<br/>",
            "<b>2)</b> The slope you wish to detect, i.e. how much do you expect the response to change per unit change in exposure?;<br/>",
            " <b>3)</b> The variance of subject-specific slopes, if a random-slope model is used;<br/>",
            " <b>4)</b> The residual variance of measured responses within-subjects;<br/>",
            " <b>5)</b> The within-subject range of the exposure values “X” at which the responses are measured;",
           "</br></br><b>About Us</b></br>",
           "This sample size calculator accompanies the forthcoming manuscript written by <a href='http://scottweichenthal.weebly.com/people.html'>Scott Weichenthal</a>,",
           " <a href='http://jillbaumgartner.weebly.com/'>Jill Baumgartner</a>, and <a href='http://www.medicine.mcgill.ca/epidemiology/hanley/'>James Hanley</a>",
           " of the McGill <a href = 'https://www.mcgill.ca/epi-biostat-occh/'>Department of Epidemiology, Biostatistics, and Occupational Health</a>.",
           " This online sample-size calculator was developed by <a href='corinne-riddell.github.io'>Corinne Riddell</a>, a postdoctoral researcher in the Department."
           ))
     })
   
   output$formula<- renderUI({
     type1err.str <- as.character(round(qnorm(p = (1-input$type1error/2)), 2))
     power.str <- as.character(round(qnorm(p = input$power), 2))
    withMathJax(helpText("The formula for a ", input$model, " is:", 
                         ifelse(input$model == "Random-intercepts model", 
                                paste0("$$n = \\frac{(", type1err.str, " +", power.str, ")^2}{\\beta^2}\\cdot\\frac{\\sigma^2_{residual}}{m\\times{MS_x}}$$"),
                                paste0("$$n = \\frac{(", type1err.str, " +", power.str, ")^2}{\\beta^2}\\cdot\\Big({\\sigma^2_{slopes} + \\frac{\\sigma^2_{residual}}{m\\times{MS_x}}}\\Big)$$")),
                        "where $n$ is the number of subjects,", 
                        "$\\beta$ is the magnitude of the slope we wish to detect, 
                        $\\sigma^2_{residual}$ is the within-subject variance of the response measure",
                        ifelse(input$model == "Random-intercepts model", 
                               "", 
                               "$\\sigma^2_{slopes}$ is the variance of subject-specific slopes,"), 
                        "$m$ is the number of within-subject measurements, and",
                        "$MS_x$ is the mean squared distance betwen the subject's $X$'s and their mean.",
                        "Below, set each of the required parameters to calculate the required sample size."))
     # HTML(paste0("The sample size required is: ", sample.size()))

   })
   
   output$conditional_example <- renderUI({
     if(input$see_ex){
       if(input$model == "Random-intercepts model"){
       HTML(paste0("Consider a hypothetical study of exposure to ultra-fine particle (UFP) and changes in the reactive hyperemia index (RHI).",
                   " Assume that we could take three measurements per person, and that we would like to detect a slope of -0.0025 per 1000 UFPs.",
                   " Also assume that the average squared distance between each person's three measurements and <i>their</i> mean measurement is ",
                   " 500. Lastly, assume that the residual variance is 0.219<sup>2</sup> which is approximately equal to 0.048. Then, we would need to have",
                   " a sample size of 40 people to detect the indicated slope with a type I error rate of 95% and 80% power."))
       }else{
         HTML(paste0("Consider a hypothetical study of exposure to ultra-fine particle (UFP) and changes in the reactive hyperemia index (RHI).",
                     " Assume that we could take three measurements per person, and that we would like to detect a slope of -0.0025 per 1000 UFPs.",
                     " Also assume that the average squared distance between each person's three measurements and <i>their</i> mean measurement is ",
                     " 500, and that the residual variance is 0.219<sup>2</sup> which is approximately equal to 0.048.",
                     " Finally, assume that the variance between the subject-specific slopes is 0.0001. Then, we would need to have",
                     " a sample size of 166 people to detect the indicated slope with a type I error rate of 95% and 80% power."))
       }
     }
         
   })
   
   output$fakeData = renderTable({
     
     if(input$num_within <= 5){
       unit <- rep("A", input$num_within)
       measure <- 1:input$num_within
     } else{
       unit <- rep("A", 5)
       measure <- c(1:3, "...", input$num_within)
     }
     
     return(data.frame(unit, measure))
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
     
     if(input$slope > 0 & input$slope <= 1){ #slope is a positive fraction or 1
       plot <- plot + scale_y_continuous(limits = c(0, 1)) 
     } else if(input$slope > 1){ #slope larger than 1
       plot <- plot + scale_y_continuous(limits = c(0, input$slope)) + scale_x_continuous(limits = c(0, input$slope))
     } else if(input$slope < -1){ #slope smaller than -1
       plot <- plot + scale_y_continuous(limits = c(input$slope, 0)) + scale_x_continuous(limits = c(0, -input$slope))
     } else if (input$slope >= -1 & input$slope <= 0){ #slope is a negative fraction or 0
       plot <- plot + scale_y_continuous(limits = c(-1, 0)) 
     }
     
     return(plot)
   })

   values <- reactive({
     v1 <- rnorm(n = input$num_within, mean = input$mean_X, sd = sqrt(input$mean_sq_dist))
     return(data.frame(v1 = v1, y = rep(0, input$num_within)))
   })
     
   output$spreadPlot <- renderUI({ #set width and height dynamically based on the number measurements within 
     plotOutput("spreadPlot2", width = ifelse(input$num_within >=20, "200px", "250px"), height = ifelse(input$num_within >= 20, "200px", "80px"))
   })
   
   output$spreadPlot2 <- renderPlot({
     if(input$num_within >= 20){
     p2 <- ggplot(data = values(), aes(x = v1)) + 
       geom_histogram(alpha = 0.5, col = "green", fill = "green", bins = input$num_within/5) +
       geom_vline(aes(xintercept = mean(values()$v1)), col = "red") + 
       theme_minimal() + geom_rug(alpha = 0.5) +
       ggtitle("Histogram of subject\nmeasurements") + 
       xlab("measurements (X's)")
     } else {
     p2 <- ggplot(data = values(), aes(x = v1, y = 0)) +
       geom_point(aes(x = mean(values()$v1), y = 0, col = "Sampled mean"), size = 2) +
       geom_point(alpha = 0.5, shape = 4, size = 2) +
       ggtitle("Sample of measurements") + 
       xlab("measurements (X's)") + 
       theme_bw() + ylab("") + theme(legend.title = element_blank()) +
       scale_y_continuous(breaks = NULL)
     }
     
     p2
   })

   # output$describe.spread <- renderUI({
   #   HTML(paste0("The underlying mean is ", input$mean_X, ". The empirical mean is ", round(mean(values()$v1), 1), ".<br/><br/>",
   #               "The average mean distance between the Xi's and ", input$mean_X, " is: ", round(mean((values()$v1 - input$mean_X)^2), 1), ".<br/><br/>",
   #               "The average mean distance between the Xi's and the empirical mean is: ", round(mean((values()$v1 - mean(values()$v1))^2), 1)))
   # 
   # })
   
   sample.size <- reactive({
     if(input$model == "Random-intercepts model"){
       #formula for random intercepts model
       n <- ((qnorm(p = (1-input$type1error/2)) + qnorm(input$power))^2/input$slope^2)*(input$var_within/(input$num_within*input$mean_sq_dist))
     } else{
       #formula for random intercepts and slope model
       n <- ((qnorm(p = (1-input$type1error/2)) + qnorm(input$power))^2/input$slope^2)*(input$var_sub + (input$var_within/(input$num_within*input$mean_sq_dist)))
     }
     
   }) 
   
   output$sampleSize <- renderUI({
     HTML(paste0("<h3>The sample size required is: ", round(sample.size(), 1), "</h1>"))
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

