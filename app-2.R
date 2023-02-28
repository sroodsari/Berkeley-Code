# ===============================================
# Fill in the following fields
# ===============================================
# Title:
# Description:
# Author: 
# Date:


# ===============================================
# Required packages
# ===============================================
library(tidyverse)
library(shiny)
# ...
#install.packages('rsconnect')


# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("4% Rule"),
  fluidRow(
    # Inputs for initial portfolio, retirement age, and withdrawal rate
    column(3,sliderInput("initial", "Initial Portfolio Amount",0, 99999999, 1000000)
           #h4("Initial Portfolio Amount") 
           ,sliderInput("age", "Retirement Age",0, 100, 60),
           sliderInput("withdrawal", "Withdrawal Rate (%)", 0, 100, 4,.1 )
    ),

    
    # Inputs for mean and standard deviation of annual return rates
    column(3, sliderInput("avgReturn", "Average Annual Rate of Return (%)", 0, 100, 10, .1 ),
           sliderInput("returnVol", "Average return volatility (%)", 0, 100, 18, .1 )
    ),
    
    # Inputs for mean and standard deviation of annual inflation rates
    column(3,sliderInput("avgInflation", "Average Inflation Rate(%)", 0, 100, 3, .1 ),
           sliderInput("inflationVol", "Average inflation volatility (%)", 0, 100, 3.5 ,.1)
           
    ),
    
    # Inputs for number of simulations, and random seed
    column(3,
           sliderInput("sim", "number of simulations", 0, 1000, 50 ),
           sliderInput("seed", "Value of Random Seed", 0, 999999, 12345 )
    )
  ),
  
  hr(),
  h4('Timelines'),
  plotOutput('plot'),
  
  hr(),
  h4('Stats'),
  verbatimTextOutput('table')
)



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {

  # you may need to create reactive objects
  # (e.g. data frame to be used for graphing purposes)
  dat <- reactive({
    sims = c()
    year = 100 - input$age
    set.seed(input$seed)
    returnRate = 0
    inflation = 0
    for(i in 1:input$sim){
      initial = input$initial
      vect = c(initial)
      for(j in 1:year){
        withdrawal = initial*((input$withdrawal/100)*(1+inflation/100))
        initial = (initial*(1+returnRate/100))-withdrawal
        returnRate = rnorm(1, input$avgReturn, input$returnVol)
        inflationRate = rnorm(1, input$avgInflation, input$inflationVol)
        vect = c(vect, initial)
      }
      sims = cbind(sims, vect)
    }
    
    timeline = 0:year
    colnames(sims) = paste0("simulation", 1:input$sim)
    sims = cbind(sims, timeline)
    dat = as.data.frame(sims)
    data1 = pivot_longer(dat,cols = starts_with("sim"), names_to = c("simulation"), values_to ="amount")
    data1%>%
      head(- as.numeric(input$sim))
  })
  
  
  # code for graph
  # (e.g. reactive data frame used for graphing purposes)
  output$plot <- renderPlot({
    q = dat() %>%
      group_by(timeline)%>%
      summarize(median = median(amount))
    tenPer = dat()%>%
      group_by(timeline)%>%
      summarize(tenthP= quantile(amount, probs = .1))
    ninetyPer = dat()%>%
      group_by(timeline)%>%
      summarize(ninetyP = quantile(amount,probs = .9))
    # replace the code below with your code!!!
    ggplot(data.frame()) + 
      geom_line(dat(), mapping = aes(x = as.numeric(timeline), y = amount, group = simulation), alpha = .8) +
      geom_line(q, mapping = aes(x = timeline, y = median, color = "50 Percentile"), size = 1) + 
      geom_line(tenPer, mapping = aes(x = timeline, y = tenthP, color = "10 Percentile"), size = 1)+
      geom_line(ninetyPer, mapping = aes(x = timeline, y = ninetyP, color = "90 Percentile"), size = 1, show.legend = TRUE)+
      geom_hline(yintercept = 0, color = "black", size = .5) +
      geom_vline(xintercept = 0, color = "black", size = .5) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = c("10 Percentile" = "green", "50 Percentile" = "red", "90 Percentile" = "blue"))+
      labs(y = "Portfolio Balance", x = "years of retirement", color = "Percentiles")
  })
  
  
  # code for statistics
  output$table <- renderPrint({
    # replace the code below with your code!!!
    
    summary(dat()$amount) 
  
  })

}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

