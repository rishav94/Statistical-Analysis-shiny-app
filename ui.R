
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel(HTML("<h1><b>Statistical Analysis")),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          radioButtons("tab_option","Select Option",choices =c( "Probability", "Hypothesis Test","Linear Regression"), selected = "Probability",inline = TRUE),# radio button
                       # condtional panel for Probability model 
                       conditionalPanel(
                         condition="input.tab_option == 'Probability'",
                         fileInput("Prob_Data1","Upload dataset",multiple = FALSE,accept =c("text/csv","text/comma-seprated-values.text/plain",".csv")),
                         selectInput("column","Select Column from Data Set",choices = ""),
                         sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 10), 
                         selectInput("Prob_model","Select Probability Model",choices = c("Normal" = "normal" , "Exponential" = "exponential" , "Uniform" = "uniform")),
                         conditionalPanel(condition = "input.Prob_model == 'exponential'",  numericInput("lam", "parameter lambda in exponential" , value = 1)),    
                         conditionalPanel(condition = "input.Prob_model == 'normal'",numericInput("mu", "parameter mu in Normal" , value = 0), numericInput("sigma", "parameter sigma in Normal" , value = 1)), 
                         numericInput("i", "support" , value = 2), 
                         conditionalPanel(condition = "input.Prob_model == 'normal'", numericInput("j1", "j in Normal" , value = 0) ),  
                         conditionalPanel(condition = "input.Prob_model == 'exponential'",numericInput("j2", "j in exponential" , value = 0) ),    
                         conditionalPanel(condition = "input.Prob_model == 'uniform'",numericInput("a", "parameter a in Normal" , value = -2),numericInput("b", "parameter b in Normal" , value = 0.8)) 
                         
                         #     condition="input.Prob_model=='Discrete'",
                         #     selectInput("Dismod","Select Discrete model", choices = c("Binomial" = "binomial","Multinomial"="multinomial"  , "Poisson" = "poisson", "Geometric" = "geometric" ))
                         #                    ), # conditional menu discrete prob model options                                            
                         #                                                               
                         # conditionalPanel(
                         #     condition="input.Prob_model=='Continous'",
                         #     selectInput("Conmod","Select Continous model", choices = c("Normal" = "normal","Exponential" = "exponential","Uniform" = "uniform")) 
                         # )    # conditionl menu for continous prob model                                                            
                                                                                       
                        
                                 ), # prob condition 
          
                       # conditional panel for Hypthesis test
                       conditionalPanel(
                         condition="input.tab_option == 'Hypothesis Test'",
                         fileInput("Hypothesis_Data","Upload dataset",multiple = FALSE,accept =c("text/csv","text/comma-seprated-values.text/plain",".csv")),
                         selectInput("Hyp_type1","Select Hypothesis Test",choices = c("Mean test","Variance test","Propotion test"))
                         #numericInput("obs","Number of observations to view:",5)              
                                        ),# hypothesis panel
                       conditionalPanel(
                         condition="input.tab_option == 'Linear Regression'",
                         fileInput("Regression_Data","Upload dataset",multiple = FALSE,accept =c("text/csv","text/comma-seprated-values.text/plain",".csv")),
                         selectInput("linear_reg1","Select linear Regression", choices = c("a","b","c"))
                        # numericInput("obs","Number of observations to view:",5)
                                       )# Linear regression
                  ),#slidebar

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
              tabPanel("Probability",h2("Dataset"),DT::dataTableOutput("Extdata1"),h3("Mean:"),verbatimTextOutput("des"),plotOutput("graph"),h3("Predicted Value:"),verbatimTextOutput("prt")),
              tabPanel("Hypothesis Test",h2("Dataset"),DT::dataTableOutput("Extdata2")),
              tabPanel("Linear Regression")
                        )#tabsetpanel
                 )#mainPanel
    )#sidebarlayout
))# shiny Ui and fluid
