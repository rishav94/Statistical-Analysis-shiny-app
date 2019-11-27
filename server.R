
library(shiny)
library(pdfetch)
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  # reading data set for prob tab
    File_Data<-reactive({
       file<- input$Prob_Data1
       if(is.null(file)) {
         return(NULL)
       }
       data =read.csv(file = file$datapath)
       data
                       }) #Prob_Data
    
    # This is to render the column names of dynamic data in drop down menu option probability tab
    observe({
      updateSelectInput(session,"column", choices = colnames(File_Data()))
    })#observe
    
    
    # this is to render the dynamic data in probability tab
    output$Extdata1= DT::renderDataTable({
              Extdatax<-File_Data()
               DT::datatable(Extdatax,options=list(lengthChange= TRUE))
                             }) #render datatable 
    # pd<-na.omit(File_Data) 
    # output$prob <- renderPrint({ 
    # 
    #         print(paste('Selected Column :',input$columns)) 
    #   
    #   
    #   px <- pd[,input$columns] 
    
       output$des<-reactive({df<-File_Data()
       clm<-df[,input$column]
     mean(clm)
      
       })
       output$graph <- renderPlot({ 
        #  print(paste('Selected Column :',input$column)) 
        # 
        # df <- File_Data() 
        # 
        # clm <- df[,input$column] 
        
                       
       if( input$Prob_model=='normal')
           {
             par(mfrow=c(1,2))  
             
             x=seq(-input$i,input$i,0.01)  
             
             plot(x,dnorm(input$j1,input$mu,input$sigma),type='l', col='red',ylab="")  
            
           }
       
        if( input$Prob_model=='exponential')
            {
              par(mfrow=c(1,2)) 
              
              x=seq(0,input$i,0.01)  
              
              plot(x,dexp(x,input$lam),type='l',col='green')
            }
        if( input$Prob_model=='uniform')
                {
                  a <- input$a 
                  
                  b <- input$b 
                  
                  n1 <- input$s 
                  
                  rand.unif <- runif(n1, min = a, max = b) 
                  
                  hist(rand.unif,  
                       
                       freq = FALSE,  
                       
                       xlab = 'x',   
                       
                       ylim = c(0, 0.4), 
                       
                       xlim = c(-3,3), 
                       
                       density = 20, 
                       
                       main = "Uniform distribution") 
                  
                  
                  curve(dunif(x, min = a, max = b),  
                        
                        from = -3, to = 3,  
                        
                        n = n1,  
                        
                        col = "darkblue",  
                        
                        lwd = 2,  
                        
                        add = TRUE,  
                        
                        yaxt = "n", 
                        
                        ylab = 'probability') 
                }
            })#output$prob
       
       output$prt <- renderPrint({ df<-File_Data()
       clm<-df[,input$column]
        
         
         Normal=rnorm(input$s,mean(clm),sd(clm))  
         
         Exp=rexp(input$s,1/mean(clm))  
        
         if (input$Prob_model == 'exponential') { 
           
           print(Exp) 
        
         } 
         
         else 
           
         { 
           print(Normal) 
          } 
     
       }) 
       
   #reading data set for hypotheses test
    File_Data2<-reactive({
      file2<- input$Hypothesis_Data
      if(is.null(file2)) {
        return(NULL)
      }
      data2 =read.csv(file = file2$datapath)
      data2
                         }) #hyp_data
    
     #this is to render the data for hypotheses test
     output$Extdata2= DT::renderDataTable({
       Extdatax2<-File_Data2()
       DT::datatable(Extdatax2,options=list(lengthChange= TRUE))
     }) #render datatable 
     pd2<-na.omit(File_Data2)
      
  
    
})# Server Function
