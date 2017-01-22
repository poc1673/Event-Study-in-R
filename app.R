
library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(broom)
library(quantmod)

options(digits=2)
raw_data <- read.table(file = "Oil_Price_Data.csv",sep = ",",fill = TRUE,header = TRUE)
raw_data$Date<- ymd(raw_data$Date)
Name_Vec <- names(raw_data)[2:length(names(raw_data))]

pct_calc <- function(X ) { X/lag(X)-1}

Gen_CP_Returns <-function(ticker){
  getSymbols(ticker)
  Company_Data <-as.data.frame( eval(parse(text=ticker)))
  names(Company_Data) <- c("Open","High","Low","Close","Volume","Adjusted")
  Company_Data <- Company_Data %>%  mutate(Return = pct_calc(Adjusted))
  return(Company_Data)}

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
           # Application title
         titlePanel("Market Model Dashboard"),
         fluidRow(
           column(2,
                  dateInput("Date1",
                              "Train Period Start",value = min(raw_data$Date),
                                raw_data$Date )),
           column(2,
                  dateInput("Date2",
                              "Train Period End", value = raw_data$Date[100],
                              raw_data$Date)),
           column(2,textInput("Company",  "Company Ticker",value ="XOM")),
         
           column(2,textInput("MKT","Industry Index",value ="GSPC")),
           
           column(2,textInput("IND","Industry Index",value ="BP"))
         ),
        fluidRow(
           displayrange <- 
           column(2,
                  dateInput("Date3",
                              "Display Start",
                              raw_data$Date[101:length(raw_data$Date)])),
           column(2,
                  dateInput("Date4",
                              "Display End",
                                 raw_data$Date[200:length(raw_data$Date)])),
     
           column(2,
                  selectInput("Tmin",
                              "Minimum T-Value",selected = 0,
                              seq(from=0, to = 10,by = .25)  ),offset = 1),
           column(2,
                  selectInput("Tmax",
                              "Maximum T-Value",selected = 10,
                              seq(from=0, to = 10,by = .25)  ))
         ),
   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        verbatimTextOutput("test"),
        plotOutput("plotting")
  ),
      # Show a plot of the generated distribution
      mainPanel(
        fluidRow(   
          column(
        dataTableOutput("table"),width = 12
        ) )
      )
   )

))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
# Take the training period specified by the user and train the market model regression:

  Date1 <-reactive( {input$Date1} )
  Date2 <- reactive({input$Date2})
  
  
  str_eval <- function(val){ eval(parse(text=val))}
  
  import_quantmod <- function(syms)
  {
    getSymbols(Symbols = syms,src = "yahoo")
    stock_data <- as.data.frame(str_eval(syms))
    stock_data$Date <- rownames(stock_data)
    return(stock_data)
  }
  
 Set_Model<- reactive({
  getSymbols(Company)
  hold <- eval(parse(text=Company))
  Company <- Gen_CP_Returns(input$Company)
 Market <- Gen_CP_Returns(input$MKT)
 Industry <- Gen_CP_Returns(input$IND)
  
  company_index <- match( input$Company,names(raw_data)   )
                       
                     industry_index <-match( input$IND,names(raw_data)   )
                     market_index <-match( input$MKT,names(raw_data)   )
                     train_data <-raw_data %>%  filter(Date >= Date1()& Date <= Date2()  )
                        hold<-as.data.frame(train_data)
                        test <- hold$Date
                        train_data[,industry_index] <-  train_data[,industry_index] -train_data[,market_index]
                        train_model <- lm( data = hold ,  train_data[,company_index]~train_data[,market_index]+  train_data[,industry_index]       )
                        names(train_model$coefficients) <- c("Intercept",input$MKT ,input$IND )
                        return(train_model)
  })

 Event_study <-  reactive({
                          train_model <- isolate(Set_Model())

                          SE<-summary(train_model)$sigma
                           test_data <- raw_data %>%  filter(Date >= Date2() )  %>% select_(input$Company,input$MKT,input$IND )
                           
                           test_data <- cbind(raw_data$Date[raw_data$Date >= Date2()],test_data)
                           
                           factor_indexes <- c(    match(input$MKT, names(test_data)),
                                                   match(input$IND, names(test_data))     )
                           
                           
                           Predicted_Values <- train_model$coefficients[1] + train_model$coefficients[2]*test_data[, factor_indexes[1]] + train_model$coefficients[3]*test_data[,factor_indexes[2]]
                          test_data<- cbind(test_data,Predicted_Values )
                          test_data$Excess_Returns <- test_data[,2] - test_data[,5]
                          
                          test_data$T <- test_data$Excess_Returns/SE

                          names(test_data)[1] <- "Date"
                           test_data <- test_data %>% rename(          "Predicted Returns" = Predicted_Values, "Excess Returns" = Excess_Returns, "T Stat" = T) %>% 
                                                      filter(Date>=input$Date3 &  Date <= input$Date4)
                           return(test_data)
                           
 })

   output$test <- renderPrint({ 
     return_model <- tidy(Set_Model())
     
    reg_summary <- summary(Set_Model())
     
      measure_names <- c("SE","Adj. R-Squared","R-Squared","F-Statistic")
     measure_vars <-  t( as.data.frame(c(  reg_summary$sigma,       reg_summary$adj.r.squared   ,  reg_summary$r.squared   ,reg_summary$fstatistic[1]  )))   
     rownames(measure_vars)<-1
     colnames(measure_vars)<-measure_names
     
    names(return_model) <- c("Factors", "Coefficient", "SE", "T-Stat", "P Value")
    print(return_model)
    print("")
    print(measure_vars)

    
    })

      output$plotting <-  renderPlot({   
      calc_WR <- function(X){
        WR <- 1
        for(i in 2:length(X)){
            WR[i] <- WR[i-1]*(1+X[i])
        }
        return(WR)
      }
      
      hold <- Event_study()
      hold <- hold %>% mutate(WR_Predicted = calc_WR(`Predicted Returns`) ) %>% 
                       mutate(WR_Actual = calc_WR(`Predicted Returns`+`Excess Returns`) ) 
      
      WR_data  <- hold %>% select(Date,WR_Predicted,WR_Actual)
      WR_data <- melt(WR_data, id.vars = "Date")
      ggplot(    data = WR_data  )+geom_line(aes(y = value,x = Date,group = variable,colour = variable))+
                                   ggtitle("Predicted and Actual Performance of $1")+xlab("") + ylab("")+
        scale_fill_continuous(guide = guide_legend()) +
        theme(legend.position="bottom") + scale_colour_discrete(name  ="",
                                                                breaks=c("WR_Predicted", "WR_Actual"),
                                                                labels=c("Predicted Value", "Actual Value"))
    })
      
      output$table <- renderDataTable( Event_study() %>% filter(`T Stat`>=input$Tmin & `T Stat`<=input$Tmax) , options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 10  ))
      
})
    
# Run the application 
shinyApp(ui = ui, server = server)

