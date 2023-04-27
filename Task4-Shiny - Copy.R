library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(caret)
library(tidyverse)
library(naniar)
library(MASS)

library(shinythemes)
library(shinyWidgets)

loan_data<-read_csv("https://media.githubusercontent.com/media/pk00787/stat656700/0d6bb7cb03bf4a1fe9d0ec44c3f36471a288a395/lending_club_loan_data.csv")
loan_data<-data.frame(loan_data)
ld<-subset(loan_data, select = -c(grade, sub_grade, emp_title,purpose, title, initial_list_status, emp_length,
                                  address, issue_d,earliest_cr_line))
figbefore<- gg_miss_var(ld)

ld$mort_acc[is.na(ld$mort_acc)]<-ifelse(ld$home_ownership=='MORTGAGE',1,0)
ld$revol_util[is.na(ld$revol_util)]<-mean(ld$revol_util,na.rm=TRUE)
ld$pub_rec_bankruptcies[is.na(ld$pub_rec_bankruptcies)]<-mean(ld$pub_rec_bankruptcies,na.rm=TRUE)

figafter<-gg_miss_var(ld)

ld$mort_acc[is.na(ld$mort_acc)]<-ifelse(ld$home_ownership=='MORTGAGE',1,0)
ld$revol_util[is.na(ld$revol_util)]<-mean(ld$revol_util,na.rm=TRUE)
ld$pub_rec_bankruptcies[is.na(ld$pub_rec_bankruptcies)]<-mean(ld$pub_rec_bankruptcies,na.rm=TRUE)

MS3 <- ld[, c(1:17)]


# Define UI --------------------------------------------------------------------

ui <- fluidPage(
    #shinythemes::themeSelector(),
    theme=shinythemes::shinytheme("simplex"),
    br(),
    navbarPage(title="Lending Club: Exploratory Data Analysis",
              
                        sidebarLayout( 
                            radioButtons("PlotChoice", "Displayed Plot", 
                                         choices = "Boxplot", 
                                         selected = "Boxplot"),
                            
                            # Inputs: Select variables to plot
                            sidebarPanel(width = 4,
                                # Select variable for y-axis
                                selectInput(
                                    inputId = "y",
                                    label = "Variables",
                                    choices = c("loan amount" ="loan_amnt", 
                                                "installment" = "installment",
                                                "interst rate" = "int_rate",
                                                "annual income"="annual_inc",
                                                "open account"="open_acc",
                                                "revolving balance"= "revol_bal",
                                                "revolving utilization"="revol_util"),
                                    selected = "installment"
                                ),
                                # Select variable for x-axis
                                selectInput(
                                    inputId = "x",
                                    label = "loan status",
                                    choices = c("loan status" = "loan_status"),
                                    selected = "loan  status"
                                ),
                                
                                selectInput(
                                    inputId = "z",
                                    label = "Colour by:",
                                    choices = c("loan status" ="loan_status"),
                                    selected = "loan_status")
                                )),
                            # Output: Show plot
                            mainPanel(fluidRow(
                                plotlyOutput(outputId = "SelectedPlot", width = "auto", height="auto")),
   
                                br())),
    
    navbarPage(title="Missing Data: Exploratory Data Analysis",
               tabPanel("Missing Data-Before Imputation",
                        Bef<-ggplotly(figbefore)
                        
               ),
               
               tabPanel("Missing Data-After Imputation",
                        aft<-ggplotly(figafter),
                        )
               
    ))

    




# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
    
    your_plot <- reactive(if (input$PlotChoice == "Boxplot"){
        
       plot_ly(ld, x=ld[,input$x], y=ld[,input$y], color=ld[,input$z],
                type = "box") %>% 
        layout(xaxis = list(title = "Loan Status"), yaxis = list(title = ~input$y),
               legend=list(title=list(text='Loan Status')), title = 'Exploratory Data Analysis')
    }
      
      )
    
    output$SelectedPlot <- renderPlotly({ 
        
        your_plot()
    })
    
    
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)

