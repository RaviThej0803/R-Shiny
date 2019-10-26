library(shiny)
library(ggplot2)
library(sqldf)
library(dplyr)
library(ggpubr)
theme_set(theme_pubr())
my_data_1 <- read.csv("House_Price_data.csv")
subset_relation <- select(my_data_1, LotArea, SalePrice, MSZoning)

ui<- fluidPage(    
  # Give the page a title
  tabsetPanel(type = "tabs",
              tabPanel("Bar Plots", 
  titlePanel("Dynamic Bar Chart of Mutiple Variables"),
  
  
    sidebarPanel(
      
      selectInput("selected_bar", label = "Select Metric for Bar Plot:",
                  choices = c("BsmtFinType1" = 1,
                              "GrLivArea" = 2, # Choices
                              "SaleType" = 3,
                              "SaleCondition" = 4),
                  selected = 1), # Default Selection
          mainPanel(
            h3(textOutput("caption")),
            plotOutput("plot")
  )
          )

  ),
     tabPanel("Line Plot", fluidRow(plotOutput("plot3"))),
  tabPanel("Histogram",
           titlePanel("Histrogram"),
           
           # Sidebar layout with input and output definitions ----
           sidebarLayout(
             # Sidebar panel for inputs ----
             sidebarPanel(
               
               sliderInput(inputId = "bins",
                           label = "Number of bins:",
                           min = 1,
                           max = 100,
                           value = 5)
             ),
             
             
             # Main panel for displaying outputs ----
             mainPanel(
               
               # Output: Histogram ----
               plotOutput(outputId = "HistPlot")
               
             )
           )
  )
    )   
 )     

server<-shinyServer(function(input, output) {
  
  dataframe <- reactive({
    # Fetching Selected Plot
    plotNumber <- as.numeric(input$selected_bar)
    
    if (plotNumber==1) {
      sql="SELECT Neighborhood,count(*) as metric FROM my_data GROUP BY Neighborhood"
    }
    
    if (plotNumber==2) {
      sql="SELECT Neighborhood,GrLivArea as metric FROM my_data GROUP BY Neighborhood"
    }
    
    if (plotNumber==3) {
      sql="SELECT Neighborhood,SaleType as metric FROM my_data GROUP BY Neighborhood"
    }  
    if (plotNumber==4) {
      sql="SELECT Neighborhood,SaleCondition as metric FROM my_data GROUP BY Neighborhood"
    }
    sqldf(sql)  
  }
  )
  
  
  output$plot<-renderPlot({
    ggplot(dataframe(),mapping = aes(x=Neighborhood,y=metric,fill = Neighborhood))+
      geom_bar(stat = 'identity')+
      xlab("Neighborhood")+
      ylab("Metric")
  }, height=1000, width=900)
  
  output$plot3 <- renderPlot({
    df <- subset_relation %>%
      group_by(MSZoning) %>%
      summarise(counts = n())    # Grouping by counts and summarizing it's values
    
    df <- df %>%
      arrange(desc(MSZoning)) %>%
      mutate(prop = round(counts*100/sum(counts), 1),
             lab.ypos = cumsum(prop) - 0.5*prop)
    
    ggplot(df, aes(MSZoning, prop)) +
      geom_linerange(
        aes(x = MSZoning, ymin = 0, ymax = prop), 
        color = "lightgray", size = 1.5
      )+
      geom_point(aes(color = MSZoning), size = 2)+
      ggpubr::color_palette("jco")+ ggtitle("Plot to show the Type of Properties ") +
      theme_pubclean()
    
  },height=900, width=800)
 
    output$HistPlot <- renderPlot({
      x    <- subset_relation$SalePrice
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      hist(x, breaks = bins, col = "yellow", border = "black",
           xlab = "SalePrice",
           main = ' ')
      
    },height=900, width=800)
}
)



shinyApp(ui, server)
