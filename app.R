# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)

udemy <- read.csv('D:/sem5/Data Visualization/DV-Project/dvproj/udemy_visualisation.csv')

head(udemy)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Analysis Dashboard")  



#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
             href = "https://www.salesforce.com")
  )
)



frow1 <- fluidRow(

  box(
    title = "Number of Subscribers in each Subject"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("subsribersBysubjects", height = "300px")
  )
  
  ,box(
    title = "Number of Subscribers by levels"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("subscribersBylevels", height = "300px")
  ) 
  
)

frow2 <- fluidRow(

  box(
    title = "Count of courses of each subject"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotlyOutput("CoursesEachSubj", height = "300px")
  )

  ,box(
    title = "Prices of each levels of courses"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotlyOutput("pricesEachLevel", height = "300px")
  )

)

frow3 <- fluidRow(
  
  box(
    title = "Number of subscribers and reviews for each level"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotlyOutput("SubsandReviewsBylevel", height = "300px")
  )
  
 
  
)



# combine the two fluid rows to make the body
body <- dashboardBody(frow1,frow2,frow3)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')

# create the server functions for the dashboard  
server <- function(input, output) { 
  

  
  #creating the plotOutput content
  
  output$subsribersBysubjects <- renderPlotly({
    p1 <- plot_ly(udemy,x=~subject.f,y=~num_subscribers,type = "bar")
    p1
  })
  
  output$subscribersBylevels <- renderPlotly({
    # p2<-ggplot(udemy, aes(x="", y=num_subscribers, fill=level.f)) +
    #   geom_bar(stat="identity", width=1) +
    #   coord_polar("y", start=0)
    # p3<-ggplotly(p2)
    # p3
    fig <- plot_ly(udemy, labels = ~level.f, values = ~num_subscribers, type = 'pie')
    fig <- fig %>% layout(title = 'Number of subscribers by levels',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  
  output$CoursesEachSubj <- renderPlotly({
    udem<-udemy
    
    udem <- udem %>% group_by(udem$subject.f)
    udem <- udem %>% summarize(count = n())
    colnames(udem)[1] <- "subject"
    fig <- udem %>% plot_ly(labels = ~subject, values = ~count)
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(title = "Count of courses of each subject",  showlegend = F,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  
  output$pricesEachLevel <- renderPlotly({
    fig <- plot_ly(udemy, y = ~price, color = ~level.f, type = "box")
    fig <- fig %>% layout(title = 'Prices of each Levels of Courses')
    
    fig
    
  })
  output$SubsandReviewsBylevel <- renderPlotly({
    fig <- plot_ly(udemy, x = ~udemy$level.f, y = ~udemy$num_subscribers, type = 'bar', name = 'Num of Subscribers', marker = list(color = 'rgb(49,130,189)'))
    fig <- fig %>% add_trace(y = ~udemy$num_reviews, name = 'Number of reviews', marker = list(color = 'rgb(204,204,204)'))
    fig <- fig %>% layout(xaxis = list(title = "", tickangle = -45),
                          yaxis = list(title = ""),
                          margin = list(b = 100),
                          barmode = 'group')
    
    fig
    
  })
  
}




shinyApp(ui, server)
