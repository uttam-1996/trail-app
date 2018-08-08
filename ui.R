library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(dashboardthemes)
logo_blue_gradient <- shinyDashboardLogoDIY(
  
  boldText = "Exceloid"
  ,mainText = ""
  ,textSize = 16
  ,badgeText = "NEW"
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = "#40E0D0"
  ,badgeBorderRadius = 3
  
)

RawData=read.csv(url("https://files.fm/u/3xv8nzt7"),stringsAsFactors = TRUE)
# levels(RawData$Sales.Representative)[levels(RawData$Sales.Representative)==""]<- "no sales representative"

# defining the header of the page 
shinyUI(
  dashboardPage( title = "::Exceloid::",#img(src="*C:\Users\kenny.kenny-PC\Downloads\taeyeon_i_hd_wallpaper_3_by_rizzie23-d9d1ofe.jpg"),
    dashboardHeader(title = logo_blue_gradient ,titleWidth = 150,
                    dropdownMenu(type = "messages"),
                    dropdownMenu(type = "notifications"),
                    dropdownMenu(type = "tasks")
    ),
    dashboardSidebar(
      uiOutput("Raw") ,
      #MEnu items for the dashboard
      menuItem(
        "Dashboard",
        badgeLabel = "new",
        badgeColor = "yellow",
        icon = icon("dashboard")
      )
    ),
    
    #dashboard Body and graphs and values start from here
    dashboardBody( shinyDashboardThemes(theme = "boe_website"),
      
      # first row
      #row for the select inputs
      fluidRow(
      
              
              #Drop box  for year
        
              
       column(2,selectInput(inputId="year", label="Year", choices  = unique(RawData$Year),width = 80)),
        
        
              #dropbox for month
        
        
        column(3,selectInput(inputId="month", label="Month", choices = c("Jan","Feb","Mar","Apr","May","Jun"),multiple = TRUE)),
        
        
              #dropbox for organization 
        
        
        column(3,(selectInput(inputId="organization",label = "Organization",choices = NULL,multiple = TRUE))),
        
        
               #drop box for weekdays
        column(4,(selectInput(inputId="weekday",label = "weekday",choices = NULL,multiple = TRUE)))           
        ),            
      
      #Info Boxes for thr dash board
      
      #second row
      
  
      fluidRow(
        
      #infobox for net Amount
        
        valueBoxOutput("NetAmount",width = 3),
      
      #infobox for NET Quantity  
        
        valueBoxOutput("Quantity")),
      
      
      
      #Third row  Row
      
      fluidRow(
        column(5,
        
      #plotting the graph for month and net quantity line Graph
      
        box("Month- Net Amount",solidHeader=TRUE,width = 500,height =300, 
            plotlyOutput("monthgraph",width = "500px",height = "250px"))),
        
            
      #plotting the donut graph for Pos_terminal Doughnut Graph
      column(3,
     
         box( "Pos Terminal Type",solidHeader = TRUE, width = 250,height = 300,
              plotlyOutput("doughnut",height = 250,width = 270)
          )),
      
      column(3,
          box("Sales Representative chart",solidHeader = T,width = 250,height = 300,
              
                plotlyOutput("pierepresent",height = 250,width = 270)
              )   
             
             )),
      
      
      
      
      #new row for the two way histogram
      
      fluidRow(
      box(
            
           
        
        div(style = 'overflow-x: scroll',plotlyOutput("histogram",height = 350,width = 900))%>%div(style = 'overflow-y: hidden') 
      )  
        
      )
      
      
            
          
            
          
        
       
    
  )
  
  
) 

)
