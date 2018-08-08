#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram



shinyServer(function(input, output,session) {
  
 
   
 
  
  #Reading the File
  
  
  RawData=read.csv("loyality.csv.csv",stringsAsFactors = TRUE)

  
    
  
  #levels(RawData$Sales.Representative)[levels(RawData$Sales.Representative)==""]<- "no sales representative"
  #select Input Codes
  
  #For the Year
  
  observe({
    
    updateSelectInput(session,"year","year",choices =unique(RawData$Year),selected = "2018")
    })
  
  #FOr the Month
  
  observe({
    req(input$month)
    updateSelectInput(session,"month","Month",choices=unique(RawData$Month),selected =unique(RawData$Month))})
  
  #FOr the ORganization
  observe({
    req(input$organization)
    updateSelectInput(session,inputId = "organization","Organization",choices = unique(RawData$Organization),selected = unique(RawData$Organization))})
  
  
  #For the weekdays
  observe({
    req(input$weekday)
    updateSelectInput(session,inputId = "weekday",choices = unique(RawData$Weekday),selected = unique(RawData$Weekday))})
  
  #value boxes
  
  #value box for Net amount
    output$NetAmount = renderValueBox({
      
      filtered<-RawData%>%
                filter(Month.of.Year %in% input$month & Organization %in% input$organization& Weekday %in% input$weekday)%>%
                select(Net.Amount)
      sum=sum(filtered$Net.Amount)
      
      valueBox(value=sum,"Net Amount")})
  #value box for  NetQuantity 
    
    output$Quantity=renderValueBox(
      {
        filteredquanity<-RawData%>%
                        filter(Month.of.Year %in% input$month & Organization %in% input$organization & Weekday %in% input$weekday)%>%
                        select(Net.Quantity)
        sumquantity=sum(filteredquanity$Net.Quantity)
        valueBox(value = sumquantity,"Net Quantity")
      }
    )
    
    #graph for the month and net amount
    
    output$monthgraph=renderPlotly({
      filtereds<-RawData%>%
        filter(Month.of.Year %in% input$month & Organization %in% input$organization & Weekday %in% input$weekday )%>%
        select(Net.Amount,Month.of.Year)
      
        month1=aggregate(filtereds$Net.Amount,by = list(filtereds$Month.of.Year), sum )
      
      
   
    
    if(is.null(input$month)|is.null(input$organization)|is.null(input$weekday))
    {
      plotly(RawData)
    }
    else{
    
    
      ggplotly(ggplot(data=month1, aes(x=month1$Group.1, y=month1$x, group=1)) +
                 geom_line(color="red")+
                 geom_point()  )%>% config(displayModeBar= F) %>% config(showLink=F)%>% layout(xaxis=list(title = "Months"),yaxis = list(title = "Net Amount"))
    }  
      
    })
    
    #doughnut graph
    
    output$doughnut=renderPlotly({
        filteredpos<-RawData%>%
          filter(Month.of.Year %in% input$month & Organization %in% input$organization & Weekday %in% input$weekday)%>%
              select(POS.Terminal.Type)
        p <- data.frame(table(RawData$POS.Terminal.Type))
          
        
          plot_ly(p,labels = p$Var1, values = p$Freq) %>%
          add_pie(hole = 0.7) %>%
          layout(showlegend = F,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% config(displayModeBar= F) %>% config(showLink=F)%>%layout(paper_bgcolor="transparent")
          
        
        
        
      
      
    })
    
    
     
    
    #Pie chart for the sales Reprentatives
    
    
    output$pierepresent=renderPlotly({
      filteredrep<-RawData%>%
        filter(Month.of.Year %in% input$month & Organization %in% input$organization & Weekday %in% input$weekday)%>%
        select(Sales.Representative)
      q <- data.frame(table(filteredrep$Sales.Representative))

      
      plot_ly(q, labels = q$Var1, values = q$Values, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),
              #The 'pull' attribute can also be used to create space between the sectors
              showlegend = FALSE) %>%
        layout(title = '',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels =FALSE))%>% config(displayModeBar= F) %>% config(showLink=F)%>%layout(paper_bgcolor="transparent")
      
      
          })
    
    
    
    
   
    
    
                          
    
    #Comparitive histogram
    
    output$histogram<- renderPlotly({
      
      filtered<-RawData%>%
        filter(Month.of.Year %in% input$month & Organization %in% input$organization & Weekday %in% input$weekday)%>%
        select(Net.Amount,Month.of.Year,Weekday)
      
      
      ggplotly(ggplot(filtered, aes(filtered$Month.of.Year, filtered$Net.Amount)) + geom_bar(aes(fill = filtered$Weekday), 
                                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
        theme(legend.position="top", legend.title = 
                element_blank(),axis.title.x=element_blank(), 
              axis.title.y=element_blank()))%>% config(displayModeBar= F) %>% config(showLink=F)%>%layout(paper_bgcolor="transparent")%>% layout(xaxis=list(title = "Months"),yaxis = list(title = "Net Amount"))
      
      
      
      
      
    })
    
    
    
  
  
  
  
  
    
  })
  

