

library(shiny)
library(plotly)


dt <- read.csv("/Users/loanrobinson/Desktop/enroll.csv")
studies <- unique(dt$study)


ui <- fluidPage(br(),br(),br(),
          column(8, style = "overflow-y:scroll;width: 100%; max-height: 600px; margin-bottom:30px;display: block;flex: 1 1 auto;",
                    plotlyOutput('pie'),
                    uiOutput("back"))

)
        

server <- function(input, output, session) {

  
  
  pie_chart <- reactiveVal()
  
 
    sub_data <- reactive({
    
      if (!length(pie_chart())) {
      return(count(dt, study, wt = y))
    }
    dt %>%
      filter(study %in% pie_chart()) %>%
      count(sub_study, wt = y)
  })
  
  

  output$pie <- renderPlotly({
    d <- setNames(sub_data(), c("labels", "values"))
    plot_ly(d, labels = ~labels, values = ~values, customdata = ~labels, type = "pie",
            marker = list(colors = c("#F7FBFF","#2171B5","#9ECAE1","#C6DBEF","#08519C","#9ECAE1","#08306B","#F7FBFF","black","#C6DBEF","#9ECAE1", "#6BAED6" ,"#4292C6"))
    ) %>% layout(title = pie_chart() %||% "Pie chart with plotly_event")
    
  })
  
  
  observe({
    cd <- event_data("plotly_click")$customdata[[1]]
    if (isTRUE(cd %in% studies)) pie_chart(cd)
  })
  
 
  output$back <- renderUI({
    if (length(pie_chart())) 
      actionButton("clear", "Back", icon("chevron-left"))
  })
  
  
  observeEvent(input$clear, pie_chart(NULL))
}

shinyApp(ui, server)