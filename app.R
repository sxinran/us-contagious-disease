library(shiny)
library(dslabs)
library(ggplot2)
library(colourpicker)
library(dplyr)
library(readr)
library(plotly)
library(shinyWidgets)

data = us_contagious_diseases

ui = fluidPage(
  h1("US Contagious Diseases"),
  sidebarLayout(
    sidebarPanel(
      h4("Inputs"),
      textInput("title", "Plot Title", placeholder = "Enter text for plot title"),
      selectInput("diseases", "X-axis", choices = levels(data$disease),
                  selected = "Hepatitis A"),
      pickerInput("states", "Color by States : ", choices = levels(data$state), 
                  options = list(`actions-box` = TRUE), multiple = T),
      sliderInput("years", "Years", min(data$year), max(data$year), value = c(1990, 2000)),
      numericInput("size", "Point size", 3, min = 1),
      sliderInput("alpha", "Alpha", min = 0, max = 1, value = 0.5),
      downloadButton(outputId = "download_data", "Download")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel(
                    title = "Plot",
                    plotlyOutput("plot"),
                    textOutput("description")
                  ),
                  tabPanel(
                    title = "Table",
                    DT::dataTableOutput("table")
                  )
      ) 
    )
  )
)

server = function(input, output){
  filtered_data = reactive({
    req(input$states)
    data = data %>%
      mutate(percentage = floor((count/population*100)*10000)/10000) %>%
      filter(disease %in% input$diseases) %>%
      filter(state %in% input$states) %>%
      filter(year >= input$years[1] & year <= input$years[2])
  })
  
  output$plot = renderPlotly({
    data = filtered_data()
    req(input$size)
    ggplotly({
      p = ggplot(data, aes(x = disease, y = percentage, color = state))+
      geom_point(size = input$size, alpha = input$alpha)+
      ggtitle(input$title)
    })
  })
  
  output$table = DT::renderDataTable({
    data = filtered_data()
    data
  })
  
  output$download_data = downloadHandler(
    filename = "us_contagious_diseases.csv",
    content = function(file){
      data = filtered_data()
      write_csv(data, file)
    }
  )
  
  output$description = renderText({
    paste0("The plot above titled \" ", input$title, " \" visualizes the relationship between ",
           input$diseases, " and Prevalence of the disease")
  })
}

shinyApp(ui = ui, server = server)