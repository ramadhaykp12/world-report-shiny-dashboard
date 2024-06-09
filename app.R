#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(readxl)

child_world <- read_excel("children-per-woman-un (worldwide)(1).xlsx",sheet = "children-per-woman-un")
child_world <- child_world %>%
  rename("child_per_woman" = "Fertility Rate")
selected_entity <- subset(child_world, Entity == "World")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fertility Rate: Children for Woman"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("EntityInput",
                        "Filter by Country:",
                        choices = unique(child_world$Entity),
                        selected = selected_entity$Entity)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("linePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  filtered_data <- reactive({
    child_world %>%
      filter(Entity == input$EntityInput)
  })

    output$linePlot <- renderPlotly({
      plot_ly(data = filtered_data(), x=~Year, y=~child_per_woman, 
              type='scatter', 
              mode='lines') %>%
        layout(title = paste("Fertility rate: children per woman"),
             xaxis=list(title='Year'),
             yaxis=list(title='Child Per Woman'))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
