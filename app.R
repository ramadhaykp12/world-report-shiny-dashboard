library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(readxl)

child_world <- read.csv("path_to_child_world.csv")
fertility_idn <- read_excel("Total Fertility Rate By Province 2020 .xlsx", sheet = "Raw Data")
fertility_edu <- read_excel("projections-of-the-number-of-children-per-woman-by-education-scenario.xlsx")
fertility_edu  <- fertility_edu  %>%
  rename("Fertility_Rate_GET" = "Fertility Rate_GET")
fertility_female_labor <- read_excel("fertility-and-female-labor-force-participation (worldwide).xlsx")
fertility_contraception <- read_excel("fertility-vs-contraception (worldwide).xlsx")
force_rate <- read_excel("proportion-of-labor-force-who-are-women.xlsx")
mean_school_female <- read.csv("path_to_mean_school_female.csv")

# Filter data for the default selection
selected_entity <- subset(child_world, Entity...1 == "World")
selected_province <- subset(fertility_idn, Province == "Aceh")

# Define UI for the application
ui <- fluidPage(
  titlePanel("Fertility Rate: Children per Woman"),
  
  navlistPanel(
    "Menu",
    tabPanel("Home",
             h2("Welcome to the Dashboard"),
             p("This dashboard provides an overview of the fertility rate (children per woman) over the years.")
    ),
    # Fertility Rate tab
    tabPanel("Fertility Rate",
             fluidRow(
               column(12,
                      wellPanel(
                        selectInput("EntityftCountry",
                                    "Filter by Country:",
                                    choices = unique(child_world$Entity...1),
                                    selected = "World")
                      )
               )
             ),
             fluidRow(
               column(12,
                      plotlyOutput("linePlotFtWorld")
               )
             ),
             fluidRow(
               column(12,
                      wellPanel(
                        selectInput("EntityftProvince",
                                    "Filter by Province:",
                                    choices = unique(fertility_idn$Province),
                                    selected = "Aceh")
                      )
               )
             ),
             fluidRow(
               column(12,
                      plotlyOutput("linePlotFtProvince")
               )
             )
    ),
    # Average year of schooling tab
    tabPanel("Average Year of Schooling",
             fluidRow(
               column(12,
                      wellPanel(
                        selectInput("EntityftCountrySchool",
                                    "Filter by Country:",
                                    choices = unique(mean_school_female$Entity),
                                    selected = "Afghanistan")
                      )
               )
             ),
             fluidRow(
               column(12,
                      plotlyOutput("linePlotFtSchool")
               )
             )
    ),
    # Projection of the fertility rate by education tab
    tabPanel("Projection of the fertility rate by education",
             fluidRow(
               column(12,
                      wellPanel(
                        selectInput("EntityftCountryEdu",
                                    "Filter by Country:",
                                    choices = unique(fertility_edu$Entity),
                                    selected = "World")
                      )
               )
             ),
             fluidRow(
               column(12,
                      plotlyOutput("linePlotFtEdu")
               )
             )
    ),
    # Woman labour Participation Rate
    tabPanel("Women Labour Participation Rate",
             fluidRow(
               column(12,
                      wellPanel(
                        selectInput("EntityftForce",
                                    "Filter by Country:",
                                    choices = unique(force_rate$Entity),
                                    selected = "World")
                      )
               )
             ),
             fluidRow(
               column(12,
                      plotlyOutput("linePlotFtForce")
               )
             )
    ),
    # Correlation between contraceptive use and fertility rate worldwide
    tabPanel("Correlation between contraceptive use and fertility rate worldwide",
             fluidRow(
               column(12,
                      wellPanel(
                        selectInput("EntityFtCon",
                                    "Filter by Country:",
                                    choices = unique(fertility_contraception$Entity),
                                    selected = "World")
                      )
               )
             ),
             fluidRow(
               column(12,
                      plotlyOutput("linePlotFtContra")
               )
             )
    ),
    # Correlation between labor participation and fertility rate
    tabPanel("Correlation between labor participation and fertility rate",
             fluidRow(
               column(12,
                      wellPanel(
                        selectInput("EntityFtFemale",
                                    "Filter by Country:",
                                    choices = unique(fertility_female_labor$Entity),
                                    selected = "World")
                      )
               )
             ),
             fluidRow(
               column(12,
                      plotlyOutput("linePlotFtFemale")
               )
             )
    )
  )
)

# Define server logic required to draw line plots
server <- function(input, output) {
  
  filtered_data_ft_wd <- reactive({
    child_world %>%
      filter(Entity...1 == input$EntityftCountry)
  })
  
  filtered_data_ft_idn <- reactive({
    fertility_idn %>%
      filter(Province == input$EntityftProvince)
  })
  
  filtered_data_ft_school <- reactive({
    mean_school_female %>%
      filter(Entity == input$EntityftCountrySchool)
  })
  
  
  output$linePlotFtWorld <- renderPlotly({
    p1 <- ggplot(filtered_data_ft_wd(), aes(x = Year, y = Child_per_Woman)) +
      geom_line() +
      geom_point() +
      labs(title = "Fertility rate: children per woman (worldwide)",
           x = 'Year',
           y = 'Child Per Woman') +
      theme_minimal()
    
    ggplotly(p1)
  })
  
  output$linePlotFtProvince <- renderPlotly({
    p2 <- ggplot(filtered_data_ft_idn(), aes(x = Year, y = Fertility_rate)) +
      geom_line() +
      geom_point() +
      labs(title = "Fertility rate: children per woman (Indonesia)",
           x = 'Year',
           y = 'Child Per Woman') +
      theme_minimal()
    
    ggplotly(p2)
  })
  
  output$linePlotFtSchool <- renderPlotly({
    p3 <- ggplot(filtered_data_ft_school(), aes(x = Year, y = Woman_mean_years_of_schooling)) +
      geom_line() +
      geom_point() +
      labs(title = "Average years of schooling",
           x = 'Year',
           y = 'Mean of Years Schooling') +
      theme_minimal()
    
    ggplotly(p3)
  })
        
  filtered_data <- reactive({
    fertility_edu[fertility_edu$Entity == input$EntityftCountryEdu, ]
  })
  
  output$linePlotFtEdu <- renderPlotly({
    ggplot(filtered_data(), aes(x = Year)) +
      geom_line(aes(y = Fertility_Rate_FT), size = 1) +
      geom_line(aes(y = Fertility_Rate_GET), size = 1) +
      geom_line(aes(y = Fertility_Rate_CER), size = 1) +
      theme_minimal()
  })
  
  filtered_force <- reactive({
    force_rate %>%
      filter(Entity == input$EntityftForce)
  })
  
  output$linePlotFtForce <- renderPlotly({
    p4 <- ggplot(filtered_force(), aes(x = Year, y = Woman_labor_force_rate)) +
      geom_line() +
      geom_point() +
      labs(title = "Average years of schooling",
           x = 'Year',
           y = 'Mean of Years Schooling') +
      theme_minimal()
    
    ggplotly(p4)
  })
  
  filtered_ft<- reactive({
    fertility_contraception %>%
      filter(Entity == input$EntityFtCon)
  })
  
  output$linePlotFtContra <- renderPlotly({
    ggplot(filtered_ft(), aes(x = Year)) +
      geom_line(aes(y = Fertility_rate), size = 1) +
      geom_line(aes(y = Contraceptive_prevalence), size = 1) +
      theme_minimal()
  })
  
  filtered_female<- reactive({
    fertility_female_labor %>%
      filter(Entity == input$EntityFtFemale)
  })
  
  output$linePlotFtFemale <- renderPlotly({
    ggplot(filtered_female(), aes(x = Year)) +
      geom_line(aes(y = Fertility_rate)) +
      geom_line(aes(y = Labor_force_participation_rate)) +

      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
