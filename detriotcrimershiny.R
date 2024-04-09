library(shiny)
library(ggplot2)
library(leaflet)
library(scales)  
library(plotly) 
library(lubridate)  

# Load the dataset
data <- read.csv("City_of_Detroit_Crime_2020_to_2023.csv", fileEncoding = "ISO-8859-1")

# Convert 'ibr_date' to Date type for time series analysis
data <- data[data$ibr_date >= as.Date("2020-01-01") & data$ibr_date <= as.Date("2022-12-31"), ]
data <- na.omit(data)
data <- data[!is.na(data$longitude) & !is.na(data$latitude) & data$longitude != 0 & data$latitude != 0, ]

# Ensure 'neighborhood' and 'precinct' are treated as factors
data$neighborhood <- as.factor(data$neighborhood)
data$precinct <- as.factor(data$precinct)

# Define UI
ui <- fluidPage(
  titlePanel("Detroit Crime Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("offenseCategory", "Offense Category:", 
                  choices = c("All", unique(data$offense_category)), 
                  selected = "All"),
      dateRangeInput("dateRange", "Date Range:", 
                     start = min(data$ibr_date, na.rm = TRUE), 
                     end = max(data$ibr_date, na.rm = TRUE)),
      selectInput("precinct", "Precinct:", 
                  choices = c("All", unique(data$precinct)), 
                  selected = "All"),
      selectInput("dayOfWeek", "Day of the Week:", 
                  choices = c("All", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), 
                  selected = "All")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("crimeMap")),
        tabPanel("Crime Categories", plotOutput("categoryPlot")),
        tabPanel("Crime Trends", plotOutput("trendPlot")),
        tabPanel("Offense Category Proportions", plotlyOutput("pieChart"))
      )
    )
  )
)
# Define server logic
server <- function(input, output) {
  # Reactive data based on selected filters
  filtered_data <- reactive({
    temp_data <- data
    
    # Ensure only rows with valid coordinates are used
    temp_data <- temp_data[!is.na(temp_data$longitude) & !is.na(temp_data$latitude), ]
    temp_data <- temp_data[temp_data$longitude >= -83.288 & temp_data$longitude <= -82.910, ]
    temp_data <- temp_data[temp_data$latitude >= 42.255 & temp_data$latitude <= 42.450, ]
    
    # Filter based on the offense category
    if (input$offenseCategory != "All") {
      temp_data <- temp_data[temp_data$offense_category == input$offenseCategory, ]
    }
    
    if (input$dayOfWeek != "All") {
      temp_data <- temp_data[temp_data$day_of_week == match(input$dayOfWeek, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")), ]
    }
    
    # Filter based on the date range
    if (!is.null(input$dateRange[1]) && !is.null(input$dateRange[2])) {
      temp_data <- temp_data[temp_data$ibr_date >= input$dateRange[1] & temp_data$ibr_date <= input$dateRange[2], ]
    }
    
    # Filter based on precinct instead of neighborhood
    if (input$precinct != "All") {
      temp_data <- temp_data[temp_data$precinct == input$precinct, ]
    }
    temp_data
  })
  
  # Map of crimes
  output$crimeMap <- renderLeaflet({
    # Ensure the reactive data is not null and contains rows
    map_data <- filtered_data()
    if (!is.null(map_data) && nrow(map_data) > 0) {
      leaflet(map_data) %>%
        addTiles() %>%
        addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                         popup = ~offense_description, 
                         color = '#A52A2A', # light pink color
                         opacity = 0.8,    # Optional: to give some transparency
                         fillOpacity = 0.8)
    } else {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -83.0458, lat = 42.3314, zoom = 10) # Default view of Detroit
    }
  })
  
  # Bar chart of crime categories
  output$categoryPlot <- renderPlot({
    if (nrow(filtered_data()) > 0) {
      ggplot(filtered_data(), aes(x = offense_category)) +
        geom_bar() +
        labs(title = "Crime by Category", x = "Category", y = "Number of Incidents") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Time trend of crimes
  output$trendPlot <- renderPlot({
    filtered_data <- filtered_data() 
    if (nrow(filtered_data) > 0) {
      filtered_data$ibr_date <- as.Date(filtered_data$ibr_date)  
      
      ggplot(filtered_data, aes(x = ibr_date)) +
        geom_line(stat = "count", aes(group = 1)) +
        scale_x_date(labels = date_format("%Y-%m")) +
        labs(title = "Crime Trends Over Time", x = "Date", y = "Number of Incidents")
    }
  })
  
  # Pie chart of offense categories
  output$pieChart <- renderPlotly({
    if (nrow(filtered_data()) > 0) {
      category_counts <- table(filtered_data()$offense_category)
      plot_ly(labels = names(category_counts), values = category_counts, type = 'pie', textinfo = 'label+percent')
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
