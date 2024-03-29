# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

# Load data
Auschwitz_Death_Certificates_1942_1943_Auschwitz <- read_csv("/cloud/project/not_my_first_Shiny/Auschwitz_Death_Certificates_1942-1943 - Auschwitz.csv")

# Process the data to count victims by nationality/category
victims_count <- Auschwitz_Death_Certificates_1942_1943_Auschwitz %>%
  group_by(Religion) %>%
  summarise(Count = n())

# Define UI for application
ui <- fluidPage(
  titlePanel("Holocaust Victims at Auschwitz"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("religion", "Select Ethnicity:", choices = unique(victims_count$Religion), multiple = TRUE)
    ),
    mainPanel(
      plotOutput("plot"),
      dataTableOutput("table")
    )
  )
)

# Define server logic
# Define server logic
server <- function(input, output) {
  # Filter data based on selected ethnicities
  filtered_data <- reactive({
    Auschwitz_Death_Certificates_1942_1943_Auschwitz %>%
      filter(Religion %in% input$religion)
  })
  
  # Generate plot based on filtered data
  output$plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Religion, fill = Religion)) +
      geom_bar() +
      labs(title = "Number of Holocaust Victims by Ethnicity", x = "Ethnicity", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Generate table based on filtered data
  output$table <- renderDataTable({
    filtered_data()
  })
}


# Run the application
shinyApp(ui = ui, server = server)
