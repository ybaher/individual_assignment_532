library(shiny)
library(nanoparquet)
library(dplyr)
library(ggplot2)
library(bslib)

# asked claude for help with some of the implementations
data <- read_parquet("data/processed copy/LondonCrimeData.parquet")

BOROUGHS <- sort(unique(data$borough))

ui <- page_sidebar(
  title = "London Crime Dashboard",
  sidebar = sidebar(
    selectInput(
      "borough",
      "Select Borough:",
      choices = BOROUGHS,
      selected = "Croydon"
    )
  ),
  value_box(
    title = "Most Common Crime",
    value = textOutput("most_common_crime"),
    theme = "primary"
  ),
  card(
    card_header("Crimes by Type"),
    plotOutput("crime_plot")
  )
)

server <- function(input, output, session) {

  # 1 reactive calc — filtered data
  filtered_data <- reactive({
    data |> filter(borough == input$borough)
  })

  # Output 1 — value box
  output$most_common_crime <- renderText({
    filtered_data() |>
      count(major_category, sort = TRUE) |>
      slice(1) |>
      pull(major_category)
  })

  # Output 2 — bar plot
  output$crime_plot <- renderPlot({
    filtered_data() |>
      count(major_category) |>
      ggplot(aes(x = reorder(major_category, n), y = n, fill = major_category)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "Crime Type", y = "Count",
           title = paste("Crime Types in", input$borough)) +
      theme_minimal()
  })
}

shinyApp(ui, server)

