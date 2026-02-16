# app.R
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(gapminder)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .kpi { padding: 14px; border-radius: 12px; background: #f6f7fb; margin-bottom: 12px; }
    .kpi .label { font-size: 12px; color: #666; }
    .kpi .value { font-size: 22px; font-weight: 700; }
  "))),
  titlePanel("Gapminder Dashboard (Shiny)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "continent",
        "Continent",
        choices = c("All", unique(as.character(gapminder$continent))),
        selected = "All"
      ),
      sliderInput(
        "year",
        "Year range",
        min = min(gapminder$year),
        max = max(gapminder$year),
        value = c(1980, 2007),
        sep = ""
      )
    ),
    mainPanel(
      fluidRow(
        column(4, div(class = "kpi", div(class="label","Countries"), div(class="value", textOutput("kpi_countries")))),
        column(4, div(class = "kpi", div(class="label","Avg Life Expectancy"), div(class="value", textOutput("kpi_lifeExp")))),
        column(4, div(class = "kpi", div(class="label","Avg GDP per Capita"), div(class="value", textOutput("kpi_gdpPercap"))))
      ),
      plotlyOutput("lifeExpPlot", height = 360),
      br(),
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {

  filtered <- reactive({
    df <- gapminder %>%
      filter(year >= input$year[1], year <= input$year[2])

    if (input$continent != "All") {
      df <- df %>% filter(continent == input$continent)
    }
    df
  })

  output$kpi_countries <- renderText({
    df <- filtered()
    length(unique(df$country))
  })

  output$kpi_lifeExp <- renderText({
    df <- filtered()
    sprintf("%.1f", mean(df$lifeExp, na.rm = TRUE))
  })

  output$kpi_gdpPercap <- renderText({
    df <- filtered()
    sprintf("%.0f", mean(df$gdpPercap, na.rm = TRUE))
  })

  output$lifeExpPlot <- renderPlotly({
    df <- filtered() %>%
      group_by(year) %>%
      summarise(lifeExp = mean(lifeExp, na.rm = TRUE), .groups = "drop")

    p <- ggplot(df, aes(year, lifeExp)) +
      geom_line() +
      geom_point() +
      labs(x = "Year", y = "Avg Life Expectancy") +
      theme_minimal(base_size = 12)

    ggplotly(p, tooltip = c("x", "y"))
  })

  output$table <- renderDT({
    filtered() %>%
      arrange(desc(year), desc(lifeExp)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
}

shinyApp(ui, server)
