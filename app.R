library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(gapminder)

ui <- fluidPage(

  tags$head(tags$style(HTML("
    body { background-color: #f4f6fb; font-family: system-ui; }

    .header {
      background: linear-gradient(135deg, #0f172a, #1e293b);
      color: white;
      padding: 20px;
      border-radius: 16px;
      margin-bottom: 16px;
      box-shadow: 0 10px 25px rgba(0,0,0,0.12);
    }

    .header h2 { margin: 0; font-size: 26px; }
    .header p { margin-top: 6px; margin-bottom: 0; color: rgba(255,255,255,0.7); }

    .kpi {
      padding: 16px;
      border-radius: 16px;
      background: white;
      box-shadow: 0 6px 18px rgba(0,0,0,0.06);
      border: 1px solid rgba(0,0,0,0.06);
      margin-bottom: 12px;
      transition: 0.2s;
    }

    .kpi:hover { transform: translateY(-3px); }

    .kpi .label { font-size: 12px; font-weight: 600; color: #64748b; }
    .kpi .value { font-size: 24px; font-weight: 800; color: #0f172a; margin-top: 6px; }

    .card {
      padding: 16px;
      border-radius: 16px;
      background: white;
      box-shadow: 0 6px 18px rgba(0,0,0,0.06);
      border: 1px solid rgba(0,0,0,0.06);
      margin-bottom: 14px;
    }

    .control-label { font-weight: 600; color: #475569; }
  "))),

  div(class = "header",
      h2("Gapminder Analytics Dashboard (Shiny)"),
      p("Filters → KPIs → Multiple charts → Table")
  ),

  sidebarLayout(
    sidebarPanel(
      class = "card",

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
      ),

      selectInput(
        "selectedYear",
        "Selected Year (Bar/Scatter)",
        choices = sort(unique(gapminder$year)),
        selected = 2007
      )
    ),

    mainPanel(
      fluidRow(
        column(3, div(class = "kpi", div(class="label","Countries"), div(class="value", textOutput("kpi_countries")))),
        column(3, div(class = "kpi", div(class="label","Avg Life Expectancy"), div(class="value", textOutput("kpi_lifeExp")))),
        column(3, div(class = "kpi", div(class="label","Avg GDP per Capita"), div(class="value", textOutput("kpi_gdpPercap")))),
        column(3, div(class = "kpi", div(class="label","Total Population"), div(class="value", textOutput("kpi_population"))))
      ),

      fluidRow(
        column(6, div(class="card", h4("Life Expectancy Trend"), plotlyOutput("lifeExpPlot", height = 320))),
        column(6, div(class="card", h4("Avg GDP per Continent"), plotlyOutput("gdpBarPlot", height = 320)))
      ),

      div(class="card",
          h4("GDP vs Life Expectancy (Bubble = Population)"),
          plotlyOutput("scatterPlot", height = 420)
      ),

      div(class="card",
          h4("Filtered Data Table"),
          DTOutput("table")
      )
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

  selectedYearData <- reactive({
    df <- gapminder %>% filter(year == as.numeric(input$selectedYear))

    if (input$continent != "All") {
      df <- df %>% filter(continent == input$continent)
    }

    df
  })

  # KPIs
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

  output$kpi_population <- renderText({
    df <- filtered()
    format(sum(df$pop, na.rm = TRUE), big.mark = ",")
  })

  # Chart 1: line chart
  output$lifeExpPlot <- renderPlotly({
    df <- filtered() %>%
      group_by(year) %>%
      summarise(lifeExp = mean(lifeExp, na.rm = TRUE), .groups = "drop")

    p <- ggplot(df, aes(year, lifeExp)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      theme_minimal(base_size = 12) +
      labs(x = "Year", y = "Avg Life Expectancy")

    ggplotly(p, tooltip = c("x", "y"))
  })

  # Chart 2: bar chart
  output$gdpBarPlot <- renderPlotly({
    df <- gapminder %>%
      filter(year == as.numeric(input$selectedYear)) %>%
      group_by(continent) %>%
      summarise(gdpPercap = mean(gdpPercap, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(gdpPercap))

    if (input$continent != "All") {
      df <- df %>% filter(continent == input$continent)
    }

    p <- ggplot(df, aes(x = reorder(continent, gdpPercap), y = gdpPercap)) +
      geom_col(fill = "#3b82f6") +
      coord_flip() +
      theme_minimal(base_size = 12) +
      labs(x = "Continent", y = "Avg GDP per Capita")

    ggplotly(p, tooltip = c("x", "y"))
  })

  # Chart 3: scatter plot
  output$scatterPlot <- renderPlotly({
    df <- selectedYearData()

    p <- ggplot(df, aes(x = gdpPercap, y = lifeExp,
                        text = paste0(
                          "Country: ", country,
                          "<br>LifeExp: ", round(lifeExp, 1),
                          "<br>GDP/Cap: ", round(gdpPercap, 0),
                          "<br>Pop: ", format(pop, big.mark = ",")
                        ),
                        size = pop,
                        color = continent)) +
      geom_point(alpha = 0.7) +
      scale_x_log10() +
      theme_minimal(base_size = 12) +
      labs(x = "GDP per Capita (log scale)", y = "Life Expectancy") +
      guides(size = "none")

    ggplotly(p, tooltip = "text")
  })

  # Table
  output$table <- renderDT({
    filtered() %>%
      arrange(desc(year), desc(lifeExp)) %>%
      datatable(
        options = list(pageLength = 12, searchHighlight = TRUE),
        rownames = FALSE
      )
  })
}

shinyApp(ui, server)
