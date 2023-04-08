# Painel - Dengue no Ceará
# Rubens O. da Cunha Júnior

# Load packages ----
library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(geobr)

# Load data ----
# Dengue data
data <- read.csv("data/data.csv", sep = ";", encoding = "UTF-8")
data$name_muni <- tolower(data$Abrangência.Geografica)
data[data$name_muni == "ererê", ]$name_muni <- "ereré"

# Summary
year <- sort(unique(data$Ano))
year_min <- min(data$Ano, na.rm = TRUE)
year_max <- max(data$Ano, na.rm = TRUE)

municipalities <- sort(tolower(unique(data$Abrangência.Geografica)))
municipalities[municipalities == "ererê"] <- "ereré"

# Spatial data
geo_muni <- geobr::read_municipality(
  code_muni = "CE",
  year = 2020,
  showProgress = FALSE
)
geo_muni$name_muni <- tolower(geo_muni$name_muni)

# UI ----
ui <- dashboardPage(
  skin = "yellow",
  title = "Dengue no Ceará",
  
  header = dashboardHeader(title = "Dengue - Ceará"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Página inicial", tabName = "home", icon = icon("home")),
      menuItem("Painel", tabName = "panel", icon = icon("dashboard"))
    )
  ),
  
  body = dashboardBody(
    
    shinyjs::useShinyjs(),
    
    tabItems(
      tabItem(
        tabName = "panel",
        fluidRow(
          shinydashboard::box(
            title = "Selecionar",
            collapsible = TRUE,
            width = "12",
            shinydashboard::box(
              solidHeader = TRUE,
              radioButtons(
                inputId = "muni_type",
                label = "Município",
                choices = list("Mostrar todos" = 1, "Selecionar" = 2),
                selected = 1,
                inline = TRUE
              ),
              selectInput(
                inputId = "muni",
                label = "",
                choices = sort(municipalities),
                selected = NULL,
                multiple = TRUE
              )
            ),
            shinydashboard::box(
              solidHeader = TRUE,
              sliderInput(
                inputId = "year",
                label = "Ano:",
                min = year_min,
                max = year_max,
                value = c(year_min, year_max),
                step = 1,
                sep = ""
              )
            )
          )
        ),
        
        fluidRow(
          infoBoxOutput(outputId = "box_total", width = "6"),
          infoBoxOutput(outputId = "box_max_muni", width = "6")
        ),
        
        fluidRow(
          shinydashboard::box(
            title = "Casos de Dengue por município",
            collapsible = TRUE,
            plotOutput(outputId = "map")
          ),
          shinydashboard::box(
            title = "Casos de Dengue por região de planejamento",
            collapsible = TRUE,
            plotOutput(outputId = "planning")
          ),
          shinydashboard::box(
            title = "Casos de Dengue por região metropolitana",
            collapsible = TRUE,
            plotOutput(outputId = "metropolitan")
          ),
          shinydashboard::box(
            title = "Casos de Dengue por ano",
            collapsible = TRUE,
            plotOutput(outputId = "year")
          )
        ),
        
        fluidRow(
          shinydashboard::box(
            title = "Dados",
            collapsible = TRUE,
            width = "12",
            dataTableOutput(outputId = "table")
          )
        ),
        
        hr(),
        
        includeHTML("www/contato.html")
      ),
      
      tabItem(
        tabName = "home",
        includeHTML("www/sobre.html")
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  # Reactive ----
  # Filter Data
  data_filter <- reactive({
    req(input$year)
    
    year_start <- which(year == input$year[1])
    year_end <- which(year == input$year[2])
    year_selection <- year[year_start:year_end]
    
    temp <- data
    temp <- filter(temp, Ano %in% year_selection)
    
    if(!is.null(input$muni)) {
      temp <- filter(temp, name_muni %in% input$muni)
    }
    return(temp)
  })
  
  # Aggregate Data by Year
  data_agg_year <- reactive({
    data_filter() %>%
      group_by(Ano) %>%
      summarize(Valor = sum(Valor, na.rm = TRUE))
  })
  # Aggregate Data by Metropolitan Region
  data_agg_metro <- reactive({
    data_filter() %>%
      group_by(regiao_metropolitana) %>%
      summarize(Valor = sum(Valor, na.rm = TRUE))
  })
  # Aggregate Data by Planning Region
  data_agg_plan <- reactive({
    data_filter() %>%
      group_by(regiao_planejamento) %>%
      summarize(Valor = sum(Valor, na.rm = TRUE))
  })
  # Geo Data
  geo_filter <- reactive({
    temp1 <- data_filter() %>%
      group_by(name_muni) %>%
      summarize(Valor = sum(Valor, na.rm = TRUE))
    temp2 <- left_join(geo_muni, temp1, by = c("name_muni" = "name_muni"))
    return(temp2)
  })
  
  # Enable/Disable Municipality input selection
  observeEvent(input$muni_type, {
    if(input$muni_type == 1) {
      updateSelectInput(session, "muni", selected = "")
      shinyjs::disable("muni")
    } else if(input$muni_type == 2) {
      shinyjs::enable("muni")
    }
  }, ignoreNULL = T)
  
  # Render Output ----
  # Map plot
  output$map <- renderPlot({
    ggplot(data = geo_filter()) +
      geom_sf(mapping = aes(fill = Valor), color = "black", size = .15) +
      scale_fill_distiller(
        palette = "Oranges",
        name = "Casos",
        trans = "reverse",
        na.value = "lightgrey") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  
  # Bar plot by Planning Region
  output$planning <- renderPlot({
    ggplot(data = data_agg_plan(),
           mapping = aes(x = reorder(regiao_planejamento, Valor),
                         y = Valor)) +
      geom_bar(stat = "identity", fill = "#fdae6b") +
      labs(x = "Região de planejamento", y = "Casos") +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Bar plot by Metropolitan Region
  output$metropolitan <- renderPlot({
    ggplot(data = data_agg_metro(),
           mapping = aes(x = regiao_metropolitana, y = Valor)) +
      geom_bar(stat = "identity", fill = "#fdae6b") +
      geom_text(mapping = aes(label = Valor),
                vjust = -0.5,
                size = 2.5,
                colour = "black") +
      labs(x = "Região metropolitana", y = "Casos") +
      scale_x_discrete(labels = c(
        "Municípios não Metropolitanos" = "Nenhuma",
        "Região Metropolitana do Cariri" = "Cariri",
        "Região Metropolitana de Sobral" = "Sobral",
        "Região Metropolitana de Fortaleza" = "Fortaleza")) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Bar plot by Year
  output$year <- renderPlot({
    ggplot(data = data_agg_year(),
           mapping = aes(x = Ano, y = Valor)) +
      geom_bar(stat = "identity", fill = "#fdae6b") +
      geom_text(mapping = aes(label = Valor),
                vjust = -0.5,
                size = 2.5,
                colour = "black") +
      labs(x = "Ano", y = "Casos") +
      theme_minimal()
  })
  
  # Table
  output$table <- renderDataTable({
    temp <- data_filter() %>%
      select(Ano, name_muni, regiao_metropolitana, regiao_planejamento, Valor) %>%
      rename("Município" = name_muni,
             "Região Metropolitana" = regiao_metropolitana,
             "Região de Planejamento" = regiao_planejamento,
             "Casos" = Valor)
  })
  
  # Info Box Total
  output$box_total <- renderInfoBox({
    temp <- sum(data_filter()$Valor, na.rm = TRUE)
    infoBox(
      title = "Total de casos",
      value = temp,
      icon = icon("viruses"),
      color = "orange"
    )
  })
  
  # Info Box Max
  output$box_max_muni <- renderInfoBox({
    temp <- max(data_filter()$Valor, na.rm = TRUE)
    temp_muni <- data_filter()[which.max(data_filter()$Valor), ]$name_muni
    infoBox(
      title = "Máximo registrado",
      subtitle = temp_muni,
      value = temp,
      icon = icon("house-medical"),
      color = "orange"
    )
  })
}

# Shiny app ----
shinyApp(ui = ui, server = server)
