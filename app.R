library(shiny)
library(shinyWidgets)
library(shinyBS)
library(tidyverse)
library(mapview)
library(leaflet)
library(sf)
library(giscoR)
library(plotly)
library(heatmaply)
library(DT)

theme_set(theme_minimal())

metadata <- read_csv("https://raw.githubusercontent.com/ale-ch/it-fuel-dashboard/main/metadata.csv")

source("https://raw.githubusercontent.com/ale-ch/it-fuel-dashboard/main/transform_dataset.R")

min_date <- min(dataset_long$date)
max_date <- max(dataset_long$date)
regions <- c(NULL, unique(dataset_long$nuts_name))
cor_methods <- c("pearson", "kendall", "spearman")
all_names <- names(dataset)[-(1:6)]
line_plot_names <- names(dataset_long)[!(names(dataset_long) 
                                         %in% c("date", "nuts_name"))]
nuts_names <- names(dataset_nuts)[!(names(dataset_nuts) %in%
                                      c("nuts_level_1", 
                                        "nuts_level_2",
                                        "nuts_level_3"))]

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "yeti"),

  navbarPage(
    "Italy Fuel Sales",
    
    tabPanel(
      "Read Me",
      includeMarkdown("README.md")
    ),
    
    tabPanel("Map", 
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("map_variable", "Select variable to visualize:", 
                             choices = all_names,
                             selected = "highway_diesel_for_engines"),
                
                 h6(textOutput("name_description")),
                 br(),
                 
                 h6(textOutput("name_unit_of_measurement")),
                 br(),
                 
                 airDatepickerInput("date",
                                    label = "Select date:",
                                    value = "2021-01",
                                    maxDate = max_date,
                                    minDate = min_date,
                                    view = "months", 
                                    minView = "months", 
                                    dateFormat = "yyyy-MM"),
                 
                 downloadButton("map_dataset_download", "Download data .tsv"),
                 
                 tags$style(type = 'text/css',
                            ".selectize-input { word-wrap : break-word;}
                          .selectize-input { word-break: break-word;}
                           .selectize-dropdown {word-wrap : break-word;}"),
               ),
               
               mainPanel(
                 width = 9,
                 
                 h4("Interactive Map"),
                 br(),
                 h6("Visualize each variable
                    in the dataset at every location on a specified date. 
                    The representation depends on spatial and temporal coverage of
                    the selected variable. For more information, consult the metadata, 
                    under the 'Data Explorer' tab."
                    ),
                 
                 mapviewOutput("map")
               )
             )
    ),
    
    tabPanel("Analysis",
             br(),
 
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 
                 h5("Line plot"), 
                 "Visualize time series for each location 
                 (only time-varying variables).",
                 br(),
                 br(),
                 
                 airDatepickerInput("daterange",
                                    label = "Date range:",
                                    value = c(min_date, max_date),
                                    maxDate = max_date,
                                    minDate = min_date,
                                    view = "months", 
                                    minView = "months",
                                    range = TRUE,
                                    dateFormat = "yyyy-MM"),
                 
                 selectInput("region", "Select region(s):", regions, multiple = TRUE,
                             selected = "ITALIA"),
                 
                 selectInput("line_plot_variable", "Select variable to visualize:", 
                             choices = line_plot_names,
                             selected = "highway_diesel_for_engines"),
                 
                 h5("Bar plot"),
                 "Visualize geographical patterns for each NUTS 2 region.",
                 br(),
                 br(),
                 
                 selectInput("bar_plot_variable", "Select variable to visualize:", 
                             choices = nuts_names),
                 
                 h5("Scatterplot, Correlation Matrix, Distribution plots"),
                 "Compare variables with each other with the interactive scatterplot and
                 distribution plots. Visualize data patterns with the correlation matrix
                 using different correlation methods.",
                 br(),
                 br(),
                 
                 airDatepickerInput("daterange2",
                                    label = "Select date:",
                                    value = c("2021-01-01", "2021-01-01"),
                                    maxDate = max_date,
                                    minDate = min_date,
                                    view = "months", 
                                    minView = "months",
                                    range = TRUE,
                                    dateFormat = "yyyy-MM"),
                 
                 selectInput("scatterplot_name1", 
                             "Variable 1 (x axis):", 
                             choices = line_plot_names,
                             selected = "highway_diesel_for_engines"),
                 
                 selectInput("scatterplot_name2", 
                             "Variable 2 (y axis):", 
                             choices = line_plot_names,
                             selected = "bank_loans1"),
                 
                 tags$style(type = 'text/css',
                            ".selectize-input { word-wrap : break-word;}
                          .selectize-input { word-break: break-word;}
                           .selectize-dropdown {word-wrap : break-word;}"),
                 
                 selectInput("cor_method", 
                             "Correlation method:", 
                             choices = cor_methods,
                             selected = "pearson"),
                 
                 tags$li(tags$a(href="https://en.wikipedia.org/wiki/Pearson_correlation_coefficient", 
                                paste("Pearson correlation coefficient"))),
                 tags$li(tags$a(href="https://en.wikipedia.org/wiki/Kendall_rank_correlation_coefficient", 
                                paste("Kendall rank correlation coefficient"))),
                 tags$li(tags$a(href="https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient", 
                                paste("Spearman's rank correlation coefficient")))
  
               ),
               
               mainPanel(
                 width = 9,
                 
                 h4("Line plot"),
                 br(),
                 plotlyOutput("line_plot"),
                 br(),
                 
                 h4("Bar plot"),
                 br(),
                 plotlyOutput("bar_plot"),
                 br(),
                 
                 h4("Scatterplot"),
                 br(),
                 plotlyOutput("scatterplot"),
                 br(),
                 
                 h4("Distribution plots"),
                 br(),
                 fluidRow(
                   column(6, plotlyOutput("density1")),
                   column(6, plotlyOutput("density2"))
                 ),
                 br(),
                 
                 fluidRow(
                   column(6, plotlyOutput("surface_plot")),
                   column(6, plotlyOutput("contour_plot"))
                 ),
                 br(),
                 
                 h4("Correlation matrix"),
                 br(),
                 plotlyOutput("corr_matrix"),
                 br(),
               )
             )
    ),
    
    navbarMenu(
      "Data Explorer",
      tabPanel("Dataset",
               br(),
               dataTableOutput("dataset_table"),
               br(),
               downloadButton("dataset_download", "Download .tsv"),
      ),
      
      tabPanel("Metadata",
               br(),
               dataTableOutput("metadata_table"),
               br(),
               downloadButton("metadata_download", "Download .tsv"),
      )
    )
  )
)


server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  dataset_reactive <- reactive(dataset)
  metadata_reactive <- reactive(metadata)
  
  map_dataset_reactive <- reactive({
    nuts3_IT <- gisco_get_nuts(
      year = 2021,
      resolution = 20, 
      nuts_level = 3,
      country = "Italy") %>%
      select(NUTS_ID, NAME_LATN)
    
    nuts3_IT_st <- st_transform(nuts3_IT, 32632)
    
    nuts3_IT_st_data <- nuts3_IT_st %>%
      left_join(dataset, by = c("NUTS_ID" = "nuts_id")) %>% 
      filter(!is.na(nuts_level_2))
    
    map_dataset <- nuts3_IT_st_data %>%
      filter(date == input$date) 
    
    map_dataset
  })
  
  output$name_description <- renderText({
    variable_description <- metadata_reactive() %>% 
      filter(Variable == input$map_variable) %>% 
      select(Description) %>% 
      as.character()
    
    variable_description
  })
  
  
  output$name_unit_of_measurement <- renderText({
    variable_unit_of_measurement <- metadata_reactive() %>% 
      filter(Variable == input$map_variable) %>% 
      select(`Unit Of Measurement`) %>% 
      as.character()
    
    variable_unit_of_measurement
  })
  
  output$map <- renderLeaflet({
    variable_description <- metadata_reactive() %>% 
      filter(Variable == input$map_variable) %>% 
      select(Description) %>% 
      as.character()
    
    variable_unit_of_measurement <- metadata_reactive() %>% 
      filter(Variable == input$map_variable) %>% 
      select(`Unit Of Measurement`) %>% 
      as.character()
    
    m <- map_dataset_reactive() %>% 
      mutate(
        description = variable_description,
        unit_of_measurement = variable_unit_of_measurement
      ) %>% 
      select(
        NUTS_ID,
        all_of(c("nuts_level_3", "nuts_level_2", "nuts_level_1")),
        date,
        all_of(input$map_variable),
        description,
        unit_of_measurement
      ) %>% 
      st_as_sf() %>% 
      mapview(zcol = input$map_variable)
    
    m@map
  })
  
  output$map_dataset_download <- downloadHandler(
    filename = "map_dataset.tsv", 
    content = function(file) {
      write_tsv(dataset_reactive() %>% 
                  filter(date == input$date) %>% 
                  select(date, nuts_id, 
                         nuts_level_2, nuts_level_3, 
                         nuts_level_1, nuts_level_0, 
                         all_of(input$map_variable)), file)
    }
  )
  
  output$line_plot <- renderPlotly({
    name <- input$line_plot_variable
    dataset_long %>% 
      filter(nuts_name %in% input$region, 
             date >= input$daterange[1],
             date <= input$daterange[2]) %>%
      select(date, nuts_name, value = all_of(name)) %>% 
      plot_ly(type = "scatter", mode = "lines", 
              color = ~nuts_name) %>% 
      add_trace(x = ~date, y = ~value) %>% 
      layout(showlegend = TRUE, 
             yaxis = list(title = name))
    
  })
  
  output$bar_plot <- renderPlotly({
    name <- input$bar_plot_variable
    
    dataset_nuts %>% 
      select(nuts_level_2, variable = all_of(name)) %>%
      count(nuts_level_2, variable) %>% 
      group_by(nuts_level_2) %>% 
      mutate(prop = n / sum(n)) %>% 
      select(-n) %>% 
      plot_ly(x = ~nuts_level_2, y = ~prop, 
              color = ~variable, type = "bar") %>% 
      layout(yaxis = list(title = name), 
             xaxis = list(title = ""), 
             barmode = 'stack')
    
  })
  
  
  output$scatterplot <- renderPlotly({
    name1 <- input$scatterplot_name1
    name2 <- input$scatterplot_name2
    
    dataset %>% 
      filter(
        date >= input$daterange2[1],
        date <= input$daterange2[2]
        ) %>% 
      select(name1 = all_of(name1), 
             name2 = all_of(name2), 
             nuts_level_1
      ) %>% 
      plot_ly(x = ~name1, y = ~name2, color = ~nuts_level_1) %>% 
      layout(
        xaxis = list(title = name1), 
        yaxis = list(title = name2)
      )
  })
  
  output$corr_matrix <- renderPlotly({
    df <- dataset %>% 
      filter(
        date >= input$daterange2[1],
        date <= input$daterange2[2]
        ) %>% 
      select(all_of(line_plot_names)) %>%
      select(where(~ !any(is.na(.)))) %>% 
      select(where(~ sd(.) != 0))
    
    # Correlation matrix (numeric variables)
    heatmaply_cor(
      cor(df, method = input$cor_method),
      symm = TRUE,
      cexRow = .0001, 
      cexCol = .0001, 
      branches_lwd = .1,
      showticklabels = c(FALSE, FALSE)
    ) %>%
      layout(
        title = paste("Correlation matrix", "-", input$cor_method))
  })
  
  output$density1 <- renderPlotly({
    name1 <- input$scatterplot_name1
    
    df <- dataset %>% 
      filter(
        date >= input$daterange2[1],
        date <= input$daterange2[2]
        ) %>% 
      select(name1 = all_of(name1))
    
    x <- df$name1
    fit <- density(x)
    
    plot_ly(x = x, type = "histogram", name = "Histogram") %>% 
      add_trace(x = fit$x, y = fit$y, 
                type = "scatter", mode = "lines", 
                fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
      layout(
        yaxis = list(title = name1),
        yaxis2 = list(overlaying = "y", side = "right"))
    
  })
  
  output$density2 <- renderPlotly({
    name2 <- input$scatterplot_name2
    
    df <- dataset %>% 
      filter(
        date >= input$daterange2[1],
        date <= input$daterange2[2]
      ) %>% 
      select(name2 = all_of(name2))
    
    x <- df$name2
    fit <- density(x)
    
    plot_ly(x = x, type = "histogram", name = "Histogram") %>% 
      add_trace(x = fit$x, y = fit$y, 
                type = "scatter", mode = "lines", 
                fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
      layout(
        yaxis = list(title = name2),
        yaxis2 = list(overlaying = "y", side = "right"))
  })
  
  output$contour_plot <- renderPlotly({
    df <- dataset %>% 
      filter(
        date >= input$daterange2[1],
        date <= input$daterange2[2]
        ) %>% 
      select(V1 = all_of(input$scatterplot_name1), 
             V2 = all_of(input$scatterplot_name2)) %>% 
      na.omit()
    
    kd <- with(df, MASS::kde2d(V1, V2, n = 50))
    
    # contour plot
    plot_ly(z = kd$z, type = "contour")
  })
  
  output$surface_plot <- renderPlotly({
    df <- dataset %>% 
      filter(
        date >= input$daterange2[1],
        date <= input$daterange2[2]
      ) %>% 
      select(V1 = all_of(input$scatterplot_name1), 
             V2 = all_of(input$scatterplot_name2)) %>% 
      na.omit()
    
    kd <- with(df, MASS::kde2d(V1, V2, n = 50))
    
    # surface plot
    plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% 
      add_surface()
  })
  
  output$dataset_download <- downloadHandler(
    filename = "dataset.tsv", 
    content = function(file) {
      write_tsv(dataset_reactive(), file)
    }
  )
  
  output$dataset_table <- renderDataTable(datatable(dataset, filter = "top"))
  
  
  output$metadata_download <- downloadHandler(
    filename = "metadata.tsv", 
    content = function(file) {
      write_tsv(metadata_reactive(), file)
    }
  )
  
  output$metadata_table <- renderDataTable(datatable(metadata,
                                                     filter = "top"))
  
}

shinyApp(ui = ui, server = server)

