library(shiny)
library(sf)
library(fs)
library(tidyverse)
library(wacolors)
library(rlang)
library(bslib)
library(terra)
library(shinyWidgets)
options(shiny.maxRequestSize=30*1024^2) 

st_isnt_empty <- Negate(st_is_empty)

zoom_over <- function(shp, cond = TRUE) {
  bbox <- st_bbox(shp$geometry[rlang::eval_tidy(rlang::enquo(cond), shp)])
  zoom_to(bbox)
}

zoom_to <- function(bbox) {
  coord_sf(xlim = c(bbox$xmin, bbox$xmax),
           ylim = c(bbox$ymin, bbox$ymax))
}

ui <- fluidPage(
  title = 'Dot Density',
  theme = bslib::bs_theme(bootswatch = 'sketchy'),
  
  titlePanel("Dot Density Mapping Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      style = "overflow-y:scroll; max-height: 85vh; position:relative;",
      fileInput("file1", label = "Input File (.rds, .geojson, or .zip)",
                multiple = FALSE,
                accept = c(".rds", '.geojson', ".zip")),
      tags$h5('Dot Properties'),
      selectizeInput("colInput", "Select dot categories:", choices = NULL, selected = NULL,
                     multiple = TRUE,
                     options = list(placeholder = 'No categories selected.')),
      #switchInput('precinctLabelInput', 'Label Precincts?', value = FALSE),
      numericInput('divisorInput', 'People per point:', 250, min = 1, max = 10000),
      selectInput('colorInput', 'Color palette:', choices = names(wacolors), selected = 'lopez'),
      textInput(inputId = 'color_label', label = 'Color Title', value = NULL),
      textInput(inputId = 'color_vector', 'Enter color labels (with commas between)', value = NULL),
      tags$h5("Geographic identifiers"),
      selectizeInput("districtInput", "Select district column:", choices = NULL, selected = NULL,
                     multiple = FALSE,
                     options = list(placeholder = 'No districts selected.')),
      switchInput('labelInput', 'Label Districts?', value = FALSE),
      selectizeInput("precinctInput", "Select precinct label column:", choices = NULL, selected = NULL,
                     multiple = FALSE,
                     options = list(placeholder = 'No precincts labels.')),
      tags$h5("Precinct Fill"),
      selectizeInput("numeratorInput", "Numerator", choices = NULL, selected = 'No fill.', multiple = FALSE,
                     options = list(placeholder = 'No fill.')),
      selectizeInput("denominatorInput", "Denominator", choices = NULL, selected = '1', multiple = FALSE),
      checkboxInput('reverseFillInput', 'Reverse fill to `1 - (Numerator / Denominator)`?', value = FALSE),
      selectInput('fillInput', 'Fill palette:', choices = names(wacolors), selected = 'puget'),
      textInput(inputId = 'fill_label', label = 'Fill Title', value = NULL),
      tags$h5('Zoom'),
      selectizeInput("zoom", "Zoom to districts?", choices = NULL, selected = NULL,
                     multiple = TRUE,
                     options = list(placeholder = 'No zoom selected.')),
    ),
    
    mainPanel(
      plotOutput("dotmap", height = '85vh')
    )
  ),
  hr(),
  a('Palettes from the {wacolors} R package.',
    href = 'https://github.com/CoryMcCartan/wacolors#the-palettes', 
    target="_blank")
)

server <- function(input, output, session) {
  shp <- reactive({
    req(input$file1)
    file1_type <- fs::path_ext(as.character(input$file1$datapath))
    if (file1_type == 'rds') {
      x <- readRDS(input$file1$datapath)
    } else  if (file1_type == 'geojson') {
      x <- sf::st_read(input$file1$datapath)
    } else {
      td <- tempdir()
      utils::unzip(zipfile = input$file1$datapath, exdir = td)
      x <- sf::st_read(fs::dir_ls(td, glob = '*.shp')[1])
    }
    
    updateSelectizeInput(session, 'colInput',
                         choices = names(x)[sapply(x, is.numeric)], 
                         server = TRUE
    )
    
    updateSelectizeInput(session, 'districtInput',
                         choices = c('No districts selected.', names(x)),
                         server = TRUE
    )
    updateSelectizeInput(session, 'precinctInput',
                         choices = c('No precinct labels.', names(x)),
                         server = TRUE
    )
    updateSelectizeInput(session, 'numeratorInput',
                         choices = c('No fill.', names(x)[sapply(x, is.numeric)]),
                         server = TRUE
    )
    updateSelectizeInput(session, 'denominatorInput',
                         choices = c('1', names(x)[sapply(x, is.numeric)]),
                         server = TRUE
    )
    x
  })
  
  pts <- reactive({
    if (is.null(input$colInput)) return(NULL)
    set.seed(1)
    map_dfr(input$colInput, function(col) {
      shp() %>% 
        terra::vect() %>% 
        terra::dots(shp(), field = col, size = input$divisorInput) %>% 
        sf::st_as_sf() %>% 
        mutate(dots_type = col)
    })
  }) %>% debounce(2000)
  
  dists <- reactive({
    if (is.null(input$districtInput) || 'No districts selected.' %in% input$districtInput) return(NULL)
    shp() %>% 
      group_by(!!rlang::sym(input$districtInput)) %>% 
      summarize()
  }) %>% debounce(500)
  
  observe({
    req(input$file1)
    updateSelectizeInput(session, inputId = "zoom",
                         choices = sort(unique(shp()[[input$districtInput]])),
                         selected = NULL
    )
  })
  
  zoomers <- reactive({
    if (is.null(input$zoom) || 'No zoom selected.' %in% input$zoom) return(NULL)
    shp() %>% 
      filter(!!rlang::sym(input$districtInput) %in% input$zoom) %>% 
      zoom_over()
  }) %>% debounce(100)
  
  output$dotmap <- renderPlot({
    p <- ggplot(data = shp())
    
    if (!is.null(shp()) && !is.null(input$numeratorInput) && input$numeratorInput == 'No fill.') {
      p <- p + geom_sf(color = ifelse(is.null(dists()) && input$numeratorInput == 'No fill.', 'black', 'white'), fill = 'grey90', lwd = 0.5) 
    } else {
      if (!input$reverseFillInput) {
        p <- p + geom_sf(color = ifelse(is.null(dists()) && input$numeratorInput == 'No fill.', 'black', 'white'), 
                         data = shp() %>% mutate(`1` = 1, new_fill = .data[[input$numeratorInput]] / .data[[input$denominatorInput]] ), 
                         mapping = aes(fill = new_fill), 
                         lwd = 0.25)
      } else {
        p <- p + geom_sf(color = ifelse(is.null(dists()) && input$numeratorInput == 'No fill.', 'black', 'white'), 
                         data = shp() %>% mutate(`1` = 1, new_fill = 1 - .data[[input$numeratorInput]] / .data[[input$denominatorInput]] ), 
                         mapping = aes(fill = new_fill), 
                         lwd = 0.25)
      }
 
      if (is.null(input$denominatorInput) || input$denominatorInput == '1') {
        p <- p + scale_fill_wa_c(palette = input$fillInput, name = input$fill_label)
      } else {
        p <- p + scale_fill_wa_c(palette = input$fillInput, name = input$fill_label, limits = c(0, 1), labels = scales::percent)
      }
    }
    
    
    
    if (!is.null(dists())) {
      suppressWarnings({
        p <- p +
          geom_sf(data = dists(), fill = NA, color = 'black', lwd = 1)
      })
    }
    
    if (!is.null(shp()) && !is.null(input$precinctInput) && input$precinctInput != 'No precinct labels.') {
      suppressWarnings({
        p <- p +
          geom_sf_text(data = shp(), mapping = aes(label = !!rlang::sym(input$precinctInput)), 
                       alpha = .9, color = 'grey10', size = 2.5)
      })
    }
    
    if (!is.null(pts())) {
      suppressWarnings({
        p <- p +
          geom_sf(data = pts(), aes(color = dots_type))
      })
    }
    
    if (!is.null(dists()) && input$labelInput) {
      suppressWarnings({
        p <- p +
          geom_sf_text(data = dists(), mapping = aes(label = !!rlang::sym(input$districtInput)), 
                       alpha = .7, color = 'black', size = 9)#,
                       #fun.geometry = function(x) sf::st_centroid(sf::st_inscribed_circle(sf::st_geometry(x), dTolerance = .1)) %>% Filter(st_isnt_empty, .))
      })
    }
    
    
    if (!is.null(zoomers())) {
      p <- p + zoomers()
    }
    
    if (input$color_vector == "") {
      p <- p + scale_color_wa_d(palette = input$colorInput, name = input$color_label)
    } else {
      p <- p + scale_color_wa_d(palette = input$colorInput, name = input$color_label,
                                labels = trimws(unlist(strsplit(input$color_vector, ","))))
    }
    
    suppressWarnings({
      p +
        theme_void()
    })
  })
  
}

shinyApp(ui = ui, server = server)
