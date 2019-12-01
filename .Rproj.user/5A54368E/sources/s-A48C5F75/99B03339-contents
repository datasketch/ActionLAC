library(shiny)
library(tidyverse)
library(dsCustom)
library(highcharter)
library(DT)
# Data
info <-  read_csv('data/ActionLAC_filter.csv', col_types = cols(.default = "c"))

mapLam <- jsonlite::fromJSON("data/latin-america.json", simplifyVector = FALSE)

ui <- 
  fluidPage(
    suppressDependencies("bootstrap"),
    tags$head(
      tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
      tags$link(rel="stylesheet", type="text/css", href="styles.css"),
      includeScript("js/iframeSizer.contentWindow.min.js")
    ),
    div ( class = 'head',
          'Mapa Acción LAC'
    ),
    
    div (class = 'results',
         div (class = 'help panel',
              uiOutput('ayuda')),
         div ( class = 'filters panel',
               h3('FILTROS DE BÚSQUEDA'),
               uiOutput('pais'),
               uiOutput('herramientas'),
               uiOutput('area'),
               uiOutput('tipo'),
               uiOutput('actor'),
               uiOutput('institucion')),
         div (class = 'map panel',
              h3('RESULTADOS AVANZADOS'),
              highchartOutput('mapa', height = 570),
         ),
         div (class = 'info panel',
              uiOutput('title_info'),
              dataTableOutput('data_view')
              #uiOutput('text_info')
         )
    )
    
  )

server <-
  function(input, output, session) {
    
    
    # Modal
    
    output$ayuda <- renderUI({
      actionButton('info_ayuda', 'INFORMACIÓN DE AYUDA', icon = icon('info-circle'))
    })
    
    # filtros
    
    output$pais <- renderUI({
      list_p <- unique(info$País)
      selectizeInput('name_pais',  HTML('<p class = "title-filter">PAÍS</p>'), list_p, selected = NULL, multiple = T,
                     options = list(
                       placeholder = "Todos los países",
                       maxItems = 1)
      )
    })
    
    output$title_info <- renderUI({
      pais <- input$name_pais
      
      if (is.null(pais)) {
        tx <- 'Todos los países'
      } else {
        tx <- pais
      } 
      
      HTML(paste0('<h3>', tx,  '</h3>'))
    })
    
    data_pais <- reactive({
      pais <- input$name_pais
      if (is.null(pais)) {
        dt <- info
      } else {
        dt <- info %>% filter(País == pais)
      }
      dt
    })
    
    
    output$herramientas <- renderUI({
      dt <- data_pais()
      if (is.null(dt)) return()
      list_h <- unique(dt$Herramienta)
      selectizeInput('name_tool',  HTML('<p class = "title-filter">HERRAMIENTA</p>'), selected = NULL, multiple = T, list_h,  options = list(
        placeholder = "Todas las herramientas",
        maxItems = 1)
      )
    })
    
    data_herramientas <- reactive({
      herramienta <- input$name_tool
      
      if (is.null(herramienta)) {
        dt <- data_pais()
      } else {
        dt <- data_pais() %>% filter(Herramienta %in% herramienta) 
      }
      
      dt 
    })
    
    output$area <- renderUI({
      dt <- data_herramientas()
      if (is.null(dt)) return()
      list_a <- unique(dt$`Área de Acción`)
      selectizeInput('name_area',  HTML('<p class = "title-filter">ÁREA DE ACCIÓN</p>'), list_a, selected = NULL, multiple = T, options = list(
        placeholder = "Todas las áreas de acción",
        maxItems = 1)
      )
    })
    
    
    data_area <- reactive({
      area <- input$name_area
      if (is.null(area)) { 
        dt <- data_herramientas()
      } else {
        dt <- data_herramientas() %>% filter(`Área de Acción` %in% area) 
      }
      
      dt
    })
    
    output$tipo <- renderUI({
      dt <- data_area()
      if (is.null(dt)) return()
      list_t <- unique(dt$`Tipo de Acción`)
      selectizeInput('name_tipo',  HTML('<p class = "title-filter">TIPO DE ACCIÓN</p>'), list_t, selected = NULL, multiple = T, options = list(
        placeholder = "Todas los tipos de acción",
        maxItems = 1)
      )
    })
    
    data_tipo <- reactive({
      tipo <- input$name_tipo
      if (is.null(tipo)) {
        dt <- data_area() 
      } else {
        dt <- data_area() %>% filter(`Tipo de Acción` %in% tipo) 
      }
      
      dt 
    })
    
    
    output$actor <- renderUI({
      dt <- data_tipo()
      if (is.null(dt)) return()
      list_ac <-  unique(dt$`Tipo de Actor`)
      selectizeInput('name_actor',  HTML('<p class = "title-filter">TIPO DE ACTOR</p>'), list_ac, selected = NULL, multiple = T, options = list(
        placeholder = "Todas los actores",
        maxItems = 1)
      )
    })
    
    data_actor <- reactive({
      actor <- input$name_actor
      if (is.null(actor)) {
        dt <- data_tipo()
      } else {
        dt <- data_tipo() %>% filter(`Tipo de Actor` %in% actor)
      }
      
      dt
    })
    
    
    output$institucion <- renderUI({
      dt <- data_tipo()
      if (is.null(dt)) return()
      list_i <- unique(dt$`Instituición o red`)
      
      div(
        HTML('<p class = "title-filter">INSTITUCIÓN</p>'),
        searchInput('name_inst', data =  list_i, placeholder = 'Búsqueda por Institución o red')
      )
    })
    
    
    
    data_filter <- reactive({
      dt <- data_actor()
      inst <- input$name_inst
      if (is.null(inst)) return()
      
      if (inst == "") {
        dt <- dt 
      } else {
        dt <- dt %>% filter(`Instituición o red` %in% inst)  
      }
      dt
    })
    
    
    observeEvent(input$hcClicked, {
      if (is.null(input$hcClicked)) return()
      updateSelectizeInput(session, 'name_pais', selected = input$hcClicked)
        
    })
    
    
    output$mapa <- renderHighchart({
      pais <- input$name_pais
      
      dt_p <- data_filter() 
      if (is.null(dt_p)) return()
      
      if (is.null(pais)) {
        dt_p <- dt_p %>% group_by(name = País) %>% summarise(z = n())
      } else {
        dt_p <- data.frame(name = pais, z = 15, color = '#1980A6') 
      }
      
      myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.name, timestamp: new Date().getTime()});}")
      
      highchart(type = "map") %>%
        hc_chart(backgroundColor = "transparent", 
                 style = list(
                   fontFamily= 'Open Sans'
                 )) %>%
        hc_add_series_map(map = mapLam, showInLegend = FALSE, nullColor = "#CCCCCC",
                          borderWidth = 1, borderColor = '#fff',
                          df = dt_p,  value = "z", joinBy = "name",
                          allowPointSelect = TRUE,
                          tooltip= list(
                            headerFormat= '',
                            pointFormat='<b>{point.name}</b>'
                          )) %>% 
        hc_legend(enabled = FALSE) %>% 
        hc_colorAxis(minColor = "#b3ddec", maxColor = "#1980A6") %>% 
        hc_plotOptions(series = list(states = list(hover = list(color = '#FFCDAA'), 
                                                   select = list(color = '#1980A6')), allowPointSelect = TRUE, 
                                     cursor = 'pointer', events = list(click = myFunc)))
    })
    
    output$text_info <- renderUI({
      data_info <- data_filter()
      if (is.null(data_info)) return()
      
      
      l<- map(1:nrow(data_info), function(i) {
                    if (identical(data_info[i,]$`Acciones climáticas`, character(0))) return('')
                   tx <- HTML(paste0('<p>', unique(data_info[i,]$`Acciones climáticas`), '</p>'))
                   tx
                  })
      l
    })
    
    
    output$data_view <- renderDataTable({
      dt <- data_filter()
      if (is.null(dt)) return()
      dt <- dt %>% select(País, `Acciones climáticas`)
      dt
    })
    
  }

shinyApp(ui, server)