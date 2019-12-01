library(dsAppLayout)
library(shiny)
library(tidyverse)
library(dsCustom)
library(hgchmagic)
library(DT)

# Data
info <-  read_csv('data/ActionLAC_filter.csv', col_types = cols(.default = "c"))
mapLam <- jsonlite::fromJSON("data/latin-america.json", simplifyVector = FALSE)


styles <- 
  "
@import url('https://fonts.googleapis.com/css?family=Raleway:400,800&display=swap');

*,
*:before,
*:after{
  box-sizing: inherit;
}

html {
  box-sizing: border-box;
  font-family: 'Raleway', sans-serif;
}

body {
  margin: 0;
  background-color: #F0F0F0
}

.head {
  background: #9FBD36;
  text-align: center;
  color: #FFFFFF;
  font-size: 35px;
  padding: 35px 0px;
}

.modal-title {
  background: #9FBD36;
}

.modal-wrapper {
    overflow: auto;
    height: 500px;
    width: 70%
}

.img-modal {
display: flex;
justify-content: center;
}

.img-cont {
 margin-left: 15px;
 border-radius: 50%;
}

.cont-modal {
padding: 0px 150px;
    text-align: center;
}

h3 {
  font-weight: 800;
  font-size: 15px;
  letter-spacing: 0.05em;
  margin: 0px 0px;
  text-transform: uppercase;
  color: #1980A6;
}

.title-filter {
  font-weight: 700;
  font-size: 17px;
  letter-spacing: 0.05em;
  text-transform: uppercase;
  color: #9FBD36;
  margin: 1px 0px;
}

.selectize-input,.input-autosuggest,.input-autosuggest input {
  border: 1px solid #F0F0F0 !important;
  background: #F0F0F0 !important; 
  color: #333333;
  font-family: inherit;
  font-size: inherit;
  line-height: 20px;
}

.input-autosuggest input {
  padding: 0px 10px !important;
}

.panel {
border-top: 2px solid  #9FBD36;
}

::-webkit-scrollbar{
  width: 4px;
  height: 4px;
}

::-webkit-scrollbar-track{
  background: #e2eaed;
}

::-webkit-scrollbar-thumb{
  background: #CCCCCC;
}

::-webkit-scrollbar:focus {
    overflow: scroll;
    display:block;
    background: #CCCCCC;
}

"
ui <- dsAppPanels( styles = styles,
                   header =  div(style="", class="head", "Mapa Acción LAC"
                   ),
                   panel(title = modalBtn(modal_id = 'info_modal', label = 'Información de ayuda'), 
                         color = "olive", collapsed = FALSE, width =  50),
                   modal(id = 'info_modal',
                         title = div( class="head-modal", "Mapa Acción LAC"
                         ),
                         div(class = 'cont-modal',
                         div(
                             p("Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam
                                nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam
                                erat volutpat. Ut wisi enim ad minim veniam, quis")
                         ),
                         div(class = "img-modal",
                             img(class = "img-cont", src = "https://fakeimg.pl/200/"),
                             img(class = "img-cont", src = "https://fakeimg.pl/200/"),
                             img(class = "img-cont", src = "https://fakeimg.pl/200/"),
                             img(class = "img-cont", src = "https://fakeimg.pl/200/"))
                   )),
                   panel(title = h3('FILTROS DE BÚSQUEDA'), color = "olive", collapsed = FALSE, width = 350,
                         head = NULL,
                         body = list(
                                 uiOutput('pais'),
                                 uiOutput('herramientas'),
                                 uiOutput('area'),
                                 uiOutput('tipo'),
                                 uiOutput('actor'),
                                 uiOutput('institucion')
                         )
                   ),
                   panel(title = h3('RESULTADOS AVANZADOS'), color = "olive", collapsed = FALSE, width = 500,
                         head = NULL,
                         body = list(
                           highchartOutput('mapa', height = 510)
                         )
                   ),
                   panel(title =  uiOutput('title_info'), color = "olive", collapsed = FALSE, width = 550,
                         head = NULL,
                         body = list(
                           highchartOutput('barras'),
                           dataTableOutput('data_view')
                         )
                   )
)

server <- function(input, output, session) {
  
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
    DT::datatable(dt,   
                  rownames = F,
                  options = list(
                    pageLength = 5, 
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                    lengthChange = F,
                    scrollX = T,
                    scrollY = T,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#9FBD36', 'color': '#fff'});",
                      "}"),
                    searching = FALSE
                  )) %>% 
      formatStyle( 0 , target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')
  })
  
  output$barras <- renderHighchart({
    dt <- data_filter()
    if (is.null(dt)) return()
    dt <- dt %>% 
           group_by(País) %>% 
             summarise(`Total Acciones climáticas` = n())
    hgch_bar_CatNum(dt, opts = list(orientation = 'hor', sort = 'desc', verLabel = ' '))
    
  })
  
}

shinyApp(ui, server)

