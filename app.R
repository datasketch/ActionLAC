library(shinypanels)
library(shiny)
library(shinyinvoer)
library(hgchmagic)
library(DT)
library(googlesheets4)
library(shinycustomloader)

sheets_deauth()

# Data
sheets_get("1XvasoAL84X0--vArR6WWTAJASo2iL5bR88vTTXWYtQE")
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
  font-weight: 500;
  font-size: 15px;
  letter-spacing: 0.07em;
  color: #9FBD36;
  margin: 0px 0px;
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

h3#panel-filtros {
 color:  #9FBD36;
}

.panel#info-cont .panel-content{
width: 100%;
height: 100%;
display: flex;
justify-content: center;
align-items: flex-start;
}

.panel#info-cont {
border-top: 2px solid  #1980A6;
}

.panel-head#close_remove {
 display: none;
}



#id-but-mod {
    white-space: nowrap;
    transform: rotate(-90deg) translateX(-43%);
    position: absolute;
    background: transparent;
    border: 0px;
    font-size: 17px;
    cursor: pointer;
    color: #1980A6;
    letter-spacing: 0.05em;
     text-transform: uppercase;
}

#body-info {
    display: contents;
}

.head-modal {
 color : #ffffff !important;
 font-size: 35px;
  padding: 25px 0px;
}

.info-tool {
 display: flex;
}


.tooltip-inf {
 cursor: pointer;
 position: relative;
 margin-left: 3px;
}

.tooltip-inf .tooltiptext {
  visibility: hidden;
  width: 170px;
  higth: auto;
  background-color: #fafafa;
  color: #9FBD36;
  position: absolute;
  z-index: 9999;
  top: 0;
  padding: 1rem;
  border: 1px solid #ccc;
  font-weight: 400;
  letter-spacing: normal;
  font-size: 0.75rem;
}

.tooltip-inf:hover .tooltiptext {
  visibility: visible;
}

"
ui <- panelsPage( styles = styles,
                   header =  div(style="", class="head", "Acciones Climáticas LAC"
                   ),
                   panel(title = h3(id = "panel-filtros", ' FILTROS DE BÚSQUEDA'), color = "olive", collapsed = FALSE, width = 250,
                         head = NULL,
                         body = withLoader(list(
                           uiOutput('herramientas'),
                           uiOutput('area'),
                           uiOutput('tipo'),
                           uiOutput('actor'),
                           uiOutput('institucion')
                         ), loader = "loader1")
                   ),
                   panel(title = h3('FILTRA POR PAÍS'), color = "olive", collapsed = FALSE, width = 350,
                         head = NULL,
                         body = list(
                            uiOutput('pais'),
                            withLoader(highchartOutput('mapa', height = 500),
                            loader = "loader1"))
                   ),
                   panel(title =  uiOutput('title_info'), color = "olive", collapsed = FALSE, width = 450,
                         head = NULL,
                         body = withLoader(list(
                           highchartOutput('barras'),
                           dataTableOutput('data_view')
                         ), loader = "loader1")
                   )
)

server <- function(input, output, session) {
  
  
  info <- reactive({
    info <- read_sheet("1XvasoAL84X0--vArR6WWTAJASo2iL5bR88vTTXWYtQE")
    info$País <- iconv(info$País,to="ASCII//TRANSLIT")
    
    info <- info %>% filter(País %in% c("Argentina", "Bolivia", "Paraguay", "Chile", "Brasil", "Ecuador", "Costa Rica", "Guatemala", "Mexico",
                                        "Venezuela", "El Salvador", "Peru"))
    
    info <- info[!grepl('no|No|NO|nO',info$`Ya incluir en el mapa?`),]
    info
  })

  
  
  # filtros
  
  output$pais <- renderUI({
    list_p <- sort(unique(info()$País))
    
    div(
      HTML('<div class= "info-tool title-filter"> PAÍS </div>'),
    selectizeInput('name_pais',  ' ', list_p, selected = NULL, multiple = T,
                   options = list(
                     placeholder = "Todos los países",
                     plugins= list('remove_button'))
    ))
  })
  
  output$title_info <- renderUI({
    pais <- input$name_pais
    
    if (is.null(pais)) {
      tx <- 'Todos los países'
    } else {
      tx <- paste0(pais, collapse = ' - ')
    } 
    
    HTML(paste0('<h3>', tx,  '</h3>'))
  })
  
  data_pais <- reactive({
    pais <- input$name_pais
    if (is.null(pais)) {
      dt <- info()
    } else {
      dt <- info() %>% filter(País %in% pais)
    }
    dt
  })
  
  
  output$herramientas <- renderUI({
    dt <- data_pais()
    if (is.null(dt)) return()
    list_h <- sort(unique(dt$Herramienta))
    div(
      HTML('<div class= "info-tool title-filter"> HERRAMIENTA  <div class="tooltip-inf"> <i class="fa fa-info-circle"></i><span class="tooltiptext">Herramienta por medio de la cuál se ha relevado el compromiso de acción climática</span</div></div></div>'),
    selectizeInput('name_tool',  ' ', selected = NULL, multiple = T, list_h,  options = list(
      placeholder = "Todas las herramientas",
      plugins= list('remove_button'))
    ))
  })
  
  data_herramientas <- reactive({
    herramienta <- input$name_tool
    
    if (is.null(herramienta)) {
      dt <- data_pais()
    } else {
      dt <- data_pais() %>% filter(Herramienta %in% herramienta) 
    }
    
    dt$Herramienta[is.na(dt$Herramienta)] <- 'Sin inf'
    dt %>% filter(Herramienta != 'Sin inf')
  })
  
  output$area <- renderUI({
    dt <- data_herramientas()
    if (is.null(dt)) return()
    list_a <- unique(dt$`Área de Acción`)
    list_a <- sort(list_a[!is.na(list_a)])
    
    div(
      HTML('<div class= "info-tool title-filter"> ÁREA DE ACCIÓN <div class="tooltip-inf"> <i class="fa fa-info-circle"></i><span class="tooltiptext">Se refiere al área de impacto de la acción climática</span</div></div></div>'),
    selectizeInput('name_area',  ' ', list_a, 
                   selected = NULL, multiple = T, 
                   options = list(
                   placeholder = "Todas las áreas de acción",
                   plugins= list('remove_button'))
    ))
  })
  
  
  data_area <- reactive({
    area <- input$name_area
    if (is.null(area)) { 
      dt <- data_herramientas()
    } else {
      dt <- data_herramientas() %>% filter(`Área de Acción` %in% area) 
    }
    
    dt$`Área de Acción`[is.na(dt$`Área de Acción`)] <- 'Sin inf'
    dt %>% filter(`Área de Acción` != 'Sin inf')
  })
  
  output$tipo <- renderUI({
    dt <- data_area()
    if (is.null(dt)) return()
    list_t <- unique(dt$`Tipo de Acción`)
    list_t <- sort(list_t[!is.na(list_t)])
    
    div(
      HTML('<div class= "info-tool title-filter"> TIPO DE ACCIÓN  <div class="tooltip-inf"> <i class="fa fa-info-circle"></i><span class="tooltiptext">Se refiere al tipo de estrategia llevada a cabo para hacer frente al cambio climático </span</div></div></div>'),
    selectizeInput('name_tipo',  ' ', list_t, selected = NULL, multiple = T, options = list(
      placeholder = "Todos los tipos de acción",
      plugins= list('remove_button'))
    ))
  })
  
  data_tipo <- reactive({
    tipo <- input$name_tipo
    if (is.null(tipo)) {
      dt <- data_area() 
    } else {
      dt <- data_area() %>% filter(`Tipo de Acción` %in% tipo) 
    }
    
    dt$`Tipo de Acción`[is.na(dt$`Tipo de Acción`)] <- 'Sin inf'
    dt %>% filter(`Tipo de Acción` != 'Sin inf')
  })
  
  
  output$actor <- renderUI({
    dt <- data_tipo()
    if (is.null(dt)) return()
    list_ac <-  sort(unique(dt$`Tipo de Actor`))
    selectizeInput('name_actor',  HTML('<p class = "title-filter">TIPO DE ACTOR</p>'), list_ac, selected = NULL, multiple = T, options = list(
      placeholder = "Todos los actores",
      plugins= list('remove_button'))
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
    list_i <- sort(unique(dt$`Instituición o red`))
    
    # div(
    #   HTML('<p class = "title-filter">INSTITUCIÓN</p>'),
    #   searchInput('name_inst', data =  list_i, placeholder = 'Búsqueda por Institución o red')
    # )
    div(
      HTML('<div class= "info-tool title-filter"> INSTITUCIÓN / MUNICIPALIDAD </div>'),
    selectizeInput('name_inst', ' ', list_i, selected = NULL, multiple = T, options = list(
      placeholder = "Todos institución/municipalidad",
      plugins= list('remove_button'))
    ))
  })
  
  
  
  data_filter <- reactive({
    dt <- data_actor()
    inst <- input$name_inst
    if (is.null(inst)) {
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
    
    dt_p <- info()
    if (is.null(dt_p)) return()
    dt_p <- dt_p %>% group_by(name = País) %>% summarise(z = n())
  
    if (!is.null(pais)) {
      dt_f <- dt_p %>% filter(!name %in% pais) 
      dt_f$color <- '#b3ddec'
      dt_i <- dt_p %>% filter(name %in% pais) 
      dt_i$color <- '#1980A6'
      dt_p <- bind_rows(dt_i, dt_f)
    }
    
    myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.name, timestamp: new Date().getTime()});}")
    
    highchart(type = "map") %>%
      hc_chart(backgroundColor = "transparent",
               margin= c(0,0,0,15),
               spacingTop = 0,
               spacingBottom = 0,
               spacingLeft = 0,
               spacingRight= 0,
               style = list(
                 fontFamily= 'Open Sans'
               )) %>%
      hc_add_series_map(map = mapLam, showInLegend = FALSE, nullColor = "#CCCCCC",
                        borderWidth = 1, borderColor = '#fff',
                        df = dt_p,  value = "z", joinBy = "name",
                        allowPointSelect = TRUE,
                        tooltip= list(
                          headerFormat= '',
                          pointFormat='<b>{point.name}</b><br>{point.value}'
                        )) %>%
      hc_legend(enabled = FALSE) %>%
      hc_colorAxis(minColor = "#b3ddec", maxColor = "#1980A6") %>%
      hc_plotOptions(series = list(states = list(hover = list(color = '#1980A6'),
                                                 select = list(color = '#1980A6')), allowPointSelect = F,
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
    dt <- dt %>% select(País, `Acciones climáticas`, Progreso, `Instituición o red`)
    #dt <- Filter(function(x) !all(is.na(x)), dt) 
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
                      "$(this.api().table().header()).css({'background-color': '#1980A6', 'color': '#fff'});",
                      "}"),
                    searching = FALSE
                  )) %>% 
      formatStyle( 0 , target= 'row',color = '#0A446B', fontSize ='13px', lineHeight='15px')
  })
  
  output$barras <- renderHighchart({
    dt <- data_filter()
    if (is.null(dt)) return()
    dt <- dt %>%
      group_by(`Área de Acción`) %>%
      summarise(`Total Acciones Climáticas por área de acción` = n())
    hgch_bar_CatNum(dt, opts = list(orientation = 'hor', sort = 'desc',
                                    horLabel = ' ', verLabel = ' ', agg_text = ' ',
                                    title = 'Total Acciones Climáticas por área de acción',
                                    theme = tma(custom = list(stylesX_lineWidth = 0, 
                                                              #height = 570,
                                                              colors = '#9FBD36',
                                                              font_family = "Raleway",
                                                              font_size = '11px'))))

  })
  
}

shinyApp(ui, server)

