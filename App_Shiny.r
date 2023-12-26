# ---- Librerias ----
library("dplyr")
library("tidyr")
library("plotly")
library("DT")
library("shiny")
library("shinydashboard")

# ---- Carga de datos ----
tabla <- read.csv("tabla.csv", sep = ",") %>% mutate(nombre_region = factor(nombre_region, levels = unique(nombre_region)), periodo = factor(periodo, levels = unique(periodo)))
tabla_toi <- tabla %>% select(c(nombre_region, periodo, toi)) %>% mutate(toi = toi) %>% spread(key = periodo, value = toi) %>% rename(Región = nombre_region)
tabla_to <- tabla %>% select(c(nombre_region, periodo, to)) %>% mutate(to = to) %>% spread(key = periodo, value = to)  %>% rename(Región = nombre_region)
tabla_tp <- tabla %>% select(c(nombre_region, periodo, tp)) %>% mutate(tp = tp) %>% spread(key = periodo, value = tp)  %>% rename(Región = nombre_region)
tabla_td <- tabla %>% select(c(nombre_region, periodo, td)) %>% mutate(td = td) %>% spread(key = periodo, value = td)  %>% rename(Región = nombre_region)
tabla_Ocupados <- tabla %>% select(c(nombre_region, periodo, Ocupados)) %>% spread(key = periodo, value = Ocupados) %>% rename(Región = nombre_region)
tabla_Desocupados <- tabla %>% select(c(nombre_region, periodo, Desocupados)) %>% spread(key = periodo, value = Desocupados) %>% rename(Región = nombre_region)
tabla_FT <- tabla %>% select(c(nombre_region, periodo, FDT)) %>% spread(key = periodo, value = FDT) %>% rename(Región = nombre_region)
tabla_PET <- tabla %>% select(c(nombre_region, periodo, PET)) %>% spread(key = periodo, value = PET) %>% rename(Región = nombre_region)
tabla_FFT <- tabla %>% select(c(nombre_region, periodo, FFT)) %>% spread(key = periodo, value = FFT) %>% rename(Región = nombre_region)
tabla_OI <- tabla %>% select(c(nombre_region, periodo, OI)) %>% spread(key = periodo, value = OI) %>% rename(Región = nombre_region)
tabla_sexos <- read.csv("tabla_sexos.csv") %>% mutate(nombre_region = factor(nombre_region, levels = unique(nombre_region)), periodo = factor(periodo, levels = unique(periodo)))
tabla_barras <- tabla %>% filter(periodo == levels(tabla$periodo)[length(levels(tabla$periodo))])
tabla_barras2 <- tabla_barras[tabla_barras$nombre_region != "Total país", ]
tabla_barras3 <- tabla_barras[tabla_barras$nombre_region == "Total país", ]

nombre_regiones <- c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", "O'Higgins", "Maule", "Biobío", "Araucanía", "Los Lagos", "Aysén", "Magallanes", "Metropolitana", "Los Ríos", "Arica y Parinacota", "Ñuble", "Total país")

# Funciones
tabla_filtrar <- function(ind, tabla, variable_diferencia){
  tabla_aux <- tabla %>% select(c(nombre_region, periodo, variable_diferencia))
  aux <- nombre_regiones[ind]
  tabla_filtrada <- subset(tabla_aux, nombre_region %in% aux)
  return(tabla_filtrada)
}

tabla_filtrar_bar <- function(ind, tabla){
  tabla$nombre_region <- as.character(tabla$nombre_region)
  aux <- nombre_regiones[ind]
  tabla_filtrada_bar  <- subset(tabla, nombre_region %in% aux)
  return(tabla_filtrada_bar)
}

tabla_filtrar_sexo <- function(ind, tabla){
  aux <- nombre_regiones[ind]
  tabla_sexos_filtrada <- tabla %>% filter(tabla$nombre_region == aux)
  return(tabla_sexos_filtrada)
}

tabla_traspuesta <- function(tabla){
  datatable(tabla, rownames = FALSE, class = "compact", extensions = c('Buttons', 'FixedColumns'), 
            options = list(dom = 'Brt', autoWidth = FALSE, fixedColumns = list(lePETColumns = 1),
              buttons = list(list(extend = 'csv', text = 'Descargar CSV'), list(extend = 'excel', text = 'Descargar Excel')),
              scrollX = TRUE, pageLength = 17, lengthMenu = FALSE,
              drawCallback = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#18137d', 'color': 'white'});",
                "  $(this.api().table().header()).find('th').css({'background-color': '#18137d',",
                "    'border-lePET': '2px solid gray','border-right': '2px solid white'",
                "  });",
                "  $(this.api().table().body()).find('tr').each(function() {",
                "    $(this).find('td').css({'border-lePET': '1px solid gray','border-right': '1px solid gray'});",
                "  });",
                "}")
            ))
}

modificar_vector <- function(ind) {
  if (length(ind) > 1) {
    return(ind)
  } else {
    if (ind < 16) {
      ind <- c(ind, ind + 1)
    } else {
      ind <- c(ind, 1)
    }
    return(ind)
  }
}

# ---- Logo ----

mis_colores <- c("#FF0000", "#00FF00", "#0000FF", "#c7c700", "#ff9100", "#8400ff", "#610f04", "#db02d8", "#000057", "#009161", "#d1026d", "#913e03", "#034a4a", "#013b0f", "#3a634a", "#4a492a", "#000000")

image_file <- "Logo_INE.png"
txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")

# ---- ui ----
ui <- dashboardPage(
  dashboardHeader(title = "Mercado Laboral",
                  tags$li(a(href = 'https://www.facebook.com/ChileINE/', icon("facebook", class = "fa fa-facebook"), title = "Facebook"), class = "dropdown"),
                  tags$li(a(href = 'https://twitter.com/ine_chile?lang=es', icon("twitter", class = "fa fa-twitter"), title = "Twitter"), class = "dropdown"),
                  tags$li(a(href = 'https://www.youtube.com/user/inechile', icon("youtube", class = "fa fa-youtube"), title = "YouTube"), class = "dropdown"),
                  tags$li(a(href = 'https://www.instagram.com/chile.ine/', icon("instagram", class = "fa fa-instagram"), title = "Instagram"), class = "dropdown"),
                  tags$li(a(href = 'https://www.ine.gob.cl/', img(src = "https://www.ine.gob.cl/images/default-source/base-ui/ine_blanco.svg", title = "Logo empresarial", height = "30px"), style = "padding-top:10px; padding-bottom:5px;"), class = "dropdown")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(" Inicio", tabName = "ML", icon = icon("home")),
      tags$hr(style = "border-top: 0.5px solid #ccc; margin-top: 5px; margin-bottom: 5px;"),
      menuItem("Tasa de ocupación informal", tabName = "TOI"),
      menuItem("Tasa de ocupación", tabName = "TO"),
      menuItem("Tasa de participación", tabName = "TP"),
      menuItem("Tasa de desocupación", tabName = "TD"),
      menuItem("Ocupados", tabName = "Ocupados"),
      menuItem("Desocupados", tabName = "Desocupados"),
      menuItem("Fuerza de Trabajo", tabName = "FT"),
      menuItem("Personas en edad de trabajar", tabName = "PET"),
      menuItem("Fuera de fuerza trabajo", tabName = "FFT"),
      menuItem("Ocupados Informales", tabName = "OI")
    ) 
  ),
  dashboardBody(tags$head(tags$style(HTML('
                                /* logo when hovered */.skin-blue .main-header .logo:hover {background-color: #18137d;}
                                /* navbar (rest of the header) */.skin-blue .main-header .navbar {background-color: #18137d;}
                                /* body */.content-wrapper, .right-side {background-color: #ffffff;}
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #18137d;
                                }/* logo when hovered */.skin-blue .main-header .logo:hover {background-color: #18137d;}
                                /* logo */.skin-blue .main-header .logo {background-color: #18137d;}
                                .modal-dialog{ width:1200px}, .modal-body{ min-height:550px}'))),
                tags$head(tags$style(type = 'text/css', HTML(".modal-dialog {width: fit-content !important; }"))),
                tags$head(tags$style(".modal-body{ min-height:650px}")),
                tabItems(
                  tabItem(tabName = "ML", 
                          h2("Resúmen:"),
                          tags$div(style = "font-size: 20px; text-align: lePET;",
                                   HTML("<p style='text-align: justify; max-width: 1000px;'><b>La Encuesta Nacional de Empleo (ENE)</b>, vigente desde el trimestre enero-marzo de 2010, es una encuesta a 
                            hogares que se aplica en viviendas particulares ocupadas, con representatividad a nivel nacional y regional. <br><br>
                            La ENE clasifica y caracteriza a todas las personas en edad de trabajar (15 años y más), según su relación con 
                            la fuerza de trabajo, identificando a quienes están dentro de ésta (personas ocupadas y desocupadas) y fuera de ésta 
                            (antiguamente denominadas como \"inactivas\"). <br><br> La ENE entrega estimaciones para trimestres móviles de los 
                            principales indicadores del mercado laboral, los que se publican oficialmente el último día hábil de cada mes. 
                            El principal indicador corresponde a la tasa de desocupación.<br><br><b>Las estadísticas de informalidad laboral</b> 
                            profundizan la información actualmente entregada por la encuesta, permitiendo caracterizar de manera más detallada 
                            la calidad de las ocupaciones que se generan en el mercado laboral. <br><br> Los principales indicadores son la 
                            Tasa de Ocupación Informal (TOI) y la Tasa de Ocupación en el Sector Informal (TOSI), ambos disponibles desde el 
                            trimestre julio-septiembre 2017. Mensualmente, junto con los resultados de la ENE, se presenta esta información, 
                            la que se publica de manera más detallada con un boletín en los meses de febrero, mayo, agosto y noviembre según 
                            la información de cada trimestre calendario."))
                  ),
                  tabItem(tabName = "TOI", h2("Tasa de ocupación informal"), 
                          h5("Puede seleccionar regiones dentro de la tabla para generar un grafico con regiones de su interés."),
                          h5("Si no selecciona nunguna región el grafico mostrará todas las regiones."),
                          h5("El gráfico de barras y el gráfico por sexo considerarán el ultimo periodo de la tabla."),
                          h5("El gráfico por sexo solo considera una región graficada a la vez."),
                          fluidRow(
                            column(width = 8),
                            column(width = 3,
                                   div(style = "display: flex; justify-content: center;",
                                       tags$style(HTML("#show_plot_button_toi_bar, #show_plot_button_toi, #show_plot_button_toi_gender { background-color: #18137d; color: white; }")),
                                       actionButton('show_plot_button_toi_bar', 'Gráfico de barras', icon('chart-column', lib = 'font-awesome')),
                                       actionButton('show_plot_button_toi', 'Gráfico de lineas', icon('chart-line', lib = 'font-awesome')),
                                       actionButton('show_plot_button_toi_gender', "Gráfico por sexo", icon('venus-mars', lib = 'font-awesome'))))),
                          div(DT::dataTableOutput("table_toi"), style = "font-size: 110%; width: 100%")),
                  tabItem(tabName = "TO", h2("Tasa de ocupación"), 
                          h5("Puede seleccionar regiones dentro de la tabla para generar un grafico con regiones de su interés."),
                          h5("Si no selecciona nunguna región el grafico mostrará todas las regiones."),
                          h5("El gráfico de barras y el gráfico por sexo considerarán el ultimo periodo de la tabla."),
                          h5("El gráfico por sexo solo considera una región graficada a la vez."),
                          fluidRow(
                            column(width = 8),
                            column(width = 3,
                                   div(style = "display: flex; justify-content: center;",
                                       tags$style(HTML("#show_plot_button_to_bar, #show_plot_button_to, #show_plot_button_to_gender { background-color: #18137d; color: white; }")),
                                       actionButton('show_plot_button_to_bar', 'Gráfico de barras', icon('chart-column', lib = 'font-awesome')),
                                       actionButton('show_plot_button_to', 'Gráfico de lineas', icon('chart-line', lib = 'font-awesome')),
                                       actionButton('show_plot_button_to_gender', "Gráfico por sexo", icon('venus-mars', lib = 'font-awesome'))))),
                          div(DT::dataTableOutput("table_to"), style = "font-size: 110%; width: 100%")),
                  tabItem(tabName = "TP", h2("Tasa de participación"), 
                          h5("Puede seleccionar regiones dentro de la tabla para generar un grafico con regiones de su interés."),
                          h5("Si no selecciona nunguna región el grafico mostrará todas las regiones."),
                          h5("El gráfico de barras y el gráfico por sexo considerarán el ultimo periodo de la tabla."),
                          h5("El gráfico por sexo solo considera una región graficada a la vez."),
                          fluidRow(
                            column(width = 8),
                            column(width = 3,
                                   div(style = "display: flex; justify-content: center;",
                                       tags$style(HTML("#show_plot_button_tp_bar, #show_plot_button_tp, #show_plot_button_tp_gender { background-color: #18137d; color: white; }")),
                                       actionButton('show_plot_button_tp_bar', 'Gráfico de barras', icon('chart-column', lib = 'font-awesome')),
                                       actionButton('show_plot_button_tp', 'Gráfico de lineas', icon('chart-line', lib = 'font-awesome')),
                                       actionButton('show_plot_button_tp_gender', "Gráfico por sexo", icon('venus-mars', lib = 'font-awesome'))))),
                          div(DT::dataTableOutput("table_tp"), style = "font-size: 110%; width: 100%")),
                  tabItem(tabName = "TD", h2("Tasa de desocupación"), 
                          h5("Puede seleccionar regiones dentro de la tabla para generar un grafico con regiones de su interés."),
                          h5("Si no selecciona nunguna región el grafico mostrará todas las regiones."),
                          h5("El gráfico de barras y el gráfico por sexo considerarán el ultimo periodo de la tabla."),
                          h5("El gráfico por sexo solo considera una región graficada a la vez."),
                          fluidRow(
                            column(width = 8),
                            column(width = 3,
                                   div(style = "display: flex; justify-content: center;",
                                       tags$style(HTML("#show_plot_button_td_bar, #show_plot_button_td, #show_plot_button_td_gender { background-color: #18137d; color: white; }")),
                                       actionButton('show_plot_button_td_bar', 'Gráfico de barras', icon('chart-column', lib = 'font-awesome')),
                                       actionButton('show_plot_button_td', 'Gráfico de lineas', icon('chart-line', lib = 'font-awesome')),
                                       actionButton('show_plot_button_td_gender', "Gráfico por sexo", icon('venus-mars', lib = 'font-awesome'))))),
                          div(DT::dataTableOutput("table_td"), style = "font-size: 110%; width: 100%")),
                  tabItem(tabName = "Ocupados", h2("Estimación del total de ocupados"), 
                          h5("Puede seleccionar regiones dentro de la tabla para generar un grafico con regiones de su interés."),
                          h5("Si no selecciona nunguna región el grafico mostrará todas las regiones."),
                          h5("El gráfico de barras y el gráfico por sexo considerarán el ultimo periodo de la tabla."),
                          h5("El gráfico por sexo solo considera una región graficada a la vez."),
                          fluidRow(
                            column(width = 8),
                            column(width = 4,
                                   div(style = "display: flex; justify-content: center;",
                                       tags$style(HTML("#show_plot_button_Ocupados, #show_plot_button_Ocupados_gender { background-color: #18137d; color: white; }")),
                                       actionButton('show_plot_button_Ocupados', 'Gráfico de lineas', icon('chart-line', lib = 'font-awesome')),
                                       actionButton('show_plot_button_Ocupados_gender', "Gráfico por sexo", icon('venus-mars', lib = 'font-awesome'))))),
                          div(DT::dataTableOutput("table_Ocupados"), style = "font-size: 110%; width: 100%")),
                  tabItem(tabName = "Desocupados", h2("Estimación del total de desocupados"), 
                          h5("Puede seleccionar regiones dentro de la tabla para generar un grafico con regiones de su interés."),
                          h5("Si no selecciona nunguna región el grafico mostrará todas las regiones."),
                          h5("El gráfico de barras y el gráfico por sexo considerarán el ultimo periodo de la tabla."),
                          h5("El gráfico por sexo solo considera una región graficada a la vez."),
                          fluidRow(
                            column(width = 8),
                            column(width = 4,
                                   div(style = "display: flex; justify-content: center;",
                                       tags$style(HTML("#show_plot_button_Desocupados, #show_plot_button_Desocupados_gender { background-color: #18137d; color: white; }")),
                                       actionButton('show_plot_button_Desocupados', 'Gráfico de lineas', icon('chart-line', lib = 'font-awesome')),
                                       actionButton('show_plot_button_Desocupados_gender', "Gráfico por sexo", icon('venus-mars', lib = 'font-awesome'))))),
                          div(DT::dataTableOutput("table_Desocupados"), style = "font-size: 110%; width: 100%")),
                  tabItem(tabName = "FT", h2("Estimación del total de fuerza de trabajo"), 
                          h5("Puede seleccionar regiones dentro de la tabla para generar un grafico con regiones de su interés."),
                          h5("Si no selecciona nunguna región el grafico mostrará todas las regiones."),
                          h5("El gráfico de barras y el gráfico por sexo considerarán el ultimo periodo de la tabla."),
                          h5("El gráfico por sexo solo considera una región graficada a la vez."),
                          fluidRow(
                            column(width = 8),
                            column(width = 4,
                                   div(style = "display: flex; justify-content: center;",
                                       tags$style(HTML("#show_plot_button_FT, #show_plot_button_FT_gender { background-color: #18137d; color: white; }")),
                                       actionButton('show_plot_button_FT', 'Gráfico de lineas', icon('chart-line', lib = 'font-awesome')),
                                       actionButton('show_plot_button_FT_gender', "Gráfico por sexo", icon('venus-mars', lib = 'font-awesome'))))),
                          div(DT::dataTableOutput("table_FT"), style = "font-size: 100%; width: 100%")),
                  tabItem(tabName = "PET", h2("Estimación del total de población en edad de trabajar"), 
                          h5("Puede seleccionar regiones dentro de la tabla para generar un grafico con regiones de su interés."),
                          h5("Si no selecciona nunguna región el grafico mostrará todas las regiones."),
                          h5("El gráfico de barras y el gráfico por sexo considerarán el ultimo periodo de la tabla."),
                          h5("El gráfico por sexo solo considera una región graficada a la vez."),
                          fluidRow(
                            column(width = 8),
                            column(width = 4,
                                   div(style = "display: flex; justify-content: center;",
                                       tags$style(HTML("#show_plot_button_PET, #show_plot_button_PET_gender { background-color: #18137d; color: white; }")),
                                       actionButton('show_plot_button_PET', 'Gráfico de lineas', icon('chart-line', lib = 'font-awesome')),
                                       actionButton('show_plot_button_PET_gender', "Gráfico por sexo", icon('venus-mars', lib = 'font-awesome'))))),
                          div(DT::dataTableOutput("table_PET"), style = "font-size: 110%; width: 100%")),
                  tabItem(tabName = "FFT", h2("Estimación del total de personas fuera de la fuerza de trabajo"), 
                          h5("Puede seleccionar regiones dentro de la tabla para generar un grafico con regiones de su interés."),
                          h5("Si no selecciona nunguna región el grafico mostrará todas las regiones."),
                          h5("El gráfico de barras y el gráfico por sexo considerarán el ultimo periodo de la tabla."),
                          h5("El gráfico por sexo solo considera una región graficada a la vez."),
                          fluidRow(
                            column(width = 8),
                            column(width = 4,
                                   div(style = "display: flex; justify-content: center;",
                                       tags$style(HTML("#show_plot_button_FFT, #show_plot_button_FFT_gender { background-color: #18137d; color: white; }")),
                                       actionButton('show_plot_button_FFT', 'Gráfico de lineas', icon('chart-line', lib = 'font-awesome')),
                                       actionButton('show_plot_button_FFT_gender', "Gráfico por sexo", icon('venus-mars', lib = 'font-awesome'))))),
                          div(DT::dataTableOutput("table_FFT"), style = "font-size: 110%; width: 100%")),
                  tabItem(tabName = "OI", h2("Estimación del total de ocupados informales"), 
                          h5("Puede seleccionar regiones dentro de la tabla para generar un grafico con regiones de su interés."),
                          h5("Si no selecciona nunguna región el grafico mostrará todas las regiones."),
                          h5("El gráfico de barras y el gráfico por sexo considerarán el ultimo periodo de la tabla."),
                          h5("El gráfico por sexo solo considera una región graficada a la vez."), 
                          fluidRow(
                            column(width = 8), 
                            column(width = 4,   
                                   div(style = "display: flex; justify-content: center;",   
                                       tags$style(HTML("#show_plot_button_OI, #show_plot_button_OI_gender { background-color: #18137d; color: white;}")),
                                       actionButton('show_plot_button_OI', 'Gráfico de lineas', icon('chart-line', lib = 'font-awesome')),
                                       actionButton('show_plot_button_OI_gender', "Gráfico por sexo", icon('venus-mars', lib = 'font-awesome')))
                                   )),
                          div(DT::dataTableOutput("table_OI"), style = "font-size: 110%; width: 100%"))
                )
  )
)

# ---- server ----
server <- function(input, output, session) {
  
  # ---- toi ----
  output$table_toi <- DT::renderDataTable({tabla_traspuesta(tabla_toi)})
  
  observeEvent(input$show_plot_button_toi, {
    showModal(modalDialog(plotlyOutput("x_toi"), easyClose = TRUE, footer = actionButton('close_plot_button_toi', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_toi, {removeModal()})

  output$x_toi = renderPlotly({
    ind = input$table_toi_rows_selected
    if (is.null(ind)) {ind = seq(1,17)}
    
    tabla_filtrada_toi <- tabla_filtrar(ind, tabla, variable_diferencia = "toi")
    
    plot_ly(data = tabla_filtrada_toi, x = ~periodo, y = ~toi, width = 1000, height = 600, color = ~nombre_region, type = 'scatter',mode = 'lines', line = list(dash = ifelse(tabla_filtrada_toi$nombre_region != "Total país", "dash", "solid")), colors = mis_colores) %>%
      layout(title = list(text = "Tasa de ocupación informal por región", y = 1.6), xaxis = list(title = "Período", tickangle = 90), hovermode = "x unified", yaxis = list(title = "Tasa de ocupación informal (%)", range = c(0, max(tabla_filtrada_toi$toi)+30)), legend = list(x = 0.5,y = 1, orientation = "h", xanchor="center"), margin = list(b = 0)) %>% 
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"), responsive = F, autosizable = F, showAxisDragHandles = FALSE,  showTips = FALSE, locale = "es") %>% 
      layout(images = list(list(source = paste('data:image/png;base64', txt, sep=','), x = 1, y = 1, sizex = 0.1, sizey = 0.1)))
  })
  
  output$x_toi_gender = renderPlotly({
    ind = input$table_toi_rows_selected
    if (is.null(ind)) {ind = 17}
    ind <- ind[1]
  
    tabla_sexos_filtrada <- tabla_filtrar_sexo(ind, tabla_sexos)
    
    plot_ly(data = tabla_sexos_filtrada, x = ~periodo, y = ~toi, color = ~sexo, width = 1000, height = 600,
            type = 'scatter', mode = 'lines', text = ~paste(sexo, ":", round(toi, 2), "%"),
            hoverinfo = "text") %>%
      layout(
        title = list(text = "Tasa de ocupación informal por sexo", y = 1.6), 
        xaxis = list(title = "Período", tickangle = 90), hovermode = "x unified",
        yaxis = list(title = "Tasa de ocupación (%)", range = c(max(tabla_sexos_filtrada$toi)-15, max(tabla_sexos_filtrada$toi)+5)),
        legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center"),
        margin = list(b = 0)
      ) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.92,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })
  
  observeEvent(input$show_plot_button_toi_gender, {
    showModal(modalDialog(plotlyOutput("x_toi_gender"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_toi_gender', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_toi_gender, {removeModal()})

  output$x_toi_bar = renderPlotly({
    ind = input$table_toi_rows_selected
    if (is.null(ind)) {ind = seq(1, 16)}

    if (length(ind) > 1) {
      if (17 %in% ind) {
        posiciones <- which(ind == 17)
        ind <- ind[-posiciones]
      }
    }
    if (any(ind == 17)) {ind = seq(1, 16)}
    
    ind <- modificar_vector(ind)
    
    tabla_filtrada_bar <- tabla_filtrar_bar(ind, tabla_barras2)
    tabla_filtrada_bar <- tabla_filtrada_bar[order(tabla_filtrada_bar$toi), ]
    tabla_filtrada_bar <- tabla_filtrada_bar %>% mutate(nombre_region = factor(nombre_region, levels = unique(nombre_region)))
    
    plot_ly(data = tabla_filtrada_bar, x = ~nombre_region, y = ~toi, type = "bar",
            width = 1000, height = 600, name = "Tasa regional") %>% 
      layout(title = "Tasa de ocupación informal por región", 
             xaxis = list(title = "Regiones", tickangle = 45), hovermode = "x unified", 
             yaxis = list(title = "Tasa de ocupación informal (%)"),
             legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center"),
             margin = list(b = 0)) %>% 
      add_lines(x = ~tabla_filtrada_bar$nombre_region, y = rep(tabla_barras3$toi, nrow(tabla_filtrada_bar)),
                name = "Tasa Nacional", line = list(color = "red")) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 1,
             y = 1, 
             sizex = 0.09,
             sizey = 0.09)))
  })
  
  observeEvent(input$show_plot_button_toi_bar, {
    showModal(modalDialog(plotlyOutput("x_toi_bar"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_toi_bar', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_toi_bar, {removeModal()})

  # ---- to ----
  output$table_to <- DT::renderDataTable({tabla_traspuesta(tabla_to)})
  
  observeEvent(input$show_plot_button_to, {
    showModal(modalDialog(plotlyOutput("x_to"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_to', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_to, {removeModal()})
  
  output$x_to = renderPlotly({
    ind = input$table_to_rows_selected
    if (is.null(ind)) {ind = seq(1,17)}

    tabla_filtrada_to <- tabla_filtrar(ind, tabla, variable_diferencia = "to")
    
    plot_ly(data = tabla_filtrada_to, x = ~periodo, y = ~to, width = 1200, height = 650,
            color = ~nombre_region, type = 'scatter',mode = 'lines',
            line = list(dash = ifelse(tabla_filtrada_to$nombre_region != "Total país", "dash", "solid")),
            colors = mis_colores) %>%
      layout(title = list(text = "Tasa de ocupación por región", y = 1.6),
             xaxis = list(title = "Período", tickangle = 90), hovermode = "x unified",
             yaxis = list(title = "Tasa de ocupación (%)", range = c(0, max(tabla_filtrada_to$to)+40)),
             legend = list(x = 0.5,y = 1, orientation = "h", xanchor="center"),
             margin = list(b = 0)) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.95,
             y = 1, 
             sizex = 0.09,
             sizey = 0.09)))
  })

  output$x_to_gender = renderPlotly({
    ind = input$table_to_rows_selected
    if (is.null(ind)) {ind = 17}
    ind <- ind[1]
    
    tabla_sexos_filtrada <- tabla_filtrar_sexo(ind, tabla_sexos)
    
    plot_ly(data = tabla_sexos_filtrada, x = ~periodo, y = ~to, color = ~sexo, width = 1000, height = 600,
            type = 'scatter', mode = 'lines', text = ~paste(sexo, ":", round(to, 2), "%"),
            hoverinfo = "text") %>%
      layout(
        title = list(text = "Tasa de ocupación por sexo", y = 1.6), 
        xaxis = list(title = "Período", tickangle = 90), hovermode = "x unified",
        yaxis = list(title = "Tasa de ocupación (%)", range = c(0, 90)),
        legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center"),
        margin = list(b = 0)
      ) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.92,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })
  
  observeEvent(input$show_plot_button_to_gender, {
    showModal(modalDialog(plotlyOutput("x_to_gender"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_to_gender', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_to_gender, {removeModal()})

  output$x_to_bar = renderPlotly({
    ind = input$table_to_rows_selected
    if (is.null(ind)) {ind = seq(1, 16)}
    
    if (length(ind) > 1) {
      if (17 %in% ind) {
        posiciones <- which(ind == 17)
        ind <- ind[-posiciones]
      }
    }
    if (any(ind == 17)) {ind = seq(1, 16)}
    
    ind <- modificar_vector(ind)

    tabla_filtrada_bar <- tabla_filtrar_bar(ind, tabla_barras2)
    tabla_filtrada_bar <- tabla_filtrada_bar[order(tabla_filtrada_bar$to), ]
    tabla_filtrada_bar <- tabla_filtrada_bar %>% mutate(nombre_region = factor(nombre_region, levels = unique(nombre_region)))
    
    plot_ly(data = tabla_filtrada_bar, x = ~nombre_region, y = ~to, type = "bar", width = 1000, height = 600,
            name = "Tasas regionales") %>% 
      layout(title = "Tasa de ocupación por región", 
             xaxis = list(title = "Regiones", tickangle = 40), hovermode = "x unified",
             yaxis = list(title = "Tasa de ocupación (%)"),
             legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center"),
             margin = list(b = 0)) %>% 
      add_lines(x = ~tabla_filtrada_bar$nombre_region, y = rep(tabla_barras3$to, nrow(tabla_filtrada_bar)),
                name = "Tasa Nacional", line = list(color = "red")) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.95,
             y = 1, 
             sizex = 0.09,
             sizey = 0.09)))
  })
  
  observeEvent(input$show_plot_button_to_bar, {
    showModal(modalDialog(plotlyOutput("x_to_bar"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_to_bar', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_to_bar, {removeModal()})

  # ---- tp ----
  output$table_tp <- DT::renderDataTable({tabla_traspuesta(tabla_tp)})
  
  observeEvent(input$show_plot_button_tp, {
    showModal(modalDialog(plotlyOutput("x_tp"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_tp', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_tp, {removeModal()})
  
  output$x_tp = renderPlotly({
    ind = input$table_tp_rows_selected
    if (is.null(ind)) {ind = seq(1,17)}

    tabla_filtrada_tp <- tabla_filtrar(ind, tabla, variable_diferencia = "tp")
    
    plot_ly(data = tabla_filtrada_tp, x = ~periodo, y = ~tp, width = 1000, height = 600,
            color = ~nombre_region, type = 'scatter',mode = 'lines',
            line = list(dash = ifelse(tabla_filtrada_tp$nombre_region != "Total país", "dash", "solid")),
            colors = mis_colores) %>%
      layout(title = list(text = "Tasa de participación por región", y = 1.6),
             xaxis = list(title = "Período", tickangle = 45), hovermode = "x unified",
             yaxis = list(title = "Tasa de participación (%)", range = c(0, max(tabla_filtrada_tp$tp)+20)),
             legend = list(x = 0.5,y = 1, orientation = "h", xanchor="center"),
             margin = list(b = 0)) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.99,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })

  output$x_tp_gender = renderPlotly({
    ind = input$table_tp_rows_selected
    if (is.null(ind)) {ind = 17}
    ind <- ind[1]
    
    tabla_sexos_filtrada <- tabla_filtrar_sexo(ind, tabla_sexos)
    
    plot_ly(data = tabla_sexos_filtrada, x = ~periodo, y = ~tp, color = ~sexo, width = 1000, height = 600,
            type = 'scatter', mode = 'lines', text = ~paste(sexo, ":", round(tp, 2), "%"),
            hoverinfo = "text") %>%
      layout(
        title = list(text = "Tasa de participación por sexo", y = 1.6), 
        xaxis = list(title = "Período", tickangle = 90), hovermode = "x unified",
        yaxis = list(title = "Tasa de participación (%)", range = c(0, 90)),
        legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center"),
        margin = list(b = 0)
      ) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.92,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })
  
  observeEvent(input$show_plot_button_tp_gender, {
    showModal(modalDialog(plotlyOutput("x_tp_gender"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_tp_gender', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_tp_gender, {removeModal()})

  output$x_tp_bar = renderPlotly({
    ind = input$table_tp_rows_selected
    if (is.null(ind)) {ind = seq(1, 16)}
    
    if (length(ind) > 1) {
      if (17 %in% ind) {
        posiciones <- which(ind == 17)
        ind <- ind[-posiciones]
      }
    }
    if (any(ind == 17)) {ind = seq(1, 16)}
    
    ind <- modificar_vector(ind)
    
    tabla_filtrada_bar <- tabla_filtrar_bar(ind, tabla_barras2)
    tabla_filtrada_bar <- tabla_filtrada_bar[order(tabla_filtrada_bar$tp), ]
    tabla_filtrada_bar <- tabla_filtrada_bar %>% mutate(nombre_region = factor(nombre_region, levels = unique(nombre_region)))
    
    plot_ly(data = tabla_filtrada_bar, x = ~nombre_region, y = ~tp, type = "bar", width = 1000, height = 600,
            name = "Tasas regionales") %>% 
      layout(title = "Tasa de pariticpación por región", 
             xaxis = list(title = "Regiones", tickangle = 40), hovermode = "x unified",
             yaxis = list(title = "Tasa de participación (%)"),
             legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center"),
             margin = list(b = 0)) %>% 
      add_lines(x = ~tabla_filtrada_bar$nombre_region, y = rep(tabla_barras3$tp, nrow(tabla_filtrada_bar)),
                name = "Tasa Nacional", line = list(color = "red")) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 1,
             y = 1, 
             sizex = 0.09,
             sizey = 0.09)))
  })
  
  observeEvent(input$show_plot_button_tp_bar, {
    showModal(modalDialog(plotlyOutput("x_tp_bar"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_tp_bar', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_tp_bar, {removeModal()})

  # ---- td ----
  output$table_td <- DT::renderDataTable({tabla_traspuesta(tabla_td)})
  
  observeEvent(input$show_plot_button_td, {
    showModal(modalDialog(plotlyOutput("x_td"), easyClose = TRUE, footer = actionButton('close_plot_button_td', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_td, {removeModal()})
  
  output$x_td = renderPlotly({
    ind = input$table_td_rows_selected
    if (is.null(ind)) {ind = seq(1,17)}
    
    tabla_filtrada_td <- tabla_filtrar(ind, tabla, variable_diferencia = "td")
    
    plot_ly(data = tabla_filtrada_td, x = ~periodo, y = ~td, width = 1000, height = 600,
            color = ~nombre_region, type = 'scatter',mode = 'lines',
            line = list(dash = ifelse(tabla_filtrada_td$nombre_region != "Total país", "dash", "solid")),
            colors = mis_colores) %>%
      layout(title = list(text = "Tasa de desocupación por región", y = 1.6),
             xaxis = list(title = "Período", tickangle = 45), hovermode = "x unified",
             yaxis = list(title = "Tasa de desocupación (%)", range = c(0, max(tabla_filtrada_td$td)+10)),
             legend = list(x = 0.5,y = 1, orientation = "h", xanchor="center"),
             margin = list(b = 0)) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.99,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })

  output$x_td_gender = renderPlotly({
    ind = input$table_td_rows_selected
    if (is.null(ind)) {ind = 17}
    ind <- ind[1]
    
    tabla_sexos_filtrada <- tabla_filtrar_sexo(ind, tabla_sexos)
    
    plot_ly(data = tabla_sexos_filtrada, x = ~periodo, y = ~td, color = ~sexo, width = 1000, height = 600,
            type = 'scatter', mode = 'lines', text = ~paste(sexo, ":", round(td, 2), "%"),
            hoverinfo = "text") %>%
      layout(
        title = list(text = "Tasa de desocupación por sexo", y = 1.6), 
        xaxis = list(title = "Período", tickangle = 90), hovermode = "x unified",
        yaxis = list(title = "Tasa de desocupación (%)", range = c(0, max(tabla_sexos_filtrada$td)+5)),
        legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center"),
        margin = list(b = 0)
      ) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.92,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })
  
  observeEvent(input$show_plot_button_td_gender, {
    showModal(modalDialog(plotlyOutput("x_td_gender"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_td_gender', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_td_gender, {removeModal()})

  output$x_td_bar = renderPlotly({
    ind = input$table_td_rows_selected
    if (is.null(ind)) {ind = seq(1, 16)}
    
    if (length(ind) > 1) {
      if (17 %in% ind) {
        posiciones <- which(ind == 17)
        ind <- ind[-posiciones]
      }
    }
    if (any(ind == 17)) {ind = seq(1, 16)}
    
    ind <- modificar_vector(ind)

    tabla_filtrada_bar <- tabla_filtrar_bar(ind, tabla_barras2)
    tabla_filtrada_bar <- tabla_filtrada_bar[order(tabla_filtrada_bar$td), ]
    tabla_filtrada_bar <- tabla_filtrada_bar %>% mutate(nombre_region = factor(nombre_region, levels = unique(nombre_region)))

    plot_ly(data = tabla_filtrada_bar, x = ~nombre_region, y = ~td, type = "bar", width = 1000, height = 600,
            name = "Tasas regionales") %>% 
      layout(title = "Tasa de desocupación por región",
             xaxis = list(title = "Regiones", tickangle = 40), hovermode = "x unified",
             yaxis = list(title = "Tasa de desocupación (%)"),
             legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center"),
             margin = list(b = 0)) %>% 
      add_lines(x = ~tabla_filtrada_bar$nombre_region, y = rep(tabla_barras3$td, nrow(tabla_filtrada_bar)),
                name = "Tasa Nacional", line = list(color = "red")) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 1,
             y = 1, 
             sizex = 0.09,
             sizey = 0.09)))
  })
  
  observeEvent(input$show_plot_button_td_bar, {
    showModal(modalDialog(plotlyOutput("x_td_bar"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_td_bar', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_td_bar, {removeModal()})

  # ---- Ocupados ----
  output$table_Ocupados <- DT::renderDataTable({tabla_traspuesta(tabla_Ocupados)})
  
  observeEvent(input$show_plot_button_Ocupados, {
    showModal(modalDialog(plotlyOutput("x_Ocupados"), easyClose = TRUE,
                          footer = actionButton('close_plot_button_Ocupados', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_Ocupados, {removeModal()})
  
  output$x_Ocupados = renderPlotly({
    ind = input$table_Ocupados_rows_selected
    if (is.null(ind)) {ind = seq(1,16)}
    
    tabla_filtrada_Ocupados <- tabla_filtrar(ind, tabla, variable_diferencia = "Ocupados")
    
    plot_ly(data = tabla_filtrada_Ocupados, x = ~periodo, y = ~Ocupados, width = 1000, height = 600,
            color = ~nombre_region, type = 'scatter',mode = 'lines',
            line = list(dash = ifelse(tabla_filtrada_Ocupados$nombre_region != "Total país", "dash", "solid")),
            colors = mis_colores) %>%
      layout(title = list(text = "Total de ocupados por región", y = 1.6),
             xaxis = list(title = "Período", tickangle = 45), hovermode = "x unified",
             yaxis = list(title = "Ocupados", range = c(0, max(tabla_filtrada_Ocupados$Ocupados)+20000)),
             legend = list(x = 0.5,y = 1, orientation = "h", xanchor="center"),
             margin = list(b = 0)) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.99,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })

  output$x_Ocupados_gender = renderPlotly({
    ind = input$table_Ocupados_rows_selected
    if (is.null(ind)) {ind = 17}
    ind <- ind[1]
    
    tabla_sexos_filtrada <- tabla_filtrar_sexo(ind, tabla_sexos)
    
    plot_ly(data = tabla_sexos_filtrada, x = ~periodo, y = ~Ocupados, color = ~sexo, width = 1000, height = 600, type = 'scatter', mode = 'lines', text = ~paste(sexo, ":", Ocupados), hoverinfo = "text") %>%
      layout(title = list(text = "Total de ocupados por sexo", y = 1.6), xaxis = list(title = "Período", tickangle = 90), hovermode = "x unified", yaxis = list(title = "Ocupados", range = c(0, max(tabla_sexos_filtrada$Ocupados)+1000000)), legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center"), margin = list(b = 0)) %>% 
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"), responsive = F, autosizable = F, showAxisDragHandles = FALSE, showTips = FALSE, locale = "es") %>% 
      layout(images = list(list(source = paste('data:image/png;base64', txt, sep=','), x = 0.92, y = 1, sizex = 0.1, sizey = 0.1)))
  })
  
  observeEvent(input$show_plot_button_Ocupados_gender, {
    showModal(modalDialog(plotlyOutput("x_Ocupados_gender"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_Ocupados_gender', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_Ocupados_gender, {removeModal()})

  # ---- Desocupados ----
  output$table_Desocupados <- DT::renderDataTable({tabla_traspuesta(tabla_Desocupados)})
  
  observeEvent(input$show_plot_button_Desocupados, {
    showModal(modalDialog(plotlyOutput("x_Desocupados"), easyClose = TRUE,
                          footer = actionButton('close_plot_button_Desocupados', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_Desocupados, {removeModal()})
  
  output$x_Desocupados = renderPlotly({
    ind = input$table_Desocupados_rows_selected
    if (is.null(ind)) {ind = seq(1,16)}
    
    tabla_filtrada_Desocupados <- tabla_filtrar(ind, tabla, variable_diferencia = "Desocupados")
    
    plot_ly(data = tabla_filtrada_Desocupados, x = ~periodo, y = ~Desocupados, width = 1000, height = 600,
            color = ~nombre_region, type = 'scatter',mode = 'lines',
            line = list(dash = ifelse(tabla_filtrada_Desocupados$nombre_region != "Total país", "dash", "solid")),
            colors = mis_colores) %>%
      layout(title = list(text = "Total de desocupados por región", y = 1.6),
             xaxis = list(title = "Período", tickangle = 45), hovermode = "x unified",
             yaxis = list(title = "Desocupados", range = c(0, max(tabla_filtrada_Desocupados$Desocupados))),
             legend = list(x = 0.5,y = 1, orientation = "h", xanchor="center"),
             margin = list(b = 0)) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list( list(source = paste('data:image/png;base64', txt, sep=','), x = 0.99, y = 1, sizex = 0.1, sizey = 0.1)))
  })

  output$x_Desocupados_gender = renderPlotly({
    ind = input$table_Desocupados_rows_selected
    if (is.null(ind)) {ind = 17}
    ind <- ind[1]
    
    tabla_sexos_filtrada <- tabla_filtrar_sexo(ind, tabla_sexos)
    
    plot_ly(data = tabla_sexos_filtrada, x = ~periodo, y = ~Desocupados, color = ~sexo, width = 1000, height = 600,
            type = 'scatter', mode = 'lines', text = ~paste(sexo, ":", Desocupados),
            hoverinfo = "text") %>%
      layout(
        title = list(text = "Total de desocupados por sexo", y = 1.6), 
        xaxis = list(title = "Período", tickangle = 90), hovermode = "x unified",
        yaxis = list(title = "Desocupados", range = c(0, max(tabla_sexos_filtrada$Desocupados)+700000)),
        legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center"),
        margin = list(b = 0)
      ) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.92,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })
  
  observeEvent(input$show_plot_button_Desocupados_gender, {
    showModal(modalDialog(plotlyOutput("x_Desocupados_gender"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_Desocupados_gender', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_Desocupados_gender, {removeModal()})

  # ---- FT ----
  output$table_FT <- DT::renderDataTable({tabla_traspuesta(tabla_FT)})
  
  observeEvent(input$show_plot_button_FT, {
    showModal(modalDialog(plotlyOutput("x_FT"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_FT', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_FT, {removeModal()})
  
  output$x_FT = renderPlotly({
    ind = input$table_FT_rows_selected
    if (is.null(ind)) {ind = seq(1,16)}
    
    tabla_filtrada_FT <- tabla_filtrar(ind, tabla, variable_diferencia = "FT")
    
    plot_ly(data = tabla_filtrada_FT, x = ~periodo, y = ~FT, width = 1000, height = 600,
            color = ~nombre_region, type = 'scatter',mode = 'lines',
            line = list(dash = ifelse(tabla_filtrada_FT$nombre_region != "Total país", "dash", "solid")),
            colors = mis_colores) %>%
      layout(title = list(text = "Personas en edad de trabajar por región", y = 1.6),
             xaxis = list(title = "Período", tickangle = 45), hovermode = "x unified",
             yaxis = list(title = "Personas en edad de trabajar", range = c(0, max(tabla_filtrada_FT$FT))),
             legend = list(x = 0.5,y = 1, orientation = "h", xanchor="center"),
             margin = list(b = 0)) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.99,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })
  
  output$x_FT_gender = renderPlotly({
    ind = input$table_FT_rows_selected
    if (is.null(ind)) {ind = 17}
    ind <- ind[1]
    
    tabla_sexos_filtrada <- tabla_filtrar_sexo(ind, tabla_sexos)
    
    plot_ly(data = tabla_sexos_filtrada, x = ~periodo, y = ~FDT, color = ~sexo, width = 1000, height = 600,
            type = 'scatter', mode = 'lines', text = ~paste(sexo, ":", FDT),
            hoverinfo = "text") %>%
      layout(
        title = list(text = "Personas en edad de trabajar por sexo", y = 1.6), 
        xaxis = list(title = "Período", tickangle = 90), hovermode = "x unified",
        yaxis = list(title = "Personas en edad de trabajar", range = c(0, max(tabla_sexos_filtrada$FDT)+1000000)),
        legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center"),
        margin = list(b = 0)
      ) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.92,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })
  
  observeEvent(input$show_plot_button_FT_gender, {
    showModal(modalDialog(plotlyOutput("x_FT_gender"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_FT_gender', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_FT_gender, {removeModal()})
  
  # ---- PET ----
  output$table_PET <- DT::renderDataTable({tabla_traspuesta(tabla_PET)})
  
  observeEvent(input$show_plot_button_PET, {
    showModal(modalDialog(plotlyOutput("x_PET"), easyClose = TRUE,
                          footer = actionButton('close_plot_button_PET', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_PET, {removeModal()})
  
  output$x_PET = renderPlotly({
    ind = input$table_PET_rows_selected
    if (is.null(ind)) {ind = seq(1,16)}
    
    tabla_filtrada_PET <- tabla_filtrar(ind, tabla, variable_diferencia = "PET")
    
    plot_ly(data = tabla_filtrada_PET, x = ~periodo, y = ~PET,   width = 1000, height = 600,
            color = ~nombre_region, type = 'scatter',mode = 'lines',
            line = list(dash = ifelse(tabla_filtrada_PET$nombre_region != "Total país", "dash", "solid")),
            colors = mis_colores) %>%
      layout(title = list(text = "Fuerza de trabajo por región", y = 1.6),
             xaxis = list(title = "Período", tickangle = 45), hovermode = "x unified",
             yaxis = list(title = "Fuerza de trabajo", range = c(0, max(tabla_filtrada_PET$PET))),
             legend = list(x = 0.5,y = 1, orientation = "h", xanchor="center"),
             margin = list(b = 0)) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.99,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })
  
  output$x_PET_gender = renderPlotly({
    ind = input$table_PET_rows_selected
    if (is.null(ind)) {ind = 17}
    ind <- ind[1]
    
    tabla_sexos_filtrada <- tabla_filtrar_sexo(ind, tabla_sexos)
    
    plot_ly(data = tabla_sexos_filtrada, x = ~periodo, y = ~PET, color = ~sexo, width = 1000, height = 600,
            type = 'scatter', mode = 'lines', text = ~paste(sexo, ":", PET), hoverinfo = "text") %>%
      layout(
        title = list(text = "Fuerza de trabajo por sexo", y = 1.6), 
        xaxis = list(title = "Período", tickangle = 90), hovermode = "x unified",
        yaxis = list(title = "Fuerza de trabajo", range = c(0, max(tabla_sexos_filtrada$PET)+1000000)),
        legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center"),
        margin = list(b = 0)
      ) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.92,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })
  
  observeEvent(input$show_plot_button_PET_gender, {
    showModal(modalDialog(plotlyOutput("x_PET_gender"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_PET_gender', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_PET_gender, {removeModal()})

  # ---- FFT ----
  output$table_FFT <- DT::renderDataTable({tabla_traspuesta(tabla_FFT)})
  
  observeEvent(input$show_plot_button_FFT, {
    showModal(modalDialog(plotlyOutput("x_FFT"), easyClose = TRUE,
                          footer = actionButton('close_plot_button_FFT', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_FFT, {removeModal()})
  
  output$x_FFT = renderPlotly({
    ind = input$table_FFT_rows_selected
    if (is.null(ind)) {ind = seq(1,16)}
    
    tabla_filtrada_FFT <- tabla_filtrar(ind, tabla, variable_diferencia = "FFT")
    
    plot_ly(data = tabla_filtrada_FFT, x = ~periodo, y = ~FFT,   width = 1000, height = 600,
            color = ~nombre_region, type = 'scatter',mode = 'lines',
            line = list(dash = ifelse(tabla_filtrada_FFT$nombre_region != "Total país", "dash", "solid")),
            colors = mis_colores) %>%
      layout(title = list(text = "Fuera de la fuerza de trabajo por región", y = 1.6),
             xaxis = list(title = "Período", tickangle = 45), hovermode = "x unified",
             yaxis = list(title = "Fuera de la fuerza de trabajo", range = c(0, max(tabla_filtrada_FFT$FFT))),
             legend = list(x = 0.5,y = 1, orientation = "h", xanchor="center"),
             margin = list(b = 0)) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.99,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })
  
  output$x_FFT_gender = renderPlotly({
    ind = input$table_PET_rows_selected
    if (is.null(ind)) {ind = 17}
    ind <- ind[1]
    
    tabla_sexos_filtrada <- tabla_filtrar_sexo(ind, tabla_sexos)
    
    plot_ly(data = tabla_sexos_filtrada, x = ~periodo, y = ~FFT, color = ~sexo, width = 1000, height = 600,
            type = 'scatter', mode = 'lines', text = ~paste(sexo, ":", FFT),
            hoverinfo = "text") %>%
      layout(
        title = list(text = "Fuera de la fuerza de trabajo por sexo", y = 1.6),
        xaxis = list(title = "Período", tickangle = 90), hovermode = "x unified",
        yaxis = list(title = "Fuera de la fuerza de trabajo", range = c(0, max(tabla_sexos_filtrada$FFT)+1000000)),
        legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center"),
        margin = list(b = 0)
      ) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.92,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })
  
  observeEvent(input$show_plot_button_FFT_gender, {
    showModal(modalDialog(plotlyOutput("x_FFT_gender"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_FFT_gender', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_FFT_gender, {removeModal()})

  # ---- OI ----
  output$table_OI <- DT::renderDataTable({tabla_traspuesta(tabla_OI)})
  
  observeEvent(input$show_plot_button_OI, {
    showModal(modalDialog(plotlyOutput("x_OI"), easyClose = TRUE,
                          footer = actionButton('close_plot_button_OI', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_OI, {removeModal()})
  
  output$x_OI = renderPlotly({
    ind = input$table_OI_rows_selected
    if (is.null(ind)) {ind = seq(1,16)}

    tabla_filtrar_OI <- tabla_filtrar(ind, tabla, variable_diferencia = "OI")
    
    plot_ly(data = tabla_filtrar_OI , x = ~periodo, y = ~OI,  width = 1000, height = 600,
            color = ~nombre_region, type = 'scatter',mode = 'lines',
            line = list(dash = ifelse(tabla_filtrar_OI$nombre_region != "Total país", "dash", "solid")),
            colors = mis_colores) %>%
      layout(title = list(text = "Ocupación informal por región", y = 1.6),
             xaxis = list(title = "Período", tickangle = 45), hovermode = "x unified",
             yaxis = list(title = "Ocupación informal", range = c(0, max(tabla_filtrar_OI$OI)+10000)),
             legend = list(x = 0.5,y = 1, orientation = "h", xanchor="center"),
             margin = list(b = 0)) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.99,
             y = 1, 
             sizex = 0.1,
             sizey = 0.1)))
  })

  output$x_OI_gender = renderPlotly({
    ind = input$table_OI_rows_selected
    if (is.null(ind)) {ind = 17}
    ind <- ind[1]
    
    tabla_sexos_filtrada <- tabla_filtrar_sexo(ind, tabla_sexos)
    
    plot_ly(data = tabla_sexos_filtrada, x = ~periodo, y = ~OI, color = ~sexo, 
            type = 'scatter', mode = 'lines', text = ~paste(sexo, ":", OI), width = 1000, height = 600,
            hoverinfo = "text") %>%
      layout(
        title = list(text = "Ocupación informal por sexo", y = 1.6),
        xaxis = list(title = "Período", tickangle = 90), hovermode = "x unified",
        yaxis = list(title = "Ocupación informal", range = c(0, max(tabla_sexos_filtrada$OI)+1000000)),
        legend = list(x = 0.5, y = 1, orientation = "h", xanchor = "center"),
        margin = list(b = 0)
      ) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "Pan", "Autoscale", "select2d", "lasso2d"),
             responsive = F, autosizable = F, showAxisDragHandles = FALSE, 
             showTips = FALSE, locale = "es") %>% 
      layout(images = list(
        list(source = paste('data:image/png;base64', txt, sep=','),
             x = 0.92, y = 1,  sizex = 0.1, sizey = 0.1)))
  })
  observeEvent(input$show_plot_button_OI_gender, {
    showModal(modalDialog(plotlyOutput("x_OI_gender"), easyClose = TRUE, 
                          footer = actionButton('close_plot_button_OI_gender', 'Cerrar', icon('times'))))})
  
  observeEvent(input$close_plot_button_OI_gender, {removeModal()})
}
shinyApp(ui, server)
