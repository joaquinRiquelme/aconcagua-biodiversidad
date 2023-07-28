library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(plotly)
library(nycflights13)
library(histoslider)
library(rlang)
library(htmltools)
library(leaflet)


parametros <- list(
  color = "#236478",
  round_digits = 1,
  font_family = "Raleway",
  font_family_code = "Source Code Pro",
  tabla_datos = "estaciones_datos",
  tabla_estaciones = "estaciones",
  paleta = c("#730000","#E60000","#FFAA00","#FFD37F","#FFFF00","#FFFFFF",
             "#8CCDEF","#00BFFF","#1D90FF","#4169E1","#0000FF"),
  etiquetas = c("Sequía excepcional", "Sequía extrema", "Sequía severa",
                "Sequía moderada", "Anormalmente seco","Normal",
                "Anormalmente húmedo","Moderadamente húmedo","Severamente húmedo",
                "Extramademente húmedo", "Excepcionalmente húmedo")
)
theme_odes <-  bs_theme(
  version = 5,
  primary = parametros$color,
  base_font = font_google(parametros$font_family),
  code_font = font_google(parametros$font_family_code)
)

# Data prep
data.inicial <- read.csv("Biodiversidad-Topografia.csv")
data.inicial$Longitud <- round(data.inicial$Longitud,2)
data.inicial$Latitud <- round(data.inicial$Latitud,2)
biodiversidad <- tibble(data.inicial[,c("Sitio","Riqueza","Shannon","Simpson")])
Sitio <- tibble(data.inicial[,c("Sitio","Longitud","Latitud")])
Topografica <- tibble(data.inicial[c("Sitio","Elevación","Pendiente.Porcentaje","Exposición")])
PRIMARY <- "#68B47D"
biodiversidad <- biodiversidad %>%
  left_join(Sitio) %>%
  left_join(Topografica)

print(biodiversidad)
print(summary(biodiversidad))
plot(biodiversidad[,c("Riqueza","Shannon","Simpson","Elevación","Pendiente.Porcentaje","Exposición")])

ui <-page_navbar(
  title  = tags$span(
    class = "title",
    tags$a(
      tags$img(src = "horizontal_SB_blanco.png", height = "30px", style = "margin-top: -5px"),
      href = "https://odes-chile.org/"
    ),
    "Biodiversidad"
  ),
  id = "nav",
  lang = "es",
  theme = theme_odes,
  fillable = TRUE,
  fillable_mobile = TRUE,
  # sidebar -----------------------------------------------------------------
  sidebar = sidebar(
    width = 400,
  #   selectInput("unidad", tags$small("Sitio"), opt_unidad),
  #   selectInput("macrozona", tags$small("Macrozona"), opt_macrozona, multiple = FALSE), # selected = "zona central",
  #   selectInput("variable", tags$small("Variable"), opt_variable, selected = "pre"),
  #   sliderTextInput("fecha", tags$small("Fecha"), opt_fecha, selected = c(tail(opt_fecha, 12 * 10)[1], tail(opt_fecha, 1))),
  #   
  #   conditionalPanel(
  #     "input.showchart",
  #     # "hchart va en 2do contitaion panel",
  #     highchartOutput("chart", width = "100%", height = "250px"),
  #     div(
  #       style="display:inline-block;float:right",
  #       downloadButton("descargar_datos_mini", "Descargar datos", class = "btn-primary btn-sm")
  #     )
  #     # tags$br(),
  #   ),
  #   conditionalPanel(
  #     "false",
  #     checkboxInput("showchart", "Mostrar información histórica"),
  #   ),
  #   # actionButton("guidess", "Guide")
  # )
  accordion(
    open = c("Sitio", "Destination"),
    accordion_panel(
      "Sitio", icon = icon("location-dot"),
      uiOutput("sitio_reset"),
      checkboxGroupInput(
        "sitio", NULL,
        choices = sort(unique(biodiversidad$Sitio)),
        inline = TRUE,
        selected = sort(unique(biodiversidad$Sitio))
      )
    ),
    accordion_panel(
      "Biodiversidad", icon = icon("seedling"),
      input_histoslider(
        "diversidad_riqueza", "Riqueza taxonómica",
        biodiversidad$Riqueza, height = 150,
        breaks=seq(1,round(max(biodiversidad$Riqueza),0)+1,1),
        options = list(
          handleLabelFormat = "0d",
          selectedColor = PRIMARY
        )
      ),
      input_histoslider(
        "diversidad_shannon", "Índice de Shannon",
        biodiversidad$Shannon, height = 150,
        breaks=seq(1,round(max(biodiversidad$Shannon),0),1),
        options = list(
          handleLabelFormat = "0d",
          selectedColor = PRIMARY
        )
      ),
      input_histoslider(
        "diversidad_simpson", "Índice de Simpson",
        biodiversidad$Simpson, height = 150, #breaks = "months",
        breaks=seq(1,5,1),
        options = list(
          handleLabelFormat = "0d",
          selectedColor = PRIMARY
        )
      )
    ),
    accordion_panel(
      "Topografía", icon = icon("mountain"),
      input_histoslider(
        "topografica_elevacion", "Elevación (msnm)",
        biodiversidad$Elevación, height = 150,
        options = list(     handleLabelFormat = "0d",
                            selectedColor = PRIMARY)
      ),
      input_histoslider(
        "topografica_pendiente", "Pendiente (%)",
        biodiversidad$Pendiente.Porcentaje, height = 150,
        breaks=seq(0,45,5),
        options = list(selectedColor = PRIMARY)
      ),
      input_histoslider(
        "topografica_exposición", "Exposición",
        biodiversidad$Exposición, height = 150,
        breaks=seq(0,360,45),
        options = list(selectedColor = PRIMARY)
      )
    )
  ))
  
  ,
  # mapa --------------------------------------------------------------------
  bslib::nav_panel(
    title = "Aplicación",
    icon  = icon("map-location-dot"),
    tags$head(
      tags$link(href = "Isotip_gradiente_azul.png", rel = "icon"),
      tags$script(src = "https://www.googletagmanager.com/gtag/js?id=G-CYG993XQRT", async = ""),
      tags$script(src = "js/ga.js"),
      includeCSS("www/css/styles.css"),
    ),
    fixedRow(
    # fluidRow(
      # column(width = 8,
      column(width = 10,
    plotOutput("plot",click = "plot_click")),#, height = "300px", click = "plot_click"),
    # )),
    fluidRow(
      column(width = 4,
             leafletOutput("mapa")),# width="40%", height="30%")),
      column(width = 6,
             tableOutput("data"))
  # ))),
  ))),
  bslib::nav_panel(
    title = "Ayuda",
    icon  = icon("question"),
    layout_column_wrap(
      width = 1,
      navset_card_tab(
        # height = 450,
        # full_screen = TRUE,
        # title = "HTML Widgets",
        nav_panel(
          "Aplicación",
          includeMarkdown("md/ayuda.md")
        ),
        nav_panel(
          "Indicadores",
          includeMarkdown("md/indicadores.md")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # 
  # biodiversidad <- reactive({
  #   biodiversidad[ is.element(biodiversidad()["Sitio"], sitio=updateSelectInput), ]
  # })
  
  
  
  output$plot <- renderPlot({
    sitios.i <- unique(input$sitio)
    riqueza.i <- summary(input$diversidad_riqueza)
    shannon.i <- summary(input$diversidad_shannon)
    simpson.i <- summary(input$diversidad_simpson)
    elevacion.i <- summary(input$topografica_elevacion)
    pendiente.i <- summary(input$topografica_pendiente)
    exposicion.i <- summary(input$topografica_exposición)
    
    biodiversidad.i <- subset(biodiversidad, is.element(Sitio, sitios.i) & 
                                Riqueza>=riqueza.i["Min."] & Riqueza<=riqueza.i["Max."]& 
                                Shannon>=shannon.i["Min."] & Shannon<=shannon.i["Max."]& 
                                Simpson>=simpson.i["Min."] & Simpson<=simpson.i["Max."]& 
                                Elevación>=elevacion.i["Min."] & Elevación<=elevacion.i["Max."]& 
                                Pendiente.Porcentaje>=pendiente.i["Min."] & Pendiente.Porcentaje<=pendiente.i["Max."]& 
                                Exposición>=exposicion.i["Min."] & Exposición<=exposicion.i["Max."])
    
    ggplot(biodiversidad.i, aes(Elevación, Riqueza)) + geom_point()+
      xlim (0, 2500)+
      ylim(0,12.5)+
      labs(title("Riqueza taxonómica vs elevación"))+
      ylab('Riqueza taxonómica') +
      xlab('Elevación (msnm)')
  }, res = 96, height = 400)
  
  
  output$mapa <-  renderLeaflet({
    sitios.i <- unique(input$sitio)
    riqueza.i <- summary(input$diversidad_riqueza)
    shannon.i <- summary(input$diversidad_shannon)
    simpson.i <- summary(input$diversidad_simpson)
    elevacion.i <- summary(input$topografica_elevacion)
    pendiente.i <- summary(input$topografica_pendiente)
    exposicion.i <- summary(input$topografica_exposición)
    
    biodiversidad.i <- subset(biodiversidad, is.element(Sitio, sitios.i) & 
                                Riqueza>=riqueza.i["Min."] & Riqueza<=riqueza.i["Max."]& 
                                Shannon>=shannon.i["Min."] & Shannon<=shannon.i["Max."]& 
                                Simpson>=simpson.i["Min."] & Simpson<=simpson.i["Max."]& 
                                Elevación>=elevacion.i["Min."] & Elevación<=elevacion.i["Max."]& 
                                Pendiente.Porcentaje>=pendiente.i["Min."] & Pendiente.Porcentaje<=pendiente.i["Max."]& 
                                Exposición>=exposicion.i["Min."] & Exposición<=exposicion.i["Max."])
    
    
    leaflet() %>% addTiles() %>%
    addCircles(data = biodiversidad.i, lat = ~Latitud, lng = ~Longitud)
  
  })
  
  
  output$data <- renderTable({
    # req(input$plot_click)
    nearPoints(biodiversidad[,c("Sitio","Riqueza","Shannon", "Simpson","Elevación","Pendiente.Porcentaje","Exposición")], input$plot_click)
  })
}


shinyApp(ui, server)

  