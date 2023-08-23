# Esto es una app de Shiny . Pueden correr la aplicación haciendo clic en
# el boton de arriba 'Run App' .
#
# Mas info:
#
#    http://shiny.rstudio.com/
#

##cargo los paquetes que voy a usar
####

library(shiny)
library(tidyverse)
library(tidyr)
library(lubridate)
library(plotly)
library(shinyWidgets)
library(highcharter)

#descargo el dataset 
temp <- tempfile()
download.file(
  "http://datos.salud.gob.ar/dataset/2eff770c-1c2b-4a22-9281-c3b5e9412086/resource/c1253897-d507-41f7-a3e1-6ed756e7243b/download/tasa-mortalidad-infantil-deis-1990-2021.csv",
  temp
)
data <- read.csv(temp)
unlink(temp)

###proceso los datos para graficar
bd <- data %>% pivot_longer(
  cols = !indice_tiempo,
  names_to = "prov",
  values_to = "TMI"
) %>%
  mutate(
    prov = str_sub(prov, 21, nchar(prov)),
    ano = year(ymd(indice_tiempo)),
    indice_tiempo = ymd(indice_tiempo),
    prov=case_when(prov == "cordoba" ~ str_to_title(prov),
                   prov == "caba"  ~ "CABA",
                   prov == "argentina" ~ str_to_title(prov),
                   prov == "corientes" ~ str_to_title(prov),
                   prov == "chaco" ~ str_to_title(prov),
                   prov == "chubut" ~ str_to_title(prov),
                   prov == "neuquen" ~ str_to_title(prov),
                   prov == "misiones" ~ str_to_title(prov),
                   prov == "jujuy" ~ str_to_title(prov),
                   prov == "catamarca" ~ str_to_title(prov),
                   prov == "corrientes" ~ str_to_title(prov),
                   prov == "formosa" ~ str_to_title(prov),
                   prov == "salta" ~ str_to_title(prov),
                   prov == "buenosaires" ~ "Buenos Aires",
                   prov == "santiagodelestero" ~ "Santiago del Estero",
                   prov == "santafe" ~ "Santa Fe",
                   prov == "tierradelfuego" ~ "Tierra del Fuego",
                   prov == "santacruz" ~ "Santa Cruz",
                   prov == "sanjuan" ~ "San Juan",
                   prov == "sanluis" ~ "San Luis",
                   prov == "lapampa" ~ "La Pampa",
                   prov == "larioja" ~ "La Rioja",
                   prov == "entrerios" ~ "Entre Rios",
                   prov == "rionegro" ~ "Rio Negro",
                   TRUE ~ prov  # Mantén el valor original para otros casos
    )
  ) 
unique(bd$prov)
# Defino UI para mi aplicación
ui <- fluidPage(
  # Titulo
  titlePanel("Mortalidad infantil en Argentina"),
  
  # Sidebar con un a select input para seleccionar
  #la o la juris que quiero visualizar
  sidebarLayout(sidebarPanel(
    selectInput(
      inputId = "provincia",
      label = "Selecione una jurisdicción:",
      choices = unique(bd$prov),
      selected = "Argentina",
      ## lo que quiero que muestre seleccionado pro defecto
      multiple = TRUE
    )
    
    
    ### una alternativa de select para usar. Puede encontrar mas opciones en
    #https://shinyapps.dreamrs.fr/shinyWidgets/
    ## prueben descomentando las siguientes 9 filas
    # ,
    # pickerInput(
    #   inputId = "provincia2",
    #   label = "Selecione una jurisdicción:",
    #   choices = unique(bd$prov),
    #   multiple = TRUE,
    #   selected = "argentina",
    #   options = list(
    #     `actions-box` = TRUE))
  ),
  
  
  # Muestro el gráfico
  mainPanel( highchartOutput("Plot"))
  ))


# Defino server
server <- function(input, output) {
  
  output$Plot <- renderHighchart({
    # armo el grafico con highchart
    hc <- highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(text = "Serie de tiempo de TMI por 1000 nacidos vivos, Argentina, periodo 1990-2021") %>%
      hc_xAxis(title = list(text = "Año")) %>%
      hc_yAxis(title = list(text = "TMI ")) %>% 
      hc_credits(
        enabled = TRUE, text = "Fuente: Datos abiertos/DEIS", href = "https://datos.gob.ar/dataset/salud-tasa-mortalidad-infantil", style = list(fontSize = "12px")
      ) %>% 
      hc_exporting(enabled = TRUE) # enable exporting option
    bd <- bd %>% filter(prov %in% input$provincia )
    niveles_prov <- unique(bd$prov)
    # Agrega una serie de datos para cada nivel de "prov"
    for (nivel in niveles_prov) {
      data_serie <- bd[bd$prov == nivel, ]  
      hc <- hc %>%
        hc_add_series(data_serie, "line", hcaes(x = ano, y = TMI), name = nivel,
                      marker = list(radius = 4))
    }
    print(hc)
    
  })
}

# Corro la application
shinyApp(ui = ui, server = server)
