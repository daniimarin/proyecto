#Universidad de Costa Rica 
#Proyecto fianl de computacional 
#Stefanny Granados y Daniela Marin


#Cargar liberias

install.packages("shiny")
install.packages("dplyr")
install.packages("ggplot2")  # Agregar ggplot2 para graficar
install.packages("countrycode")

library(shiny)
library(dplyr)
library(ggplot2)
library(countrycode)

# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# UI de la aplicación
#se creo el primer boton
ui <- fluidPage(
  titlePanel("Estadísticas de las votaciones"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "checkbox_votos", 
                         label = h5("Seleccionar votos:"),
                         choices = list("Sí" = 1, "Se abstiene" = 2, "No" = 3),
                         selected = 1),
      actionButton(inputId = "entrada2", label = h5("Porcentaje país")),#genera la tabla de porcentaje por pais
      radioButtons("entrada_grafico", "Seleccionar tipo de gráfico:",#crea las casillas para seleccionar geafico
                   choices = list("Gráfico porcentaje por año" = "porcentaje_anno", "Gráfico de línea" = "linea", " Gráfico Facet" = "facet"),
                   selected = "Gráfico porcentaje por año")
    ),
    mainPanel(
      plotOutput("grafico_votos"),#ejecucion del boton
      tableOutput("tabla_votos"),
      tableOutput("tabla_pais")
    )
  )
)

# Server de la aplicación
#primera parte de la tarea
server <- function(input, output) {
  
  datos_filtrados <- reactive({
    Votos1 <- votes %>%
      filter(vote %in% input$checkbox_votos) %>%
      mutate(year = 1945 + session,
             country = countrycode(ccode, "cown", "country.name")) %>%
      mutate(country = recode(country,
                              'United States of America' = 'United States',
                              'United Kingdom of Great Britain and Northern Ireland' = 'United Kingdom'))
    Votos1
  })
  #tabla por tipo de voto
  output$tabla_votos <- renderTable({
    votos_filtrados <- datos_filtrados()
    
    votos_resumen <- votos_filtrados %>%
      group_by(vote) %>%
      summarise(
        Total = n(),
        Porcentaje = (Total / nrow(votos_filtrados)) * 100
      )
    
    votos_resumen
  })
  #ordena tabla de menor a mayor
  observeEvent(input$entrada2, {
    Votos1 <- datos_filtrados()
    Orden_Pais <- Votos1 %>%
      filter(vote == 1) %>%
      group_by(country) %>%
      summarise(
        Total = n(),
        Porcentaje = (Total / nrow(Votos1)) * 100
      ) %>%
      arrange(desc(Porcentaje))
    
    output$tabla_pais <- renderTable({
      Orden_Pais
    })
  })
  #crear graficos finales
      output$grafico_votos <- renderPlot({
        Votos1 <- datos_filtrados()
        
        if (input$entrada_grafico == "") {
          return(NULL)
        }
        
        if (input$entrada_grafico == "linea") {
          Porcentaje_Anno_Pais <- Votos1 %>%
            filter(vote == 1) %>%
            group_by(year, country) %>%
            summarise(
              Total = n(),
              .groups = 'drop' # para evitar mensajes de agrupación
            ) %>%
            mutate(Porcentaje = (Total / sum(Total)) * 100)
          ggplot(Porcentaje_Anno_Pais, aes(x = year, y = Porcentaje, color = country, group = country)) +
            geom_line() +
            geom_point() +
            labs(title = "Porcentaje de votos 'Sí' por año y país",
                 x = "Año",
                 y = "Porcentaje",
                 color = "País") +
            theme_minimal()
        } else if (input$entrada_grafico == "facet") {
          Porcentaje_Anno_Pais <- Votos1 %>%
            filter(vote == 1) %>%
            group_by(year, country) %>%
            summarise(
              Total = n(),
              .groups = 'drop' # para evitar mensajes de agrupación
            ) %>%
            mutate(Porcentaje = (Total / sum(Total)) * 100)
          ggplot(Porcentaje_Anno_Pais, aes(x = year, y = Porcentaje)) +
            geom_line() +
            geom_point() +
            labs(title = "Porcentaje de votos 'Sí' por año",
                 x = "Año",
                 y = "Porcentaje") +
            facet_wrap(~country) +
            theme_minimal()
        } else if (input$entrada_grafico == "porcentaje_anno") {
          Porcentaje_Anno <- Votos1 %>%
            filter(vote == 1) %>%
            group_by(year) %>%
            summarise(
              Total = n(),
              Porcentaje = (Total / nrow(Votos1)) * 100
            )
          
          ggplot(Porcentaje_Anno, aes(x = year, y = Porcentaje, group = 1)) +
            geom_line(color = "green") +
            geom_point(color = "blue") + 
            labs(title = "Porcentaje de votos 'Sí' por año",
                 x = "Año",
                 y = "Porcentaje") +
            scale_x_continuous(breaks = seq(min(Porcentaje_Anno$year), max(Porcentaje_Anno$year), by = 10)) +
            theme_minimal()
        }
      })
    }
    
    # Ejecutar la aplicación Shiny
shinyApp(ui = ui, server=server)
    
          
          

    
      
  
