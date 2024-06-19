#Universidad de Costa Rica 
#Proyecto fianl de computacional 
#Stefanny Grnados y Daniela Marin


# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# UI de la aplicación
ui <- fluidPage(
  titlePanel("Votación"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("checkbox_votos", "Seleccionar votos:",
                         choices = list("Sí" = 1, "Se abstiene" = 2, "No" = 3),
                         selected = 1),
      actionButton("btn_actualizar", "Actualizar"),
      actionButton("btn_pais", "Porcentaje país"),
      radioButtons("radio_grafico", "Seleccionar tipo de gráfico:",
                   choices = list("Porcentaje por año" = "porcentaje_anno", "Gráfico de línea" = "linea", "Facet" = "facet"),
                   selected = "")
    ),
    mainPanel(
      plotOutput("grafico_votos"),
      tableOutput("tabla_votos"),
      tableOutput("tabla_pais")
    )
  )
)

# Server de la aplicación
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
  
  observeEvent(input$btn_pais, {
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
  # Server de la aplicación
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
    
    observeEvent(input$btn_pais, {
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
    # Server de la aplicación
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
      
      observeEvent(input$btn_pais, {
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
      
      output$grafico_votos <- renderPlot({
        Votos1 <- datos_filtrados()
        
        if (input$radio_grafico == "") {
          return(NULL)
        }
        
        if (input$radio_grafico == "linea") {
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
        } else if (input$radio_grafico == "facet") {
          Porcentaje_Anno_Pais <- Votos1 %>%
            filter(vote == 1) %>%
            group_by(year, country) %>%
            summarise(
              Total = n(),
              .groups = 'drop' # para evitar mensajes de agrupación
            ) %>%
            mutate(Porcentaje = (Total / sum(Total)) * 100)
          
          
          

    
      
  
