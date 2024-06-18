#Proyecto fianl 
#Stefanny y Daniela 
# 


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