#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(openxlsx)
library(data.table)
library(ckanr)


ckanr_setup(url = "https://datos.minfin.gob.gt/")
id_ejecucion <- "3635b9c2-5f0e-43ec-b3ce-4a006e029c57"
id_ejecucion2016 <- "c1043a6e-a4af-478a-b06e-ebc2eb0e9a3"
sql <- paste0('SELECT DISTINCT "Tipo Gobierno","Entidad"   from \"', id_ejecucion, '"\  WHERE "Tipo Gobierno"=\'ADMINISTRACIÓN CENTRAL\'  ')
categorias <- ds_search_sql(sql, as = 'table')
categorias <- as.list( categorias$records$Entidad )


# datos <- as.data.table( read.xlsx('/mnt/Datos/GitHub/Transparencia/ejecucion_mensualizada_2017.xlsx', sheet = 1) )
# entidades <- datos %>%
#   select(Entidad,Tipo.Gobierno) %>%
#   filter(Tipo.Gobierno == "ADMINISTRACIÓN CENTRAL")
# entidades <- levels( as.factor(entidades[[1]]) )  

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Ejecución presupuestaria"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
     sidebarPanel(
       selectInput("Entidad", h4("Seleccione la entidad"),
                   choices = categorias,
                   selected = NULL
       ),
       

      selectInput("Opcion", h4("¿Qué desea consultar de esta entidad?"),
                     choices = c("Unidad ejecutora" = "1",  "Programa" = "2", "Ejecución por mes" = "3" )
      ),
      
      conditionalPanel(
        condition = "input.Opcion == '1' ",
        actionButton('btAccion','Ver las unidades ')
      ),
      
      conditionalPanel(
        condition = "input.Opcion == '2' ",
        actionButton('btAccion','Ver los Programas ')
      ),
      
      conditionalPanel(
        condition = "input.Opcion == '3' ",
        actionButton('btAccion','Ver los meses disponibles ')
      ),
      
      uiOutput(outputId = "Opciones")
      
     ),



     
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  ## keep track of elements inserted and not yet removed
  inserted <- c()
  
  observeEvent(
    input$btAccion, {
      btn <- input$btAccion
      id <- paste0('txt', btn)
      label_opciones <- ""
      if(input$Opcion == 1 )
        label_opciones = "Selecciones las unidades ejecutoras:"
      else if( input$Opcion == 2 )
        label_opciones = "Selecciones los programas:"
      output$Opciones <- renderUI({
        selectInput("OpcionesLista",label_opciones, c("j"))
      })
    }
  )
  
  # consulta_sql = paste0('select distinct "Programa", "Entidad" from "',
  #                       id_ejecucion, '" where "Entidad" = \'', input$Entidad, "\'")
  # resultado <- ds_search_sql(consulta_sql, as = 'table')
  
   output$distPlot <- renderPlot({
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

