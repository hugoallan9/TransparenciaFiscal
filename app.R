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
sql <- paste0('SELECT DISTINCT "Tipo Gobierno","Entidad"   from \"', id_ejecucion, '"\  WHERE "Tipo Gobierno"=\'ADMINISTRACIÓN CENTRAL\'  ')
categorias <- ds_search_sql(sql, as = 'table')
categorias <- categorias$records$Entidad


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
    selectInput("Entidad", h4("Seleccione la entidad"),
                choices = categorias
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

