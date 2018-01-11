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
library(htmltools)
library(funcionesINE)
Sys.setlocale("LC_ALL","es_GT.utf8")
anual()

ckanr_setup(url = "https://datos.minfin.gob.gt/")
id_ejecucion <- "3635b9c2-5f0e-43ec-b3ce-4a006e029c57"
id_ejecucion2016 <- "c1043a6e-a4af-478a-b06e-ebc2eb0e9a3"
sql <- paste0('SELECT DISTINCT "Tipo Gobierno","Entidad"   from \"', id_ejecucion, '"\  WHERE "Tipo Gobierno"=\'ADMINISTRACIÓN CENTRAL\'  ')
categorias <- ds_search_sql(sql, as = 'table')
categorias <- as.list( categorias$records$Entidad )



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
      
      div(h5(htmlOutput("itemsOpciones"), align = "left"), style = "color:black"),
      
      actionButton("hacerGrafica", "Ver información")

     ),



      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  output$itemsOpciones <- renderUI({
    if(input$Opcion == 1){
      mostrable <-  checkboxGroupInput("unidadesEjecutoras", h5("Seleccione las unidades ejecutoras de su interés:"),
                                       choices = c("TuUnidad")
                                       
                                       )
    }else if(input$Opcion == 2){
      consulta_sql = paste0('select distinct "Programa", "Entidad" from "',
                            id_ejecucion, '" where "Entidad" = \'', input$Entidad , "\'")
      resultado <- ds_search_sql(consulta_sql, as = 'table')
      mostrable <-  checkboxGroupInput("programas", h5("Seleccione las unidades ejecutoras de su interés:"),
                                       choices = resultado$records$Programa
                                       
      )
    }else if(input$Opcion == 3){
      consulta_sql = paste0('select distinct "Mes", "Entidad" from "',
                            id_ejecucion, '" where "Entidad" = \'', input$Entidad , "\'")
      resultado <- ds_search_sql(consulta_sql, as = 'table')
      mostrable <-  checkboxGroupInput("meses", h5("Seleccione las unidades ejecutoras de su interés:"),
                                       choices = resultado$records$Mes
                                       
      )
      
    }
  return(mostrable)
  })  
  
  inputParaelIn <- function(lista){
    formato <- ""
    i <- 0
    for( x in lista  ){
      i <- i + 1
      if ( length(lista) == i ){
        formato <- paste0(formato,paste0('\'',x, '\''))
      }else{
        formato <- paste0(formato,paste0('\'',x, '\', '))  
      }
      
    }
    return(formato)
  }
  
  retardo <- eventReactive(input$hacerGrafica, {
    #Acá va la consulta.
    consulta_sql = ""
    if(input$Opcion == 1){
      consulta_sql = "select"
    }else if( input$Opcion == 2 ){
      print( inputParaelIn( input$programas ) )
      consulta_sql = paste0('select "Programa", "Entidad", sum("Devengado") as "Devengado" from  "', id_ejecucion, '" where "Entidad" = \'', input$Entidad,
      '\' and "Programa" in (',inputParaelIn(input$programas), ') group by "Programa", "Entidad"' )
      print(consulta_sql)
      resultado <- ds_search_sql(consulta_sql, as = 'table')
      print(resultado)
    }else if( input$Opcion == 3 ){
      
    }
    return(resultado$records)
    
  })
  
  output$distPlot <- renderPlot({
    data<- retardo()
    data[,3] <- sapply(data[,3], as.numeric)
    g <- graficaBar(data[,c(2,3)])
    return(g)
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)

