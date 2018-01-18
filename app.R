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
web()

data <- read.csv('Alimentación_para_Transparencia_Fiscal.csv')

nombres.meses <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO",
                   "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")

current_year <- format(Sys.Date(), "%Y")


data <- data[ which(data$Ejercicio == current_year), ]

categorias <- levels( data$Entidad )
  




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
      resultado <- data %>%
        distinct( Entidad, Unidad.Ejecutora )%>%
        filter(Entidad == input$Entidad)
      mostrable <-  checkboxGroupInput("unidadesEjecutoras", h5("Seleccione las unidades ejecutoras de su interés:"),
                                       choices = resultado$Unidad.Ejecutora
                                       
                                       )
    }else if(input$Opcion == 2){
      resultado <- data %>%
        distinct( Entidad, Programa )%>%
        filter(Entidad == input$Entidad)
      mostrable <-  checkboxGroupInput("programas", h5("Seleccione las unidades ejecutoras de su interés:"),
                                       choices = resultado$Programa  
                                       
      )
    }else if(input$Opcion == 3){
      resultado <- data %>%
        distinct( Entidad, Nombre.Mes ) %>%
        filter( Entidad == input$Entidad )
      factor( resultado$Nombre.Mes, levels = nombres.meses)
      #levels( resultado$Nombre.Mes ) <- nombres.meses
      meses <- factor( as.character( resultado$Nombre.Mes ), levels = nombres.meses) 
      mostrable <-  checkboxGroupInput("meses", h5("Seleccione las unidades ejecutoras de su interés:"),
                                       choices = sort(meses) 
                                       
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
      resultado <- data %>%
        select( Unidad.Ejecutora, Devengado, Entidad) %>%
        filter(Entidad == input$Entidad, Unidad.Ejecutora %in% input$unidadesEjecutoras   ) %>%
        group_by( Unidad.Ejecutora ) %>%
        summarise( Devengado = sum(Devengado) )
    }else if( input$Opcion == 2 ){
      resultado <- data %>%
        select(Programa, Devengado, Entidad) %>%
        filter(Entidad == input$Entidad, Programa %in% input$programas   ) %>%
        group_by(Programa) %>%
        summarise( Devengado = sum(Devengado) )
      print(resultado)
    }else if( input$Opcion == 3 ){
      resultado <- data %>%
        select( Nombre.Mes, Devengado, Entidad) %>%
        filter(Entidad == input$Entidad, Nombre.Mes  %in% input$meses   ) %>%
        group_by( Nombre.Mes ) %>%
        summarise( Devengado = sum(Devengado) )
    }
    return(resultado)
    
  })
  
  output$distPlot <- renderPlot({
    data<- retardo()
    data[,2] <- sapply(data[,2], as.numeric)
    g <- graficaCol(data)
    g <- etiquetasHorizontales(g)
    return(retocarGrafica(g))
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)

