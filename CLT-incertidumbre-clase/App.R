library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cosmo"),
  titlePanel('Ejercicio sobre distribuciones y evaluación de la incertidumbre en R'),
  
  selectInput(
    inputId = 'num',
    label = 'Por favor, seleccione el último dígito de su documento de identidad',
    choices = c(0,1,2,3,4,5,6,7,8,9),
    selected = NULL,
    multiple = FALSE,
    selectize = TRUE,
    width = NULL,
    size = NULL
  ),

  # sliderInput(inputId = 'num',
  #             label = 'Por favor, seleccione el último dígito de su documento de identidad',
  #             min = 0,
  #             max = 9,
  #             value = 0),
  
  mainPanel('A continuación, puede encontrar los resultados experimantales a partir de los cuales debe encontrar la concentración de la solución problema mediante 2 métodos analíticos:'),
  
  
  
  mainPanel(dataTableOutput('data')),
  
  sidebarPanel(
    

    
    # Button
    downloadButton("downloadData", "Descargar")
    
  )
)

datos <- list(10)
masa.muestra <- c(rep(0.050,10),
                  rep(0.100,10),
                  rep(0.250,10))

volumen.aforo <- c(rep(0.050,10),
                   rep(0.100,10),
                   rep(0.250,10))


alicuota.mL <- c(rep(10,10),
                 rep(NA, 5),
                 rep(10,5),
                 rep(NA,10))

alicuota.g <- c(rep(NA,10),
                rep(10, 5),
                rep(NA,5),
                rep(10,10))

balanza.alicuota <- c(rep(NA,10),
                      rep('XXX', 5),
                      rep(NA,5),
                      rep('XXX',10))

pH.final <- c(rep(8,5),
              rep(10, 5),
              rep(10,5),
              rep(NA,15))

clase.bureta <- c(rep('A',5),
                  rep('B', 5),
                  rep('A',5),
                  rep(NA,15))

balanza.secado <- c(rep(NA,5),
                    rep(NA, 5),
                    rep(NA,5),
                    rep('XXX',15))

balanza.final <- c(rep(NA,5),
                   rep(NA, 5),
                   rep(NA,5),
                   rep('XXX',5),
                   rep('YYY',5),
                   rep('ZZZ',5))


tabla <- cbind(masa.muestra, volumen.aforo, alicuota.mL, alicuota.g, balanza.alicuota, pH.final, clase.bureta, balanza.secado, balanza.final)
colnames(tabla) <- c('masa muestra (g)', 
                     'volumen dilución (L)', 
                     'alicuota (mL)', 
                     'alicuota (g)',
                     'balanza alicuota', 
                     'pH final', 
                     'clase bureta', 
                     'balanza secado', 
                     'balanza final')

tabla <- as.data.frame(tabla)
# triple <- list(3)
# 
# for(i in 1:3){
#   triple[[i]] <- tabla
#   
# }
# for(i in 1:10){
#   datos[[i]] <- triple
#   
# }


server <- function(input, output){


  

   
  
 # 
     # tabla$masa.final <- res
     
output$data <- renderDataTable({
  
  set.seed(1234)
  
  res<- rnorm(30, mean = 0.1+(as.numeric(input$num)*0.1), sd = 0.001)

  res<- as.data.frame(res)
tabla$masa.final <- res

tabla

 
     })

tablon <- reactive({tabla})
     

# Downloadable csv of selected dataset ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste("resultados", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(res, file, row.names = FALSE)
  }
)


}

shinyApp(ui = ui, server= server)
