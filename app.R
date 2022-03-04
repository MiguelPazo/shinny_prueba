################
################ MIGUEL PAZO SANCHEZ
################

# install.packages("tidyverse")
# install.packages("ggplot2")

library(shiny)
library(ggplot2)


ui <- fluidPage(
    titlePanel("Práctica 1"),

    sidebarLayout(
        sidebarPanel(
            helpText("Pulsa para usar el dataset faithful. Por defecto se usará MPG"),
            
            checkboxInput("checkFaith", "Usar faithful", value = FALSE),
            
            helpText("La opción variable solo se usará cuendo estemos usando el dataset MPG"),
            
            selectInput("selectMpg", "Variable de MPG:",
                        c("displ" = "displ",
                          "cyl" = "cyl",
                          "cty" = "cty",
                          "hwy" = "hwy")),
            
            sliderInput("bins",
                        "Cantidad de columnas:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        mainPanel(
           plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {

    output$distPlot <- renderPlot({
      
        colnames(faithful) 
        xLabel <- "x"
        gTitle <- ""
        data <- NULL
      
        if(!input$checkFaith) {
          gTitle <- "Gráfica con datos de MPG"
          xLabel <- input$selectMpg
          
          data <- mpg
          
          switch(input$selectMpg, 
                 "displ" = {
                   x <- mpg$displ
                 },
                 "cyl" = {
                   x <- mpg$cyl
                 },
                 "cty" = {
                   x <- mpg$cty
                 },
                 "hwy" = {
                   x <- mpg$hwy
                 }
          )
        } else {
          gTitle <- "Gráfica con datos de faithful"
          xLabel <- "waiting"
          
          data <- faithful
          x <- faithful$waiting
        }
        
        grafica <- ggplot(data=data, aes(x=x)) + 
          geom_histogram(fill="skyblue", 
                         alpha=1, 
                         bins = input$bins) +
          ylab("count") +
          xlab(xLabel) +
          ggtitle(gTitle) +
          theme_minimal()
        
        grafica
    })
}

shinyApp(ui = ui, server = server)
