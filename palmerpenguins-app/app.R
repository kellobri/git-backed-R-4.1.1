#
# Reference a basic multiple bookmark buttons example on Shiny Dev Center:
# https://shiny.rstudio.com/articles/bookmarking-state.html
#

library(shiny)
library(palmerpenguins)
library(ggplot2)
library(dplyr)

penguin_data <- penguins %>%
    select(-c(sex, year)) %>%
    na.omit()

vars <- setdiff(names(penguin_data), c("species","island"))

ui <- fluidPage(
    tabsetPanel(id = "tabs",
                tabPanel("Cluster",
                         titlePanel("Palmer Penguins k-means clustering"),
                         sidebarPanel(
                                      selectInput('xcol', 'X Variable', vars),
                                      selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
                                      numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
                         ),
                         mainPanel(plotOutput('kmeans')
                         )
                ),
                tabPanel("Visualize",
                         titlePanel("Palmer Penguins are fun to Visualize"),
                         sidebarPanel(width = 3,
                                      selectInput('xpen', 'X Variable', vars),
                                      selectInput('ypen', 'Y Variable', vars, selected = vars[[2]])
                         ),
                         mainPanel(plotOutput('penguinviz')
                         )
                )
    )
)

server <- function(input, output) {
    
    selectedData <- reactive({
        penguin_data[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
        kmeans(selectedData(), input$clusters)
    })
    
    output$kmeans <- renderPlot({
        palette(c("darkorange", "purple", "cyan4", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    output$penguinviz <- renderPlot({
        ggplot(data = penguins, 
               aes_string(x = input$xpen,
                          y = input$ypen)) +
            geom_point(aes(color = species, 
                           shape = species),
                       size = 3,
                       alpha = 0.8) +
            theme_minimal() +
            scale_color_manual(values = c("darkorange","purple","cyan4")) +
            labs(color = "Penguin species",
                 shape = "Penguin species") +
            theme(legend.position = c(0.1, 0.1),
                  legend.background = element_rect(fill = "white", color = NA),
                  plot.title.position = "plot",
                  plot.caption = element_text(hjust = 0, face= "italic"),
                  plot.caption.position = "plot")
    })
}

shinyApp(ui, server)