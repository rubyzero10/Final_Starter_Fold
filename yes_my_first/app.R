#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Distribution of song popularity in generation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput(
            inputId = "number_of_bins",
            label = "Number of bins:",
            min = 1,
            max = 50,
            value = 30
          )
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
        # generate bins based on input$bins from ui.R
      kpop_b |>
        #filter(popularity < 20) |> 
        ggplot(aes(x = popularity, fill = generation)) + 
        geom_histogram(position = "dodge", bins = input$number_of_bins)+
        theme_minimal()+
        scale_fill_brewer(palette = "Set2") +
        labs(x = "Song popularity",
             y = "Occurances",
             fill = "Generation")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
