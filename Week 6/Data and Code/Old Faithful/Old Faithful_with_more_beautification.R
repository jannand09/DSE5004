# ggplot version of the Old Faithful Geyser example

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 40,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textOutput("testText")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # draw the histogram with the specified number of bins
        g <- ggplot(data = faithful) + 
          geom_histogram(aes(x=waiting),bins=input$bins)+
          xlim(40,100)+         # keeps the x-axis constant
          xlab("Time between eruptions (mins)") +
          ggtitle("Eruption distribution") + 
          theme_minimal() +
          theme(plot.title=element_text(size = 16, face = "bold"), #Title Font and Size
                axis.text=element_text(size=12), #Axis Label Font and Size
                axis.title=element_text(size=14,face="bold")) #Axis Title Font and Size
        print(g)
    })
    
    output$testText <- renderText({paste0(input$bins," bins")})
}

# Run the application 
shinyApp(ui = ui, server = server)
