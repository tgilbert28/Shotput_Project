library(shiny)


# Define UI for application that draws a histogram
ui = fluidPage(

    # Application title
    titlePanel("Shotput Throw Distance Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("o",
                        "Shotput Release Angle:",
                        min = 1,
                        max = 45,
                        value = 36,
                        step = 0.5),
            sliderInput("starth",
                        "Shotput Release Height:",
                        min = 1,
                        max = 3,
                        value = 1.6,
                        step = 0.1),
            sliderInput("v",
                        "Shotput Release Velocity:",
                        min = 1,
                        max = 20,
                        value = 14,
                        step = 0.1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server = function(input, output) {

    output$distPlot = renderPlot({
        x = c(0:30)
        v <- as.integer(input$v)
        rad = (3.14/180)
        o <- as.integer(input$o)
        g = 9.8
        starth <- as.integer(input$starth)
        
        r = (as.numeric(v^2) * as.numeric((sin(2*(o*rad)))) / as.numeric(g))
        
        h = (r/2)
        
        k = ((as.numeric(v^2) * as.numeric((sin(o*rad))^2) / as.numeric((2 * g)) + as.numeric(starth)))
        
        dist = (as.numeric(-16.62131) + as.numeric(2.31246*v) + as.numeric(0.1185*o) + as.numeric(0.79426*starth))
        
        a = (as.numeric(-k) / as.numeric((dist - h)^2))
        c = (as.numeric(starth))
        b = ((-(as.numeric(a) * as.numeric(dist^2) + as.numeric(c))) / as.numeric(dist))
        
        
        y = (as.numeric(a) * as.numeric(x^2) + as.numeric(b) * as.numeric(x) + as.numeric(c) )
        updatedist = round(dist, digits = 2)
        plot(x, y, type = 'l', ylim = c(0,10), main = paste("Distance:",updatedist,"Meters"), xlab = "Distance (meters)", ylab = "Height (meters)")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
