
library(shiny)

myBetaCon <- function(a, b, n, x) {
  # Numbers for plotting
  numbers <- seq(0, 1, by = 0.01)
  # Beta Parameters for Posterior
  alpha <- x + a
  beta <- n - x + b
  # Posterior
  post <- stats::dbeta(x = numbers, shape1 = alpha, shape2 = beta)
  # Plot
  plot(numbers,
       post,
       type = "l",
       col = "hotpink",
       ylab = "Posterior",
       xlab = "Theta",
       lwd = 2.5
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
    # Application title
    titlePanel("Shiny Beta"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("a",
                        "a",
                        min = 1,
                        max = 20,
                        value = 5),

            sliderInput("b",
                        "b",
                        min = 1,
                        max = 20,
                        value = 7),

            sliderInput("n",
                        "Number of Trials:",
                        min = 1,
                        max = 50,
                        value = 10),

            sliderInput("x",
                        "Number of Successes:",
                        min = 1,
                        max = 20,
                        value = 7)

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    aa <- reactive(input$a)
    bb <- reactive(input$b)
    nn <- reactive(input$n)
    xx <- reactive(input$x)

    output$distPlot <- renderPlot({
      myBetaCon(aa(), bb(), nn(), xx())

    })
}

# Run the application
shinyApp(ui = ui, server = server)
