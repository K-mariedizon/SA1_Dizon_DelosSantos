library(shiny)

ui <- fluidPage(
  titlePanel("Random Variable Analysis"),
  tabsetPanel(
    tabPanel("Univariate",
             sidebarLayout(
               sidebarPanel(
                 textInput("values_uni", "Enter the values of the random variable (separated by spaces):"),
                 textInput("probabilities_uni", "Enter the probabilities associated with each value (separated by spaces):"),
                 actionButton("calculate_uni", "Calculate")
               ),
               mainPanel(
                 textOutput("mean_uni"),
                 textOutput("variance_uni"),
                 plotOutput("pmf_uni"),
                 plotOutput("cdf_uni")
               )
             )),
    tabPanel("Bivariate",
             sidebarLayout(
               sidebarPanel(
                 textAreaInput("values_bi", "Enter the values of the random variable (separated by spaces, use newline for different values):"),
                 textAreaInput("probabilities_bi", "Enter the probabilities associated with each value (separated by spaces, use newline for different values):"),
                 actionButton("calculate_bi", "Calculate")
               ),
               mainPanel(
                 plotOutput("marginal_x"),
                 plotOutput("marginal_y"),
                 plotOutput("conditional_x_given_y"),
                 plotOutput("conditional_y_given_x")
               )
             ))
  )
)

server <- function(input, output) {
  observeEvent(input$calculate_uni, {
    x <- as.numeric(strsplit(input$values_uni, " ")[[1]])
    p <- as.numeric(strsplit(input$probabilities_uni, " ")[[1]])
    
    if (!all(p >= 0 & p <= 1)) {
      output$mean_uni <- renderText("Probabilities must be in the interval [0, 1]")
      output$variance_uni <- renderText("")
      output$pmf_uni <- renderPlot(NULL)
      output$cdf_uni <- renderPlot(NULL)
      return()
    }
    
    if (sum(p) != 1) {
      output$mean_uni <- renderText("Probabilities must sum to one")
      output$variance_uni <- renderText("")
      output$pmf_uni <- renderPlot(NULL)
      output$cdf_uni <- renderPlot(NULL)
      return()
    }
    
    mean_val <- sum(x * p)
    variance_val <- sum((x - mean_val)^2 * p)
    
    output$mean_uni <- renderText(paste("Mean:", mean_val))
    output$variance_uni <- renderText(paste("Variance:", variance_val))
    output$pmf_uni <- renderPlot({
      barplot(p, names.arg = x, xlab = "Random Variable", ylab = "Probability",
              main = "Probability Mass Function (PMF)")
    })
    output$cdf_uni <- renderPlot({
      plot(x, cumsum(p), type = "s", xlab = "Random Variable", ylab = "Cumulative Probability",
           main = "Cumulative Distribution Function (CDF)")
    })
  })
  
  observeEvent(input$calculate_bi, {
    values <- as.matrix(read.table(text = input$values_bi, header = FALSE))
    probabilities <- as.matrix(read.table(text = input$probabilities_bi, header = FALSE))
    
    if (!all(probabilities >= 0 & probabilities <= 1)) {
      output$mean_x_bi <- renderText("Probabilities must be in the interval [0, 1]")
      output$mean_y_bi <- renderText("Probabilities must be in the interval [0, 1]")
      output$variance_x_bi <- renderText("")
      output$variance_y_bi <- renderText("")
      output$pmf_x_bi <- renderPlot(NULL)
      output$pmf_y_bi <- renderPlot(NULL)
      output$cdf_x_bi <- renderPlot(NULL)
      output$cdf_y_bi <- renderPlot(NULL)
      output$marginal_x <- renderPlot(NULL)
      output$marginal_y <- renderPlot(NULL)
      output$conditional_x_given_y <- renderPlot(NULL)
      output$conditional_y_given_x <- renderPlot(NULL)
      return()
    }
    
    if (sum(probabilities) != 1) {
      output$mean_x_bi <- renderText("Probabilities must sum to one")
      output$mean_y_bi <- renderText("Probabilities must sum to one")
      output$variance_x_bi <- renderText("")
      output$variance_y_bi <- renderText("")
      output$pmf_x_bi <- renderPlot(NULL)
      output$pmf_y_bi <- renderPlot(NULL)
      output$cdf_x_bi <- renderPlot(NULL)
      output$cdf_y_bi <- renderPlot(NULL)
      output$marginal_x <- renderPlot(NULL)
      output$marginal_y <- renderPlot(NULL)
      output$conditional_x_given_y <- renderPlot(NULL)
      output$conditional_y_given_x <- renderPlot(NULL)
      return()
    }
    
    x <- values[, 1]
    y <- values[, 2]
    p <- as.vector(probabilities)
    
    
    marginal_x <- colSums(probabilities)
    marginal_y <- rowSums(probabilities)
    
    
    conditional_x_given_y <- probabilities / marginal_y
    conditional_y_given_x <- t(t(probabilities) / marginal_x)
    
    output$marginal_x <- renderPlot({
      barplot(marginal_x, names.arg = x, xlab = "Random Variable X", ylab = "Probability",
              main = "Marginal Distribution of X")
    })
    
    output$marginal_y <- renderPlot({
      barplot(marginal_y, names.arg = y, xlab = "Random Variable Y", ylab = "Probability",
              main = "Marginal Distribution of Y")
    })
    
    output$conditional_x_given_y <- renderPlot({
      barplot(conditional_x_given_y, beside = TRUE, legend.text = TRUE, xlab = "Random Variable X | Y",
              ylab = "Probability", main = "Conditional Distribution of X given Y")
    })
    
    output$conditional_y_given_x <- renderPlot({
      barplot(conditional_y_given_x, beside = TRUE, legend.text = TRUE, xlab = "Random Variable Y | X",
              ylab = "Probability", main = "Conditional Distribution of Y given X")
    })
  })
}

shinyApp(ui = ui, server = server)
