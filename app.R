library(shiny)
library(ggplot2)
library(latex2exp)

url.link <- paste(
  "All source code for this demo can be found on",
  a("GitHub.",
    href = "https://github.com/mark-andrews/binomial_test_v2",
    target = "_blank"
  )
)

ui <- fluidPage(
  withMathJax(),

  # Application title
  titlePanel("Inference of number of gold coins in a box"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText(
        "Imagine we have a box with gold and silver coins. 
        We sample, with replacement, \\(n\\) coins from this box and
        count the number of gold coins, which we will denote by \\(m\\).",
        br(),
        br(),
        "From this data, i.e., \\(m\\) gold coins in a sample of \\(n\\) coins, what can we infer about the true proportion of gold coins in the box?",
        br(),
        br(),
        "We can use statistical sampling theory to calculate how
        compatible the observed data are with any hypothesis 
        about the true proportion.",
        br(),
        br(),
        "The measure of compatibility is known as the", em("p-value."),
        br(),
        br(),
        
      "Calculate the p-value for a hypothesis test on the true value
       of a coin's bias after observing the outcomes of a series of coin flips.
       Use the controls to specify the total number of coin flips,
       and the total number of times a Heads is the observed outcome.
       The areas shaded in blue represent the probability of observing an outcome
       as or more extreme than the one observed assuming the hypothesis being tested is true."),
      sliderInput("n",
        "Sample size \\(n\\):",
        min = 10,
        max = 100,
        value = 50,
        step = 5
      ),
      uiOutput("n.obs.slider"),
      sliderInput("theta",
        "Hypothesized proportion of gold coins in the box: ",
        min = 0,
        max = 1.0,
        step = 0.1,
        value = 0.5
      ),
      htmlOutput("text")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", width = "100%")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$n.obs.slider <- renderUI({
    sliderInput("n.obs",
      withMathJax("Observed number of gold coins (\\(m\\)) in the sample of size \\(n\\): "),
      min = 0,
      max = input$n,
      step = 1,
      value = max(1, 0.4 * input$n)
    )
  })

  output$text <- renderUI({
    withMathJax()

    # Do I have to repeat this? Probably not, but anyway.
    if (is.null(input$n.obs)) {
      n.obs <- max(1, 0.4 * input$n)
    } else {
      n.obs <- input$n.obs
    }

    results <- binom.test(n.obs,
      input$n,
      p = input$theta
    )

    stmt.1 <- sprintf(
      "Having sampled %d coins, of which %d are gold and so %d are silver,
      using the sampling distribution, we can see that
      the probability of observing an outcome <em>as or more extreme</em> than this,
      assuming that the true proportion of gold coins in the box is %2.2f, is p=%2.3f. 
      This is the area shaded in <font color='#56B4E9'>blue</font>. This is the <em>p-value</em>.",
      input$n,
      n.obs,
      input$n - n.obs,
      input$theta,
      results$p.value
    )

    stmt.2 <- sprintf(
      "The 95%% confidence interval on the proportion of gold coins is from %2.3f to %2.3f.
      Were we to test the hypothesis that the true proportion is any value in this range, it's p-value would be greater than 0.05.
      ",
      results$conf.int[1],
      results$conf.int[2]
    )

    HTML(paste(stmt.1, stmt.2, url.link, sep = "<br/>"))
  })

  output$distPlot <- renderPlot(
    {
      withMathJax()

      expected.value <- input$n * input$theta

      x <- seq(0, input$n)
      y <- dbinom(x, size = input$n, prob = input$theta)

      if (is.null(input$n.obs)) {
        n.obs <- max(1, 0.4 * input$n)
      } else {
        n.obs <- input$n.obs
      }

      z <- abs(expected.value - x) >= abs(expected.value - n.obs)

      Df <- data.frame(
        x = x,
        y = y,
        z = z
      )

      ggplot(Df,
        mapping = aes(x = x, y = y, fill = z)
      ) +
        geom_col(width = 0.65) +
        theme_classic() +
        guides(fill = "none") +
        scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
        xlab("Observed number of Heads") +
        ylab("Probability") +
        ggtitle(sprintf('Binomial sampling distribution: Trials=%d, probability of \"success\" is %2.2f.', input$n, input$theta))
    },
    height = 600,
    width = 800
  )
}

# Run the application
shinyApp(ui = ui, server = server)
