library(shiny)

navbarPage("YelpBusinesses",
  tabPanel("Map"
           # FILL ME
  ),
  tabPanel("Categories co-ocurrences"
           # FILL ME
  ),
  tabPanel("Clients heatmap",
    sidebarLayout(
      sidebarPanel(
        sliderInput("bins",
          "Number of bins:",
          min = 1,
          max = 50,
          value = 30)
        ),
             
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot")
      )
    )
  )
)
