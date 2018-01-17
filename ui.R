library(shiny)

navbarPage("YelpBusinesses", fluid = TRUE,
  tabPanel("Map"
           # FILL ME
  ),
  tabPanel("Categories co-ocurrences",
    sidebarLayout(
      sidebarPanel(
        selectInput('order', 'Order',
          c("by Name", "by Frequency", "by Cluster")            
        )
      ),
      mainPanel(
        plotOutput("adjMatrix", height = "700px")
      )
    )
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
