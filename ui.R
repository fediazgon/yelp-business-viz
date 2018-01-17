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
        checkboxInput("smooth", "Smooth", FALSE)
      ),
      mainPanel(
        fluidRow(
          plotOutput("heatmapCheckin", height = "300px"),
          plotOutput("heatmapCheckin2", height = "300px")
        )
      )
    )
  )
)
