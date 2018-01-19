library(shiny)

navbarPage("YelpBusinesses", fluid = TRUE,
  tabPanel("Map"
           # FILL ME
  ),
  tabPanel("Categories co-ocurrences",
    sidebarLayout(
      sidebarPanel(
        checkboxInput("different", "Color categories in different clusters", FALSE),
        selectInput('order', 'Order',
          c("by Name", "by Frequency", "by Cluster")            
        )
      ),
      mainPanel(
        plotOutput("adjMatrix", height = "550px")
      )
    )
  ),
  tabPanel("Clients heatmap",
    sidebarLayout(
      sidebarPanel(
        checkboxInput("smooth", "Smooth", FALSE),
        selectInput("category", "Category:",
                    c("All", "Restaurants", "Shopping", "Food", "Health & Medical", "Nightlife", "Bars"))
      ),
      mainPanel(
        plotOutput("heatmapCheckin", height = "280px"),
        plotOutput("heatmapTimetable", height = "280px")
      )
    )
  )
)
