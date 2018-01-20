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
        ),
        p(HTML(paste0("This matrix shows the number of times categories co-occur in a business.
          This work is based on the popular diagram made by Mike Bostock, ",
                      a(href = "https://bost.ocks.org/mike/miserables/", "Les Misérables Co-occurrence"), "."
                      )
               )
          ),
        p("Different colors represent categories in different communities in the underlying graph.
          The darker the color, the higher the frequency the two categories occur.")
      ),
      mainPanel(
        plotOutput("adjMatrix", height = "800px")
      )
    )
  ),
  tabPanel("Clients heatmap",
    sidebarLayout(
      sidebarPanel(
        checkboxInput("smooth", "Smooth", FALSE),
        selectInput("category", "Category:",
                    c("All", "Restaurants", "Shopping", "Food", "Health & Medical", "Nightlife", "Bars")),
        p("These two heatmaps show the peak check-in hours (top) and the range of hours in which a businnes
          is more likely to be open (bottom)."),
        p("Check-in is done by the clients with the yelp app when they are within a close proximity to
          the business location.")
      ),
      mainPanel(
        plotOutput("heatmapCheckin", height = "400px"),
        plotOutput("heatmapTimetable", height = "400px")
      )
    )
  )
)
