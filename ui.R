library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  "Stars" = "stars",
  "Review count" = "review_count_log"
)

navbarPage("YelpBusinesses", fluid = TRUE,
           tabPanel("Map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 130, left = "20", right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Explorer"),
                                      
                                      selectInput("color", "Color", vars),
                                      selectInput("size", "Size", vars, selected = "adultpop"),
                                        sliderInput("stars_r", "Stars range:", 
                                                    min = 1, max = 5,
                                                    step = 0.5,
                                                    value = c(1, 5)),
                                      conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                       # Only prompt for threshold when coloring or sizing by superzip
                                                       numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      ),
                                      
                                      plotOutput("histStars", height = 200),
                                      plotOutput("scatterReviewsCheckin", height = 250)
                        )
                    )
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
