library(shiny)

library(jsonlite)
library(tibble)
library(igraph)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

library(lattice)
library(latticeExtra)
library(stringr)

################################
############ README ############
################################

#' I decided to do all the expensive computations before the shinyServer() function is called
#' This means that, when you start the application, you are not going to see anything for a few
#' seconds. Try to use the sample datasets I provided, it will take 10 seconds to load everything.

################################
####### CONFIG VARIABLES #######
################################

MATRIX_SHAPE = 50
ADJ_PALETTE <- brewer.pal(9, "Set1")
HEATMAP_PALETTE <- rev(heat.colors(30, alpha = 1))
MAX_ROWS_OPENING_HOURS = 5000

################################
########## LOAD DATA ###########
################################

yelp <- stream_in(file("data/business_sample.json"))
yelp_flat <- flatten(yelp)

checkin <- flatten(stream_in(file("data/checkin_sample.json")))

################################
########### HEATMAP ############
################################

# Let's have fun. Vol. 2

# Generate all the posible hours
# Guess where is the Easter Egg here
hours <- seq(from = ISOdate(1993, 2, 2, 0, 0), by = "hour", length.out = 24)
hours <- as.character(format(hours, "%H:%M"))
hours <- gsub("^0", "", hours)
# Generate by a complex procedure, the days of the week
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

checkinSum <- colSums(checkin[,-1], na.rm = TRUE)
# 169 columns / 7 days => data every hour (24)

checkinMatrix <-
  matrix(
    data = 0,
    nrow = 24,
    ncol = 7,
    dimnames = list(hours, days)
  )

for (day in days) {
  for (hour in hours) {
    column_name <- paste("time.", day, ".", hour, sep = "")
    checkinMatrix[hour, day] <- checkinSum[column_name]
  }
}

# Ok. That was easy

opening_hours <- yelp[["hours"]]
opening_hours_n_rows <- length(opening_hours[,1])
if (opening_hours_n_rows < MAX_ROWS_OPENING_HOURS) {
  MAX_ROWS_OPENING_HOURS <- opening_hours_n_rows
}

openingHoursMatrix <-
  matrix(
    data = 0,
    nrow = 24,
    ncol = 7,
    dimnames = list(hours, days)
  )

# THE COMPLEXITY IF THIS METHOD IS INSANE. DON'T EVEN LOOK AT IT
for (day in days) {
  # Retrieve the timetables for all the businesses for a certain day
  # i.e., Monday -> [B1 {10:00-21:00}, B2 {12:00-00:00}, ... BN]
  timetables_day <- opening_hours[1:MAX_ROWS_OPENING_HOURS, day]
  for (timetable in timetables_day) {
    # Sometimes is missing
    if (!is.na(timetable)) {
      # E.g., if we have 10:00-21:00. Extract 10 and 21
      open_close <-
        sapply(strsplit(timetable, "-"), function(x) {
          as.numeric(substr(x, start = 1, stop = nchar(x) - 3))
        })
      open_h <- open_close[1]
      close_h <- open_close[2]
      close_day <- 2
      if (open_h > close_h) {
        close_day <- 3  # HAHAHAHAHA (it means 'next day')
      }
      range <-
        seq(
          from = ISOdate(1993, 2, 2, open_h, 0),
          to = ISOdate(1993, 2, close_day, close_h, 0),
          by = "hour"
        )
      range <- as.character(format(range, "%H:%M"))
      range <- gsub("^0", "", range)
      # YAL (Yet Another Loop)
      for (hour in range) {
        openingHoursMatrix[hour, day] <- openingHoursMatrix[hour, day] + 1
      }
    }
  }
}

# Function called in shinyServer()
heatmapPlot <- function(data, title, smooth = FALSE) {
  
  panel = panel.levelplot
  contour = FALSE
  
  if (smooth) {
    panel = panel.2dsmoother
    contour = TRUE
  }
  
  levelplot(
    data,
    col.regions = HEATMAP_PALETTE,
    xlab = "",
    ylab = "",
    main = title,
    panel = panel,
    contour = contour,
    scales = list(x = list(rot = 90))
  )
  
}

################################
######### END HEATMAP ##########
################################

################################
####### ADJACENCY MATRIX #######
################################

# Let's have fun

# list(A, B, C) => (A) <-edge-> (B) and (A) <-edge-> (C)
# list(A, B, C) => adjMatrix['A', 'B'] += 1 and adjMatrix['A', 'C'] += 1
addAdjListToAdjMatrix <- function(adjList, adjMatrix) {
  n_elements = length(adjList)
  # Safe check
  if (n_elements < 2) {
    return(adjMatrix)  # Nothing to do here
  }
  first = adjList[1]
  for (adjacent in adjList[2:n_elements]) {
    adjMatrix[first, adjacent] <- adjMatrix[first, adjacent] + 1  # REALLY R? No +=?
    return(adjMatrix)
  }
}
  
categories_per_business <- yelp_flat[["categories"]]  # "categories" column contains lists
categories_all <- unlist(categories_per_business)
categories_ocurrences <- as.data.frame(sort(table(categories_all), decreasing = T))  # black magic
# cat("There is a total of", nrow(categories_ocurrences), "categories")
# head(categories_ocurrences, 10)

# Keep the most popular categories
categories <- categories_ocurrences[1:MATRIX_SHAPE, "categories_all"]
  
adjMatrix <-
  matrix(
    data = 0,  # not adjacent by default. Also is the edge weight
    nrow = MATRIX_SHAPE,
    ncol = MATRIX_SHAPE,
    dimnames = list(categories, categories)  # rows and columns names
  )
  
categories_per_business
# Maybe we can use a fancy funcion like 'lapply'
for (business_cat in categories_per_business) {
  if (length(business_cat) > 2) {
    # I need to filter those categories that we chose not to include
    adjList = business_cat[sapply(business_cat, function(x) { is.element(x, categories) } )]
    adjMatrix = addAdjListToAdjMatrix(adjList, adjMatrix)
  }
}

# WUBBA LUBBA DUB DUB
# https://matthewlincoln.net/2014/12/20/adjacency-matrix-plots-with-r-and-ggplot2.html

# Create iGraph object
graph <- graph.adjacency(adjMatrix, weighted=TRUE)

# Calculate various network properties, adding them as attributes
# to each node/vertex
V(graph)$comm <- membership(optimal.community(graph))
V(graph)$freq <- strength(graph)
V(graph)$degree <- degree(graph)

# Re-generate dataframes for both nodes and edges, now containing
# calculated network attributes
node_list <- get.data.frame(graph, what = "vertices")

# Determine a community for each edge. If two nodes belong to the
# same community, label the edge with that community. If not,
# the edge community value is 'NA'
edge_list <- get.data.frame(graph, what = "edges") %>%
  inner_join(node_list %>% select(name, comm), by = c("from" = "name")) %>%
  inner_join(node_list %>% select(name, comm), by = c("to" = "name")) %>%
  mutate(group = ifelse(comm.x == comm.y, comm.x, NA) %>% factor())

# I used this histogram the make the 'breaks' below
# hist(edge_list[edge_list$weight < 500, ])

edge_list$weight.cat <- cut(
  edge_list$weight,
  breaks = c(0, 1, 10, 50, 100, 200, 500, 1000, 2000, Inf),
  labels = c("0", "<10", "10<x<50", "50<x<100", "100<x<200",
             "200<x<500", "500<1000", "1000<2000", ">2000"),
  right = FALSE
)

# Function called in shinyServer()
matrixPlot <- function(order = "by Name") {
  
  node_order <- sort(node_list$name, decreasing = TRUE)
  
  # Don't know how to switch
  if (order == "by Frequency") {
    node_order <- (node_list %>% arrange(freq))$name
  } else if (order == "by Cluster") {
    node_order <- (node_list %>% arrange(comm))$name
  }
  
  # Reorder edge_list 'from' and 'to' factor levels based on
  # the specified order
  plot_data <- edge_list %>% mutate(to = factor(to, levels = node_order),
                                    from = factor(from, levels = rev(node_order)))
  
  p <-
    ggplot(plot_data, aes(x = from, y = to, fill = group, alpha = weight.cat)) +
    geom_raster(hjust = 0, vjust = 0) +
    # Because we need the x and y axis to display every node,
    # not just the nodes that have connections to each other,
    # make sure that ggplot does not drop unused factor levels
    scale_x_discrete(drop = FALSE, position = "top") +
    scale_y_discrete(drop = FALSE) +
    scale_fill_manual(values = ADJ_PALETTE) +
    theme_bw() +
    theme(
      panel.background = element_rect(colour = "white", fill="#FAFAFA"),
      panel.grid.major = element_line(colour = "white"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      # Rotate the x-axis lables so they are legible
      axis.text.x = element_text(angle = -270, hjust = 0),
      # Force the plot into a square aspect ratio
      aspect.ratio = 1,
      # Hide the legend (optional)
      legend.position = "none")
  
  return(p)
}

################################
##### END ADJACENCY MATRIX #####
################################

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$adjMatrix <- renderPlot({
    order <- input$order
    matrixPlot(order)
  })
  
  output$heatmapCheckin <- renderPlot({
    smooth <- input$smooth
    heatmapPlot(checkinMatrix, title = "Clients check-in", smooth = smooth)
  })
  
  output$heatmapCheckin2 <- renderPlot({
    smooth <- input$smooth
    heatmapPlot(openingHoursMatrix, title = "Business timetable (sample)", smooth = smooth)
  })
  
})
