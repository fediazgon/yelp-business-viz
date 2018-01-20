#' \author{Fernando Díaz}
#' \author{Giorgio Ruffa}
#' Technical School of Madrid (UPM)
#' Master's Programme in Data Science (EIT Digital Master School)

library(shiny)

library(jsonlite)
library(tibble)
library(igraph)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(viridisLite)

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

ADJ_MATRIX_SHAPE = 60
ADJ_MATRIX_PALETTE <- brewer.pal(9, "Set1")

HEATMAP_PALETTE <- magma(20)
# Increase it at your own risk
HEATMAP_TIMETABLES_SAMPLE <- 5000

################################
########## LOAD DATA ###########
################################

business_df <- flatten(stream_in(file("data/business_sample.json")))
checkin_df <- flatten(stream_in(file("data/checkin_sample.json")))

hourColumns <- grep("hours.*", names(business_df), value=T)

times_df <- merge(checkin_df, business_df[,c("business_id", "categories", hourColumns)], by = "business_id")
times_filtered_df <- cbind(times_df)

################################
########### HEATMAP ############
################################

# Let's have fun. Vol. 2

# Generate all the posible hours
hours <- sprintf("%d:00",seq(0, 23))
# Generate, using a complex procedure, the days of the week
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

filterByCategory <- function(category) {
  times_filtered_df <- cbind(times_df)
  if (category != "All") {
    # I've tried a million times with a more readable solution
    times_filtered_df[, "belongs"] <-
      apply(times_df["categories"], 1, function(x)
        is.element(category, x[[1]]))
    # '<<-' modifies the df in the global scope
    times_filtered_df <<- subset(times_filtered_df, belongs)
  }
}

computeCheckinMatrix <- function() {
  
  checkin <- times_filtered_df[, sapply(times_filtered_df, is.numeric)]
  checkinSum <- colSums(checkin, na.rm = TRUE)  # named list
  
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
  
  return(checkinMatrix)
  
}

computeTimetableMatrix <- function(updateProgress) {
 
  timetables_df <- subset(times_filtered_df, select = grep("hours.*", names(times_filtered_df)))
  names(timetables_df) <- gsub("hours.", "", names(timetables_df))
  
  n_elements <- nrow(timetables_df)
  if (n_elements < HEATMAP_TIMETABLES_SAMPLE) {
    # This value os local to the function
    HEATMAP_TIMETABLES_SAMPLE <- n_elements
  }
  
  timetableMatrix <-
    matrix(
      data = 0,
      nrow = 24,
      ncol = 7,
      dimnames = list(hours, days)
    )
  
  # THE COMPLEXITY IF THIS THING IS INSANE. DON'T EVEN LOOK AT IT
  days_left = 7
  for (day in days) {
    updateProgress(value = 1 / days_left, detail = day)
    days_left = days_left - 1
    # Retrieve the timetables for all the businesses for a certain day
    # i.e., Monday -> [B1 {10:00-21:00}, B2 {12:00-00:00}, ... BN]
    timetables_day <- timetables_df[1:HEATMAP_TIMETABLES_SAMPLE, day]
    for (timetable in timetables_day) {
      # Sometimes is missing
      if (!is.na(timetable)) {
        # TODO: extract with regex
        open_close <-
          sapply(strsplit(timetable, "-"), function(x) {
            as.numeric(substr(x, start = 1, stop = nchar(x) - 3))
          })
        open_h <- open_close[1]
        close_h <- open_close[2]
        hours_open <- NULL
        if (open_h > close_h) {
          hours_open <- sprintf("%d:00",seq(open_h, 23))
          hours_open <- c(hours_open, sprintf("%d:00",seq(0, close_h)))
        } else {
          hours_open <- sprintf("%d:00",seq(open_h, close_h))
        }
        # YAL (Yet Another Loop)
        for (hour in hours_open) {
          timetableMatrix[hour, day] <- timetableMatrix[hour, day] + 1
        }
      }
    }
  }
  
  return(timetableMatrix)
   
}

heatmapPlot <- function(data, title, smooth) {
  
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
  
categories_per_business <- business_df[["categories"]]  # "categories" column contains lists
categories_all <- unlist(categories_per_business)
categories_ocurrences <- as.data.frame(sort(table(categories_all), decreasing = T))  # black magic
# cat("There is a total of", nrow(categories_ocurrences), "categories")
# head(categories_ocurrences, 10)

# Keep the most popular categories
categories <- categories_ocurrences[1:ADJ_MATRIX_SHAPE, "categories_all"]
  
adjMatrix <-
  matrix(
    data = 0,  # not adjacent by default. Also is the edge weight
    nrow = ADJ_MATRIX_SHAPE,
    ncol = ADJ_MATRIX_SHAPE,
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
matrixPlot <- function(order = "by Name", colorDifferentClusters = FALSE) {
  
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
    theme_bw() +
    theme(
      panel.background = element_rect(colour = "white", fill = "#FAFAFA"),
      panel.grid.major = element_line(colour = "white"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      # Rotate the x-axis lables so they are legible
      axis.text.x = element_text(angle = -270, hjust = 0),
      # Force the plot into a square aspect ratio
      aspect.ratio = 1,
      # Hide the legend (optional)
      legend.position = "none")

  if (colorDifferentClusters) {
    p <- p + scale_fill_manual(values = ADJ_MATRIX_PALETTE, na.value = "black")
  } else {
    p <- p + scale_fill_manual(values = ADJ_MATRIX_PALETTE)
  }
    
  return(p)
}

################################
##### END ADJACENCY MATRIX #####
################################

shinyServer(function(input, output) {
  
  ################################
  
  output$adjMatrix <- renderPlot({
    order <- input$order
    matrixPlot(order, input$different)
  })
  
  ################################
  
  # Terribe workaround?
  recalculationsNeeded <- reactiveVal(2)
  
  observeEvent({
    list(input$category)
  }, {
    recalculationsNeeded(2)
  })
  
  filterHeatmapValues <- reactive({ filterByCategory(input$category) })
  
  checkinMatrix <- NULL
  output$heatmapCheckin <- renderPlot({
    
    filterHeatmapValues()
    
    if (recalculationsNeeded()) {
      checkinMatrix <<- computeCheckinMatrix()
      recalculationsNeeded(recalculationsNeeded() - 1)
    }
    
    heatmapPlot(checkinMatrix, title = "Clients check-in", input$smooth)
  })
  
  timetableMatrix <- NULL
  output$heatmapTimetable <- renderPlot({
    
    # It's ok calling the function two times. It's only executed once
    # See: https://shiny.rstudio.com/articles/reactivity-overview.html
    filterHeatmapValues()
    
    progress <- shiny::Progress$new()
    progress$set(message = "Agregatting timetable data", value = 0)
    on.exit(progress$close())
    
    updateProgress <- function(value, detail = NULL) {
      progress$set(value = value, detail = detail)
    }
    
    if (recalculationsNeeded()) {
      timetableMatrix <<- computeTimetableMatrix(updateProgress)
      recalculationsNeeded(recalculationsNeeded() - 1)
    }
    
    heatmapPlot(timetableMatrix, title = "Business timetable (sample)", input$smooth)
  })
  
  ################################
  
}) 
