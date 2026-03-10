library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggExtra)
library(stringr)
library(purrr)
library(DT)

# ============================================================================
# UI DEFINITION
# ============================================================================
ui <- fluidPage(
  titlePanel("Multi-network Social Media Analytics Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # File upload widget
      fileInput("file", "Upload Social Media CSV",
                accept = c(".csv")),
      
      hr(),
      
      # network selection dropdown
      uiOutput("network_selector"),
      
      hr(),
      
      # Dynamic impressions slider - adjusts based on data
      uiOutput("impressions_slider"),
      
      hr(),
      
      # Dynamic engagements/reactions slider - adjusts based on data
      uiOutput("engagements_slider"),
      
      hr(),
      
      # Toggle between Engagements and Reactions
      checkboxInput("use_reactions", 
                    "Use Reactions instead of Engagements",
                    value = FALSE),
      
      hr(),
      
      # Dynamic Post Type selector - options change based on network
      uiOutput("post_type_selector"),
      
      hr(),
      
      # Dynamic Content Type selector - options change based on network
      uiOutput("content_type_selector"),
      
      hr(),
      
      # Enable Post Link Clicks filter (when available in data)
      uiOutput("link_clicks_toggle"),
      
      hr(),
      
      # Color coding options for scatter plot
      selectInput("color_by", 
                  "Color Points By:",
                  choices = c("Content.Type", "Post.Type", "Network"),
                  selected = "Content.Type"),
      
      hr(),
      
      # Tag filtering dropdown
      uiOutput("tag_selector"),
      
      hr(),
      
      # Date range filter
      dateRangeInput("date_range", 
                     "Date Range:",
                     start = NULL,
                     end = NULL),
      
      hr(),
      
      # Reset all filters to defaults
      actionButton("reset", "Reset Filters", 
                   class = "btn-warning")
    ),
    
    # Main content area with tabbed interface
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot",
                 plotOutput("scatter_plot", height = "600px"),
                 hr(),
                 verbatimTextOutput("r_squared")),
        
        tabPanel("Summary Statistics",
                 h4("Data Summary"),
                 verbatimTextOutput("summary_stats"),
                 hr(),
                 h4("Filtered Data"),
                 DTOutput("data_table")),
        
        tabPanel("Tag Analysis",
                 h4("Most Common Tags"),
                 plotOutput("tag_plot"),
                 hr(),
                 DTOutput("tag_table")),
        
        tabPanel("Time Series",
                 plotOutput("time_series"),
                 hr(),
                 plotOutput("engagement_rate"))
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================
server <- function(input, output, session) {
  
  # --------------------------------------------------------------------------
  # network CONFIGURATION
  # Define network-specific options for post types and content types
  # --------------------------------------------------------------------------
  network_config <- list(
    Instagram = list(
      post_types = c("Post", "Reel", "Story"),
      content_types = c("Photo", "Video", "Carousel")
    ),
    Facebook = list(
      post_types = c("Post", "Story"),
      content_types = c("Text", "Photo", "Video", "Link", "Carousel")
    ),
    X = list(
      post_types = c("Post", "Quote"),
      content_types = c("Text", "Photo", "Video", "Link")
    ),
    LinkedIn = list(
      post_types = c("Post"),
      content_types = c("Text", "Photo", "Video", "Link", "Carousel")
    )
  )
  
  # --------------------------------------------------------------------------
  # HELPER FUNCTION: Split comma-separated tags
  # Handles special case of "Student Centered, Innovation Driven" tag
  # --------------------------------------------------------------------------
  split_tags <- function(tag_string) {
    if (is.na(tag_string) || tag_string == "") {
      return(character(0))
    }
    
    # Temporarily replace problematic tag to avoid splitting on its comma
    temp_string <- gsub("Student Centered, Innovation Driven", 
                        "SCID_placeholder", 
                        tag_string, 
                        fixed = TRUE)
    
    # Split on commas and trim whitespace
    tags <- str_split(temp_string, ",")[[1]] %>%
      str_trim()
    
    # Restore original tag name
    tags <- gsub("SCID_placeholder", 
                 "Student Centered, Innovation Driven", 
                 tags, 
                 fixed = TRUE)
    
    return(tags)
  }
  
  # --------------------------------------------------------------------------
  # REACTIVE: Load and validate raw data from CSV
  # Performs data type conversions and basic validation
  # --------------------------------------------------------------------------
  raw_data <- reactive({
    req(input$file)
    
    tryCatch({
      data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      
      # Define required columns - network column is now required
      required_cols <- c("Date", "Network", "Post.Type", "Content.Type", 
                         "Impressions", "Reach", "Tags")
      
      # Check for missing required columns
      missing_cols <- setdiff(required_cols, names(data))
      if (length(missing_cols) > 0) {
        showNotification(
          paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
          type = "error",
          duration = NULL
        )
        return(NULL)
      }
      
      # Parse dates and select/clean columns
      data <- data %>%
        mutate(Date = mdy_hm(Date)) %>%
        mutate(Date = as.Date(Date)) %>%
        arrange(Date)
      
      # Standardize network names
      data <- data %>%
        mutate(Network = case_when(
          tolower(Network) %in% c("instagram", "ig") ~ "Instagram",
          tolower(Network) %in% c("facebook", "fb") ~ "Facebook",
          tolower(Network) %in% c("x", "twitter") ~ "X",
          tolower(Network) %in% c("linkedin", "li") ~ "LinkedIn",
          TRUE ~ Network
        ))
      
      # Convert numeric columns, handling commas in numbers
      numeric_cols <- c("Impressions", "Reach", "Engagements", "Reactions",
                        "Likes", "Comments", "Shares", "Saves", "Video.Views",
                        "Post.Link.Clicks")
      
      # Only convert columns that exist in the data
      existing_numeric_cols <- intersect(numeric_cols, names(data))
      
      data <- data %>%
        mutate(across(all_of(existing_numeric_cols), 
                      ~as.numeric(gsub(",", "", as.character(.)))))
      
      showNotification("Data loaded successfully!", type = "message", duration = 3)
      return(data)
      
    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = NULL
      )
      return(NULL)
    })
  })
  
  # --------------------------------------------------------------------------
  # REACTIVE: Get available networks from data
  # --------------------------------------------------------------------------
  available_networks <- reactive({
    req(raw_data())
    unique(raw_data()$Network) %>% sort()
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: network selector dropdown
  # Includes "All networks" option plus individual networks
  # --------------------------------------------------------------------------
  output$network_selector <- renderUI({
    req(available_networks())
    
    choices <- c("All networks", available_networks())
    
    selectInput("selected_network",
                "Select network:",
                choices = choices,
                selected = "All networks",
                multiple = FALSE)
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Dynamic Post Type selector
  # Options change based on selected network
  # --------------------------------------------------------------------------
  output$post_type_selector <- renderUI({
    req(raw_data())
    
    # Default to "All networks" if input not ready yet
    selected_network <- if (is.null(input$selected_network)) {
      "All networks"
    } else {
      input$selected_network
    }
    
    if (selected_network == "All networks") {
      # If all networks, show all unique post types from data
      all_types <- unique(raw_data()$Post.Type) %>% 
        na.omit() %>% 
        sort()
      
      checkboxGroupInput("post_type", 
                         "Post Types:",
                         choices = all_types,
                         selected = all_types)
    } else {
      # Show network-specific post types
      config <- network_config[[selected_network]]
      if (!is.null(config)) {
        checkboxGroupInput("post_type", 
                           "Post Types:",
                           choices = config$post_types,
                           selected = config$post_types)
      } else {
        # Fallback if network not in config
        network_types <- raw_data() %>%
          filter(Network == selected_network) %>%
          pull(Post.Type) %>%
          unique() %>%
          na.omit() %>%
          sort()
        
        checkboxGroupInput("post_type", 
                           "Post Types:",
                           choices = network_types,
                           selected = network_types)
      }
    }
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Dynamic Content Type selector
  # Options change based on selected network
  # --------------------------------------------------------------------------
  output$content_type_selector <- renderUI({
    req(raw_data())
    
    # Default to "All networks" if input not ready yet
    selected_network <- if (is.null(input$selected_network)) {
      "All networks"
    } else {
      input$selected_network
    }
    
    if (selected_network == "All networks") {
      # If all networks, show all unique content types from data
      all_types <- unique(raw_data()$Content.Type) %>% 
        na.omit() %>% 
        sort()
      
      checkboxGroupInput("content_type", 
                         "Content Types:",
                         choices = all_types,
                         selected = all_types)
    } else {
      # Show network-specific content types
      config <- network_config[[selected_network]]
      if (!is.null(config)) {
        checkboxGroupInput("content_type", 
                           "Content Types:",
                           choices = config$content_types,
                           selected = config$content_types)
      } else {
        # Fallback if network not in config
        network_types <- raw_data() %>%
          filter(Network == selected_network) %>%
          pull(Content.Type) %>%
          unique() %>%
          na.omit() %>%
          sort()
        
        checkboxGroupInput("content_type", 
                           "Content Types:",
                           choices = network_types,
                           selected = network_types)
      }
    }
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Link Clicks toggle (only show if column exists in data)
  # --------------------------------------------------------------------------
  output$link_clicks_toggle <- renderUI({
    req(raw_data())
    
    if ("Post.Link.Clicks" %in% names(raw_data())) {
      list(
        checkboxInput("enable_link_clicks",
                      "Filter by Post Link Clicks",
                      value = FALSE),
        
        conditionalPanel(
          condition = "input.enable_link_clicks == true",
          sliderInput("link_clicks_min",
                      "Minimum Link Clicks:",
                      min = 0,
                      max = max(raw_data()$Post.Link.Clicks, na.rm = TRUE),
                      value = 0)
        )
      )
    } else {
      p("Post Link Clicks data not available", style = "color: #888; font-style: italic;")
    }
  })
  
  # --------------------------------------------------------------------------
  # REACTIVE: Update date range when data loads
  # --------------------------------------------------------------------------
  observe({
    req(raw_data())
    data <- raw_data()
    
    updateDateRangeInput(session, "date_range",
                         start = min(data$Date, na.rm = TRUE),
                         end = max(data$Date, na.rm = TRUE))
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Dynamic impressions slider
  # Range adjusts based on data distribution (mean + 2.5 SD)
  # --------------------------------------------------------------------------
  output$impressions_slider <- renderUI({
    if (is.null(input$file)) {
      sliderInput("impressions_max", 
                  "Max Impressions Filter:",
                  min = 0,
                  max = 100000,
                  value = 100000)
    } else {
      req(raw_data())
      
      # Calculate reasonable max based on distribution
      max_val <- mean(raw_data()$Impressions, na.rm = TRUE) + 
        (2.5 * sd(raw_data()$Impressions, na.rm = TRUE))
      
      # Make sure max_val is greater than 0 and not Inf
      max_val <- max(c(max_val, max(raw_data()$Impressions, na.rm = TRUE)), na.rm = TRUE)
      
      if (is.infinite(max_val) || is.na(max_val) || max_val <= 0) {
        max_val <- 100000
      }
      
      sliderInput("impressions_max", 
                  "Max Impressions Filter:",
                  min = 0,
                  max = ceiling(max_val),
                  value = ceiling(max_val))
    }
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Dynamic engagements/reactions slider
  # Label and data source change based on toggle
  # --------------------------------------------------------------------------
  output$engagements_slider <- renderUI({
    # Determine label based on toggle
    label <- if (!is.null(input$use_reactions) && input$use_reactions) {
      "Max Reactions Filter:"
    } else {
      "Max Engagements Filter:"
    }
    
    if (is.null(input$file)) {
      sliderInput("engagements_max",
                  label,
                  min = 0,
                  max = 5000,
                  value = 5000)
    } else {
      req(raw_data())
      
      # Use appropriate column based on toggle
      if (!is.null(input$use_reactions) && input$use_reactions && "Reactions" %in% names(raw_data())) {
        metric_col <- raw_data()$Reactions
      } else if ("Engagements" %in% names(raw_data())) {
        metric_col <- raw_data()$Engagements
      } else {
        # Fallback to default values
        return(sliderInput("engagements_max",
                           label,
                           min = 0,
                           max = 5000,
                           value = 5000))
      }
      
      # Calculate reasonable max based on distribution
      max_val <- mean(metric_col, na.rm = TRUE) + 
        (2.5 * sd(metric_col, na.rm = TRUE))
      
      # Make sure max_val is valid
      max_val <- max(c(max_val, max(metric_col, na.rm = TRUE)), na.rm = TRUE)
      
      if (is.infinite(max_val) || is.na(max_val) || max_val <= 0) {
        max_val <- 5000
      }
      
      sliderInput("engagements_max",
                  label,
                  min = 0,
                  max = ceiling(max_val),
                  value = ceiling(max_val))
    }
  })
  
  # --------------------------------------------------------------------------
  # REACTIVE: Filtered data based on all user selections
  # Applies network, date, post type, content type, and metric filters
  # --------------------------------------------------------------------------
  filtered_data <- reactive({
    req(raw_data())
    
    data <- raw_data()
    
    # Filter by network (only if input is ready and not "All networks")
    if (!is.null(input$selected_network) && 
        input$selected_network != "All networks") {
      data <- data %>%
        filter(Network == input$selected_network)
    }
    
    # Apply impressions filter (only if input exists)
    if (!is.null(input$impressions_max)) {
      data <- data %>%
        filter(Impressions <= input$impressions_max)
    }
    
    # Determine which engagement metric to use and apply filter
    if (!is.null(input$engagements_max)) {
      engagement_col <- if (!is.null(input$use_reactions) && 
                            input$use_reactions && 
                            "Reactions" %in% names(data)) {
        "Reactions"
      } else if ("Engagements" %in% names(data)) {
        "Engagements"
      } else {
        NULL
      }
      
      if (!is.null(engagement_col)) {
        data <- data %>%
          filter(!!sym(engagement_col) <= input$engagements_max)
      }
    }
    
    # Filter by post type (only if input exists and has values)
    if (!is.null(input$post_type) && length(input$post_type) > 0) {
      data <- data %>%
        filter(Post.Type %in% input$post_type)
    }
    
    # Filter by content type (only if input exists and has values)
    if (!is.null(input$content_type) && length(input$content_type) > 0) {
      data <- data %>%
        filter(Content.Type %in% input$content_type)
    }
    
    # Apply date filter (only if input exists)
    if (!is.null(input$date_range) && length(input$date_range) == 2) {
      data <- data %>%
        filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    }
    
    # Apply tag filter if not "All Tags" (only if input exists)
    if (!is.null(input$tag_types) && input$tag_types != "All Tags") {
      data <- data %>%
        filter(grepl(input$tag_types, Tags, fixed = TRUE))
    }
    
    # Apply link clicks filter if enabled
    if (!is.null(input$enable_link_clicks) && 
        input$enable_link_clicks && 
        "Post.Link.Clicks" %in% names(data) &&
        !is.null(input$link_clicks_min)) {
      data <- data %>%
        filter(Post.Link.Clicks >= input$link_clicks_min)
    }
    
    data
  })
  
  # --------------------------------------------------------------------------
  # REACTIVE: Data with expanded tag columns
  # Converts comma-separated tags into individual columns
  # --------------------------------------------------------------------------
  data_with_tags <- reactive({
    req(filtered_data())
    
    tryCatch({
      data <- filtered_data()
      
      # Handle cases where Tags column might be all NA or empty
      if (all(is.na(data$Tags) | data$Tags == "")) {
        return(data)
      }
      
      # Split tags and create individual columns
      data %>%
        mutate(
          tag_list = map(Tags, split_tags),
          max_tags = max(map_int(tag_list, length), na.rm = TRUE)
        ) %>%
        mutate(
          tag_columns = map(tag_list, ~{
            max_t <- first(max_tags)
            if (length(.x) == 0) {
              rep(NA_character_, max_t)
            } else {
              c(.x, rep(NA_character_, max(0, max_t - length(.x))))
            }
          })
        ) %>%
        unnest_wider(tag_columns, names_sep = "_") %>%
        rename_with(~str_replace(.x, "tag_columns_", "Tag_"), 
                    starts_with("tag_columns_")) %>%
        select(-tag_list, -max_tags)
      
    }, error = function(e) {
      return(filtered_data())
    })
  })
  
  # --------------------------------------------------------------------------
  # REACTIVE: Extract unique tags for dropdown filter
  # --------------------------------------------------------------------------
  available_tags <- reactive({
    req(data_with_tags())
    
    tryCatch({
      tag_cols <- grep("^Tag_", names(data_with_tags()), value = TRUE)
      
      if (length(tag_cols) == 0) {
        return(character(0))
      }
      
      # Get all unique tags from tag columns
      unique_tags <- data_with_tags() %>%
        select(all_of(tag_cols)) %>%
        pivot_longer(everything(), values_to = "tag") %>%
        filter(!is.na(tag), tag != "") %>%
        pull(tag) %>%
        unique() %>%
        sort()
      
      return(unique_tags)
      
    }, error = function(e) {
      return(character(0))
    })
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Tag selector dropdown
  # --------------------------------------------------------------------------
  output$tag_selector <- renderUI({
    tags_available <- available_tags()
    
    if (length(tags_available) > 0) {
      selectInput("tag_types", 
                  "Choose Tags:",
                  choices = c("All Tags", tags_available),
                  selected = "All Tags")
    } else {
      p("No tags available", style = "color: #888; font-style: italic;")
    }
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Scatter plot with marginal density plots
  # X-axis: Impressions, Y-axis: Engagements or Reactions
  # --------------------------------------------------------------------------
  output$scatter_plot <- renderPlot({
    req(filtered_data())
    
    if (nrow(filtered_data()) == 0) {
      plot.new()
      text(0.5, 0.5, "No data matches current filters.\nTry adjusting your filter settings.", 
           cex = 1.5, col = "red")
      return()
    }
    
    # Determine Y-axis metric
    y_metric <- if (!is.null(input$use_reactions) && 
                    input$use_reactions && 
                    "Reactions" %in% names(filtered_data())) {
      "Reactions"
    } else if ("Engagements" %in% names(filtered_data())) {
      "Engagements"
    } else {
      plot.new()
      text(0.5, 0.5, "No engagement metric available in data", cex = 1.5, col = "red")
      return()
    }
    
    # Determine color variable (ensure it exists in data)
    color_var <- if (!is.null(input$color_by) && 
                     input$color_by %in% names(filtered_data())) {
      sym(input$color_by)
    } else {
      sym("Content.Type")
    }
    
    # Create scatter plot
    p <- ggplot(filtered_data(), 
                aes(Impressions, !!sym(y_metric), color = !!color_var)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
      theme_minimal() +
      labs(title = paste("Impressions vs", y_metric),
           x = "Impressions",
           y = y_metric) +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 16, face = "bold"),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10))
    
    # Add marginal density plots
    tryCatch({
      ggMarginal(p, type = "density", groupColour = TRUE, groupFill = TRUE)
    }, error = function(e) {
      # If marginal plots fail, return the base plot
      print(p)
    })
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: R-squared value and observation count
  # Regression of Engagements/Reactions on Impressions
  # --------------------------------------------------------------------------
  output$r_squared <- renderText({
    req(filtered_data())
    
    if (nrow(filtered_data()) < 3) {
      return("Insufficient data for regression (need at least 3 observations)")
    }
    
    # Determine Y variable
    y_metric <- if (!is.null(input$use_reactions) && 
                    input$use_reactions && 
                    "Reactions" %in% names(filtered_data())) {
      "Reactions"
    } else if ("Engagements" %in% names(filtered_data())) {
      "Engagements"
    } else {
      return("No engagement metric available")
    }
    
    # Build regression model
    tryCatch({
      formula_str <- paste(y_metric, "~ Impressions")
      model <- lm(as.formula(formula_str), data = filtered_data())
      r_sqrd <- signif(summary(model)$r.squared, digits = 3)
      
      paste0("R-squared: ", r_sqrd, "\n",
             "Number of observations: ", nrow(filtered_data()))
    }, error = function(e) {
      paste("Error calculating R-squared:", e$message)
    })
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Summary statistics for numeric columns
  # --------------------------------------------------------------------------
  output$summary_stats <- renderPrint({
    req(filtered_data())
    
    # Select numeric columns that exist in the data
    numeric_cols <- c("Impressions", "Reach", "Engagements", "Reactions",
                      "Likes", "Comments", "Shares", "Saves", 
                      "Video.Views", "Post.Link.Clicks")
    
    existing_cols <- intersect(numeric_cols, names(filtered_data()))
    
    if (length(existing_cols) == 0) {
      cat("No numeric columns available for summary")
    } else {
      filtered_data() %>%
        select(all_of(existing_cols)) %>%
        summary()
    }
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Interactive data table with filtering
  # --------------------------------------------------------------------------
  output$data_table <- renderDT({
    req(filtered_data())
    
    datatable(filtered_data(), 
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top")
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Bar chart of top 15 most common tags
  # --------------------------------------------------------------------------
  output$tag_plot <- renderPlot({
    req(data_with_tags())
    
    tryCatch({
      tag_cols <- grep("^Tag_", names(data_with_tags()), value = TRUE)
      
      if (length(tag_cols) == 0) {
        plot.new()
        text(0.5, 0.5, "No tags available in filtered data", cex = 1.5)
        return()
      }
      
      # Count tag occurrences
      tag_counts <- data_with_tags() %>%
        select(all_of(tag_cols)) %>%
        pivot_longer(everything(), values_to = "tag") %>%
        filter(!is.na(tag), tag != "") %>%
        count(tag, sort = TRUE) %>%
        head(15)
      
      if (nrow(tag_counts) == 0) {
        plot.new()
        text(0.5, 0.5, "No tags found in data", cex = 1.5)
        return()
      }
      
      # Create horizontal bar chart
      ggplot(tag_counts, aes(x = reorder(tag, n), y = n)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Top 15 Most Common Tags",
             x = "Tag",
             y = "Count") +
        theme(plot.title = element_text(size = 14, face = "bold"))
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating tag plot:", e$message), cex = 1)
    })
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Table of all tag counts
  # --------------------------------------------------------------------------
  output$tag_table <- renderDT({
    req(data_with_tags())
    
    tryCatch({
      tag_cols <- grep("^Tag_", names(data_with_tags()), value = TRUE)
      
      if (length(tag_cols) == 0) {
        return(datatable(data.frame(Message = "No tags available")))
      }
      
      # Create tag frequency table
      tag_data <- data_with_tags() %>%
        select(all_of(tag_cols)) %>%
        pivot_longer(everything(), values_to = "tag") %>%
        filter(!is.na(tag), tag != "") %>%
        count(tag, sort = TRUE)
      
      if (nrow(tag_data) == 0) {
        return(datatable(data.frame(Message = "No tags found in data")))
      }
      
      datatable(tag_data, 
                options = list(pageLength = 20),
                colnames = c("Tag", "Count"))
      
    }, error = function(e) {
      datatable(data.frame(Error = paste("Error processing tags:", e$message)))
    })
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Time series plot of key metrics
  # Shows Impressions, Engagements/Reactions, and Reach over time
  # --------------------------------------------------------------------------
  output$time_series <- renderPlot({
    req(filtered_data())
    
    tryCatch({
      if (nrow(filtered_data()) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for selected filters", cex = 1.5)
        return()
      }
      
      # Determine engagement metric
      engagement_metric <- if (!is.null(input$use_reactions) && 
                               input$use_reactions && 
                               "Reactions" %in% names(filtered_data())) {
        "Reactions"
      } else if ("Engagements" %in% names(filtered_data())) {
        "Engagements"
      } else {
        NULL
      }
      
      # Select metrics to plot
      if (!is.null(engagement_metric)) {
        plot_data <- filtered_data() %>%
          select(Date, Impressions, !!sym(engagement_metric), Reach) %>%
          pivot_longer(-Date, names_to = "Metric", values_to = "Value")
      } else {
        plot_data <- filtered_data() %>%
          select(Date, Impressions, Reach) %>%
          pivot_longer(-Date, names_to = "Metric", values_to = "Value")
      }
      
      # Create line plot
      ggplot(plot_data, aes(Date, Value, color = Metric)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        theme_minimal() +
        labs(title = "Metrics Over Time",
             x = "Date",
             y = "Count") +
        theme(legend.position = "bottom",
              plot.title = element_text(size = 14, face = "bold"))
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating time series:", e$message), cex = 1)
    })
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Engagement rate over time by post type
  # Calculates (Engagements or Reactions / Impressions) * 100
  # --------------------------------------------------------------------------
  output$engagement_rate <- renderPlot({
    req(filtered_data())
    
    tryCatch({
      if (nrow(filtered_data()) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for selected filters", cex = 1.5)
        return()
      }
      
      # Determine engagement metric
      engagement_metric <- if (!is.null(input$use_reactions) && 
                               input$use_reactions && 
                               "Reactions" %in% names(filtered_data())) {
        "Reactions"
      } else if ("Engagements" %in% names(filtered_data())) {
        "Engagements"
      } else {
        plot.new()
        text(0.5, 0.5, "No engagement metric available", cex = 1.5)
        return()
      }
      
      # Calculate engagement rate
      plot_data <- filtered_data() %>%
        mutate(Engagement_Rate = ifelse(Impressions > 0,
                                        (!!sym(engagement_metric) / Impressions) * 100,
                                        0))
      
      # Create line plot
      ggplot(plot_data, aes(Date, Engagement_Rate, color = Post.Type)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        geom_smooth(method = 'lm', color = 'black') +
        theme_minimal() +
        labs(title = paste(engagement_metric, "Rate Over Time"),
             x = "Date",
             y = paste(engagement_metric, "Rate (%)")) +
        theme(legend.position = "bottom",
              plot.title = element_text(size = 14, face = "bold"))
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating engagement rate plot:", e$message), cex = 1)
    })
  })
  
  # --------------------------------------------------------------------------
  # OBSERVER: Reset all filters to defaults
  # --------------------------------------------------------------------------
  observeEvent(input$reset, {
    req(raw_data())
    
    data <- raw_data()
    
    # Reset date range
    updateDateRangeInput(session, "date_range",
                         start = min(data$Date, na.rm = TRUE),
                         end = max(data$Date, na.rm = TRUE))
    
    # Reset network selection
    updateSelectInput(session, "selected_network",
                      selected = "All networks")
    
    # Reset reactions toggle
    updateCheckboxInput(session, "use_reactions", value = FALSE)
    
    # Reset link clicks toggle
    if (!is.null(input$enable_link_clicks)) {
      updateCheckboxInput(session, "enable_link_clicks", value = FALSE)
    }
    
    # Reset color by
    updateSelectInput(session, "color_by", selected = "Content.Type")
    
    showNotification("Filters reset to defaults", type = "message", duration = 2)
  })
}

# ============================================================================
# RUN APPLICATION
# ============================================================================
shinyApp(ui = ui, server = server)
