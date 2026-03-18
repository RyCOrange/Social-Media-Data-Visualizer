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
      fileInput("file", "Upload Social Media CSV (Optional)",
                accept = c(".csv")),
      
      hr(),
      
      # YouTube file upload widget
      fileInput("youtube_file", "Upload YouTube CSV (Optional)",
                accept = c(".csv")),
      
      p("Upload at least one CSV file to begin.", 
        style = "color: #666; font-size: 0.9em; font-style: italic;"),
      
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
      
      # Dynamic YouTube Views slider - adjusts based on data
      uiOutput("youtube_views_slider"),
      
      hr(),
      
      # Dynamic YouTube Likes slider - adjusts based on data
      uiOutput("youtube_likes_slider"),
      
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
      # Show message when no data is uploaded
      conditionalPanel(
        condition = "!output.has_data",
        div(style = "text-align: center; padding: 50px;",
            h3("Welcome to Social Media Analytics Dashboard"),
            p("Please upload at least one CSV file to begin analyzing your data."),
            tags$ul(style = "text-align: left; display: inline-block;",
                    tags$li("Upload a Social Media CSV for Instagram, Facebook, X, or LinkedIn data"),
                    tags$li("Upload a YouTube CSV for YouTube-specific analytics"),
                    tags$li("Or upload both to see combined insights")
            )
        )
      ),
      
      # Show tabs when data is available
      conditionalPanel(
        condition = "output.has_data",
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
                   plotOutput("engagement_rate")),
          
          tabPanel("YouTube Analytics",
                   h4("YouTube Metrics"),
                   plotOutput("youtube_views_vs_likes"),
                   hr(),
                   plotOutput("youtube_subscribers_plot"),
                   hr(),
                   plotOutput("youtube_likes_dislikes"),
                   hr(),
                   DTOutput("youtube_table")),
          
          tabPanel("Debug Info",
                   h4("Data Loading Diagnostics"),
                   h5("Main CSV Status:"),
                   verbatimTextOutput("debug_main_data"),
                   hr(),
                   h5("YouTube CSV Status:"),
                   verbatimTextOutput("debug_youtube_data"),
                   hr(),
                   h5("Combined Data Status:"),
                   verbatimTextOutput("debug_combined_data"),
                   hr(),
                   h5("Filtered Data Status:"),
                   verbatimTextOutput("debug_filtered_data"),
                   hr(),
                   h5("Available Networks:"),
                   verbatimTextOutput("debug_networks"),
                   hr(),
                   h5("Sample of Combined Data (First 10 Rows):"),
                   DTOutput("debug_sample_table"))
        )
      )  # Close conditionalPanel for tabs
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================
server <- function(input, output, session) {
  
  # --------------------------------------------------------------------------
  # OUTPUT: Flag to check if any data is loaded
  # --------------------------------------------------------------------------
  output$has_data <- reactive({
    !is.null(combined_raw_data())
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  
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
    ),
    YouTube = list(
      post_types = c("Video"),
      content_types = c("Video")
    )
  )
  
  # --------------------------------------------------------------------------
  # HELPER FUNCTION: Normalize column names for fuzzy matching
  # Removes separators (periods, underscores, spaces) and converts to lowercase
  # --------------------------------------------------------------------------
  normalize_col_name <- function(name) {
    tolower(gsub("[._\\s-]", "", name))
  }
  
  # --------------------------------------------------------------------------
  # HELPER FUNCTION: Find column by fuzzy name matching
  # Returns the actual column name from data that matches the target
  # --------------------------------------------------------------------------
  find_column <- function(data, target_name) {
    normalized_target <- normalize_col_name(target_name)
    actual_names <- names(data)
    normalized_actual <- sapply(actual_names, normalize_col_name)
    
    match_idx <- which(normalized_actual == normalized_target)
    if (length(match_idx) > 0) {
      return(actual_names[match_idx[1]])
    }
    return(NULL)
  }
  
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
  # REACTIVE: Load and validate YouTube data from CSV
  # --------------------------------------------------------------------------
  youtube_data <- reactive({
    # Don't require YouTube file - it's optional
    if (is.null(input$youtube_file)) {
      return(NULL)
    }
    
    tryCatch({
      cat("\n=== YOUTUBE DATA LOADING DEBUG ===\n")
      data <- read.csv(input$youtube_file$datapath, stringsAsFactors = FALSE)
      cat("CSV read successfully. Rows:", nrow(data), "Columns:", ncol(data), "\n")
      cat("Original column names:", paste(names(data), collapse = ", "), "\n")
      
      # Define required columns for YouTube with fuzzy matching
      required_mapping <- list(
        Content = "content",
        Video.title = "videotitle",
        Video.publish.time = "videopublishtime",
        Duration = "duration",
        Likes = "likes",
        Subscribers.gained = "subscribersgained",
        Dislikes = "dislikes"
      )
      
      # Define optional columns
      optional_mapping <- list(
        Views = "views",
        Comments = "comments",
        Shares = "shares"
      )
      
      # Find actual column names using fuzzy matching
      actual_cols <- list()
      missing_cols <- c()
      
      cat("\nFuzzy column matching:\n")
      for (standard_name in names(required_mapping)) {
        found_col <- find_column(data, standard_name)
        if (!is.null(found_col)) {
          actual_cols[[standard_name]] <- found_col
          cat("  ✓", standard_name, "->", found_col, "\n")
        } else {
          missing_cols <- c(missing_cols, standard_name)
          cat("  ✗", standard_name, "-> NOT FOUND\n")
        }
      }
      
      # Check for missing required columns
      if (length(missing_cols) > 0) {
        cat("\nMISSING COLUMNS:", paste(missing_cols, collapse = ", "), "\n")
        showNotification(
          paste("Missing YouTube columns:", paste(missing_cols, collapse = ", "),
                "\nAvailable columns:", paste(names(data), collapse = ", ")),
          type = "error",
          duration = NULL
        )
        return(NULL)
      }
      
      # Rename columns to standard names
      for (standard_name in names(actual_cols)) {
        names(data)[names(data) == actual_cols[[standard_name]]] <- standard_name
      }
      cat("\nColumns renamed successfully\n")
      
      # Process optional columns
      optional_cols <- list()
      cat("\nChecking optional columns:\n")
      for (standard_name in names(optional_mapping)) {
        found_col <- find_column(data, standard_name)
        if (!is.null(found_col)) {
          optional_cols[[standard_name]] <- found_col
          names(data)[names(data) == found_col] <- standard_name
          cat("  ✓", standard_name, "->", found_col, "\n")
        } else {
          cat("  -", standard_name, "-> not found (optional)\n")
        }
      }
      
      # Parse dates - try multiple formats
      cat("\nParsing dates...\n")
      cat("Sample of Video.publish.time values:", paste(head(data$Video.publish.time, 3), collapse = ", "), "\n")
      
      data <- data %>%
        mutate(
          Date = tryCatch({
            # Try mdy format first (e.g., "Mar 16, 2026")
            parsed_date <- mdy(Video.publish.time, quiet = TRUE)
            if (all(is.na(parsed_date))) {
              # Try ymd_hms format
              parsed_date <- ymd_hms(Video.publish.time, quiet = TRUE)
            }
            if (all(is.na(parsed_date))) {
              # Try mdy_hms format
              parsed_date <- mdy_hms(Video.publish.time, quiet = TRUE)
            }
            if (all(is.na(parsed_date))) {
              # Try dmy format
              parsed_date <- dmy(Video.publish.time, quiet = TRUE)
            }
            if (all(is.na(parsed_date))) {
              # Try ymd format
              parsed_date <- ymd(Video.publish.time, quiet = TRUE)
            }
            as.Date(parsed_date)
          }, error = function(e) {
            # Last resort - try as.Date directly
            as.Date(Video.publish.time)
          })
        )
      
      cat("Sample of parsed dates:", paste(head(data$Date, 3), collapse = ", "), "\n")
      
      # Check if date parsing was successful
      if (all(is.na(data$Date))) {
        cat("WARNING: All dates are NA after parsing!\n")
        showNotification(
          "Warning: Could not parse dates in YouTube data. Please check Video.publish.time format.",
          type = "warning",
          duration = 5
        )
      }
      
      # Transform to match main data structure
      cat("\nTransforming data structure...\n")
      data <- data %>%
        mutate(
          Network = "YouTube",
          Post.Type = "Video",
          Content.Type = "Video",
          Impressions = NA_real_,  # YouTube data doesn't have impressions
          Reach = NA_real_,
          Engagements = Likes,  # Map Likes to Engagements
          Reactions = Likes,    # Also map to Reactions
          Video.Views = if ("Views" %in% names(.)) Views else NA_real_,
          Comments = if ("Comments" %in% names(.)) Comments else NA_real_,
          Shares = if ("Shares" %in% names(.)) Shares else NA_real_,
          Saves = NA_real_,
          Post.Link.Clicks = NA_real_,
          Tags = "",  # Empty string - don't show internal Content codes to users
          YouTube.Title = Video.title,
          YouTube.Duration = Duration,
          YouTube.Subscribers.Gained = Subscribers.gained,
          YouTube.Dislikes = Dislikes,
          YouTube.Content = Content  # Store Content code in YouTube-specific column
        ) %>%
        select(Date, Network, Post.Type, Content.Type, Impressions, Reach,
               Engagements, Reactions, Likes, Comments, Shares, Saves,
               Video.Views, Post.Link.Clicks, Tags, YouTube.Title, 
               YouTube.Duration, YouTube.Subscribers.Gained, YouTube.Dislikes,
               YouTube.Content) %>%
        arrange(Date)
      
      cat("Transformation complete!\n")
      cat("Final data: Rows =", nrow(data), ", Columns =", ncol(data), "\n")
      cat("Sample Likes values:", paste(head(data$Likes, 3), collapse = ", "), "\n")
      cat("Sample Engagements values:", paste(head(data$Engagements, 3), collapse = ", "), "\n")
      cat("=== END YOUTUBE DEBUG ===\n\n")
      
      showNotification(
        paste("YouTube data loaded successfully!", nrow(data), "videos found."), 
        type = "message", 
        duration = 3
      )
      return(data)
      
    }, error = function(e) {
      showNotification(
        paste("Error loading YouTube data:", e$message),
        type = "error",
        duration = NULL
      )
      return(NULL)
    })
  })
  
  # --------------------------------------------------------------------------
  # REACTIVE: Load and validate raw data from CSV
  # Performs data type conversions and basic validation
  # --------------------------------------------------------------------------
  raw_data <- reactive({
    # Don't require main file anymore - it's optional
    if (is.null(input$file)) {
      return(NULL)
    }
    
    tryCatch({
      data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      
      # Define required columns with fuzzy matching
      required_mapping <- list(
        Date = "date",
        Network = "network",
        Post.Type = "posttype",
        Content.Type = "contenttype",
        Impressions = "impressions",
        Reach = "reach",
        Tags = "tags"
      )
      
      # Find actual column names using fuzzy matching
      actual_cols <- list()
      missing_cols <- c()
      
      for (standard_name in names(required_mapping)) {
        found_col <- find_column(data, standard_name)
        if (!is.null(found_col)) {
          actual_cols[[standard_name]] <- found_col
        } else {
          missing_cols <- c(missing_cols, standard_name)
        }
      }
      
      # Check for missing required columns
      if (length(missing_cols) > 0) {
        showNotification(
          paste("Missing required columns:", paste(missing_cols, collapse = ", "),
                "\nAvailable columns:", paste(names(data), collapse = ", ")),
          type = "error",
          duration = NULL
        )
        return(NULL)
      }
      
      # Rename columns to standard names
      for (standard_name in names(actual_cols)) {
        names(data)[names(data) == actual_cols[[standard_name]]] <- standard_name
      }
      
      # Also handle optional numeric columns with fuzzy matching
      optional_cols <- c("Engagements", "Reactions", "Likes", "Comments", 
                         "Shares", "Saves", "Video.Views", "Post.Link.Clicks")
      
      for (col_name in optional_cols) {
        found_col <- find_column(data, col_name)
        if (!is.null(found_col) && found_col != col_name) {
          names(data)[names(data) == found_col] <- col_name
        }
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
          tolower(Network) %in% c("youtube", "yt") ~ "YouTube",
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
  # REACTIVE: Combined data from both sources
  # --------------------------------------------------------------------------
  combined_raw_data <- reactive({
    main_data <- raw_data()
    yt_data <- youtube_data()
    
    # If neither file is uploaded, return NULL
    if (is.null(main_data) && is.null(yt_data)) {
      return(NULL)
    }
    
    # If only main data exists, return it
    if (!is.null(main_data) && is.null(yt_data)) {
      return(main_data)
    }
    
    # If only YouTube data exists, return it
    if (is.null(main_data) && !is.null(yt_data)) {
      return(yt_data)
    }
    
    # If both exist, combine them
    if (!is.null(main_data) && !is.null(yt_data)) {
      # Get all unique columns from both datasets
      all_cols <- union(names(main_data), names(yt_data))
      
      # Add missing columns to main_data with NA
      for (col in setdiff(all_cols, names(main_data))) {
        main_data[[col]] <- NA
      }
      
      # Add missing columns to yt_data with NA
      for (col in setdiff(all_cols, names(yt_data))) {
        yt_data[[col]] <- NA
      }
      
      # Now both have same columns, combine them
      combined <- bind_rows(main_data, yt_data) %>%
        arrange(Date)
      
      cat("Combined data created. Total rows:", nrow(combined), "\n")
      cat("YouTube-specific columns preserved:", 
          paste(grep("^YouTube\\.", names(combined), value = TRUE), collapse = ", "), "\n")
      
      return(combined)
    }
    
    return(NULL)
  })
  
  # --------------------------------------------------------------------------
  # REACTIVE: Get available networks from data
  # --------------------------------------------------------------------------
  available_networks <- reactive({
    req(combined_raw_data())
    unique(combined_raw_data()$Network) %>% sort()
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
    req(combined_raw_data())
    
    # Default to "All networks" if input not ready yet
    selected_network <- if (is.null(input$selected_network)) {
      "All networks"
    } else {
      input$selected_network
    }
    
    if (selected_network == "All networks") {
      # If all networks, show all unique post types from data
      all_types <- unique(combined_raw_data()$Post.Type) %>% 
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
        network_types <- combined_raw_data() %>%
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
    req(combined_raw_data())
    
    # Default to "All networks" if input not ready yet
    selected_network <- if (is.null(input$selected_network)) {
      "All networks"
    } else {
      input$selected_network
    }
    
    if (selected_network == "All networks") {
      # If all networks, show all unique content types from data
      all_types <- unique(combined_raw_data()$Content.Type) %>% 
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
        network_types <- combined_raw_data() %>%
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
    req(combined_raw_data())
    
    if ("Post.Link.Clicks" %in% names(combined_raw_data())) {
      list(
        checkboxInput("enable_link_clicks",
                      "Filter by Post Link Clicks",
                      value = FALSE),
        
        conditionalPanel(
          condition = "input.enable_link_clicks == true",
          sliderInput("link_clicks_min",
                      "Minimum Link Clicks:",
                      min = 0,
                      max = max(combined_raw_data()$Post.Link.Clicks, na.rm = TRUE),
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
    req(combined_raw_data())
    data <- combined_raw_data()
    
    updateDateRangeInput(session, "date_range",
                         start = min(data$Date, na.rm = TRUE),
                         end = max(data$Date, na.rm = TRUE))
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Dynamic impressions slider
  # Range adjusts based on data distribution (mean + 2.5 SD)
  # --------------------------------------------------------------------------
  output$impressions_slider <- renderUI({
    if (is.null(combined_raw_data())) {
      sliderInput("impressions_max", 
                  "Max Impressions Filter:",
                  min = 0,
                  max = 100000,
                  value = 100000)
    } else {
      req(combined_raw_data())
      
      # Calculate reasonable max based on distribution
      max_val <- mean(combined_raw_data()$Impressions, na.rm = TRUE) + 
        (2.5 * sd(combined_raw_data()$Impressions, na.rm = TRUE))
      
      # Make sure max_val is greater than 0 and not Inf
      max_val <- max(c(max_val, max(combined_raw_data()$Impressions, na.rm = TRUE)), na.rm = TRUE)
      
      if (is.infinite(max_val) || is.na(max_val) || max_val <= 0) {
        max_val <- 100000
      }
      
      sliderInput("impressions_max", 
                  "Max Impressions Filter:",
                  min = 0,
                  max = ceiling(max_val),
                  value = c(0, ceiling(max_val)))
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
    
    if (is.null(combined_raw_data())) {
      sliderInput("engagements_max",
                  label,
                  min = 0,
                  max = 5000,
                  value = 5000)
    } else {
      req(combined_raw_data())
      
      # Use appropriate column based on toggle
      if (!is.null(input$use_reactions) && input$use_reactions && "Reactions" %in% names(combined_raw_data())) {
        metric_col <- combined_raw_data()$Reactions
      } else if ("Engagements" %in% names(combined_raw_data())) {
        metric_col <- combined_raw_data()$Engagements
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
                  value = c(0, ceiling(max_val)))
    }
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Dynamic YouTube Views slider
  # Range adjusts based on data distribution (mean + 1 SD)
  # --------------------------------------------------------------------------
  output$youtube_views_slider <- renderUI({
    if (is.null(combined_raw_data())) {
      return(NULL)
    }
    
    req(combined_raw_data())
    
    # Check if we have YouTube data with Video.Views
    if (!"Video.Views" %in% names(combined_raw_data())) {
      return(NULL)
    }
    
    yt_data <- combined_raw_data() %>%
      filter(Network == "YouTube" & !is.na(Video.Views))
    
    if (nrow(yt_data) == 0) {
      return(NULL)
    }
    
    # Calculate reasonable max based on distribution
    max_val <- mean(yt_data$Video.Views, na.rm = TRUE) + 
      (sd(yt_data$Video.Views, na.rm = TRUE))
    
    if (is.infinite(max_val) || is.na(max_val) || max_val <= 0) {
      max_val <- 100000
    }
    
    sliderInput("youtube_views_max",
                "Max YouTube Views Filter:",
                min = 0,
                max = ceiling(max_val),
                value = c(0, ceiling(max_val - (0.75 * max_val))))
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: Dynamic YouTube Likes slider
  # Range adjusts based on data distribution (mean + 1 SD)
  # --------------------------------------------------------------------------
  output$youtube_likes_slider <- renderUI({
    if (is.null(combined_raw_data())) {
      return(NULL)
    }
    
    req(combined_raw_data())
    
    # Check if we have YouTube data with Likes
    yt_data <- combined_raw_data() %>%
      filter(Network == "YouTube" & !is.na(Likes))
    
    if (nrow(yt_data) == 0) {
      return(NULL)
    }
    
    # Calculate reasonable max based on distribution
    max_val <- mean(yt_data$Likes, na.rm = TRUE) + 
      (sd(yt_data$Likes, na.rm = TRUE))
    
    # Make sure max_val is valid
    max_val <- max(c(max_val, max(yt_data$Likes, na.rm = TRUE)), na.rm = TRUE)
    
    if (is.infinite(max_val) || is.na(max_val) || max_val <= 0) {
      max_val <- 5000
    }
    
    sliderInput("youtube_likes_max",
                "Max YouTube Likes Filter:",
                min = 0,
                max = ceiling(max_val),
                value = c(0, ceiling(max_val)))
  })
  
  # --------------------------------------------------------------------------
  # REACTIVE: Filtered data based on all user selections
  # Applies network, date, post type, content type, and metric filters
  # --------------------------------------------------------------------------
  filtered_data <- reactive({
    req(combined_raw_data())
    
    data <- combined_raw_data()
    
    # Filter by network (only if input is ready and not "All networks")
    if (!is.null(input$selected_network) && 
        input$selected_network != "All networks") {
      data <- data %>%
        filter(Network == input$selected_network)
    }
    
    # Apply impressions filter (only if input exists)
    if (!is.null(input$impressions_max)) {
      data <- data %>%
        filter(Impressions <= input$impressions_max | is.na(Impressions))
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
    
    # Apply YouTube Views filter (only if input exists)
    if (!is.null(input$youtube_views_max) && "Video.Views" %in% names(data)) {
      data <- data %>%
        filter(is.na(Video.Views) | Video.Views <= input$youtube_views_max)
    }
    
    # Apply YouTube Likes filter (only if input exists)
    if (!is.null(input$youtube_likes_max)) {
      data <- data %>%
        filter(Network != "YouTube" | is.na(Likes) | Likes <= input$youtube_likes_max)
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
    
    # Filter out NA values for plotting
    plot_data <- filtered_data() %>%
      filter(!is.na(Impressions) & !is.na(!!sym(y_metric)))
    
    if (nrow(plot_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No valid data for scatter plot.\n(YouTube data lacks Impressions)", 
           cex = 1.5, col = "orange")
      return()
    }
    
    # Create scatter plot
    p <- ggplot(plot_data, 
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
    
    # Filter out NA values
    plot_data <- filtered_data() %>%
      filter(!is.na(Impressions))
    
    if (nrow(plot_data) < 3) {
      return("Insufficient data for regression (need at least 3 observations)")
    }
    
    # Determine Y variable
    y_metric <- if (!is.null(input$use_reactions) && 
                    input$use_reactions && 
                    "Reactions" %in% names(plot_data)) {
      "Reactions"
    } else if ("Engagements" %in% names(plot_data)) {
      "Engagements"
    } else {
      return("No engagement metric available")
    }
    
    # Further filter for Y metric
    plot_data <- plot_data %>%
      filter(!is.na(!!sym(y_metric)))
    
    if (nrow(plot_data) < 3) {
      return("Insufficient data for regression (need at least 3 observations)")
    }
    
    # Build regression model
    tryCatch({
      formula_str <- paste(y_metric, "~ Impressions")
      model <- lm(as.formula(formula_str), data = plot_data)
      r_sqrd <- signif(summary(model)$r.squared, digits = 3)
      
      paste0("R-squared: ", r_sqrd, "\n",
             "Number of observations: ", nrow(plot_data))
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
                      "Video.Views", "Post.Link.Clicks", "YouTube.Subscribers.Gained",
                      "YouTube.Dislikes")
    
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
          pivot_longer(-Date, names_to = "Metric", values_to = "Value") %>%
          filter(!is.na(Value))
      } else {
        plot_data <- filtered_data() %>%
          select(Date, Impressions, Reach) %>%
          pivot_longer(-Date, names_to = "Metric", values_to = "Value") %>%
          filter(!is.na(Value))
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
      
      # Calculate engagement rate (filter out rows without Impressions)
      plot_data <- filtered_data() %>%
        filter(!is.na(Impressions) & Impressions > 0) %>%
        mutate(Engagement_Rate = (!!sym(engagement_metric) / Impressions) * 100)
      
      if (nrow(plot_data) == 0) {
        plot.new()
        text(0.5, 0.5, "No data with Impressions available\n(YouTube data excluded)", 
             cex = 1.5, col = "orange")
        return()
      }
      
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
  # OUTPUT: YouTube Views vs Likes scatter plot with marginal density
  # --------------------------------------------------------------------------
  output$youtube_views_vs_likes <- renderPlot({
    tryCatch({
      req(filtered_data())
      
      # Filter for YouTube data only
      yt_data <- filtered_data() %>%
        filter(Network == "YouTube")
      
      # Check if required columns exist
      if (!"Video.Views" %in% names(yt_data)) {
        plot.new()
        text(0.5, 0.5, "Video.Views column not found.\nPlease ensure your YouTube CSV has a 'Video.Views' or 'Views' column.", 
             cex = 1.2)
        return()
      }
      
      if (!"Likes" %in% names(yt_data)) {
        plot.new()
        text(0.5, 0.5, "Likes column not found", cex = 1.5)
        return()
      }
      
      # Filter for non-NA values
      plot_data <- yt_data %>%
        filter(!is.na(Video.Views) & !is.na(Likes) & Video.Views > 0)
      
      if (nrow(plot_data) == 0) {
        plot.new()
        text(0.5, 0.5, "No YouTube data with both Views and Likes available.\nCheck that your YouTube CSV includes view counts.", 
             cex = 1.2)
        return()
      }
      
      # Create scatter plot
      p <- ggplot(plot_data, aes(Video.Views, Likes)) +
        geom_point(size = 3, alpha = 0.7, color = "#FF0000") +
        geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
        theme_minimal() +
        labs(title = "YouTube Views vs Likes",
             subtitle = paste("Based on", nrow(plot_data), "videos"),
             x = "Views",
             y = "Likes") +
        scale_x_continuous(labels = scales::comma) +
        scale_y_continuous(labels = scales::comma) + 
        theme(plot.title = element_text(size = 16, face = "bold"),
              plot.subtitle = element_text(size = 12),
              axis.title = element_text(size = 12))
      
      # Add marginal density plots
      tryCatch({
        ggMarginal(p, type = "density", fill = "#FF0000", alpha = 0.3)
      }, error = function(e) {
        # If marginal plots fail, return the base plot
        print(p)
      })
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating Views vs Likes plot:\n", e$message), 
           cex = 1, col = "red")
    })
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: YouTube Subscribers Gained Over Time
  # --------------------------------------------------------------------------
  output$youtube_subscribers_plot <- renderPlot({
    tryCatch({
      req(filtered_data())
      
      # Filter for YouTube data only
      yt_data <- filtered_data() %>%
        filter(Network == "YouTube")
      
      # Check if YouTube.Subscribers.Gained column exists
      if (!"YouTube.Subscribers.Gained" %in% names(yt_data)) {
        plot.new()
        text(0.5, 0.5, "YouTube.Subscribers.Gained column not found", cex = 1.5)
        return()
      }
      
      # Filter for non-NA values
      yt_data <- yt_data %>%
        filter(!is.na(YouTube.Subscribers.Gained))
      
      if (nrow(yt_data) == 0) {
        plot.new()
        text(0.5, 0.5, "No YouTube data available", cex = 1.5)
        return()
      }
      
      ggplot(yt_data, aes(Date, YouTube.Subscribers.Gained)) +
        geom_line(linewidth = 1, color = "steelblue") +
        geom_point(size = 3, color = "steelblue") +
        theme_minimal() +
        labs(title = "Subscribers Gained Over Time",
             x = "Date",
             y = "Subscribers Gained") +
        theme(plot.title = element_text(size = 14, face = "bold"))
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating plot:\n", e$message), cex = 1, col = "red")
    })
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: YouTube Likes vs Dislikes
  # --------------------------------------------------------------------------
  output$youtube_likes_dislikes <- renderPlot({
    tryCatch({
      req(filtered_data())
      
      # Filter for YouTube data only
      yt_data <- filtered_data() %>%
        filter(Network == "YouTube")
      
      # Check if required columns exist
      if (!"Likes" %in% names(yt_data)) {
        plot.new()
        text(0.5, 0.5, "Likes column not found", cex = 1.5)
        return()
      }
      
      if (!"YouTube.Dislikes" %in% names(yt_data)) {
        plot.new()
        text(0.5, 0.5, "YouTube.Dislikes column not found", cex = 1.5)
        return()
      }
      
      # Filter for non-NA Likes
      yt_data <- yt_data %>%
        filter(!is.na(Likes))
      
      if (nrow(yt_data) == 0) {
        plot.new()
        text(0.5, 0.5, "No YouTube data available", cex = 1.5)
        return()
      }
      
      # Prepare data for plotting - handle NA in Dislikes
      plot_data <- yt_data %>%
        mutate(YouTube.Dislikes = ifelse(is.na(YouTube.Dislikes), 0, YouTube.Dislikes)) %>%
        select(Date, Likes, YouTube.Dislikes) %>%
        pivot_longer(-Date, names_to = "Metric", values_to = "Count") %>%
        mutate(Metric = ifelse(Metric == "Likes", "Likes", "Dislikes"))
      
      ggplot(plot_data, aes(Date, Count, fill = Metric)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("Likes" = "#4CAF50", "Dislikes" = "#F44336")) +
        theme_minimal() +
        labs(title = "Likes vs Dislikes Over Time",
             x = "Date",
             y = "Count") +
        scale_x_continuous(labels = scales::comma) +
        scale_y_continuous(labels = scales::comma) + 
        theme(legend.position = "bottom",
              plot.title = element_text(size = 14, face = "bold"))
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating plot:\n", e$message), cex = 1, col = "red")
    })
  })
  
  # --------------------------------------------------------------------------
  # OUTPUT: YouTube Data Table
  # --------------------------------------------------------------------------
  output$youtube_table <- renderDT({
    tryCatch({
      req(filtered_data())
      
      # Filter for YouTube data only
      yt_data <- filtered_data() %>%
        filter(Network == "YouTube")
      
      if (nrow(yt_data) == 0) {
        return(datatable(data.frame(Message = "No YouTube data available")))
      }
      
      # Select YouTube-specific columns that exist
      available_cols <- c("Date", "YouTube.Title", "Likes", "YouTube.Dislikes", 
                          "YouTube.Subscribers.Gained", "YouTube.Duration", "Tags")
      existing_cols <- intersect(available_cols, names(yt_data))
      
      if (length(existing_cols) == 0) {
        return(datatable(data.frame(Message = "No YouTube columns available in filtered data")))
      }
      
      # Select only existing columns
      yt_display <- yt_data %>%
        select(all_of(existing_cols))
      
      # Create display names
      display_names <- existing_cols
      display_names <- gsub("YouTube\\.", "", display_names)
      display_names <- gsub("\\.", " ", display_names)
      
      datatable(yt_display, 
                options = list(pageLength = 10, scrollX = TRUE),
                filter = "top",
                colnames = display_names)
    }, error = function(e) {
      datatable(data.frame(Error = paste("Error loading YouTube data:", e$message)))
    })
  })
  
  # --------------------------------------------------------------------------
  # DEBUG OUTPUTS
  # --------------------------------------------------------------------------
  output$debug_main_data <- renderPrint({
    if (is.null(input$file)) {
      cat("No main CSV uploaded\n")
    } else if (is.null(raw_data())) {
      cat("Main CSV uploaded but failed to load\n")
      cat("Check error notifications above\n")
    } else {
      data <- raw_data()
      cat("Main CSV loaded successfully!\n")
      cat("Rows:", nrow(data), "\n")
      cat("Columns:", ncol(data), "\n")
      cat("Column names:", paste(names(data), collapse = ", "), "\n")
      cat("\nDate range:", as.character(min(data$Date, na.rm = TRUE)), 
          "to", as.character(max(data$Date, na.rm = TRUE)), "\n")
      cat("\nNetworks found:", paste(unique(data$Network), collapse = ", "), "\n")
      cat("\nFirst few dates:\n")
      print(head(data$Date, 5))
    }
  })
  
  output$debug_youtube_data <- renderPrint({
    if (is.null(input$youtube_file)) {
      cat("No YouTube CSV uploaded\n")
    } else if (is.null(youtube_data())) {
      cat("YouTube CSV uploaded but failed to load\n")
      cat("Check error notifications above\n")
    } else {
      data <- youtube_data()
      cat("YouTube CSV loaded successfully!\n")
      cat("Rows:", nrow(data), "\n")
      cat("Columns:", ncol(data), "\n")
      cat("Column names:", paste(names(data), collapse = ", "), "\n")
      cat("\nDate range:", as.character(min(data$Date, na.rm = TRUE)), 
          "to", as.character(max(data$Date, na.rm = TRUE)), "\n")
      cat("\nSample of parsed dates:\n")
      print(head(data$Date, 5))
      cat("\nSample of Likes values:\n")
      print(head(data$Likes, 5))
      cat("\nSample of Engagements values:\n")
      print(head(data$Engagements, 5))
    }
  })
  
  output$debug_combined_data <- renderPrint({
    if (is.null(combined_raw_data())) {
      cat("No combined data available\n")
      cat("Make sure at least one CSV is uploaded\n")
    } else {
      data <- combined_raw_data()
      cat("Combined data created successfully!\n")
      cat("Total rows:", nrow(data), "\n")
      cat("Total columns:", ncol(data), "\n")
      cat("Column names:", paste(names(data), collapse = ", "), "\n")
      cat("\nNetworks in combined data:\n")
      print(table(data$Network))
      cat("\nDate range:", as.character(min(data$Date, na.rm = TRUE)), 
          "to", as.character(max(data$Date, na.rm = TRUE)), "\n")
    }
  })
  
  output$debug_filtered_data <- renderPrint({
    if (is.null(filtered_data())) {
      cat("No filtered data available\n")
    } else {
      data <- filtered_data()
      cat("Filtered data:\n")
      cat("Total rows after filtering:", nrow(data), "\n")
      cat("\nCurrent filter settings:\n")
      cat("- Selected Network:", ifelse(is.null(input$selected_network), "NULL", input$selected_network), "\n")
      cat("- Date Range:", 
          ifelse(is.null(input$date_range), "NULL", 
                 paste(input$date_range[1], "to", input$date_range[2])), "\n")
      cat("- Post Types:", ifelse(is.null(input$post_type), "NULL", 
                                  paste(input$post_type, collapse = ", ")), "\n")
      cat("- Content Types:", ifelse(is.null(input$content_type), "NULL", 
                                     paste(input$content_type, collapse = ", ")), "\n")
      cat("\nNetworks in filtered data:\n")
      print(table(data$Network))
    }
  })
  
  output$debug_networks <- renderPrint({
    if (is.null(available_networks())) {
      cat("No networks detected\n")
    } else {
      cat("Available networks:\n")
      print(available_networks())
      cat("\nNetwork selector choices:\n")
      cat(paste(c("All networks", available_networks()), collapse = ", "))
    }
  })
  
  output$debug_sample_table <- renderDT({
    if (is.null(combined_raw_data())) {
      return(datatable(data.frame(Message = "No data available")))
    }
    
    sample_data <- combined_raw_data() %>%
      head(10) %>%
      select(any_of(c("Date", "Network", "Post.Type", "Content.Type", 
                      "Impressions", "Engagements", "Likes", "Tags")))
    
    datatable(sample_data, options = list(scrollX = TRUE))
  })
  
  # --------------------------------------------------------------------------
  # OBSERVER: Reset all filters to defaults
  # --------------------------------------------------------------------------
  observeEvent(input$reset, {
    req(combined_raw_data())
    
    data <- combined_raw_data()
    
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