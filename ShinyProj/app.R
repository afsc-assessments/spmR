library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(readr)
library(stringr)

# Function to parse the fisheries data
parse_fisheries_data <- function(file_path) {
  
  # Read the entire file
  lines <- readLines(file_path)
  
  # Find reference points (first few lines)
  ref_points <- list()
  alt_start_line <- which(grepl("^Alternative", lines))[1]
  
  if (!is.na(alt_start_line) && alt_start_line > 1) {
    header_lines <- lines[1:(alt_start_line - 1)]
    for (line in header_lines) {
      line <- str_trim(line)
      if (nchar(line) > 0 && grepl("\\s+", line)) {
        parts <- str_split(line, "\\s+")[[1]]
        if (length(parts) >= 2) {
          # Try to convert to numeric, skip if it fails
          tryCatch({
            ref_points[[parts[1]]] <- as.numeric(parts[2])
          }, warning = function(w) {}, error = function(e) {})
        }
      }
    }
  }
  
  # Find all alternative sections
  alt_starts <- which(grepl("^Alternative \\d+", lines))
  
  all_data <- list()
  
  for (i in seq_along(alt_starts)) {
    alt_num <- str_extract(lines[alt_starts[i]], "\\d+")
    
    # Determine the end of this alternative
    if (i < length(alt_starts)) {
      alt_end <- alt_starts[i + 1] - 1
    } else {
      alt_end <- length(lines)
    }
    
    alt_lines <- lines[(alt_starts[i] + 1):alt_end]
    
    # Find data sections within this alternative
    catch_start <- which(grepl("^Catch", alt_lines))
    ssb_start <- which(grepl("^Spawning_Biomass", alt_lines))
    f_start <- which(grepl("^Fishing_mortality", alt_lines))
    biomass_start <- which(grepl("^Total_Biomass", alt_lines))
    
    # Store section positions
    section_positions <- c()
    if(length(catch_start) > 0) section_positions <- c(section_positions, catch_start[1])
    if(length(ssb_start) > 0) section_positions <- c(section_positions, ssb_start[1])
    if(length(f_start) > 0) section_positions <- c(section_positions, f_start[1])
    if(length(biomass_start) > 0) section_positions <- c(section_positions, biomass_start[1])
    
    section_positions <- sort(section_positions)
    
    alt_data <- list()
    
    # Process each section
    sections_to_process <- list(
      catch = if(length(catch_start) > 0) catch_start[1] else NULL,
      ssb = if(length(ssb_start) > 0) ssb_start[1] else NULL,
      f_mort = if(length(f_start) > 0) f_start[1] else NULL,
      total_biomass = if(length(biomass_start) > 0) biomass_start[1] else NULL
    )
    
    for (section_name in names(sections_to_process)) {
      start_idx <- sections_to_process[[section_name]]
      
      if (!is.null(start_idx)) {
        # Find the end of this section
        next_sections <- section_positions[section_positions > start_idx]
        if (length(next_sections) > 0) {
          end_idx <- min(next_sections) - 1
        } else {
          end_idx <- length(alt_lines)
        }
        
        # Skip the header line and read data
        data_start <- start_idx + 1
        if (data_start <= end_idx) {
          section_lines <- alt_lines[data_start:end_idx]
          # Remove empty lines and non-data lines
          section_lines <- section_lines[!grepl("^\\s*$", section_lines)]
          section_lines <- section_lines[grepl("^\\s*\\d", section_lines)] # Lines starting with numbers
          
          if (length(section_lines) > 0) {
            # Parse the data
            data_list <- list()
            for (line in section_lines) {
              parts <- str_split(str_trim(line), "\\s+")[[1]]
              if (length(parts) >= 2) {
                data_list[[length(data_list) + 1]] <- parts
              }
            }
            
            if (length(data_list) > 0) {
              # Convert to data frame
              max_cols <- max(sapply(data_list, length))
              data_matrix <- matrix(NA, nrow = length(data_list), ncol = max_cols)
              
              for (j in seq_along(data_list)) {
                row_data <- data_list[[j]]
                data_matrix[j, 1:length(row_data)] <- row_data
              }
              
              # Create column names based on the section
              if (section_name == "catch") {
                col_names <- c("Year", "C0", "Cabc", "Cofl", "LowCI_Catch", "Median_Catch", "Mean_Catch", "UpperCI_Catch", "Stdev_Catch")
              } else if (section_name == "ssb") {
                col_names <- c("Year", "SSB100", "SSBabc", "SSBofl", "LowCI_SSB", "Median_SSB", "Mean_SSB", "UpperCI_SSB", "Stdev_SSB")
              } else if (section_name == "f_mort") {
                col_names <- c("Year", "F0", "Fabc", "Fofl", "LowCI_F", "Median_F", "Mean_F", "UpperCI_F", "Stdev_F")
              } else if (section_name == "total_biomass") {
                col_names <- c("Year", "B100", "Babc", "Bofl", "LowCI_Biom", "Median_Biom", "Mean_Biom", "UpperCI_Biom", "Stdev_Biom")
              }
              
              # Trim column names to actual data
              col_names <- col_names[1:min(length(col_names), ncol(data_matrix))]
              
              df <- as.data.frame(data_matrix[, 1:length(col_names)], stringsAsFactors = FALSE)
              colnames(df) <- col_names
              
              # Convert numeric columns with error handling
              df$Year <- as.numeric(df$Year)
              for (col in 2:ncol(df)) {
                df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
              }
              
              alt_data[[section_name]] <- df
            }
          }
        }
      }
    }
    
    if (length(alt_data) > 0) {
      all_data[[paste0("Alternative_", alt_num)]] <- alt_data
    }
  }
  
  return(list(reference_points = ref_points, alternatives = all_data))
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Fisheries Projection Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Visualizations", tabName = "plots", icon = icon("chart-line")),
      menuItem("Data Tables", tabName = "tables", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Upload tab
      tabItem(tabName = "upload",
        fluidRow(
          box(title = "Data Upload", status = "primary", solidHeader = TRUE, width = 12,
            fileInput("file", "Choose fisheries projection file",
                     accept = c(".txt", ".dat", ".csv", ".out")),
            
            conditionalPanel(
              condition = "output.data_loaded",
              h4("Reference Points:"),
              verbatimTextOutput("ref_points"),
              
              h4("Available Alternatives:"),
              verbatimTextOutput("alternatives_list")
            )
          )
        )
      ),
      
      # Plots tab
      tabItem(tabName = "plots",
        fluidRow(
          box(title = "Controls", status = "primary", solidHeader = TRUE, width = 3,
            conditionalPanel(
              condition = "output.data_loaded",
              selectInput("selected_alternative", "Select Alternative:",
                         choices = NULL),
              
              checkboxInput("show_abc", "Show ABC Reference Lines", value = TRUE),
              checkboxInput("show_ofl", "Show OFL Reference Lines", value = FALSE),
              checkboxInput("show_ci", "Show Confidence Intervals", value = FALSE)
            )
          ),
          
          box(title = "Four Panel Plot", status = "primary", solidHeader = TRUE, width = 9,
            conditionalPanel(
              condition = "output.data_loaded",
              plotlyOutput("four_panel_plot", height = "600px")
            )
          )
        )
      ),
      
      # Tables tab
      tabItem(tabName = "tables",
        fluidRow(
          box(title = "Data Selection", status = "primary", solidHeader = TRUE, width = 3,
            conditionalPanel(
              condition = "output.data_loaded",
              selectInput("table_alternative", "Select Alternative:",
                         choices = NULL),
              selectInput("table_metric", "Select Metric:",
                         choices = c("Catch" = "catch",
                                   "Spawning Biomass" = "ssb", 
                                   "Fishing Mortality" = "f_mort",
                                   "Total Biomass" = "total_biomass"))
            )
          ),
          
          box(title = "Data Table", status = "primary", solidHeader = TRUE, width = 9,
            conditionalPanel(
              condition = "output.data_loaded",
              DTOutput("data_table")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    data = NULL,
    loaded = FALSE
  )
  
  # File upload
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      values$data <- parse_fisheries_data(input$file$datapath)
      values$loaded <- TRUE
      
      # Update alternative choices
      alt_choices <- names(values$data$alternatives)
      names(alt_choices) <- gsub("_", " ", alt_choices)
      
      updateSelectInput(session, "selected_alternative", 
                       choices = alt_choices,
                       selected = alt_choices[1])
      
      updateSelectInput(session, "table_alternative", 
                       choices = alt_choices,
                       selected = alt_choices[1])
      
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Output for data loaded status
  output$data_loaded <- reactive({
    return(values$loaded)
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Reference points display
  output$ref_points <- renderText({
    req(values$data)
    if (length(values$data$reference_points) > 0) {
      paste(names(values$data$reference_points), values$data$reference_points, 
            sep = ": ", collapse = "\n")
    } else {
      "No reference points found"
    }
  })
  
  # Alternatives list
  output$alternatives_list <- renderText({
    req(values$data)
    alt_names <- names(values$data$alternatives)
    paste(gsub("_", " ", alt_names), collapse = "\n")
  })
  
  # Four panel plot
  output$four_panel_plot <- renderPlotly({
    req(input$selected_alternative, values$data)
    
    alt_data <- values$data$alternatives[[input$selected_alternative]]
    
    plots <- list()
    
    # Catch plot
    if (!is.null(alt_data$catch)) {
      p1 <- plot_ly(data = alt_data$catch, x = ~Year, y = ~Mean_Catch, 
                    type = 'scatter', mode = 'lines+markers', name = 'Mean Catch') %>%
        layout(title = "Catch", xaxis = list(title = "Year"), 
               yaxis = list(title = "Catch (t)", rangemode="tozero"))
      
      if (input$show_abc && "Cabc" %in% colnames(alt_data$catch)) {
        p1 <- p1 %>% add_trace(y = ~Cabc, name = "ABC", line = list(dash = "dash"))
      }
      if (input$show_ofl && "Cofl" %in% colnames(alt_data$catch)) {
        p1 <- p1 %>% add_trace(y = ~Cofl, name = "OFL", line = list(dash = "dot"))
      }
      
      plots$catch <- p1
    }
    
    # SSB plot
    if (!is.null(alt_data$ssb)) {
      p2 <- plot_ly(data = alt_data$ssb, x = ~Year, y = ~Mean_SSB, 
                    type = 'scatter', mode = 'lines+markers', name = 'Mean SSB') %>%
        layout(title = "Spawning Stock Biomass", xaxis = list(title = "Year"), 
               yaxis = list(title = "SSB (t)", rangemode="tozero"))
      
      if (input$show_abc && "SSBabc" %in% colnames(alt_data$ssb)) {
        p2 <- p2 %>% add_trace(y = ~SSBabc, name = "SSB ABC", line = list(dash = "dash"))
      }
      
      plots$ssb <- p2
    }
    
    # F mortality plot
    if (!is.null(alt_data$f_mort)) {
      p3 <- plot_ly(data = alt_data$f_mort, x = ~Year, y = ~Mean_F, 
                    type = 'scatter', mode = 'lines+markers', name = 'Mean F') %>%
        layout(title = "Fishing Mortality", xaxis = list(title = "Year"), 
               yaxis = list(title = "F", rangemode="tozero"))
      
      if (input$show_abc && "Fabc" %in% colnames(alt_data$f_mort)) {
        p3 <- p3 %>% add_trace(y = ~Fabc, name = "F ABC", line = list(dash = "dash"))
      }
      if (input$show_ofl && "Fofl" %in% colnames(alt_data$f_mort)) {
        p3 <- p3 %>% add_trace(y = ~Fofl, name = "F OFL", line = list(dash = "dot"))
      }
      
      plots$f_mort <- p3
    }
    
    # Total biomass plot
    if (!is.null(alt_data$total_biomass)) {
      p4 <- plot_ly(data = alt_data$total_biomass, x = ~Year, y = ~Mean_Biom, 
                    type = 'scatter', mode = 'lines+markers', name = 'Mean Total Biomass') %>%
        layout(title = "Total Biomass", xaxis = list(title = "Year"), 
               yaxis = list(title = "Biomass (t)", rangemod="tozero"))
      
      if (input$show_abc && "Babc" %in% colnames(alt_data$total_biomass)) {
        p4 <- p4 %>% add_trace(y = ~Babc, name = "B ABC", line = list(dash = "dash"))
      }
      
      plots$total_biomass <- p4
    }
    
    # Combine plots
    if (length(plots) == 4) {
      subplot(plots$catch, plots$ssb, plots$f_mort, plots$total_biomass, 
              nrows = 2, shareX = TRUE, titleY = TRUE, margin = 0.05) %>%
        layout(title = paste("Fisheries Projections -", gsub("_", " ", input$selected_alternative)))
    } else {
      plot_ly() %>% layout(title = "No data available for selected alternative")
    }
  })
  
  # Data table
  output$data_table <- renderDT({
    req(input$table_alternative, input$table_metric, values$data)
    
    alt_data <- values$data$alternatives[[input$table_alternative]]
    
    if (!is.null(alt_data[[input$table_metric]])) {
      datatable(alt_data[[input$table_metric]], 
                options = list(scrollX = TRUE, pageLength = 15),
                rownames = FALSE) %>%
        formatRound(columns = 2:ncol(alt_data[[input$table_metric]]), digits = 3)
    } else {
      datatable(data.frame(Message = "No data available"), options = list(dom = 't'))
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)