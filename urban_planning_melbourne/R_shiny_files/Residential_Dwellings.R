library('shiny')
library('ggplot2')
library('ggiraph')
library('leaflet')
library('dplyr')
library('tidyr')
library('sf')
library('stringr')
library('bslib')

mortgage_data <- read.csv("../Data/Residential dwellings/2021Census_G38_VIC_LGA.csv", 
                          na.strings = c("", "NA"),
                          stringsAsFactors = FALSE,
                          fileEncoding = "UTF-8-BOM")

###################
# Data processing #
###################

# Define dwelling structure options
dwelling_structure_choices <- list(
  "Separate house" = "Separate_hse",
  "Semi-detached, row or terrace house, townhouse etc." = "sd_row_tce_hs_th",
  "Flat or apartment" = "Flat_apart",
  "Other dwelling" = "Other_dwg",
  "Not stated" = "ns",
  "All dwelling structure" = "Tot"
)

# Filter out mortgage level data only
mortgage_columns <- mortgage_data %>%
  select(LGA_CODE_2021, starts_with("M_"))

# Filter data for City of Melbourne (LGA_CODE_2021 == "LGA24600")
filtered_data <- mortgage_columns %>%
  filter(LGA_CODE_2021 == "LGA24600")

# Remove LGA_CODE_2021 after filtering
filtered_data <- filtered_data %>%
  select(-LGA_CODE_2021)

# Filter out the total columns and dwelling-specific columns
total_columns <- filtered_data %>%
  select(ends_with("Tot"))
dwelling_columns <- filtered_data %>%
  select(-ends_with("Tot"))

# Pivot the dwelling-specific data longer
melted_dwelling_data <- dwelling_columns %>%
  pivot_longer(
    cols = everything(),
    names_to = "Mortgage_Level_Dwelling",
    values_to = "Count"
  )

# Separate the mortgage level and dwelling type into separate columns
melted_dwelling_data <- melted_dwelling_data %>%
  separate(Mortgage_Level_Dwelling, into = c("Mortgage_Level", "Dwelling_Type"), sep = "_DS_", extra = "merge")

# Combine the totals with the melted dwelling data
final_data <- bind_rows(
  melted_dwelling_data,
  total_columns %>%
    pivot_longer(
      cols = everything(),
      names_to = "Mortgage_Level",
      values_to = "Count"
    ) %>%
    mutate(Dwelling_Type = "Total")
)

                                       
######
# UI #
######

# Define the UI
ui <- fluidPage(
  # Set theme to match Tableau dashboard
  theme = bs_theme(
    bg = "#ffffff",  
    fg = "#000000",
    primary = "#90ccd4"
  ),
  # Set font and floating panel layout
  tags$head(
    tags$style(HTML("
      #page-title {
        font-size: 20px; 
        font-weight: bold;
      }
      body {
        font-family: 'Heiti SC', sans-serif; /* 
      }
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Heiti SC', sans-serif;
      }
      .shiny-input-container {
        font-family: 'Heiti SC', sans-serif; /* Consistent font in inputs */
      }
      .floating-panel label {
        font-size: 14px; 
      }
      .floating-panel select {
        font-size: 8px; 
      }
      .floating-panel {
        background-color: #ffffff; 
        border-radius: 8px;
        padding: 8px;
        box-shadow: 0px 2px 10px rgba(0,0,0,0.1);
        position: absolute;
        top: 20px;
        left: 20px;
        z-index: 100;
      }
      #graph-container {
        padding: 0px; 
        margin: 0 auto; 
      }
      .girafe-container {
        max-width: 100%; /* Ensure graph container does not exceed available space */
      }
    "))
  ),
  

  # Floating dropdown list
  absolutePanel(
    class = "floating-panel",
    top = 12, left = 12, width = 200, draggable = TRUE,
    selectInput("DS", "Types of Dwelling Structure:",
                choices = names(dwelling_structure_choices),
                selected = "All dwelling structure")
  ),
  
  titlePanel(
    title = div(id = "page-title", "Number of Dwellings by Monthly Mortgage Level")
  ),
  # Left aligned graph using fluidRow and column
  fluidRow(
    column(
      width = 1,  # Allocate 9 columns out of 12 to make it aligned more to the left
      girafeOutput('mortgage_DS', width = "550px", height = "375px")
    )
  )
)

##########
# Server #
##########

server <- function(input, output, session) {
  
  # Reactive expression to return data based on dwelling structure selected
  getFilteredData <- reactive({
    dwelling_structure <- dwelling_structure_choices[[input$DS]]
    
    if (dwelling_structure == "Tot") {
      # Summarize total count for each mortgage level across all dwelling types
      complete_data <- final_data %>%
        group_by(Mortgage_Level) %>%
        summarize(Count = sum(Count, na.rm = TRUE))
    } else {
      # Filter data based on selected dwelling structure
      filtered_data <- final_data %>%
        filter(Dwelling_Type == dwelling_structure)
      
      # Ensure all mortgage levels are present, even if count is 0
      complete_data <- filtered_data %>%
        tidyr::complete(Mortgage_Level = unique(final_data$Mortgage_Level), fill = list(Count = 0))
    }
    
    # Format Mortgage_Level for better readability
    complete_data <- complete_data %>%
      mutate(Mortgage_Level = str_replace_all(Mortgage_Level, c(
        "M_0_299" = "0-299",
        "M_300_449" = "300-449",
        "M_450_599" = "450-599",
        "M_600_799" = "600-799",
        "M_800_999" = "800-999",
        "M_1000_1399" = "1000-1399",
        "M_1400_1799" = "1400-1799",
        "M_1800_2399" = "1800-2399",
        "M_2400_2999" = "2400-2999",
        "M_3000_3999" = "3000-3999",
        "M_4000_over" = "4000+"
      )))
    
    # Set the levels of Mortgage_Level to ensure correct ordering
    complete_data$Mortgage_Level <- factor(complete_data$Mortgage_Level, levels = c(
      "0-299", "300-449", "450-599", "600-799", "800-999", "1000-1399",
      "1400-1799", "1800-2399", "2400-2999", "3000-3999", "4000+"
    ))
    
    # Remove rows with NA Mortgage_Level
    complete_data <- complete_data %>%
      filter(!is.na(Mortgage_Level) & !is.na(Count))
    
    
    complete_data
  })
  
  output$mortgage_DS <- renderGirafe({
    data <- getFilteredData()
    
    p <- ggplot(data, aes(x = Mortgage_Level, y = Count, tooltip = paste("Count:", Count))) +
      geom_bar_interactive(stat = "identity", fill = "#90ccd4") +  
      theme_minimal() +
      labs(
        #title = "Total Number of Dwellings at Different Monthly Mortgage Levels",
        x = "Monthly Mortgage Level ($)",
        y = "Number of Dwellings"
      ) +
      theme(
        panel.background = element_rect(fill = "white", color = "white"),  
        plot.background = element_rect(fill = "white", color = "white"),   
        axis.line = element_blank(),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  
        text = element_text(family = "Heiti SC", color = "#000000"),  
        plot.title = element_text(size = 14, face = "bold"),      
        axis.title = element_text(size = 12),                      
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
      )
    
    girafe(ggobj = p)
  })
}


##########
# Run App #
##########

shinyApp(ui = ui, server = server, options=list(port=6245))