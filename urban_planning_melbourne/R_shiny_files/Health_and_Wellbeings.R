library(shiny)
library(sf)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(tidyr)
library(ggrepel)
library(bslib)

# Load data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- read.csv("../Data/Health and Wellbeing/age_gender.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
data1 <- read.csv("../Data/Health and Wellbeing/employeement_status.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

births_tab <- tabPanel(
  sidebarLayout(
    sidebarPanel(
      style = "font-family: 'heiti SC', sans-serif; padding: 5px;",  # Set desired height

      selectInput(
        inputId = "view_by",
        label = "View by",
        choices = c("Age", "Gender", "Employment Status"),
        selected = "Age",
        width = "100%"  # Ensure dropdown fills the sidebar width
      )
    ),
    mainPanel(
       girafeOutput('plot_births') # Location for the plot
    )
  )
)

# USER INTERFACE
ui <- navbarPage(
  title = '',
  # Set theme to match Tableau dashboard
  theme = bs_theme(
    bg = "#ffffff",  
    fg = "#000000",
  ),
  tags$head(
    tags$style(HTML("
      .navbar {
        margin-bottom: 0px; 
      }
    "))
  ),
  births_tab # Tab panel
)

# SHINY SERVER 
server <- function(input, output, session) {
  
  # Parse query string to get data from Tableau
  getQueryStringData <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  
  # Reactive data based on selection
  selected_data <- reactive({
    # If no disease specified, return an empty data frame
    if (!("disease" %in% names(getQueryStringData()))) {
      return(data.frame())
    }
    
    # Get the chosen disease from the query string
    disease <- getQueryStringData()$disease
    
    # Filter data based on user selection (Age, Gender, Employment Status)
    if (input$view_by == "Gender") {
      result <- data %>%
        filter(Disease.Type == disease) %>%
        group_by(Gender) %>%
        summarize(Total = sum(Count)) %>%
        mutate(Percentage = Total / sum(Total) * 100)
    } else if (input$view_by == "Age") {
      result <- data %>%
        filter(Disease.Type == disease) %>%
        group_by(Age) %>%
        summarize(Total = sum(Count)) %>%
        mutate(Percentage = Total / sum(Total) * 100)
    } else {
      result <- data1 %>%
        filter(Disease.Type == disease) %>%
        group_by(`Employment.Status`) %>%
        summarize(Total = sum(Count)) %>%
        mutate(Percentage = Total / sum(Total) * 100)
    }
    result
  })
  
  # Render the plot
  output$plot_births <- renderGirafe({
    plot_data <- selected_data()
    
    # Check if plot_data is empty
    if (nrow(plot_data) == 0) {
      return(NULL) # If no data, render nothing
    }
    
    # Define the label column for the tooltip and fill aesthetic
    label_column <- if (input$view_by == "Employment Status") "Employment.Status" else input$view_by
    
    wrap_text <- function(text, width) {
      stringr::str_wrap(text, width = width)
    }
    
    if (any(nchar(unique(plot_data[[label_column]])) > 15)) {
      plot_data[[label_column]] <- wrap_text(plot_data[[label_column]], 15)
    }
    
    # Ensure the tooltip column includes the percentage and class name
    plot_data$tooltip <- paste(
      label_column, ":", plot_data[[label_column]], 
      "<br>Total:", plot_data$Total
    )
    
    # Define a consistent color palette for all views
    color_palette <- c("#fecc5c","#41b6c4", "#a1dab4", "#d7b5d8", "#2b8cbe") 
    
    # Create a pie chart with annotations
    p <- ggplot(plot_data, aes(x = "", y = Total, fill = .data[[label_column]])) +
      geom_bar_interactive(stat = "identity", width = 1, aes(tooltip = tooltip)) +
      coord_polar("y") +
      geom_label_repel(aes(label = paste0(round(Percentage, 1), "%")),
                       position = position_stack(vjust = 0.2),
                       show.legend = FALSE,
                       size = 7,
                       color = "black",             # Background color
                       family = "Heiti SC") +
      theme_void() +
      theme(
        legend.position = "top",          # Move legend to the top
        legend.text = element_text(size = 17, family='heiti SC'),   # Increase legend text font size
        legend.key.size = unit(1, "cm"),        # Increase the size of the legend keys
      ) +
      theme(legend.title = element_blank()) +
      scale_fill_manual(values = color_palette)  # Use the consistent color palette
    
    girafe(ggobj = p, height_svg = 5.5, width_svg = 6)
  })
}


# RUN SHINY 
shinyApp(ui, server, options = list(port=6342))
