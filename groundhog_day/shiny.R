library(leaflet)
library(shiny)
library(sf)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(tidyr)

# load datasts
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
predictions <- read.csv("predictions.csv", stringsAsFactors = FALSE)
groundhogs <- read.csv("groundhogs.csv", stringsAsFactors = FALSE)


# Data wrangling
#################################
# get maximum and minimum year
min_year <- min(predictions$year)
max_year <- max(predictions$year)

# get rid of missing values
predictions <- predictions[!is.na(predictions$shadow), ]
# convert TRUE and FALSE to Winter and Spring
long_pred <- predictions %>%
  group_by(year) %>%
  summarise(
    Winter = sum(shadow == TRUE, na.rm = TRUE), # ignore na
    Spring = sum(shadow == FALSE, na.rm = TRUE)) %>%
  gather(key = "Category", value = "Count", Winter, Spring)

# set the count of the years without preedictions to be 0
for (i in min_year: max_year) {
  if (!i %in% long_pred$year) {
    add <- data.frame(
      year = c(i, i),
      Category = c('Spring', 'Winter'),
      Count = c(0, 0)
    )
    long_pred <- rbind(long_pred, add)
  }
}

# add columns for stacked bar charts
long_pred <- long_pred %>%
  group_by(year) %>%
  mutate(Category2 = ifelse(Category == "Winter", "Spring", "Winter"),
         Count2 = ifelse(Category2 == "Winter", 
                         long_pred[(long_pred$year == year) & (long_pred$Category == 'Winter'), ]['Count'], 
                         long_pred[(long_pred$year == year) & (long_pred$Category == 'Spring'),]['Count']) 
  ) %>%
  ungroup()

# add the final prediction of each year
long_pred$prediction <- ifelse(long_pred$Count > long_pred$Count2, long_pred$Category, long_pred$Category2)
# no records for that year
long_pred[long_pred$Count == 0 & long_pred$Count2 == 0, ]$prediction <- 'No prediction'
# the final prediction is tie if the number of spring and winter predictions are the same in that year
long_pred[long_pred$Count == long_pred$Count2, ]$prediction <- 'Tie'

# groundhog_map popup
makeGhPopup <- function(row) {
  paste0('<img src=', row$image, ' alt="groundhog" width="100">', br(), br(),
          strong(row$name), br(), 
         'City: ', row$city, br(),
         'Region: ', row$region, br(),  
         'Country: ', row$country, br(),         
         'Active: ', row$active
  )
}
groundhogs$popup <- by(groundhogs, seq_len(nrow(groundhogs)), makeGhPopup)

joined <- merge(predictions, groundhogs)
# add name and color of the icon
joined$predIcon <- 'snowflake-o'                                
joined$predIcon[joined$shadow == 'FALSE'] <- 'pagelines'
joined$predColor <- 'blue'                            
joined$predColor[joined$shadow == 'FALSE'] <- 'pink'

# add columns for agreement and contribution rate that will be calculated later
names <- groundhogs$name
groundhogs$agr_rate <- 0
groundhogs$act_rate <- 0
groundhogs$correct_count <- 0

# calculate the correct prediction count of each groundhog
for (name in names) {
  latest <- joined[joined$name == name, ]
  all_pred <- merge(latest, distinct(long_pred[c('year', 'prediction')]), by='year')
  all_pred$agr_rate <- ifelse((all_pred$shadow == 'FALSE' & all_pred$prediction == 'Spring') | 
                                (all_pred$shadow == 'TRUE' & all_pred$prediction == 'Winter') |
                                (all_pred$prediction == 'Tie'), 
                              'correct', 'wrong')
  correct_pred <- nrow(all_pred[all_pred$agr_rate == 'correct', ])
  latest <- latest[which.max(latest$year), ]
  
  # agreement rate is calculated by all correct predictions / all predictions made by that groundhog
  groundhogs[groundhogs$name == name, ]$agr_rate <- round(correct_pred / latest$predictions_count, 4)*100
  # active rate is calculated by active years of that groundhog / all years  
  groundhogs[groundhogs$name == name, ]$act_rate <- round(latest$predictions_count / (max_year-min_year), 4)*100
  groundhogs[groundhogs$name == name, ]$correct_count <- correct_pred
}

# calculate the average agreement rate and contribution rate for all groundhogs in every region
regions <- groundhogs %>%
  group_by(region) %>%
  summarise(
    # region agreement rate equals to correct predictions / all predictions of that region, note that each region has different number of predictions 
    agr_rate = round(sum(correct_count) / sum(predictions_count), 4)*100,  
    # region contribution rate equals to prediction count in each region / total prediction count
    con_rate = round(sum(predictions_count) / sum(groundhogs$predictions_count), 4)*100, 
    country = first(country)
  ) 
# reshape data for plotting
long_reg <- gather(regions, key='rate', value='value', -c(region, country))


# Shiny tabs
#################################
pred_tab <- tabPanel(
  title='Predictions', # tab name in navbar
  h2('Earlier Spring or Longer Winter?'),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId='year',
        label='year',
        choices=c('After 2001'=2001,
                  '1981 - 2000'=1981,
                  '1961 - 1980'=1961,
                  '1941 - 1960'=1941,
                  '1921 - 1940'=1921,
                  '1901 - 1920'=1901,
                  'Earlier than 1900'=1887),
        selected='After 2001'    
      ),
      br(),
      h5('About the Groundhog Day'),
      p('A lighthearted holiday celebrated annually across North America in which ‘prognosticating’ animals predict the onset of spring.'),
      p('Traditionally, on February 2nd, various groundhogs across North America are consulted by local public figures (often a mayor) for an annual weather prediction:'),
      p('If the groundhog ‘sees its shadow’, it means six more weeks of winter.'),
      p('If the groundhog doesn’t see its shadow, spring will come early.'),
      br(),br(),
      p('Data from: tidytuesday'),
      actionButton("open_link", "Click me to view the data")
    ),
    mainPanel(
      girafeOutput('plot_pred', height=350), # create a named location to insert a ggplot plot
      leafletOutput('map_groundhogs_year', height=500)
    )
  )
)

groundhog_tab <- tabPanel(
  title='Groundhog Info',
  h2('Where Are The Groundhogs?'),
  sidebarLayout(
    sidebarPanel(
      htmlOutput('groundhog_num'),
      br(),
      radioButtons(
        'active', label = 'Status',
        choices = c('All', 'Active', 'Inactive'),
        selected = 'All'
      ),
      radioButtons(
        'country1', label = 'Country',
        choices = c('All', 'USA', 'Canada'),
        selected = 'All'
      ),
      br(),
      br(),
      htmlOutput('groundhog_stats')  # area to display text
    ),
    mainPanel(
      leafletOutput('map_groundhogs', height=500),
      girafeOutput('agr_line_chart', height=300)
    )
  )
)

leaderboard_tab <- tabPanel(
  title='Region Stats',
  h2('Which Regions\' Groundhogs are More Contributive and Agreeable to the Result?'),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        'sorter', label = 'Sorted by',
        choices = c('Region Name', 'Agreement Rate', 'Contribution Rate'),
        selected = 'Region Name'
      ),
      radioButtons(
        'country2', label = 'Country',
        choices = c('All', 'USA', 'Canada'),
        selected = 'All'
      ),
    ),
    mainPanel(
      girafeOutput('plot_leaderboard', height=450),
      girafeOutput('reg_line_chart', height=300)
    )
  )
)

# User interface
#################################
ui <- navbarPage(
  id='mypage', 
  title='Groundhog Day!', # page title
  pred_tab, 
  groundhog_tab,
  leaderboard_tab,
  
  tags$style(HTML("
    .map-title {
      position: absolute;
      top: 1px;
      right: 1px;
      background-color: rgba(255, 255, 255, 0.8); /* Optional: Add background */
      padding: 10px;
      border-radius: 5px;
    }
  ")),
)

# Shiny server
#################################
server <- function(input, output, session) {
  # clickable button that goes to the data source webpage
  observeEvent(input$open_link, {
    utils::browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-01-30/readme.md")
  })
  
  # stacked bar chart of each year's prediction count
  output$plot_pred <- renderGirafe({
    # get the range of years selected by the user
    min_year <- as.numeric(input$year)
    if (min_year == 1887) max_year <- 1920
    else if (min_year == 2001) max_year <- 2023
    else max_year <- min_year + 19
    
    p<- ggplot(long_pred[(long_pred$year <= max_year) & (long_pred$year >= min_year), ], 
               aes(x = factor(year), 
                   y=Count, 
                   fill=Category,
                   tooltip=sprintf("Prediction: %s\n%s: %d\n%s: %d", 
                                   prediction,
                                   'Spring', ifelse(Category=='Spring', as.numeric(Count), as.numeric(Count2)),
                                   'Winter', ifelse(Category=='Winter', as.numeric(Count), as.numeric(Count2)) ),
                   data_id=year)) +
        geom_bar_interactive(stat = "identity", position='stack') +
        labs(x = "Year", y = "Count", title=paste0('Predictions from ', min_year, ' to ', max_year)) +
        scale_fill_manual(values=c('#ff8ee8', '#37a7d9')) +
        scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1))))) +
        theme(legend.position = "top",
              panel.background=element_blank(),
              panel.grid.major.y=element_line(color='#e2e2e2', linetype='dotted'),
              axis.ticks = element_blank(),
              axis.text.x = element_text(size=10, angle = 60, hjust = 1, margin = margin(t = 0, r = 0, b = 10, l = 0)),  
              axis.text.y = element_text(size=10, margin = margin(t = 0, r = 0, b = 0, l = 10)),
              plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
              axis.title.x = element_text(size = 12, face = "bold"),      
              axis.title.y = element_text(size = 12, face = "bold"))
    girafe(ggobj=p, height_svg=4, width_svg = 8)
  })
  
  rv <- reactiveVal(NULL) # a variable to store the year of the bar clicked by the user
  observeEvent(input$plot_pred_selected, {
    updateSelectInput(session, 'year', selected=input$plot_pred_selected) 
    session$sendCustomMessage(type='plot_pred_set', message=character(0))
    updateNavbarPage(session, 'mypage', selected='Pred')
    rv(input$plot_pred_selected)
  })
  
  output$groundhog_num <- renderText({
    num <- nrow(getFilteredGhData())
    paste("<b>", num, " groundhogs found.</b>")
  })
  
  output$map_groundhogs_year <- renderLeaflet({
    year <- rv()
    # no year selected, stop
    if(is.null(year)) {
      return()
    }
    # filter the data of the selected year
    filtered <- joined[joined$year == year, ]
    
    # map of location of the groundhogs and their predictions
    leaflet(filtered) %>%
      addProviderTiles(providers$CartoDB) %>%
      addAwesomeMarkers(lng=~longitude,
                        lat=~latitude, 
                        icon=~awesomeIcons(library='fa', 
                                           icon=predIcon,
                                           markerColor=predColor,
                                           iconColor='#ffffff'), 
                        label=~name,
                        popup=~popup) %>%
      addControl(
        html = paste0("<div class='map-title'><h4>Predictions of ", year, "</h4></div>"),
        position = "topright",
        className = "map-title"
      )
  })
  
  # reactive data filter for groundhog map
  getFilteredGhData <- reactive({
    filter(groundhogs, 
           if (input$active == 'All') TRUE 
            else if (input$active == 'Active') active == 'TRUE' 
            else active == 'FALSE',
           if (input$country1 == 'All') TRUE else country == input$country1)
    
  })
  
  # map of location of the groundhogs
  output$map_groundhogs <- renderLeaflet({
    
    leaflet(getFilteredGhData()) %>% # get data from the filter
      addProviderTiles(providers$CartoDB.Voyager) %>%
      addAwesomeMarkers(lng=~longitude,
                        lat=~latitude, 
                        icon=~awesomeIcons(library='fa', 
                                           icon='star',
                                           markerColor='red',
                                           iconColor='#ffffff'), 
                        label=~name,
                        popup=~popup,
                        layerId=~name) 
  })
  
  # statistics of the groundhog selected by the user
  observeEvent(input$map_groundhogs_marker_click, {
    name <- input$map_groundhogs_marker_click$id
    # all prediction made by that groundhog
    filtered_data <- groundhogs[groundhogs$name == name, ]
    
    # update the text output based on the clicked marker
    output$groundhog_stats <- renderText({
      paste("<b>", name, "</b><br>",
            "Total Prediction Count: <b>", filtered_data$predictions_count, "</b><br>",
            "Average Agreement Rate: <b>", filtered_data$agr_rate, "%</b><br>",
            "Active Rate: <b>", filtered_data$act_rate,"%</b><br><br>",
            "(Active Rate implies the number of predictions made by this groundhog across all years)")
    })
  })
  
  output$agr_line_chart <- renderGirafe({
    name <- input$map_groundhogs_marker_click$id
    # If no name selected, stop
    if(is.null(name)) return()
    
    agr_line_data <- joined %>%
      merge(unique(long_pred[, c('year', 'prediction')]), by='year', all.x=TRUE) %>%
      filter(name == !!name) %>%
      select(shadow, year, prediction) %>%
      mutate(correct = ifelse((shadow == TRUE & prediction == "Winter") |
                                (shadow == FALSE & prediction == "Spring"), 1, 0),
             acc_agr = round(cumsum(correct) / row_number(), 4) * 100)
    
    year_range <- unique(agr_line_data$year)
    if(length(year_range) <= 20) {
      year_labels <- year_range  
    } else {
      year_labels <- year_range[seq(1, length(year_range), by = 5)]  
    } 
    
    p <- ggplot(agr_line_data) +
      aes(x = factor(year, levels = year_range), y=acc_agr, group=name) +
      geom_line_interactive() +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + 
      scale_x_discrete(breaks = year_labels) +
      labs(x = "Year", y = "Rate (%)", title = paste0("Accumulative Agreement Rate of ", name)) +
      theme(panel.background=element_blank(),
            panel.grid.major.y=element_line(color='#e2e2e2'),
            axis.ticks=element_blank(),
            axis.text.x = element_text(size=12, angle = 45, hjust = 1, margin = margin(t = 0, r = 0, b = 10, l = 0)),
            axis.text.y = element_text(size=12, margin = margin(t = 0, r = 0, b = 0, l = 10)),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12)) 

    girafe(ggobj=p, height_svg=4, width_svg = 8)
    
  })

  getFilteredRegData <- reactive({
    filter(long_reg, 
           if (input$country2 == 'All') TRUE else country == input$country2)
  })
  
  # scatter plot of average agreement rate and contribution year across all years
  output$plot_leaderboard <- renderGirafe({
    if (input$sorter == "Region Name") {
      # sort by region name alphabetically 
      sorter <- rev(levels(factor(regions$region)))
    } else if (input$sorter == 'Agreement Rate') {  
      # sort by agreement rate from high to low
      sorter <- regions$region[order(regions$agr_rate, decreasing = FALSE)]
    } else if (input$sorter == 'Contribution Rate') {  
      # sort by con_rate from high to low
      sorter <- regions$region[order(regions$con_rate, decreasing = FALSE)]
    }
    
    p <- ggplot(getFilteredRegData(), aes(x = value, y= factor(region, levels = sorter), color=rate, data_id=region)) +
      geom_point_interactive(stat = "identity", 
                             aes(tooltip = paste0(region, ifelse(rate=="agr_rate", "<br>Agreement Rate: ", "<br>Contribution Rate: "), value, '%')), 
                             size = 4) +
      theme_minimal() +
      labs(y = "Regions",
           x = "Rate (%)") +
      scale_color_manual(labels = c('Agreement Rate', 'Contribution Rate'), values = c("#ef8a62", "#67a9cf")) +
      theme(legend.position = "top",
            legend.title=element_blank(),
            legend.text=element_text(size=12),
            panel.background=element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size=12, angle=0, hjust = 1, margin = margin(t = 0, r = 0, b = 10, l = 0)),
            axis.text.y = element_text(size=12, margin = margin(t = 0, r = 0, b = 0, l = 10)),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12))
    
    girafe(ggobj=p, height_svg = 6, width_svg = 8)
  })
  
  rv2 <- reactiveValues(value = NULL)
  observeEvent(input$plot_leaderboard_selected, {
    updateSelectInput(session, 'region', selected = input$plot_leaderboard_selected) 
    session$sendCustomMessage(type = 'plot_leaderboard_set', message = character(0))
    updateNavbarPage(session, 'mypage', selected = 'Leaderboard')
    rv2$value <- input$plot_leaderboard_selected
  })
  
  output$reg_line_chart <- renderGirafe({
    region <- rv2$value
    # Check if the region is NULL
    if (is.null(region)) {
      return(NULL)  # Return NULL if no region is selected
    }
    
    groundhogs_reg <- groundhogs[groundhogs$region == region, ]
    groundhogs_reg_long <- gather(groundhogs_reg, key = "type", value = "count", correct_count, predictions_count)
    # create a unique id so that only one bar will be highlighted when hovering 
    groundhogs_reg_long$data_id <- paste(groundhogs_reg_long$shortname, groundhogs_reg_long$type, sep = "_")

    
    # Grouped bar chart of total prediction count and correct count
    p <- ggplot(groundhogs_reg_long, 
                aes(x = shortname, 
                    y = count, 
                    fill = type, 
                    tooltip = count,
                    data_id = data_id)) +
      geom_bar_interactive(stat = "identity", 
                           position = position_dodge(width = 0.6), 
                           width = 0.6, 
                           hover_css = "fill: orange;") +  # Change color on hover
      labs(title = paste("Groundhogs in ", region), 
           x = "Short Name", 
           y = "Count") +
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1))))) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 7)) +
      scale_fill_manual(labels = c('Correct Prediction Count', 'Prediction Count'), 
                        values = c("#af8dc3", "#5ab4ac")) +
      theme(legend.position = "top",
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            panel.background = element_blank(),
            panel.grid.major.y = element_line(color = '#e2e2e2', linetype = 'dashed'),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 12, margin = margin(t = 0, r = 0, b = 10, l = 0)),  
            axis.text.y = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 10)),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
            axis.title.x = element_text(size = 12), 
            axis.title.y = element_text(size = 12)
      )  
    
    girafe(ggobj = p, height_svg = 4, width_svg = 10)
  })
  
  
  
}

# Run shiny
shinyApp(ui, server)