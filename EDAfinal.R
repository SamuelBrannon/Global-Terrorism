library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(lubridate)

mydata <- read.csv("globalterrorism.csv")

# Use only Africa and Middle East
mydata_filtered <- mydata[mydata$region_txt %in% c("Middle East & North Africa", "Sub-Saharan Africa"), ]

# Find the number of attacks per country
country_counts <- mydata_filtered %>%
  group_by(country_txt) %>%
  summarise(attacks = n()) %>%
  ungroup()

# Make map of Africa
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Load country counts onto the map
africa_data <- africa %>%
  left_join(country_counts, by = c("name" = "country_txt"))

# Make color scale
pal <- colorNumeric(palette = c("green", "yellow", "red"), domain = africa_data$attacks, na.color = "grey")

# Get the top ten target types without unknown
top_target_types <- mydata_filtered %>%
  filter(targtype1_txt != "Unknown") %>%
  count(targtype1_txt) %>%
  arrange(desc(n)) %>%
  head(10)

# Filter for years 2013 to 2017
mydata_filtered_dates <- mydata %>%
  filter(iyear >= 2013 & iyear <= 2017) %>%
  mutate(Date = make_date(iyear, imonth, iday))

# Find the monthly attacks
monthly_attacks <- mydata_filtered_dates %>%
  group_by(Year = iyear, Month = imonth) %>%
  summarise(Attacks = n()) %>%
  ungroup() %>%
  mutate(Date = make_date(Year, Month, 1))


# Filter the data by decade
mydata_filtered_decades <- mydata %>%
  filter(iyear >= 1970) %>%
  mutate(Decade = case_when(
    iyear >= 1970 & iyear <= 1979 ~ "1970s",
    iyear >= 1980 & iyear <= 1989 ~ "1980s",
    iyear >= 1990 & iyear <= 1999 ~ "1990s",
    iyear >= 2000 & iyear <= 2009 ~ "2000s",
    iyear >= 2010 & iyear <= 2017 ~ "2010-17"
  )) %>%
  group_by(Decade, Month = imonth) %>%
  summarise(Avg_Attacks = n() / length(unique(iyear))) %>%
  ungroup()

topfive <- mydata_filtered %>%
  filter(country_txt == "Somalia" | country_txt == "Nigeria" | country_txt == "Algeria" | country_txt == "Egypt" | country_txt == "Libya")

# Define the UI
ui <- fluidPage(
  titlePanel("Terrorist Attacks in Africa"),
  
  # Create tabs
  tabsetPanel(
    
    # Tab 1: Attack Locations Map
    tabPanel("Attack Locations",
             sidebarLayout(
               sidebarPanel(
                 helpText("This map shows the locations of terrorist attacks in Middle East & North Africa and Sub-Saharan Africa.")
               ),
               mainPanel(
                 leafletOutput("attack_map")
               )
             )
    ),
    
    # Tab 2: Number of Attacks by Country Map
    tabPanel("Attacks by Country",
             sidebarLayout(
               sidebarPanel(
                 helpText("This map shows the number of terrorist attacks per country in Africa, colored by number of attacks.")
               ),
               mainPanel(
                 leafletOutput("country_map")
               )
             )
    ),
    
    # Tab 3: Time Series of Attacks (2013-2017) or Average Monthly Attacks per Decade
    tabPanel("Time Series",
             sidebarLayout(
               sidebarPanel(
                 helpText("This time series plot shows the number of terrorist attacks per month from 2013 to 2017 or the average monthly attacks for each decade."),
                 selectInput("time_series_choice", "Choose Time Series:",
                             choices = c("Monthly Terrorist Attacks (2013-2017)", "Average Monthly Attacks by Decade"))
               ),
               mainPanel(
                 plotOutput("time_series_plot")
               )
             )
    ),
    
    # Tab 4: Attack Types Histogram
    tabPanel("Histograms",
             sidebarLayout(
               sidebarPanel(
                 helpText("This histogram shows the frequency of different types of terrorist attacks in Middle East & North Africa and Sub-Saharan Africa."),
                 radioButtons("hist_choice", "Choose a histogram:", choices = c("Attack Types", "Target Types"))
               ),
               mainPanel(
                 plotOutput("selected_histogram")
               )
             )
    ),
    
    # Tab 5: Chi-Square Test for Attack Type and Region
    tabPanel("Chi-Square Test",
             mainPanel(
               h3("Chi-Square Test Result"),
               verbatimTextOutput("chi_square_result")
             )
    ),
    
    # Tab 6: Top five countries with the most attacks
    tabPanel("Top Five",
             sidebarLayout(
               sidebarPanel(
                 helpText("This histogram shows the distribution of terrorist attacks across the top five countries: Somalia, Nigeria, Algeria, Egypt, and Libya."),
                 sliderInput("year_range", 
                             "Select Year Range:", 
                             min = 1970, max = 2017, 
                             value = c(1970, 2017),
                             sep = ""),
                 actionButton("reset_slider", "Reset Slider") # Reset to full range
               ),
               mainPanel(
                 plotOutput("top_five_histogram") 
               )
             )
    )
    
    
  )
)

# Define the server
server <- function(input, output, session) {
  
  # Tab 1: Attack Locations Map
  output$attack_map <- renderLeaflet({
    leaflet(data = mydata_filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        radius = 3,
        color = "red",
        stroke = FALSE,
        fillOpacity = 0.4
      ) %>%
      setView(lng = 20, lat = 10, zoom = 3)
  })
  
  # Tab 2: Number of Attacks by Country Map
  output$country_map <- renderLeaflet({
    leaflet(data = africa_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(attacks),
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        popup = ~paste(name, "<br>", "Attacks:", attacks)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = africa_data$attacks,
        title = "Number of Attacks",
        labFormat = labelFormat(suffix = " attacks")
      ) %>%
      setView(lng = 20, lat = 10, zoom = 3)
  })
  
  # Tab 3: Time Series Plot (2013-2017 or Average Monthly Attacks by Decade)
  output$time_series_plot <- renderPlot({
    if(input$time_series_choice == "Monthly Terrorist Attacks (2013-2017)") {
      ggplot(monthly_attacks, aes(x = factor(Month, levels = 1:12), y = Attacks, color = factor(Year), group = Year)) +
        geom_line(size = 1) + 
        geom_point(size = 2) +
        scale_color_manual(values = c("2013" = "blue", "2014" = "green", "2015" = "red", "2016" = "orange", "2017" = "purple")) +  # Assign colors for each year
        theme_minimal() +
        labs(
          title = "Monthly Terrorist Attacks in Africa (2013-2017)",
          x = "Month",
          y = "Number of Attacks",
          color = "Year"
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          legend.position = "bottom"
        )
    } else if(input$time_series_choice == "Average Monthly Attacks by Decade") {
      ggplot(mydata_filtered_decades, aes(x = factor(Month, levels = 1:12), y = Avg_Attacks, color = Decade, group = Decade)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        scale_color_manual(values = c("1970s" = "blue", "1980s" = "green", "1990s" = "red", "2000s" = "orange", "2010-17" = "purple")) +  # Assign colors for each decade
        theme_minimal() +
        labs(
          title = "Average Monthly Terrorist Attacks by Decade",
          x = "Month",
          y = "Average Number of Attacks",
          color = "Decade"
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          legend.position = "bottom"
        )
    }
  })
  
  # Tab 4: Attack Types Histogram
  output$selected_histogram <- renderPlot({
    if(input$hist_choice == "Attack Types") {
      ggplot(mydata_filtered, aes(x = attacktype1_txt)) +
        geom_bar(fill = "lightblue") +
        theme_minimal() +
        labs(title = "Distribution of Attack Types", x = "Attack Type", y = "Frequency") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$hist_choice == "Target Types") {
      ggplot(top_target_types, aes(x = reorder(targtype1_txt, n), y = n)) +
        geom_bar(stat = "identity", fill = "lightgreen") +
        theme_minimal() +
        labs(title = "Top 10 Target Types", x = "Target Type", y = "Frequency") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Tab 5: Chi-Square Test
  output$chi_square_result <- renderText({
    
    pre_message <- "Comparing attack type and country\n"
    
    table_data <- table(mydata_filtered$attacktype1_txt, mydata_filtered$country_txt)
    chisq_result <- chisq.test(table_data)
    
    result_text <- paste("Chi-squared =", round(chisq_result$statistic, 2), 
                         "\nDegrees of freedom =", chisq_result$parameter, 
                         "\np-value =", format.pval(chisq_result$p.value))
    
    mess1 <- "\nA large Chi-squared value indicates that there is a large"
    mess2 <- "discrepancy between the expected and observed data meaning"
    mess3 <- "the variables are likely dependent."
    mess4 <- "A p-value so small means that we reject the null" 
    mess5 <- "hypothesis that there is no relationship, and we conclude" 
    mess6 <- " that the attack type and country dependent."
    
    paste(pre_message, result_text, mess1, mess2, mess3, mess4, mess5, mess6, sep = "\n")
  })
  
  # Tab 6: Top Five Countries
  observeEvent(input$reset_slider, {
    # Reset the slider to its full range when the button is clicked
    updateSliderInput(session, "year_range", value = c(1970, 2017))
  })
  
  output$top_five_histogram <- renderPlot({
    # Filter the data based on the selected year range
    data_to_plot <- topfive %>%
      filter(iyear >= input$year_range[1] & iyear <= input$year_range[2])
    
    ggplot(data_to_plot, aes(x = country_txt, fill = country_txt)) +
      geom_histogram(stat = "count", color = "black", binwidth = 1, show.legend = FALSE) +
      labs(
        title = paste0(
          "Histogram of Terrorist Attacks (", 
          input$year_range[1], " - ", input$year_range[2], ")"
        ),
        x = "Country",
        y = "Number of Attacks"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12)
      )
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
