library(shiny)
library(tidyverse)
library(DT)
library(rnaturalearth)
library(rnaturalearthdata)

# Load csv file
albums <- read.csv("my_favorite_albums.csv", header = TRUE, stringsAsFactors = TRUE)

country_counts <- table(albums$Country)

ui <- fluidPage(
  # App title
  titlePanel("♫ My Favorite Albums ♫"),
  br(),
  
  # Tabs
  tabsetPanel(
    br(),
    
    # 'Table' tab
    tabPanel("List", dataTableOutput("music_table"), br()),
    
    # 'Statistics' tab
    tabPanel("Statistics",
             br(),
             p("Just some visuals and few comments:"),
             br(),
             br(),
             h4("Distribution of albums by year:"),
             br(),
             fluidRow(
               column(width = 12,
                      plotOutput("year_distribution", height=400)
               )
             ),
             br(),
             p("Overall I definetifely enjoyed mostly the music from the late '60 and early '70, as well as the music from the '90."),
             br(),
             br(),
             h4("Distribution of albums by genre:"),
             br(),
             fluidRow(
               column(width = 12,
                      plotOutput("genre_distribution", height=700)
               )
             ),
             br(),
             p("Although it has been difficult to categorize music by genre, particularly for some albums (yes, I know, terms like 'alternative rock' or 'electronica' can be way too naive!), I have notice how my preferences include Jazz, Ambient, Krautrock, Progressive, and Psychedelic Rock."),
             br(),
             br(),
             h4("Distribution of albums by country:"),
             br(),
             fluidRow(
               column(width = 12,
                      plotOutput("map", height=400)
               )
             ),
             p("United States got most albums from my list. UK and Germany to follow. Well, I got influenced mainly by western music what can I say : )"),
             br(),
    ),
    
    # 'About' tab
    tabPanel("About",
             h4("Hello and welcome!"),
             br(),
             p("This is my first App and I created it to share my love for music and to explore a few possibilities of Shiny and R."),
             br(),
             p("The App allows you to browse through a list of my favorite albums and explore some simple data visualizations about them."),
             br(),
             p("I had this idea to create a list of my favorite albums for a long time, and finally, I decided to build something. However, the order is not very accurate! While the first part certainly reflects those albums that were more meaningful to me than the second, I found it nearly impossible to decide which album I like the most on too many occasions. Hence, take the ranking with a grain of salt!"),
             br(),
             p("Originally, I wanted to make a list of 100 albums, but the list has grown quickly in a few days, and the more I think, the more albums I list...so yes, I surely forgot something. Eventually, I came up with this version, but it is not my final list at all, though it reflects a lot of my tastes."),
             br(),
             p("Enjoy")
    )
  )
)

server <- function(input, output) {
  
  # Add path to the www folder
  addResourcePath(prefix = "images", directoryPath = "./www")
  
  # Create the table with Spotify and Youtube logos
  output$music_table <- renderDataTable({
    
    br()
    albums %>% 
      select(-c(X)) %>% 
      mutate(Link = paste0("<a href='", Link, "'>", ifelse(grepl("spotify", Link, ignore.case = TRUE),
                                                           "<img src='images/spotify-logo.png' width='25px'/>",
                                                           ifelse(grepl("youtu", Link, ignore.case = TRUE),
                                                                  "<img src='images/youtube-logo.png' width='25px'/>",
                                                                  "")), "</a>"),
             Link = as.character(Link)) %>% 
      datatable(escape = FALSE, 
                options = list(pageLength = 25, 
                               columnDefs = list(list(className = "dt-center", targets = 4)),
                               dom = 'lBfrtip'),
                rownames = FALSE,
                class = 'cell-border stripe')
  })
  
  # Create the year distribution plot
  output$year_distribution <- renderPlot({
    
    p <- ggplot(albums, aes(x = Year)) +
      geom_histogram(binwidth = 1, color = "black", fill = "steelblue") +
      xlab("Year") + ylab("Number of albums") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
            panel.background = element_rect(fill = "white")) +
      scale_x_continuous(breaks = seq(min(albums$Year), max(albums$Year), by = 1)) +
      scale_y_continuous(breaks = seq(0, max(table(albums$Year)), by = 1))
    
    if (!is.null(input$year_distribution_hover)) {
      year <- input$year_distribution_hover
      album_details <- albums %>% filter(Year == year)
      p <- p + geom_text(data = album_details, aes(x = Year, y = 5, label = Album), vjust = -1, hjust = 0, size = 3)
    }
    
    p
    
  })
  
  # Create the genre distribution plot in descending order
  output$genre_distribution <- renderPlot({
    albums %>%
      separate_rows(Genre, sep = ", ") %>%
      group_by(Genre) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = count, y = reorder(Genre, count))) +
      geom_col(fill = "#0072B2") +
      ylab("Genre") + xlab("Number of albums") +
      theme(axis.text.y = element_text(size=12),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
            panel.background = element_rect(fill = "white")) +
      scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 1), expand = c(0, 0))
  }, height = 700)
  
  # Create the map plot
  output$map <- renderPlot({
    
    # Create a data frame with the country counts
    df <- data.frame(
      country = names(country_counts),
      count = as.numeric(country_counts),
      stringsAsFactors = FALSE
    )
    
    # Apply log transformation on count variable
    df$count <- log10(df$count)
    
    # Get the map data using the rnaturalearth package and remove Antarctica continent
    map_data <- ne_countries(scale = "medium", returnclass = "sf", continent = c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")) %>%
      sf::st_set_crs("+proj=longlat +datum=WGS84")
    
    # Join the country data with the map data
    map_data <- merge(map_data, df, by.x = "name", by.y = "country", all.x = TRUE)
    
    # Create the map plot
    ggplot(map_data) +
      geom_sf(aes(fill = count)) +
      scale_fill_gradientn(colors = c("lightblue", "#0072B2")) +
      theme_void() +
      theme(legend.position = "none")
  }, height = 400)
  
}

# Run the app
shinyApp(ui = ui, server = server)
