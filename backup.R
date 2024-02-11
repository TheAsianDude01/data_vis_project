# Going to be a tabset panel with one panel exploring a specific video game someone wants
# the first panel is goign to a be a general exploration of all video games, however
# Going to explore the specific sales range, the specific publishers, etc.


# Loading in libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(DT)
library(knitr)
library(kableExtra)

# Loading in data
vgsales <- read.csv('data/vgsales.csv')

# Getting list of all unique video game titles
vg_names <- unique(vgsales$Name)
# Defining function to filter based on platform
to_be_platform <- function(df, group) {
  if (group != 'All Platforms')
    filter(df, Platform == group)
  else
    df
}
# Defining function to filter based on year
to_be_year <- function(df, group) {
  if (group == 'All Years')
    df
  else
    filter(df, Year == group)
}
# Defining function to filter based on publisher
to_be_publisher <- function(df, group) {
  if (group == 'All Publishers')
    df
  else
    filter(df, Publisher == group)
}
# Defining function to filter based on genre
to_be_genre <- function(df, group) {
  if (group == 'All Genres')
    df
  else
    filter(df, Genre == group)
}


# Defining function to choose list of platforms based on input
platforms <- function(name) {
  platform = filter(vgsales, Name == name)
  unique(platform$Platform) %>% 
    append('All Platforms', 0)
}
# Defining function to choose a year based on input
years <- function(name) {
  year = filter(vgsales, Name == name)
  unique(year$Year) %>%
    append('All Years', 0)
}
# Defining function to choose Publisher based on input
publishers <- function(name) {
  publisher = filter(vgsales, Name == name)
  unique(publisher$Publisher) %>% 
    append('All Publishers', 0)
}
# Defining function to choose genre based on input
genres <- function(name) {
  genre = filter(vgsales, Name == name)
  unique(genre$Genre) %>% 
    append('All Genres', 0)
}



# Workspace starts here
something <- vgsales %>% 
  to_be(group = 'All Platforms') %>% 
  mutate(Name = case_when(
    Name == 'Wii Sports' ~ 'Wii Sports',
    Name != 'Wii Sports' ~ 'Average'
  )) %>% 
  group_by(Name) %>% 
  summarise(NA_Sales = mean(NA_Sales),
            EU_Sales = mean(EU_Sales),
            JP_Sales = mean(JP_Sales),
            Global_Sales = mean(Global_Sales),
            Other_Sales = mean(Other_Sales)) %>% 
  slice(match(c('Wii Sports', 'Average'), Name))

something <- something %>% 
  pivot_longer(cols = c('NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales', 'Global_Sales'),
               names_to = c('Type'),
               values_to = c('Sales'))

ggplot(something, aes(Type, Sales, fill = Name)) +
  geom_col()

something <- vgsales %>% 
  to_be(group = 'GC')

# Workspace ends here


# Define UI ----
ui <- fluidPage(
  titlePanel('Video Game Sales'),
  sidebarLayout(
    sidebarPanel(
      
      # Input for the video game
      selectizeInput(
        'title',
        'Choose Your "Character"!',
        choices = NULL
      ),
      
      # Input for specific platform
      radioButtons('platform',
                   'Choose your platform!',
                   choices = "All Platforms"),
      
      # Input for specific year
      radioButtons('year',
                   'Choose your year!',
                   choices = 'All Years'),
      
      # Input for specific publisher
      radioButtons('publisher',
                   'Choose your publisher!',
                   choices = 'All Publishers'),
      
      # Input for specific genre
      radioButtons('genre',
                   'Choose your genre!',
                   choices = 'All Genres')
      
    ),
    mainPanel(
      tableOutput('summary'),
      plotOutput('bar')
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  # Adding in select input server side (to make faster)
  updateSelectizeInput(session, 'title', choices = vg_names, server = TRUE, selected = 'Super Mario Galaxy')
  
  # Creating reactive Platforms list based off of the input title 
  observeEvent(input$title, {
    updateRadioButtons(session, 'platform',
                       choices = platforms(input$title),
                       selected = 'All Platforms')
  })
  
  # Creating reactive Years list based off of the input title
  observeEvent(input$title, {
    updateRadioButtons(session, 'year',
                       choices = years(input$title),
                       selected = 'All Years')
  })
  
  # Creating reactive Publishers list based off of the input title
  observeEvent(input$title, {
    updateRadioButtons(session, 'publisher',
                       choices = publishers(input$title),
                       selected = 'All Publishers')
  })
  
  # Creating reactive Genres list based off of the input title
  observeEvent(input$title, {
    updateRadioButtons(session, 'genre',
                       choices = genres(input$title),
                       selected = 'All Genres')
  })
  
  vgdata <- reactive({vgsales %>% 
      to_be_platform(group = input$platform) %>% 
      to_be_year(group = input$year) %>% 
      to_be_publisher(group = input$publisher) %>% 
      to_be_genre(group = input$genre) %>% 
      mutate(Video_Game = case_when(
        Name == input$title ~ input$title,
        Name != input$title ~ 'Average'
      )) %>% 
      group_by(Video_Game) %>% 
      summarise(Global_Sales = round(mean(Global_Sales), 2),
                NA_Sales = round(mean(NA_Sales), 2),
                EU_Sales = round(mean(EU_Sales), 2),
                JP_Sales = round(mean(JP_Sales), 2),
                Other = round(mean(Other_Sales), 2)) %>% 
      slice(match(c(input$title, 'Average'), Video_Game))})
  
  
  # Adding in output for table
  output$summary <- function(){
    vgdata() %>% 
      kable('html',
            caption = 'Sales are in millions and in Global Currencies') %>% 
      kable_styling('striped', full_width = TRUE)
  }
  
  
  # Adding in output for plot
  output$bar <- renderPlot({
    
    # Grouping data by the game
    vgsales <- vgsales %>% 
      to_be_platform(group = input$platform) %>% 
      to_be_year(group = input$year) %>% 
      to_be_publisher(group = input$publisher) %>% 
      to_be_genre(group = input$genre) %>% 
      mutate(Name = case_when(
        Name == input$title ~ input$title,
        Name != input$title ~ 'Average'
      )) %>% 
      group_by(Name) %>% 
      summarise(NA_Sales = mean(NA_Sales),
                EU_Sales = mean(EU_Sales),
                JP_Sales = mean(JP_Sales),
                Global_Sales = mean(Global_Sales),
                Other_Sales = mean(Other_Sales)) %>% 
      # Turning data into something more able to be plotted
      pivot_longer(cols = c('NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales', 'Global_Sales'),
                   names_to = c('Type'),
                   values_to = c('Sales'))
    
    # Creating plot
    ggplot(vgsales, aes(Type, Sales, fill = Name)) +
      geom_col(position = 'dodge') +
      labs(y = 'Total Sales (in Millions)',
           x = 'Region',
           title = paste('Video Game Sales of', input$title, 'v.s. Other Video Games')) +
      scale_x_discrete(limits = c('Global_Sales', 'NA_Sales', 'JP_Sales', 'EU_Sales', 'Other_Sales'),
                       labels = c('Global', 'North America', 'Japan', 'Europe', 'Other')) +
      theme_minimal()
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

