# Loading in libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(DT)
library(knitr)
library(kableExtra)
library(rlang)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(sf)

# DATA WORK
# Loading in data
vgsales <- read.csv('data/vgsales.csv')
world <- ne_countries(scale = 'medium', returnclass = 'sf')
# Getting list of all unique video game titles
vg_names <- unique(vgsales$Name)
# Creating list of all types of sales in vgsales
regions <- colnames(vgsales)[7:11]
# Creating list and df of ISO3 country names in EU, NA, JP, Other and Global
EUR <- c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST', 'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 'SVN', 'ESP', 'SWE')
vidEUR <- data.frame(adm0_a3 = EUR,
                     Region = rep(1, 27))
NorA <- c('AIA', 'ATG',	'ABW', 'BHS',	'BRB', 'BLZ',	'BMU', 'BES', 'VGB', 'CAN', 'CYM', 'CRI', 'CUB', 'CUW', 'DMA', 'DOM', 'SLV', 'GRL', 'GRD', 'GLP', 'GTM', 'HTI', 'HND', 'JAM', 'MTQ', 'MEX', 'SPM', 'MSR', 'ANT', 'KNA', 'NIC', 'PAN', 'PRI', 'BES', 'BES', 'SXM', 'KNA', 'LCA', 'SPM', 'VCT',	'TTO', 'TCA', 'USA', 'VIR')
vidNorA <- data.frame(adm0_a3 = NorA,
                      Region = rep(1, 44))
JP <- c('JPN')
vidJP <- data.frame(adm0_a3 = JP,
                    Region = c(1))
OTHER <- c('AFG',	'ALA',	'ALB',	'DZA',	'ASM',	'AND',	'AGO',	'AIA',	'ATA',	'ATG',	'ARG',	'ARM',	'ABW',	'AUS',	'AUT',	'AZE',	'BHS',	'BHR',	'BGD',	'BRB',	'BLR',	'BEL',	'BLZ',	'BEN',	'BMU',	'BTN',	'BOL',	'BES',	'BIH',	'BWA',	'BVT',	'BRA',	'IOT',	'BRN',	'BGR',	'BFA',	'BDI',	'KHM',	'CMR',	'CAN',	'CPV',	'CYM',	'CAF',	'TCD',	'CHL',	'CHN',	'CXR',	'CCK',	'COL',	'COM',	'COG',	'COD',	'COK',	'CRI',	'CIV',	'HRV',	'CUB',	'CUW',	'CYP',	'CZE',	'DNK',	'DJI',	'DMA',	'DOM',	'ECU',	'EGY',	'SLV',	'GNQ',	'ERI',	'EST',	'ETH',	'FLK',	'FRO',	'FJI',	'FIN',	'FRA',	'GUF',	'PYF',	'ATF',	'GAB',	'GMB',	'GEO',	'DEU',	'GHA',	'GIB',	'GRC',	'GRL',	'GRD',	'GLP',	'GUM',	'GTM',	'GGY',	'GIN',	'GNB',	'GUY',	'HTI',	'HMD',	'VAT',	'HND',	'HKG',	'HUN',	'ISL',	'IND',	'IDN',	'IRN',	'IRQ',	'IRL',	'IMN',	'ISR',	'ITA',	'JAM',	'JPN',	'JEY',	'JOR',	'KAZ',	'KEN',	'KIR',	'PRK',	'KOR',	'XKX',	'KWT',	'KGZ',	'LAO',	'LVA',	'LBN',	'LSO',	'LBR',	'LBY',	'LIE',	'LTU',	'LUX',	'MAC',	'MKD',	'MDG',	'MWI',	'MYS',	'MDV',	'MLI',	'MLT',	'MHL',	'MTQ',	'MRT',	'MUS',	'MYT',	'MEX',	'FSM',	'MDA',	'MCO',	'MNG',	'MNE',	'MSR',	'MAR',	'MOZ',	'MMR',	'NAM',	'NRU',	'NPL',	'NLD',	'NCL',	'NZL',	'NIC',	'NER',	'NGA',	'NIU',	'NFK',	'MNP',	'NOR',	'OMN',	'PAK',	'PLW',	'PSE',	'PAN',	'PNG',	'PRY',	'PER',	'PHL',	'PCN',	'POL',	'PRT',	'PRI',	'QAT',	'SRB',	'REU',	'ROU',	'RUS',	'RWA',	'BLM',	'SHN',	'KNA',	'LCA',	'MAF',	'SPM',	'VCT',	'WSM',	'SMR',	'STP',	'SAU',	'SEN',	'SYC',	'SLE',	'SGP',	'SXM',	'SVK',	'SVN',	'SLB',	'SOM',	'ZAF',	'SGS',	'SSD',	'ESP',	'LKA',	'SDN',	'SUR',	'SJM',	'SWZ',	'SWE',	'CHE',	'SYR',	'TWN',	'TJK',	'TZA',	'THA',	'TLS',	'TGO',	'TKL',	'TON',	'TTO',	'TUN',	'TUR',	'XTX',	'TKM',	'TCA',	'TUV',	'UGA',	'UKR',	'ARE',	'GBR',	'USA',	'UMI',	'URY',	'UZB',	'VUT',	'VEN',	'VNM',	'VGB',	'VIR',	'WLF',	'ESH',	'YEM',	'ZMB',	'ZWE')
OTHER <- OTHER[!(OTHER %in% c(EUR, NorA, JP))]
vidOTHER <- data.frame(adm0_a3 = OTHER,
                       Region = rep(1, 184))
vidGLOBAL <- data.frame(adm0_a3 = c(OTHER, NorA, JP, EUR),
                        Region = rep(1, 256))
# List of all publishers arranged in desc number of works
all_publishers <- vgsales %>% 
  group_by(Publisher) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  append('All Publishers', 0)

#------------------------------------------------------------------------------------------

# FUNCTIONS LIST
# Creating function to add a colors column to the data
coloring <- function(platform) {
  if (platform %in% list("Wii", 'NES', 'GB', 'DS', 'SNES', 'GBA', '3DS', 'N64', 'GC', 'WiiU'))
    {return('red')}
  if (platform %in% list('X360', 'XB', 'XOne'))
    {return('limegreen')}
  if (platform %in% list('PS3', 'PS2', 'PS4', 'PS', 'PSP', 'PSV'))
    {return('darkblue')}
  if (platform %in% list('2600', 'GEN', 'GG', 'SCD', 'DC'))
    {return('#0D98BA')}
  else
    {return('#71797E')}
}
# Function to get "coloring" based off of a title
get_color <- function(title) {
  row = vgsales %>% 
    filter(Name == title)
  color = unique(row$coloring)
  return(color[[1]])
}
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
# Defining function to filter based on outliers
to_be_outliers <- function(df, outlier) {
  if (outlier == 'Yes')
    filter(df, Global_Sales <= 1.5*(quantile(Global_Sales, .75) - quantile(Global_Sales, .25)) + quantile(Global_Sales, .75))
  else
    df
}
# Defining function to choose list of platforms based on name
platforms <- function(name) {
  platform = filter(vgsales, Name == name)
  unique(platform$Platform) %>% 
    append('All Platforms', 0)
}
# Defining function to choose list of years based on name
years <- function(name) {
  year = filter(vgsales, Name == name)
  unique(year$Year) %>%
    append('All Years', 0)
}
# Defining function to choose list of publishers based on name
publishers <- function(name) {
  publisher = filter(vgsales, Name == name)
  unique(publisher$Publisher) %>% 
    append('All Publishers', 0)
}
# Defining function to choose list of genres based on name
genres <- function(name) {
  genre = filter(vgsales, Name == name)
  unique(genre$Genre) %>% 
    append('All Genres', 0)
}

#----------------------------------------------------------------------------------------------

# DATA TRANSFORMATION
vgsales$coloring <- apply(vgsales[c('Platform')], 1, FUN = coloring)
# Filtering out NA values in vgsales$Year, then turning it into a datetime object for comparison
proper_year <- subset(vgsales, vgsales$Year != 'N/A')
proper_year$Year <- as.POSIXct(proper_year$Year, format = '%Y')
# Creating url for main panel url link
url <- a('data', href = 'https://www.kaggle.com/datasets/gregorut/videogamesales')
# Creating datasets for Tab Panel 1, output$all_hist
Globe = left_join(world, vidGLOBAL, by = 'adm0_a3', multiple = 'all')
North_America = left_join(world, vidNorA, by = 'adm0_a3', multiple = 'all')
Japan = left_join(world, vidJP, by = 'adm0_a3', multiple = 'all')
Other = left_join(world, vidOTHER, by = 'adm0_a3', multiple = 'all')
Europe = left_join(world, vidEUR, by = 'adm0_a3', multiple = 'all')

#---------------------------------------------------------------------------------------------


# Define UI ----
ui <- fluidPage(
  titlePanel('Video Game Sales'),
  sidebarLayout(
    sidebarPanel(
      uiOutput('tab'),
      conditionalPanel(
        condition = 'input.tabselected == 1',
        # Region input
        selectInput("region", label = 'Select a region to explore sales in:', 
                           choices = list("Global" = regions[[5]], 
                                          "North America" = regions[[1]], 
                                          "Europe" = regions[[2]],
                                          'Japan' = regions[[3]],
                                          'Other Countries' = regions[[4]]),
                           selected = 1),
        # Date input
        dateRangeInput("date", label = 'Select a period of years to explore sales in:',
                       format = 'yyyy',
                       startview = 'decade',
                       start = '1980-01-01'),
        # Publisher input
        selectizeInput('all_publisher', label = "Select a publisher's work to explore:",
                       choices = all_publishers,
                       selected = 'Nintendo'),
        # Genre input
        selectInput('all_genre', label = 'Select a genre to explore:',
                    choices = c('All Genres', unique(vgsales$Genre))),
        # Outlier input
        radioButtons('outliers', label = 'Exclude Outliers in Sales?',
                     choices = c('Yes', 'No')),
        # Adding in little blurb
        'Explore how well various games sold across the different platforms, publishers, and genres
        that are available. Switch between the various tabs to explore how all games
        in general have sold, how your favorite game sold compared to the average, and how well two different games 
        compare against each other. Start by selecting one of the different tabs and, most of all, tinker with the 
        different options offered to explore all the classic video games created throughout the years! '
        ),
      
      conditionalPanel(
        condition = 'input.tabselected == 2',
        # Game input
        selectizeInput(
          'title',
          'Choose Your Game!',
          choices = NULL
        ),
        # Platform comparison input
        radioButtons('platform',
                     'Select the Platform to Compare Against:',
                     choices = "All Platforms"),
        # Year comparison input
        radioButtons('year',
                     'Select the Year to Compare Against:',
                     choices = 'All Years'),
        # Publisher comparison input
        radioButtons('publisher',
                     'Select the Publisher to Compare Against:',
                     choices = 'All Publishers'),
        
        # Genre comparison input
        radioButtons('genre',
                     'Select the Genre to Compare Against',
                     choices = 'All Genres')
        ),
      
      conditionalPanel(
        condition = 'input.tabselected == 3',
        # First game input
        selectizeInput(
          'title1',
          'Choose your first player!',
          choices = NULL
        ),
        # Second game input
        selectizeInput(
          'title2',
          'Choose your second player!',
          choices = NULL
        )
      )
      ),
    mainPanel(
      tabsetPanel(
        # First tab for all games
        tabPanel(
          '"Coop Mode" All the games!', 
          value = 1, 
          tableOutput('all_summary'),
          plotOutput('all_hist')
          ),
        
        # Second tab for one game
        tabPanel(
          '"Survival Mode" Choose your game!', value = 2,
          tableOutput('summary'),
          plotOutput('bar')
          ),
        
        # Third tab for game v.s. game
        tabPanel(
          '"PvP Mode" Game v.s. Game!', value = 3,
          tableOutput('summaries'),
          plotOutput('bars')
        ),
        id = 'tabselected')
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  # Updating buttons server side to make loading faster
  updateSelectizeInput(session, 'title', choices = vg_names, server = TRUE, selected = 'Super Mario Galaxy')
  updateSelectizeInput(session, 'title1', choices = vg_names, server = TRUE, selected = 'Super Mario Galaxy')
  updateSelectizeInput(session, 'title2', choices = vg_names, server = TRUE, selected = 'Super Mario Galaxy 2')
  
  # Main Panel Text
  output$tab <- renderUI({
    tagList('The', url, 'used in this application is thanks to Gregory Smith from Kaggle')
  })

  #---------------------------------------------------------------------------------------
  
  # Tab Panel 1 Code
  # Adding in fill switch for barplots based on input$title
  color_fill <- reactive({
    get_color(input$title)
  })
  # Creating proper map data for the input chosen
  countryfill <- reactive(switch(
    input$region,
    'Global_Sales' = Globe,
    'NA_Sales' = North_America,
    'JP_Sales' = Japan,
    'Other_Sales' = Other,
    'EU_Sales' = Europe,
    'Global_Sales'
  ))
  # Creating reactive output for name of region
  regionname <- reactive(switch(
    input$region,
    'Global_Sales' = 'Global',
    'NA_Sales' = 'North America',
    'JP_Sales' = 'Japan',
    'Other_Sales' = 'Other Countries',
    'EU_Sales' = 'Europe'
  ))
  # Function to change data based on the year, publisher, outliers, and genre output chosen chosen
  all_vgdata <- reactive({proper_year %>% 
    filter(
      Year >= input$date[1] & Year <= input$date[2]
    ) %>% 
    to_be_publisher(group = input$all_publisher) %>% 
    to_be_outliers(outlier = input$outliers) %>% 
    to_be_genre(group = input$all_genre)
  })
  # Output for summary table
  output$all_summary <- function(){
    all_vgdata() %>%
      summarise(
        Region = input$region,
        Minimum_Sales = min(!! sym(input$region)),
        Q1_Sales = quantile(!! sym(input$region), .25),
        Median_Sales = median(!! sym(input$region)),
        Mean_Sales = round(mean(!! sym(input$region)), 2),
        Q3_Sales = quantile(!! sym(input$region), .75),
        Max_Sales = max(!! sym(input$region))
        ) %>% 
      kable('html',
            caption = 'Sales are in millions and in Global Currencies') %>% 
      kable_styling('striped', full_width = TRUE)
  }
  # Output for the plot
  output$all_hist <- renderPlot({
    h <- ggplot(all_vgdata(), aes(x = (!! sym(input$region)))) +
      geom_histogram(bins = 35, fill = '#5E5E5E', color = 'white') +
      theme_minimal() +
      labs(y = 'Number of Games',
           x = 'Sales (in millions)',
           title = 'Number of Games v.s. Number of Sales by Region') +
      theme(plot.title = element_text(size = 20))
    m <- ggplot(data = countryfill()) +
      geom_sf(aes(fill = Region),
              show.legend = FALSE) +
      scale_fill_gradient(low = 'black', high = 'grey', na.value = 'white') +
      theme_void() +
      labs(title = regionname()) +
      theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
    h + inset_element(m, left = 0.5, bottom = .5, right = 1, top = 1)
  })
  
  #----------------------------------------------------------------------------------------
  
  # Tab Panel 2 Code
  # Creating reactive Platforms input based off of the input title 
  observeEvent(input$title, {
    updateRadioButtons(session, 'platform',
                       choices = platforms(input$title),
                       selected = 'All Platforms')
  })
  # Creating reactive Years input based off of the input title
  observeEvent(input$title, {
    updateRadioButtons(session, 'year',
                       choices = years(input$title),
                       selected = 'All Years')
  })
  # Creating reactive Publishers input based off of the input title
  observeEvent(input$title, {
    updateRadioButtons(session, 'publisher',
                       choices = publishers(input$title),
                       selected = 'All Publishers')
  })
  # Creating reactive Genres input based off of the input title
  observeEvent(input$title, {
    updateRadioButtons(session, 'genre',
                       choices = genres(input$title),
                       selected = 'All Genres')
  })
  
  # Creating reactive vgdata based off of the inputs chosen
  vgdata <- reactive({vgsales %>% 
      to_be_platform(group = input$platform) %>% 
      to_be_year(group = input$year) %>% 
      to_be_publisher(group = input$publisher) %>% 
      to_be_genre(group = input$genre) %>% 
      mutate(Video_Game = case_when(
        Name == input$title ~ input$title,
        Name != input$title ~ 'Average'
      )) %>% 
      group_by(Video_Game)})
  
  # Adding in output for table in Choose your game!
  output$summary <- function(){
    vgdata() %>% 
      summarise(Global_Sales = round(mean(Global_Sales), 2),
                NA_Sales = round(mean(NA_Sales), 2),
                EU_Sales = round(mean(EU_Sales), 2),
                JP_Sales = round(mean(JP_Sales), 2),
                Other = round(mean(Other_Sales), 2)) %>% 
      slice(match(c(input$title, 'Average'), Video_Game)) %>%
      kable('html',
            caption = 'Sales are in millions and in Global Currencies') %>% 
      kable_styling('striped', full_width = TRUE)
  }
  
  
  # Adding in output for plot in Choose your game!
  output$bar <- renderPlot({
    # Creating new dataset so I can reorder data for the plot
    vgsales <- vgdata() %>% 
      summarise(NA_Sales = mean(NA_Sales),
                EU_Sales = mean(EU_Sales),
                JP_Sales = mean(JP_Sales),
                Global_Sales = mean(Global_Sales),
                Other_Sales = mean(Other_Sales)) %>% 
      pivot_longer(cols = c('NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales', 'Global_Sales'),
                   names_to = c('Type'),
                   values_to = c('Sales'))
    # Refactoring data for the plot
    vgsales$Name <- factor(vgsales$Video_Game, levels = c('Average', input$title))
    
    # Creating plot
    ggplot(vgsales, aes(Type, Sales, fill = Name)) +
      geom_col(position = 'dodge') +
      labs(y = 'Total Sales (in Millions)',
           x = 'Region',
           title = paste('Video Game Sales of', input$title, 'v.s. Other Video Games'),
           fill = 'Video Game') +
      scale_x_discrete(limits = c('Global_Sales', 'NA_Sales', 'JP_Sales', 'EU_Sales', 'Other_Sales'),
                       labels = c('Global', 'North America', 'Japan', 'Europe', 'Other')) +
      scale_fill_manual(limits = c(input$title, 'Average'), values = c(color_fill(), 'black')) +
      theme_minimal()
  })
  
  #------------------------------------------------------------------------------------------
  
  # Tab Panel 3 Code
  # Adding in output for barplots in Game v.s. Game
  output$bars <- renderPlot({
    # Creating the bar data for our plot
    bars_data <- vgsales %>% 
      filter((Name == input$title1) | (Name == input$title2)) %>% 
      group_by(Name) %>% 
      summarise(NA_Sales = mean(NA_Sales),
                EU_Sales = mean(EU_Sales),
                JP_Sales = mean(JP_Sales),
                Global_Sales = mean(Global_Sales),
                Other_Sales = mean(Other_Sales)) %>% 
      pivot_longer(cols = c('NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales', 'Global_Sales'),
                   names_to = c('Type'),
                   values_to = c('Sales'))
    # Reordering the factors of names in plot so they are always in same position
    bars_data$Name <- factor(bars_data$Name, levels = c(input$title1, input$title2))
    # Creating bar plot
    ggplot(bars_data, aes(Type, Sales, fill = Name)) +
      geom_col(position = 'dodge') +
      theme_minimal() +
      scale_x_discrete(limits = c('Global_Sales', 'NA_Sales', 'JP_Sales', 'EU_Sales', 'Other_Sales'),
                       labels = c('Global', 'North America', 'Japan', 'Europe', 'Other')) +
      scale_fill_manual(values = c('#C3C3C3', '#FFC90E')) +
      labs(y = 'Total Sales (in Millions)',
           x = 'Region',
           title = paste0(input$title1, "'s Sales v.s. ", input$title2, "'s Sales"),
           fill = 'Video Game')
  })
  
  # Creating the table data for game v.s. game
  tabledata <- reactive({vgsales %>% 
      filter((Name == input$title1) | (Name == input$title2)) %>% 
      mutate(Video_Game = case_when(
        Name == input$title1 ~ input$title1,
        Name == input$title2 ~ input$title2
      )) %>% 
      group_by(Video_Game) %>% 
      summarise(Global_Sales = round(mean(Global_Sales), 2),
                NA_Sales = round(mean(NA_Sales), 2),
                EU_Sales = round(mean(EU_Sales), 2),
                JP_Sales = round(mean(JP_Sales), 2),
                Other = round(mean(Other_Sales), 2)) %>% 
      slice(match(c(input$title1, input$title2), Video_Game))})
  # Creating output table for game v.s. game
  output$summaries <- function(){
    tabledata() %>% 
      kable('html',
            caption = 'Sales are in millions and in Global Currencies') %>% 
      kable_styling('striped', full_width = TRUE)
  }
}

# Run the app ----
shinyApp(ui = ui, server = server)
