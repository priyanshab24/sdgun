library(shiny)
library(dplyr)
library(tidyverse)
library(maps)
library(shinydashboard)
library(RColorBrewer)
library(dygraphs)
library(readxl)
library(shinymanager)
library(rvest)
library(devtools)
library(plotly)
library(shinya11y)
library(DT)
library(ggplot2)
library(dplyr)
library(forecast)
library(shinycssloaders)
library(rsconnect)

# rsconnect::setAccountInfo(name='priyanshakhera', token='4121000EA145025646AB79A94790772D', secret='rsy91sSksAu/GCq+OwZZUfm+92OkmOzFtPnMIfbL')
#devtools::install_github("ewenme/shinya11y")

#Read the sustainable development dataset 
sdg

#Load the world data   
library(sf)

# Install and load required package
library(rnaturalearth)
library(rnaturalearthdata)

 
# Load countries
world <- ne_countries(scale = "medium", returnclass = "sf")


#merge data 
world <- merge(world, sdg, by.x = "admin", by.y = "country", all.x = TRUE)

#App login Credentials 
credentials <- data.frame(
  user = c("priyansha"), 
  password = c("shiny"), 
  logo_path = c("Logo.png"),
  stringsAsFactors = FALSE
)


#Create UI 
ui <-function(request){
  
 dashboardPage(
  dashboardHeader(
    title = span(img(src="UNlogo.png",height=40),"SDG Dashboard") #Strathclyde Logo 
    
  ),
  dashboardSidebar(
  
    sidebarMenu(
      # menuItem("User Guide", tabName = "user_guide",icon=icon("book")), #user guide for users
      menuItem("Overall Score by Country", tabName = "overallscore", icon = icon("line-chart")),
      menuItem("Goal Scores by Country", tabName = "Goal_Scores", icon = icon("bar-chart")),
      menuItem("Continental Progress", tabName = "continental", icon = icon("globe")),
      menuItem("Regional Comparisons", tabName = "Regional", icon = icon("table")),
     menuItem("Download Dataset", tabName = "download_data", icon = icon("download")),  
    bookmarkButton(id="bookmark")
  )
  ),
  dashboardBody(
    use_tota11y(),
    tags$head(
      tags$style(
        HTML(
          "
      /* Logo */
      .skin-blue .main-header .logo {
        background-color: #89cff0; /* Soft blue */
      }
      /* Logo when hovered */
      .skin-blue .main-header .logo:hover {
        background-color: #89cff0;
      }
      /* Rest of the header */
      .skin-blue .main-header .navbar {
        background-color: #89cff0;
      }
      /* Main sidebar */
      .skin-blue .main-sidebar {
        background-color: #aec6cf; /* Soft gray-blue */
      }
      /* Active selected tab in the sidebar menu */
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
        background-color: #d7b9c0; /* Soft pink */
      }
      /* Highlight links in the sidebar menu */
      .skin-blue .main-sidebar .sidebar .sidebar-menu a {
        background-color: #d1d1d1; /* Light gray */
        color: #000000;
      }
      /* Toggle button */
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
        background-color: #d7b9c0; /* Soft pink */
      }
      /* Main body */
      .skin-blue .content-wrapper {
        background-color: #f0e5d8; /* Cream */
      }
      "
        )
      )
    ),
    
    
  
    tabItems(

      # User Guide Tab
      tabItem(
        tabName = "user_guide",
        fluidRow(
          box(
            title = "User Guide",
            width = 12,
            "Add your user guide content here."
          )
        )
      ),
      # Download Dataset Tab
      tabItem(
        tabName = "download_data",
        fluidRow(
          box(
            title = "Download SDG Dataset",
            width = 12,
            downloadButton("download_data_button", "Download SDG Dataset")
          )
        )
      ),
      tabItem(
        tabName = "overallscore",
        
          fluidPage(
            box(
                  title = "Select year",
                  selectInput(inputId = "year",
                              label = NULL,
                              choices = unique(world$year),
                              selected = "2015"),
                  width = 12
                ),

              box(
              width=12,
            titlePanel("Overall Score by Country and Continents over time: "),
                   withSpinner( plotlyOutput("overall_score_plot"
                                           ))
          )
        )
        ),
        
      #'Goal Scores tab
    tabItem(
      tabName = "Goal_Scores",
      fluidPage(
        box(width=12,
          selectInput("country", "Select Country:", choices = unique(sdg$country), multiple = TRUE,selected="India")
        
        ),
        box(
        plotlyOutput("goal_score_plot"),
        width=12
        )
      )

    ),
    #Continental progress tab 
    tabItem(
      tabName = "continental",
      fluidPage(
       withSpinner( plotlyOutput("choropleth_map",height = "800px", width = "1000px")
        )),
      box(dataTableOutput("continent_table"))
      ),
    
    #Regional comparison tab 
   tabItem(
     tabName = "Regional",
     fluidPage(
    titlePanel("Comparisons between United Nations Regional Groups"),
    fluidRow(
      tabsetPanel(
        tabPanel("Africa", plotlyOutput("plot_africa")),
        tabPanel("Asia", plotlyOutput("plot_asia")),
        tabPanel("Europe", plotlyOutput("plot_europe")),
        tabPanel("Americas", plotlyOutput("plot_americas")),
        tabPanel("Oceania", plotlyOutput("plot_oceania"))
      ), 
      titlePanel("Data table showing UN Regional Groups score by countries over time "),
      DTOutput("regional_table")
     )
   )
   )
    )
  )
)
}
        
# ui <- secure_app(ui)
      
      # Define server
      server <- function(input, output,session) {
        observe(session$doBookmark())
        output$bookmark_ui <- renderUI({
          bookmarkButton(id="bookmark")
        })
        
    #Render overall score plot
        output$overall_score_plot <- renderPlotly({
          req(input$year)
          
          plot1 <- world %>%
            filter(year == input$year) %>% 
            ggplot(aes(x = admin, y = sdg_index_score, fill = continent)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "Country", y = "SDG Index Score") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
          
          ggplotly(plot1) %>%
            layout(title = paste("Overall Score for the year", input$year))
        })

     
        # Create choropleth map for continental progress
        output$choropleth_map <- renderPlotly({
          choropleth_map <- plot_ly() %>% 
          add_trace(
            data = world,
            type = "choropleth",
            locations = ~admin,
            z = ~sdg_index_score,
            text = ~paste("Country: ", admin, "<br>Continent: ", continent, "<br>Score: ", sdg_index_score),  # Custom tooltip text
            locationmode = "country names",
            colorscale = "Viridis",
            colorbar = list(title = "Score"),
            showscale = TRUE
          ) %>%
          layout(
            title = "Scores by Country in Each Continent",
            geo = list(
              showframe =FALSE,
              showcoastlines = TRUE,
              projection = list(type = "mercator")
            )
          )
        # Customize layout
        choropleth_map <- choropleth_map %>%
          layout(
            title = list(text = "Scores by Country in Each Continent", font = list(size = 24)),  # Larger title font
            margin = list(l = 50, r = 50, b = 50, t = 100),  # Adjust margins for better layout
            legend = list(x = 0.8, y = 0.1)  # Position legend
          )
        
        # Display map
        return(choropleth_map)
        })
        
#Render plot for Individual scores for each goal by country over time
        output$goal_score_plot <- renderPlotly({
          # Filter data based on selected countries
          # Define the color palette
          palette <- RColorBrewer::brewer.pal(3, "Set2")
          
          # Filter and prepare data
          plot_data <- sdg %>%
            filter(country %in% input$country) %>%
            pivot_longer(
              cols = starts_with("goal_"),
              names_to = "goal",
              values_to = "score"
            ) %>%
            arrange(country, goal)  # Arrange the data by country and goal
          
          # Create the plotly plot
          plot_ly(plot_data, x = ~goal, y = ~score, color = ~country, text = ~paste("Country: ", country, "<br>Year: ", year, "<br>Score: ", score), type = "bar") %>%
            layout(title = paste("Score by SDG Goal for country",input$country),
                   xaxis = list(title = "SDG Goal"),
                   yaxis = list(title = "Score"),
                   colorway = palette,
                   barmode = "group", 
                   legend = list(orientation = "h", x = 0.5, y = -0.2), 
                   hoverlabel = list(bgcolor = "white", font = list(family = "Arial", size = 12)),  # Customize hover label appearance
                   showlegend = TRUE)
                  
        })

 ####Regional Groups comparison       
       
        centroid <- st_centroid(world)
        world$longitude <- st_coordinates(centroid)[, 1]
        world$latitude <- st_coordinates(centroid)[, 2]
        
        # Filter data for each regional group
        africa_data <- world %>%
          filter(region_un == "Africa")
        
        asia_data <- world %>%
          filter(region_un == "Asia")
        
        europe_data <- world %>%
          filter(region_un == "Europe")
        
        americas_data <- world %>%
          filter(region_un == "Americas")
        
        oceania_data <- world %>%
          filter(region_un == "Oceania")
        
        # Create plots for each region
        plot_africa <- plot_ly(africa_data, x = ~longitude, y = ~latitude, color = ~sdg_index_score, 
                               text = ~paste(iso_a3, "<br>SDG Score:", sdg_index_score)) %>%
          add_markers() %>%
          layout(title = "SDG Score for Africa", xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"))
        
        plot_asia <- plot_ly(asia_data, x = ~longitude, y = ~latitude, color = ~sdg_index_score, 
                             text = ~paste(iso_a3, "<br>SDG Score:", sdg_index_score)) %>%
          add_markers() %>%
          layout(title = "SDG Score for Asia", xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"))
        
        plot_europe <- plot_ly(europe_data, x = ~longitude, y = ~latitude, color = ~sdg_index_score, 
                               text = ~paste(iso_a3, "<br>SDG Score:", sdg_index_score)) %>%
          add_markers() %>%
          layout(title = "SDG Score for Europe", xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"))
        
        plot_americas <- plot_ly(americas_data, x = ~longitude, y = ~latitude, color = ~sdg_index_score, 
                                 text = ~paste(iso_a3, "<br>SDG Score:", sdg_index_score)) %>%
          add_markers() %>%
          layout(title = "SDG Score for Americas", xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"))
        
        plot_oceania <- plot_ly(oceania_data, x = ~longitude, y = ~latitude, color = ~sdg_index_score, 
                                text = ~paste(iso_a3, "<br>SDG Score:", sdg_index_score)) %>%
          add_markers() %>%
          layout(title = "SDG Score for Oceania", xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"))
        
        # Render the plots
        output$plot_africa <- renderPlotly({
          plot_africa
        })
        
        output$plot_asia <- renderPlotly({
          plot_asia
        })
        
        output$plot_europe <- renderPlotly({
          plot_europe
        })
        
        output$plot_americas <- renderPlotly({
          plot_americas
        })
        
        output$plot_oceania <- renderPlotly({
          plot_oceania
        })
        
       
        output$regional_table <- renderDT({
          datatable(
           world[,c("admin","iso_a3","sdg_index_score","region_un","year")],
            options = list(
              dom = 'tip', 
              paging = TRUE, # Enable pagination
              searching = TRUE, # Enable search box
              ordering = TRUE, # Enable sorting
              pageLength = 10, # Number of rows per page
              lengthMenu = c(5, 10, 15, 20), # Control the number of rows per page
              autoWidth = TRUE, # Adjust table width automatically
              rownames = FALSE, # Hide row names
              language = list(
                search = "Search:", # Customize search box label
                info = "Showing _START_ to _END_ of _TOTAL_ entries", # Customize info text
                paginate = list(
                  first = "First", 
                  previous="Previous",
                
                  last = "Last"
                )
              )
            )
          )
        })
        
        # res_auth <- secure_server(
        #   check_credentials = check_credentials(credentials)
        # )
        # 
        # Placeholder for download button
        output$download_data_button <- downloadHandler(
          filename = function() {
            "C:/Users/Priyansha Khera/Downloads/Sustainale_Development_Goals (1).RData"
          },
          content = function(file) {
            write.csv(sdg, file, row.names = FALSE)
          }
        )
        
      }
      # Run the app
      shinyApp(ui, server,enableBookmarking = "server")
      
      