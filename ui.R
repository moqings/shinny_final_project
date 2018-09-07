library(shinydashboard)
library(plotly)
library(dygraphs)

dashboardPage(skin = "black",
              dashboardHeader(title = "U.S. Violent Crime Rate 1975-2014"),
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Intro to dataset", tabName = "Intro", 
                           icon = icon("bar-chart")),
                  menuItem("Location", tabName = "Location", icon = icon("map")),
                  menuItem("Time", tabName = "time", icon = icon("clock-o"))
                )
              ),
              
              dashboardBody(
                tabItems(
                  tabItem(tabName = "Intro",
                          tabsetPanel(type = "tabs",
                                      tabPanel("Crimes",
                                               selectInput(inputId = "chart1_crime_type",
                                                           label = "Types of Crime",
                                                           choices = c("violent crimes", 
                                                                       "homicides", 
                                                                       "rapes", 
                                                                       "assaults", 
                                                                       "robberies"),
                                                           selected = "violent crimes"),
                                               plotOutput("hist_type"),
                                               sliderInput(inputId = "year_chart1",
                                                           label = "Year",
                                                           min = 1975, max = 2014, 
                                                           value = 2014, step = 1, 
                                                           sep = "")
                                      ),
                                      tabPanel("Correlation",
                                               selectInput(inputId = "crime_cor_var",
                                                           label = "Select Type of Crime:",
                                                           choices = c("Homicides", "Rapes", "Assaults", "Robberies"),
                                                           selected = "Homicides"),
                                               
                                               checkboxInput(inputId = "crime_cor_var_trend", 
                                                             label = "Show Trend Line", 
                                                             value = FALSE), 
                                               
                                               plotlyOutput(outputId = "scatter_crime_cor")
                                      )
                          )
                  ),
                  
                  
                  tabItem(tabName = "Location",
                          tabsetPanel(type = "tabs",
                                      tabPanel("State by Map",
                                               radioButtons("radio2", h3("Select Type of Crime"),
                                                            choices = c("Homicides" = 1,
                                                                        "Rapes" = 2,
                                                                        "Assaults" = 3,
                                                                        "All of the Above" = 4)),
                                               sliderInput("slider_radio2", h3("Select Period"),
                                                           min = 1975, max = 2014, value = c(1975, 2014),
                                                           sep = ""),
                                               
                                               plotOutput("radio2", height = "500px", width = "500px")
                                      ),
                                      
                                      tabPanel("State by Bar",
                                               radioButtons("radio1", h3("Select Type of Laws Enforced"),
                                                            choices = c("Dealer Permit Required" = 1,
                                                                        "Buyer Permit Required" = 2,
                                                                        "Prohibitions for high-risk gun possession" = 3)),
                                               plotOutput("radio1")
                                      ),
                                      
                                      
                                      
                                      tabPanel("Subregion",
                                               sliderInput(inputId = "dendrogram_year",
                                                           label = "Year",
                                                           min = 1975, max = 2014, value = 2014, step = 1,
                                                           sep = ""),
                                               
                                               
                                               plotOutput(outputId = "dendrogram")
                                      ),
                                      
                                      tabPanel("Crimes vs. GDP",
                                               
                                               checkboxInput(inputId = "GDP_homicides_trend", 
                                                             label = "Show Trend Line for Homicides", 
                                                             value = FALSE), 
                                               
                                               checkboxInput(inputId = "GDP_rapes_trend", 
                                                             label = "Show Trend Line for Rapes", 
                                                             value = FALSE), 
                                               
                                               checkboxInput(inputId = "GDP_assaults_trend", 
                                                             label = "Show Trend Line for Assaults", 
                                                             value = FALSE), 
                                               
                                               checkboxInput(inputId = "GDP_robberies_trend", 
                                                             label = "Show Trend Line for Robberies", 
                                                             value = FALSE), 
                                               
                                               plotlyOutput(outputId = "scatter_GDP")
                                      )
                                      
                                      
                          )
                  ),
                  
                  tabItem(tabName = "time",
                          tabsetPanel(type = "tabs",
                                      tabPanel("Exploring national crime trends through time",  
                                               dygraphOutput(outputId = "dygraph_all", height = "500")
                                      ),
                                      tabPanel("Total crime by city",  
                                               dygraphOutput(outputId = "dygraph_city", height = "500")
                                      )
                                      
                                      
                          )
                  )
                  
                )
              )
)




