library(tidyverse)
library(dendextend)
library(gridExtra)
library(plotly)
library(threejs)
library(dygraphs)
library(timeSeries)
library(xts)
library(maps)
library(mapproj)

crimes <- read_csv("report.csv")

complete.rows <- complete.cases(crimes)
crimes <- crimes[complete.rows,]

crimes <- mutate(crimes, 
                 state = substr(agency_jurisdiction, 
                                nchar(as.character(agency_jurisdiction))-1, 
                                nchar(as.character(agency_jurisdiction))), 
                 national_region = ifelse(is.element(state, c("WA", "OR", "ID", "MT", 
                                                              "WY", "CA", "NV", "UT", 
                                                              "CO", "AZ", "NM")), 
                                          "West", 
                                          ifelse(is.element(state, c("ND", "SD", "NE", 
                                                                     "KS", "MN", "IA", 
                                                                     "MO", "IL", "WI", 
                                                                     "MI", "IN", "OH")), 
                                                 "Midwest", 
                                                 ifelse(is.element(state, 
                                                                   c("PA", "NY", "NJ", 
                                                                     "CT", "RI", "MA", 
                                                                     "VT", "NH", "ME")), 
                                                        "Northeast", "South"))))

GDP <- read.csv("GDP.csv")

gun <- read_csv("raw_data.csv")


colorblind_palette <- c("#7C0607", "#D0A8A8", "#AEAFCF", "#1F28A2")


function(input, output) {
  
  
  
  
  
  output$hist_type <- renderPlot({
    
    print(input$chart1_crime_type)
    
    if (input$chart1_crime_type == "violent crimes"){
      print(max(crimes$report_year))
      print(input$year_chart1)
      barchart <- ggplot(subset(crimes, report_year == input$year_chart1), 
                         aes(x = crimes_percapita)) + 
        geom_histogram(color = "black") + 
        labs(title = "Distribution of Total Crime Rate", 
             x = "Number of Crimes", 
             y = "Frequency")
    }
    
    if (input$chart1_crime_type == "homicides"){
      barchart <- ggplot(subset(crimes, report_year == input$year_chart1), 
                         aes(x = homicides_percapita)) + 
        geom_histogram(color = "black") + 
        labs(title = "Distribution of Homicides Rate", 
             x = "Number of Crimes", 
             y = "Frequency")
    }
    
    if (input$chart1_crime_type == "rapes"){
      barchart <- ggplot(subset(crimes, report_year == input$year_chart1), 
                         aes(x = rapes_percapita)) + 
        geom_histogram(color = "black")  + 
        labs(title = "Distribution of Rape Rate", 
             x = "Number of Crimes", 
             y = "Frequency")
    }
    
    if (input$chart1_crime_type == "assaults"){
      barchart <- ggplot(subset(crimes, report_year == input$year_chart1), 
                         aes(x = assaults_percapita)) + 
        geom_histogram(color = "black")  + 
        labs(title = "Distribution of Assault Rate", 
             x = "Number of Crimes", 
             y = "Frequency")
    }
    
    if (input$chart1_crime_type == "robberies"){
      barchart <- ggplot(subset(crimes, report_year == input$year_chart1), 
                         aes(x = robberies_percapita)) + 
        geom_histogram(color = "black") + 
        labs(title = "Distribution of Robberie Rate", 
             x = "Number of Crimes", 
             y = "Frequency")
    }
    
    return (barchart)
    
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
  
  
  output$boxplot <- renderPlotly({
    if (input$chart2_crime_type == "violent crimes"){
      p <- plot_ly(crimes, y = ~violent_crimes, color = I("black"), 
                   alpha = 0.1, boxpoints = "suspectedoutliers")
      p1 <- p %>% add_boxplot(x = "Overall")
      p2 <- p %>% add_boxplot(x = ~report_year)
      boxplot <- subplot(
        p1, p2, shareY = TRUE,
        widths = c(0.2, 0.8), margin = 0
      )
    }
    
    if (input$chart2_crime_type == "homicides"){
      p <- plot_ly(crimes, y = ~homicides, color = I("black"), 
                   alpha = 0.1, boxpoints = "suspectedoutliers")
      p1 <- p %>% add_boxplot(x = "Overall")
      p2 <- p %>% add_boxplot(x = ~report_year)
      boxplot <- subplot(
        p1, p2, shareY = TRUE,
        widths = c(0.2, 0.8), margin = 0
      ) 
    }
    
    if (input$chart2_crime_type == "rapes"){
      p <- plot_ly(crimes, y = ~rapes, color = I("black"), 
                   alpha = 0.1, boxpoints = "suspectedoutliers")
      p1 <- p %>% add_boxplot(x = "Overall")
      p2 <- p %>% add_boxplot(x = ~report_year)
      boxplot <- subplot(
        p1, p2, shareY = TRUE,
        widths = c(0.2, 0.8), margin = 0
      ) 
    }
    
    if (input$chart2_crime_type == "assaults"){
      p <- plot_ly(crimes, y = ~assaults, color = I("black"), 
                   alpha = 0.1, boxpoints = "suspectedoutliers")
      p1 <- p %>% add_boxplot(x = "Overall")
      p2 <- p %>% add_boxplot(x = ~report_year)
      boxplot <- subplot(
        p1, p2, shareY = TRUE,
        widths = c(0.2, 0.8), margin = 0
      )
    }
    
    if (input$chart2_crime_type == "robberies"){
      p <- plot_ly(crimes, y = ~robberies, color = I("black"), 
                   alpha = 0.1, boxpoints = "suspectedoutliers")
      p1 <- p %>% add_boxplot(x = "Overall")
      p2 <- p %>% add_boxplot(x = ~report_year)
      boxplot <- subplot(
        p1, p2, shareY = TRUE,
        widths = c(0.2, 0.8), margin = 0
      ) 
    }
    
    layout(boxplot, showlegend = FALSE)
    
  })
  
  
  
  
  output$dendrogram <- renderPlot({
    
    get_colors <- function(x, palette = colorblind_palette) {
      palette[match(x, unique(x))]
    }
    
    dat_by_state_and_year <- crimes %>% 
      group_by(report_year, state, national_region) %>% 
      summarize(avg_crimes_percapita = mean(crimes_percapita), 
                avg_homicides_percapita = mean(homicides_percapita), 
                avg_rapes_percapita = mean(rapes_percapita), 
                avg_assaults_percapita = mean(assaults_percapita), 
                avg_robberies_percapita = mean(robberies_percapita))
    
    # require year as a user input
    dat_target_year <- subset(dat_by_state_and_year, 
                              report_year == input$dendrogram_year)
    
    dat_cont <- dat_target_year[, c("avg_homicides_percapita", "avg_rapes_percapita", 
                                    "avg_assaults_percapita", "avg_robberies_percapita")]
    
    dat_cont_scale <- dat_cont %>% scale()
    dist_dat <- dat_cont_scale %>% dist()
    hc_dat_complete <- hclust(dist_dat, method = "complete")
    
    dendro <- hc_dat_complete %>%
      as.dendrogram %>%
      set("labels", dat_target_year$state, order_value = TRUE) %>%
      set("labels_col", get_colors(dat_target_year$national_region), 
          order_value = TRUE) %>% 
      set("labels_cex", 1) %>%
      ggplot() + 
      labs(title = "Similarities in Crimes between States in the U.S.", 
           x = "States", y = "Distance", label = "Region") + 
      theme(axis.text.x  = element_blank(),
            axis.ticks.x = element_blank(),
            panel.background = element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank()) + 
      ylim(-2, 10)
    
    return(dendro)
    
  })
  
  output$scatter_GDP <- renderPlotly({
    
    scatter_GDP <- ggplot(GDP, aes(x = crime_percapita, y = GDP_percapita)) + 
      geom_point(aes(color = crime_type, size = pop, text = stateNames, 
                     frame = year, ids = X), alpha = 0.5, show.legend = FALSE) + 
      labs(
        title = "Correlation between GDP and Crimes in States", 
        x = "Crimes per Capita", 
        y = "GDP per Capita", 
        color = "Region", 
        size = "Population"
      )
    
    if (input$GDP_homicides_trend) {
      
      scatter_GDP <- scatter_GDP + 
        geom_smooth(data = subset(GDP, crime_type == "Homicides"), 
                    method = "lm")
      
    }
    
    if (input$GDP_rapes_trend) {
      
      scatter_GDP <- scatter_GDP + 
        geom_smooth(data = subset(GDP, crime_type == "Rapes"), 
                    method = "lm")
      
    }
    
    if (input$GDP_assaults_trend) {
      
      scatter_GDP <- scatter_GDP + 
        geom_smooth(data = subset(GDP, crime_type == "Assaults"), 
                    method = "lm")
      
    }
    
    if (input$GDP_robberies_trend) {
      
      scatter_GDP <- scatter_GDP + 
        geom_smooth(data = subset(GDP, crime_type == "Robberies"), 
                    method = "lm")
      
    }
    
    scatter_GDP <- ggplotly(scatter_GDP, 
                            tooltip = c("y", "x", "text", "color", "size"))
    
    return(scatter_GDP)
    
  })
  
  output$scatter_crime_cor <- renderPlotly({
    
    # plot rates
    # require specify the y variable
    if (input$crime_cor_var == "Homicides") {
      
      if (!input$crime_cor_var_trend) {
        
        graph.1out <- ggplot(crimes, aes(x = rapes_percapita, y = homicides_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          labs(x = "Rapes per Capita", y = "Homicides per Capita")
        
        graph.1out <- ggplotly(graph.1out, tooltip = c("y", "x", "text"))
        
        graph.2out <- ggplot(crimes, aes(x = assaults_percapita, y = homicides_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          labs(title = "Correlation between Crimes in Cities", 
               x = "Assaults per Capita", y = "Homicides per Capita")
        
        graph.2out <- ggplotly(graph.2out, tooltip = c("y", "x", "text"))
        
        graph.3out <- ggplot(crimes, aes(x = robberies_percapita, y = homicides_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          labs(x = "Robberies per Capita", y = "Homicides per Capita")
        
        graph.3out <- ggplotly(graph.3out, tooltip = c("y", "x", "text"))
        
        subplot(graph.1out, graph.2out, graph.3out, nrows = 1, 
                titleX = TRUE, shareY = TRUE, shareX = FALSE) %>% 
          style(traces = c(1,2,3,4,5,6,7,8), showlegend = FALSE)
        
      } else {
        
        graph.1out <- ggplot(crimes, aes(x = rapes_percapita, y = homicides_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          geom_smooth(method = "lm") + 
          labs(x = "Rapes per Capita", y = "Homicides per Capita")
        
        graph.1out <- ggplotly(graph.1out, tooltip = c("y", "x", "text"))
        
        graph.2out <- ggplot(crimes, aes(x = assaults_percapita, y = homicides_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          geom_smooth(method = "lm") + 
          labs(title = "Correlation between Crimes in Cities", 
               x = "Assaults per Capita", y = "Homicides per Capita")
        
        graph.2out <- ggplotly(graph.2out, tooltip = c("y", "x", "text"))
        
        graph.3out <- ggplot(crimes, aes(x = robberies_percapita, y = homicides_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          geom_smooth(method = "lm") + 
          labs(x = "Robberies per Capita", y = "Homicides per Capita")
        
        graph.3out <- ggplotly(graph.3out, tooltip = c("y", "x", "text"))
        
        subplot(graph.1out, graph.2out, graph.3out, nrows = 1, 
                titleX = TRUE, shareY = TRUE, shareX = FALSE) %>% 
          style(traces = c(1,2,3,4,5,6,7,8,9,10), showlegend = FALSE)
        
      }
      
      
    } else if (input$crime_cor_var == "Rapes") {
      
      if (!input$crime_cor_var_trend) {
        
        graph.1out <- ggplot(crimes, aes(x = homicides_percapita, y = rapes_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          labs(x = "Homicides per Capita", y = "Rapes per Capita")
        
        graph.1out <- ggplotly(graph.1out, tooltip = c("y", "x", "text"))
        
        graph.2out <- ggplot(crimes, aes(x = assaults_percapita, y = rapes_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          labs(title = "Correlation between Crimes in Cities", 
               x = "Assaults per Capita", y = "Rapes per Capita")
        
        graph.2out <- ggplotly(graph.2out, tooltip = c("y", "x", "text"))
        
        graph.3out <- ggplot(crimes, aes(x = robberies_percapita, y = rapes_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          labs(x = "Robberies per Capita", y = "Rapes per Capita")
        
        graph.3out <- ggplotly(graph.3out, tooltip = c("y", "x", "text"))
        
        subplot(graph.1out, graph.2out, graph.3out, nrows = 1, 
                titleX = TRUE, shareY = TRUE, shareX = FALSE) %>% 
          style(traces = c(1,2,3,4,5,6,7,8), showlegend = FALSE)
        
      } else {
        
        graph.1out <- ggplot(crimes, aes(x = homicides_percapita, y = rapes_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          geom_smooth(method = "lm") + 
          labs(x = "Homicides per Capita", y = "Rapes per Capita")
        
        graph.1out <- ggplotly(graph.1out, tooltip = c("y", "x", "text"))
        
        graph.2out <- ggplot(crimes, aes(x = assaults_percapita, y = rapes_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          geom_smooth(method = "lm") + 
          labs(title = "Correlation between Crimes in Cities", 
               x = "Assaults per Capita", y = "Rapes per Capita")
        
        graph.2out <- ggplotly(graph.2out, tooltip = c("y", "x", "text"))
        
        graph.3out <- ggplot(crimes, aes(x = robberies_percapita, y = rapes_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          geom_smooth(method = "lm") + 
          labs(x = "Robberies per Capita", y = "Rapes per Capita")
        
        graph.3out <- ggplotly(graph.3out, tooltip = c("y", "x", "text"))
        
        subplot(graph.1out, graph.2out, graph.3out, nrows = 1, 
                titleX = TRUE, shareY = TRUE, shareX = FALSE) %>% 
          style(traces = c(1,2,3,4,5,6,7,8,9,10), showlegend = FALSE)
        
      }
      
    } else if (input$crime_cor_var == "Assaults") {
      
      if (!input$crime_cor_var_trend) {
        
        graph.1out <- ggplot(crimes, aes(x = homicides_percapita, y = assaults_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          labs(x = "Homicides per Capita", y = "Assaults per Capita")
        
        graph.1out <- ggplotly(graph.1out, tooltip = c("y", "x", "text"))
        
        graph.2out <- ggplot(crimes, aes(x = rapes_percapita, y = assaults_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          labs(title = "Correlation between Crimes in Cities", 
               x = "Rapes per Capita", y = "Assaults per Capita")
        
        graph.2out <- ggplotly(graph.2out, tooltip = c("y", "x", "text"))
        
        graph.3out <- ggplot(crimes, aes(x = robberies_percapita, y = assaults_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          labs(x = "Robberies per Capita", y = "Assaults per Capita")
        
        graph.3out <- ggplotly(graph.3out, tooltip = c("y", "x", "text"))
        
        subplot(graph.1out, graph.2out, graph.3out, nrows = 1, 
                titleX = TRUE, shareY = TRUE, shareX = FALSE) %>% 
          style(traces = c(1,2,3,4,5,6,7,8), showlegend = FALSE)
        
      } else {
        
        graph.1out <- ggplot(crimes, aes(x = homicides_percapita, y = assaults_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          geom_smooth(method = "lm") + 
          labs(x = "Homicides per Capita", y = "Assaults per Capita")
        
        graph.1out <- ggplotly(graph.1out, tooltip = c("y", "x", "text"))
        
        graph.2out <- ggplot(crimes, aes(x = rapes_percapita, y = assaults_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          geom_smooth(method = "lm") + 
          labs(title = "Correlation between Crimes in Cities", 
               x = "Rapes per Capita", y = "Assaults per Capita")
        
        graph.2out <- ggplotly(graph.2out, tooltip = c("y", "x", "text"))
        
        graph.3out <- ggplot(crimes, aes(x = robberies_percapita, y = assaults_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          geom_smooth(method = "lm") + 
          labs(x = "Robberies per Capita", y = "Assaults per Capita")
        
        graph.3out <- ggplotly(graph.3out, tooltip = c("y", "x", "text"))
        
        subplot(graph.1out, graph.2out, graph.3out, nrows = 1, 
                titleX = TRUE, shareY = TRUE, shareX = FALSE) %>% 
          style(traces = c(1,2,3,4,5,6,7,8,9,10), showlegend = FALSE)
        
      }
      
    } else if (input$crime_cor_var == "Robberies") {
      
      if (!input$crime_cor_var_trend) {
        
        graph.1out <- ggplot(crimes, aes(x = homicides_percapita, y = robberies_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          labs(x = "Homicides per Capita", y = "Robberies per Capita")
        
        graph.1out <- ggplotly(graph.1out, tooltip = c("y", "x", "text"))
        
        graph.2out <- ggplot(crimes, aes(x = rapes_percapita, y = robberies_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          labs(title = "Correlation between Crimes in Cities", 
               x = "Rapes per Capita", y = "Robberies per Capita")
        
        graph.2out <- ggplotly(graph.2out, tooltip = c("y", "x", "text"))
        
        graph.3out <- ggplot(crimes, aes(x = assaults_percapita, y = robberies_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          labs(x = "Assaults per Capita", y = "Robberies per Capita")
        
        graph.3out <- ggplotly(graph.3out, tooltip = c("y", "x", "text"))
        
        subplot(graph.1out, graph.2out, graph.3out, nrows = 1, 
                titleX = TRUE, shareY = TRUE, shareX = FALSE) %>% 
          style(traces = c(1,2,3,4,5,6,7,8), showlegend = FALSE)
        
      } else {
        
        graph.1out <- ggplot(crimes, aes(x = homicides_percapita, y = robberies_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          geom_smooth(method = "lm") + 
          labs(x = "Homicides per Capita", y = "Robberies per Capita")
        
        graph.1out <- ggplotly(graph.1out, tooltip = c("y", "x", "text"))
        
        graph.2out <- ggplot(crimes, aes(x = rapes_percapita, y = robberies_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          geom_smooth(method = "lm") + 
          labs(title = "Correlation between Crimes in Cities", 
               x = "Rapes per Capita", y = "Robberies per Capita")
        
        graph.2out <- ggplotly(graph.2out, tooltip = c("y", "x", "text"))
        
        graph.3out <- ggplot(crimes, aes(x = assaults_percapita, y = robberies_percapita)) + 
          geom_point(aes(color = national_region, text = agency_jurisdiction, 
                         frame = report_year, ids = agency_jurisdiction), 
                     show.legend = FALSE, alpha = 0.7) + 
          geom_smooth(method = "lm") + 
          labs(x = "Assaults per Capita", y = "Robberies per Capita")
        
        graph.3out <- ggplotly(graph.3out, tooltip = c("y", "x", "text"))
        
        subplot(graph.1out, graph.2out, graph.3out, nrows = 1, 
                titleX = TRUE, shareY = TRUE, shareX = FALSE) %>% 
          style(traces = c(1,2,3,4,5,6,7,8,9,10), showlegend = FALSE)
        
      }
      
    }
    
  })
  
  
  
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  
  
  
  
  
  
  output$radio1 <- renderPlot({
    crimes_2014 <- subset(crimes, report_year == 2014)
    
    
    num.crimes.2014 <- crimes_2014 %>%
      group_by(state) %>%
      summarize(total = sum(crimes_percapita))
    
    state.data <- data_frame(state.abb, state.name)
    
    num.crimes.2014 <- 
      num.crimes.2014 %>% left_join(state.data, by = c("state" = "state.abb"))
    
    gun_2014 <- subset(gun, year == 2014)
    
    merged <- 
      num.crimes.2014 %>% left_join(gun_2014, by = c("state.name" = "state"))
    
    if (input$radio1 == 1) {
      ggplot(merged, aes(x = reorder(state, -total), y = total, 
                         fill = factor(permit + reportdealer + recordsdealer))) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = colorblind_palette[1:4]) + 
        labs(title = "Crime Rate by U.S. State in 2014",
             x     = "U.S. State",
             y     = "Number of Violent Crimes", 
             fill = "Number of Laws related to dealer") + 
        theme(legend.position = "bottom")
    } else if (input$radio1 == 2) {
      ggplot(merged, aes(x = reorder(state, -total), y = total, 
                         fill = factor(permit + registration + fingerprint))) +
        scale_fill_manual(values = colorblind_palette[1:4]) + 
        geom_bar(stat = "identity") +
        labs(title = "Crime Rate by U.S. State in 2014",
             x     = "U.S. State",
             y     = "Number of Violent Crimes", 
             fill = "Number of Laws related to buyer") + 
        theme(legend.position = "bottom")
    } else if (input$radio1 == 3) {
      ggplot(merged, aes(x = reorder(state, -total), y = total, 
                         fill = factor(violent + danger + alcoholism))) +
        scale_fill_manual(values = colorblind_palette[1:4]) + 
        geom_bar(stat = "identity") +
        labs(title = "Crime Rate by U.S. State in 2014",
             x     = "U.S. State",
             y     = "Number of Violent Crimes", 
             fill = "Number of Laws related to Prohibitions for high-risk gun possession") + 
        theme(legend.position = "bottom")
    }
  })
  
  output$radio2 <- renderPlot({
    first.year <- input$slider_radio2[1]
    last.year <- input$slider_radio2[2]
    crimes <- subset(crimes, (report_year >= first.year 
                              & report_year <= last.year))
    num.crimes.state <- crimes %>%
      group_by(state) %>%
      summarize(total = sum(crimes_percapita))
    
    homicide.state <- crimes %>%
      group_by(state) %>%
      summarize(homicides = sum(homicides_percapita))
    
    rape.state <- crimes %>%
      group_by(state) %>%
      summarize(rapes = sum(rapes_percapita))
    
    assault.state <- crimes %>%
      group_by(state) %>%
      summarize(assaults = sum(assaults_percapita))
    
    crimes.by.state <- left_join(homicide.state, rape.state,
                                 by = c("state" = "state"))
    
    crimes.by.state <- left_join(crimes.by.state, assault.state,
                                 by = c("state" = "state"))
    
    crimes.by.state <- left_join(crimes.by.state, num.crimes.state,
                                 by = c("state" = "state"))
    
    state.borders <- map_data("state")
    
    state.data <- data_frame(state.abb, state.name)
    
    crimes.by.state <- 
      crimes.by.state %>% left_join(state.data, by = c("state" = "state.abb"))
    
    crimes.by.state$state.name <- tolower(crimes.by.state$state.name)
    
    state.borders <- state.borders %>% 
      left_join(crimes.by.state, by = c("region" = "state.name"))
    
    
    if (input$radio2 == 1) {
      ggplot(state.borders) + 
        geom_polygon(aes(x = long, y = lat, group = group,
                         fill = homicides/1000), color = "black") + 
        scale_fill_gradientn(colours = c("white", "grey50", "dodgerblue4")) +
        theme_void() +
        coord_map("polyconic") + 
        labs(title = paste("Homicides Rate by State from", 
                           first.year, "to", last.year, sep = " "),
             fill = "Homicides Rate") + 
        theme(legend.position = "bottom")
    } else if (input$radio2 == 2) {
      ggplot(state.borders) + 
        geom_polygon(aes(x = long, y = lat, group = group,
                         fill = rapes), color = "black") + 
        scale_fill_gradientn(colours = c("white", "grey50", "dodgerblue4")) +
        theme_void() +
        coord_map("polyconic") + 
        labs(title = paste("Rapes Rate by State from", 
                           first.year, "to", last.year, sep = " "),
             fill = "Rapes Rate") + 
        theme(legend.position = "bottom")
    } else if (input$radio2 == 3) {
      ggplot(state.borders) + 
        geom_polygon(aes(x = long, y = lat, group = group,
                         fill = assaults), color = "black") + 
        scale_fill_gradientn(colours = c("white", "grey50", "dodgerblue4")) +
        theme_void() +
        coord_map("polyconic") + 
        labs(title = paste("Assaults Rate by State from", 
                           first.year, "to", last.year, sep = " "),
             fill = "Assaults Rate") + 
        theme(legend.position = "bottom")
    } else if (input$radio2 == 4) {
      ggplot(state.borders) + 
        geom_polygon(aes(x = long, y = lat, group = group,
                         fill = total), color = "black") + 
        scale_fill_gradientn(colours = c("white", "grey50", "dodgerblue4")) +
        theme_void() +
        coord_map("polyconic") + 
        labs(title = paste("Crimes Rate by State from", 
                           first.year, "to", last.year, sep = " "),
             fill = "Crimes Rate") + 
        theme(legend.position = "bottom")
    }
  })
  
  
  output$sams_plot <- renderPlot({
    
    my_plot <- ggplot(data = dat(), aes(x = imdb_score)) + 
      geom_histogram(aes(y = ..density..),
                     bins = input$n_breaks, color = "black", fill = "red") + 
      labs(
        title = "Distribution of IMDB Score",
        x = "Average IMDB Score (1-10) of Movie"
      ) + theme_bw()
    
    if (input$individual_obs) {
      my_plot <- my_plot + geom_rug()
    }
    
    if (input$density) {
      my_plot <- my_plot + 
        geom_density(color = "blue", adjust = input$bw_adjust)
    }
    
    my_plot
  })
  
  #### dygraph 1
  
  # New York City, NY
  nyc <- subset(crimes, agency_jurisdiction == "New York City, NY")
  city_vars <- c("report_year", "violent_crimes")
  nyc <- nyc[city_vars]
  colnames(nyc)[2] <- "nyc_violent"
  # Los Angeles, CA
  la <- subset(crimes, agency_jurisdiction == "Los Angeles, CA")
  la <- la[city_vars]
  colnames(la)[2] <- "la_violent"
  # Chicago, IL
  chi <- subset(crimes, agency_jurisdiction == "Chicago, IL")
  chi <- chi[city_vars]
  colnames(chi)[2] <- "chi_violent"
  # Detroit, MI
  det <- subset(crimes, agency_jurisdiction == "Detroit, MI")
  det <- det[city_vars]
  colnames(det)[2] <- "det_violent"
  # Dallas, TX
  dal <- subset(crimes, agency_jurisdiction == "Dallas, TX")
  dal <- dal[city_vars]
  colnames(dal)[2] <- "dal_violent"
  # Philadelphia, PA
  phi <- subset(crimes, agency_jurisdiction == "Philadelphia, PA")
  phi <- phi[city_vars]
  colnames(phi)[2] <- "phi_violent"
  # Las Vegas, NV
  lv <- subset(crimes, agency_jurisdiction == "Las Vegas, NV")
  lv <- lv[city_vars]
  colnames(lv)[2] <- "lv_violent"
  # Baltimore, MD
  bal <- subset(crimes, agency_jurisdiction == "Baltimore, MD")
  bal <- bal[city_vars]
  colnames(bal)[2] <- "bal_violent"
  # Miami-Dade County, FL
  md <- subset(crimes, agency_jurisdiction == "Miami-Dade County, FL")
  md <- md[city_vars]
  colnames(md)[2] <- "md_violent"
  # Houston, TX
  hou <- subset(crimes, agency_jurisdiction == "Houston, TX")
  hou <- hou[city_vars]
  colnames(hou)[2] <- "hou_violent"
  # Memphis, TN
  mem <- subset(crimes, agency_jurisdiction == "Memphis, TN")
  mem <- mem[city_vars]
  colnames(mem)[2] <- "mem_violent"
  
  cities <- nyc
  
  cities <- merge(cities, la, by.x = "report_year")
  cities <- merge(cities, chi, by.x = "report_year")
  cities <- merge(cities, det, by.x = "report_year")
  cities <- merge(cities, dal, by.x = "report_year")
  cities <- merge(cities, phi, by.x = "report_year")
  cities <- merge(cities, lv, by.x = "report_year")
  cities <- merge(cities, bal, by.x = "report_year")
  cities <- merge(cities, md, by.x = "report_year")
  cities <- merge(cities, hou, by.x = "report_year")
  cities <- merge(cities, mem, by.x = "report_year")
  
  
  output$dygraph_city <- renderDygraph({
    dygraph(cities, main = "Violent Crimes by City") %>%
      dySeries("nyc_violent", label = "New York City, NY") %>%
      dySeries("la_violent", label = "Los Angeles, CA") %>%
      dySeries("chi_violent", label = "Chicago, IL") %>%
      dySeries("det_violent", label = "Detroit, MI") %>%
      dySeries("dal_violent", label = "Dallas, TX") %>%
      dySeries("phi_violent", label = "Philadelphia, PA") %>%
      dySeries("lv_violent", label = "Las Vegas, NV") %>%
      dySeries("bal_violent", label = "Baltimore, MD") %>%
      dySeries("md_violent", label = "Miami-Dade County, FL") %>%
      dySeries("hou_violent", label = "Houston, TX") %>%
      dySeries("mem_violent", label = "Memphis, TN") %>%
      dyRangeSelector(height = 20) %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE) %>%
      dyLegend(labelsSeparateLines = TRUE)
  })
  
  
  # dygraph 2
  
  output$html_widget1 <- renderScatterplotThree({
    
    z <- seq(-10, 10, 0.01)
    x <- cos(z)
    y <- sin(z)
    scatterplot3js(x,y,z, color=rainbow(length(z)))
  })  
  
  moving_average <- function(tt, time_series, ww) {
    #  Throw an error if the window width is too big
    if (ww > length(time_series))  
      stop("Window width is greater than length of time series")
    
    #  If the window width is greater than the time point, return NA
    if (tt < ww)  return(NA)
    
    # find where the window starts
    window_start <- tt - ww + 1
    #return the average of the window
    return (mean(time_series[window_start:tt]))
  }
  
  get_moving_averages <- function(time_series, ww) {
    #  Throw an error if the window width is too big
    if (ww > length(time_series))  
      stop("Window width is greater than length of time series")
    
    #initialized an array to store result
    result <- replicate(length(time_series), NA)
    
    #loop through all points in time series
    #and find moving average for each
    for (i in 1: length(time_series)){
      current_average <- moving_average(i, time_series, ww)
      result[i] <- current_average
    }
    
    return (result)
  }
  
  num.crimes.year <- crimes %>%
    group_by(report_year) %>%
    summarize(violent_total = sum(violent_crimes))
  
  num.crimes.year <-mutate(num.crimes.year,
                           moving_average = get_moving_averages(
                             num.crimes.year$violent_total, 4))
  
  nyc <- subset(crimes, agency_jurisdiction == "New York City, NY")
  nycvars <- c("report_year", "violent_crimes")
  nyc <- nyc[nycvars]
  colnames(nyc)[1] <- "report_year"
  colnames(nyc)[2] <- "nyc_violent"
  
  num.crimes.year <- merge(num.crimes.year, nyc, by.x = "report_year")
  
  
  
  presAnnotation <- function(dygraph, x, text) {
    dygraph %>%
      dyAnnotation(x, text, attachAtBottom = TRUE, width = 200)
  }
  
  output$dygraph_all <- renderDygraph({
    dygraph(num.crimes.year, main = "Violent Crime Totals Over Time") %>%
      dySeries("violent_total", label = "National total") %>%
      dySeries("moving_average", label = "National moving Average") %>%
      dySeries("nyc_violent", label = "New York City total") %>%
      dyOptions(drawPoints = TRUE) %>%
      dyShading(from = "1993", to = "2001", color = "#ADD8E6") %>%
      presAnnotation("1997", text = "Clinton-Gore Administration") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(labelsSeparateLines = TRUE)
    
    
    
  })
  
}