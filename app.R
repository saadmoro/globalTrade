#
# Written by Saad Moro, 4/13 - 4/15 2024
# Data sourced from UN Comtrade Database (https://comtradeplus.un.org/)
#
#
# This is a Shiny web application. 
# In RStudio, you can run the app by clicking on "Run App" above.
#
#TODO: Ensure empty data is replaced with NA so that every country/year pairing
#is completely full.
#

library(shiny)
library(dplyr)
library(rstudioapi)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggstream)
library(plotly)
library(scales)
library(tseries)
library(forecast)
library(RColorBrewer)

#Set working directory to the folder this file is in
working_dir <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(working_dir))


#
#
#   LOAD AND PREPARE UN COMTRADE DATA
#
#


#Function that cleans UN Comtrade data
clean_comtrade <- function(dirty_comtrade){
  dirty_comtrade <- dirty_comtrade %>%
    select("RefYear", "ReporterISO", "ReporterDesc",
           "PartnerISO", "PartnerDesc", "FlowDesc", "PrimaryValue" )
  dirty_comtrade <- dirty_comtrade %>%
    mutate(ReporterDesc = case_when(
      ReporterISO == "CIV" ~ "Ivory Coast",
      ReporterISO == "TUR" ~ "Turkey",
      ReporterISO == "BOL" ~ "Bolivia",
      ReporterISO == "HKG" ~ "Hong Kong",
      ReporterISO == "VNM" ~ "Vietnam",
      TRUE ~ ReporterDesc
    ))
  dirty_comtrade <- dirty_comtrade %>%
    mutate(PartnerDesc = case_when(
      PartnerISO == "CIV" ~ "Ivory Coast",
      PartnerISO == "TUR" ~ "Turkey",
      PartnerISO == "BOL" ~ "Bolivia",
      PartnerISO == "HKG" ~ "Hong Kong",
      PartnerISO == "VNM" ~ "Vietnam",
      TRUE ~ PartnerDesc
    ))
  return(dirty_comtrade)
}


#Load UN Comtrade Data
un_comtrade <- read_csv('./UN_Comtrade_2021.csv')
un_comtrade <- clean_comtrade(un_comtrade)


#Get full data for all years
setwd("./YearlyData")

yearly_files <- list.files(pattern = "\\.csv")

#Read all of the .csv files and combine TAKES A LONG TIME 
un_comtrade_yearly <- yearly_files %>%
  lapply(read_csv) %>%
  bind_rows()

#Clean yearly UN Comtrade data
un_comtrade_yearly <- clean_comtrade(un_comtrade_yearly)


#Return working directory to previous value
working_dir <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(working_dir))

#Get World Map data 
wmap <- ne_countries(scale = "small", returnclass = "sf")

#wmap is missing Hong Kong, Bahrain & Singapore, get them from a higher detail dataset
wmap_with_extras <- ne_countries(scale = "medium", returnclass = "sf")
extras <- wmap_with_extras[wmap_with_extras$iso_a3_eh %in% c("HKG", "BHR", "SGP"),]
wmap <- rbind(wmap, extras)

#Remove Antarctica to increase default zoom in plotly map
wmap <- wmap %>%
  filter(iso_a3_eh != c("ATF", "ATA"))


#Get National data
national_data <- un_comtrade %>%
  filter(FlowDesc == "Export" & ReporterISO == "UKR")

#Some country names are out of order; fix this
un_comtrade <- un_comtrade %>%
  arrange(ReporterDesc)

#Fetch key-value pairs of each ISO3 code and associated country name
unique_countries <- unique(un_comtrade[,c("ReporterDesc", "ReporterISO")])
codelist <- pivot_wider(unique_countries, names_from = ReporterDesc, values_from = ReporterISO)


#Color Scale: Used in streamgraph
#Colors are from the Viridis scale, discretized on the following website:
#https://waldyrious.net/viridis-palette-generator/
stream_colors <- c("#fde725", "#b5de2b", "#6ece58", "#35b779", "#1f9e89",
                   "#26828e", "#31688e", "#3e4989", "#482878", "#440154")

#Custom format used to shorten trade value data
money_format <- function(x) {
  sapply(x, function(y) {
    if (is.na(y)) {
      return(NA)
    } else if (abs(y) >= 1e12) {
      paste(formatC(abs(x) / 1e12, format = "f", digits = 2), "T")
    } else if (abs(y) >= 1e9) {
      paste(formatC(abs(x) / 1e9, format = "f", digits = 2), "B")
    } else if (abs(y) >= 1e6) {
      paste(formatC(abs(x) / 1e6, format = "f", digits = 2), "M")
    } else if (abs(y) >= 1e3) {
      paste(formatC(abs(y) / 1e3, format = "f", digits = 2), "k")
    } else {
      as.character(abs(y))
    }
  })
}


#
#
#     SHINY APP DEFINITION
#
#


ui <- fluidPage(

  #Shiny App title
  titlePanel("Phenomenology of the Global Economy, 2000-2023"),

  tags$h3("Data sourced from UN Comtrade Database. Results are in $USD 
          and inflation is accounted for.", style = "text-align: left; font-size: 16px;"),
  tags$head(
    tags$style(HTML("
      .shiny-text-output {
        margin-top: 10px;
        margin-bottom: 10px;
        padding: 10px;
      }
    "))
  ),
  #Get inputs from side bar
  fluidRow(
    column(12,
           sidebarPanel(
             selectInput(inputId = "country",
                         label = "Country:",
                         choices = codelist,
                         selected = "USA"),
             sliderInput("primaryYear", "Year:",
                         2000, 2023, value = 2021, step = 1, sep = "")),
    column(6,
           radioButtons(inputId = "flow_type", "Flow direction:",
                        choices = c("Export", "Import"))),
    column(6,
           sliderInput("diffYears", "Years to Compare Trades Between",
                       2000, 2023, value = c(2000, 2023), sep = ""))

    ),

    mainPanel(plotlyOutput("mapOutput", width = "850px", height = "500px"),
              plotlyOutput("diffMap", width = "850px", height = "500px"),
              plotOutput("streamgraph", width = "850px", height = "500px"),
              br(),
              h3("\nCan we forecast trade volume with an ARIMA model?"),
              textOutput("arimaExplanation"),
              textOutput("modelType"),
              plotOutput("forecast", width = "750px", height = "400px"))

  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$mapOutput <- renderPlotly({
      
      #Grab trade data specific to a country and trade direction
      national_data <- filter(un_comtrade_yearly, FlowDesc == input$flow_type & 
                                ReporterISO == input$country & RefYear == input$primaryYear)
      
      # Check if filtered data is empty
      # if (nrow(national_data) == 0) {
      #   return("No data available for the selected criteria.")  
      # }
      
      #Use key-value pair data that we created earlier to fetch full name
      country_name <- unique_countries[unique_countries$ReporterISO == input$country,]$ReporterDesc
      
      #Calculate specific trade value
      national_data$hover_text <- paste(country_name,
                                        ifelse(input$flow_type == "Export", " Exports: $", " Imports: $"),
                                        sapply(national_data$PrimaryValue, money_format),
                                        ifelse(input$flow_type == "Export", " to ", " from "),
                                        national_data$PartnerDesc, sep = "")
      
      #Merge comtrade data with world map data
      trade_map_merged <- left_join(wmap, national_data, by = c("iso_a3_eh" = "PartnerISO"))
      
      
      #Generate plot title
      if (input$flow_type == "Export"){
        plot_title = paste("Exports from ", country_name, " to the World, ", 
                           toString(input$primaryYear), sep = "")
      } else {
        plot_title = paste("Imports to ", country_name, " from the World, ",
                           toString(input$primaryYear), sep = "")
      }
      
      
      #Scale trade value for a more readable map
      trade_map_merged$PrimaryValue <- log(trade_map_merged$PrimaryValue, base = exp(1))
      
      #
      # Plot choropleth map based on chosen country and trade direction
      #
      
      choropleth <- ggplot(data = trade_map_merged) +
          geom_sf(aes(fill = PrimaryValue, text = hover_text), color = "white", size = 0.1) +
          geom_sf(data = trade_map_merged[trade_map_merged$iso_a3_eh == input$country,],
                  fill = "black", color = "black", size = 1, alpha = 0.7) +
          coord_sf(crs = "+proj=robin") +
          scale_fill_viridis_c(option = "plasma", na.value = 'lightgrey',
                               guide = "none") +
          labs(title = plot_title) +
          theme_minimal()
      
      #Convert ggplot2 choropleth to plotly object
      plotly_choropleth <- ggplotly(choropleth, tooltip = "text")
      
      plotly_choropleth <- plotly_choropleth %>% layout(showlegend = FALSE) 
      
      plotly_choropleth
      
    })
    output$diffMap <- renderPlotly({
      #
      # Plot a choropleth which shows the difference in trade within the selected range
      #
      
      #Grab and rename year range
      start_year <- input$diffYears[1]
      end_year <- input$diffYears[2]
      
      #Filter data by country and flow direction
      tradediff_all <- filter(un_comtrade_yearly, FlowDesc == input$flow_type & ReporterISO == input$country)
      
      #Use key-value pair data that we created earlier to fetch full name
      country_name <- unique_countries[unique_countries$ReporterISO == input$country,]$ReporterDesc
      
      
      #Not all countries have all years represented: catch them if this is the case
      
      #First filter based on the start and end years, then give each year its own column,
      #then take the difference between those two columns.
      tradediff <- tradediff_all %>%
        filter(RefYear == start_year | RefYear == end_year) %>%
        pivot_wider(names_from = RefYear, values_from = PrimaryValue) %>%
        mutate(tradeValue = .data[[toString(end_year)]] - .data[[toString(start_year)]])
      
      #Generate plot title
      if (input$flow_type == "Export"){
        plot_title = paste("Change in Exports from ", country_name, " to the World, ",
                           toString(start_year), " to ", toString(end_year), sep = "")
      } else {
        plot_title = paste("Change in Imports to ", country_name, " from the World, ",
                           toString(start_year), " to ", toString(end_year), sep = "")
      }
        
      tradediff$ScaledValue <- sign(tradediff$tradeValue) * log1p(abs(tradediff$tradeValue))
      
      #FIX THIS:
      tradediff$hover_text <- paste(country_name,
                                        ifelse(input$flow_type == "Export", " exported $", " imported $"),
                                        sapply(tradediff$tradeValue, money_format),
                                        ifelse(tradediff$tradeValue >= 0, " more in ", " less in "),
                                        end_year, " than in ", start_year,
                                        ifelse(input$flow_type == "Export", " to ", " from "),
                                        tradediff$PartnerDesc, sep = "")
      
      #Merge comtrade data with world map data
      trade_map_merged <- left_join(wmap, tradediff, by = c("iso_a3_eh" = "PartnerISO"))
      
      choropleth_diff <- ggplot(data = trade_map_merged) +
        geom_sf(aes(fill = ScaledValue, text = hover_text), color = "white", size = 0.1) +
        geom_sf(data = trade_map_merged[trade_map_merged$iso_a3_eh == input$country,],
                fill = "black", color = "black", size = 1, alpha = 0.7) +
        coord_sf(crs = "+proj=robin") +
        #scale_fill_gradient2(low = "red", mid = "white", high = "green", guide = "none") +
        scale_fill_gradient2(low = "#c51b7d", mid = "white", high = "#4d9221", guide = "none") +
        labs(title = plot_title) +
        theme_minimal()
      
      #Convert ggplot2 choropleth to plotly object
      plotly_choropleth_diff <- ggplotly(choropleth_diff, tooltip = "text")
      
      plotly_choropleth_diff <- plotly_choropleth_diff %>% layout(showlegend = FALSE) 
      
      plotly_choropleth_diff
      
    })
    output$streamgraph <- renderPlot({
      
      #
      #The modified streamgraph is NOT rendered in Plotly, since the smooth
      #curvature of its output makes Plotly run too slowly, for too little
      #information gain. 
      #
      
      
      #Get full country name
      country_name <- unique_countries[unique_countries$ReporterISO == input$country,]$ReporterDesc
      
      #Filter yearly data to be for a given country.
      #We don't use the same data above since the map data contains
      #many extra variables that slow down computation. The economic
      #data is much lighter and easier to use.
      
      stream_data <- un_comtrade_yearly %>%
        filter(ReporterISO == input$country & FlowDesc == input$flow_type)
      
      #Find top 10 trading partners by total trade volume
      top_partners <- stream_data %>%
        group_by(PartnerDesc) %>%
        summarize(TotalTrades = sum(PrimaryValue, na.rm = TRUE)) %>%
        top_n(10, TotalTrades) %>%
        arrange(desc(TotalTrades)) %>%
        pull(PartnerDesc) #Get names of top 10 trading partners for given flow
      
      #Get yearly data for the top 10 partners
      yearly_trades <- stream_data %>%
        filter(PartnerDesc %in% top_partners) %>%
        mutate(PartnerDesc = factor(PartnerDesc, levels = top_partners)) %>%
        group_by(RefYear, PartnerDesc) %>%
        summarise(YearlyTrades = sum(PrimaryValue, na.rm = TRUE), .groups = 'drop')
      
      
      #Get title for streamgraph
      if (input$flow_type == "Export"){
        stream_title <- paste("Yearly Exports to ", country_name,
                              "'s Top 10 Export Partners", sep = "")
      } else {
        stream_title <- paste("Yearly Imports from ", country_name,
                              "'s Top 10 Import Partners", sep = "")
      }
      
      
      #Plot streamgraph of top trading partners in a given direction
      ggplot(data = yearly_trades,
              aes(RefYear, YearlyTrades, fill = PartnerDesc,
                  label = PartnerDesc, color = PartnerDesc)) +
          geom_stream(type = "ridge", bw=1) +
          scale_fill_manual(values = stream_colors, name = "Top 10 Trading Partners") +
          scale_color_manual(values = stream_colors, guide = "none") +
          scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
          theme_minimal() +
          labs(title = stream_title) +
          theme(plot.title = element_text(size = 20)) +
          xlab("Year") +
          ylab("Volume of Goods Traded ($USD)") +
          labs(caption = expression(bold("Data source:") ~ "UN Comtrade Database")) +
          theme(plot.caption = element_text(hjust = 0, face = "plain", color = "black"))
          
      
    })
    output$arimaExplanation <- renderText({
      paste("An in-depth forecast of future trade volume would require a number of",
            "covariates. It would attempt to forecast future supply and demand,",
            "and based on current events and trading relationships have goods meet",
            "their markets. But in our case, we only have trading history. Given",
            "historical data just of the imports and exports coming from a country",
            "is it possible to forecast future import and export volume? To test",
            "this hypothesis, we can fit an ARIMA model to our data.\n\n")
    })
    output$modelType <- renderText({
      
      #Prepare time series data
      ts_data <- un_comtrade_yearly %>%
        filter(ReporterISO == input$country, FlowDesc == input$flow_type) %>%
        group_by(RefYear) %>%
        summarize(TotalTradeValue = sum(PrimaryValue, na.rm = TRUE))
      
      set.seed(0)
      
      #Fit ARIMA(p,d,q) model
      arima_model <- auto.arima(ts_data$TotalTradeValue)
      
      #Grab name of ARIMA model
      arimasummary <- capture.output(summary(arima_model))
      
      model_order <- trimws(arimasummary[2])
      
      if (model_order == "ARIMA(0,1,0)") {
        "Our best-fit set of parameters for the ARIMA model gives us ARIMA(0,1,0). 
        This means that we have fit a random walk model, and our best guess for 
        future forecasts is that we will stay exactly where we are now. Of course,
        the future is not stationary. Take this model with a grain of salt."
      } else if (model_order == "ARIMA(0,1,0) with drift") {
        "Our best-fit set of parameters for the ARIMA model gives us ARIMA(0,1,0). 
        This means that we have fit a random walk model, and our best guess for 
        future forecasts is that we will stay exactly where we are now. Of course,
        the future is not stationary. Take this model with a grain of salt."
      } else if (model_order == "ARIMA(0,0,0) with zero mean") {
        "We have fit an ARIMA(0,0,0) model to our data. 
        Our model just represents white noise."
      } else {
        paste("Remarkably, we fit a", model_order, "model to our data.", 
              "We have discovered an autoregressive structure in the data,
              meaning that our predictions tell us more information than 
              if we were just playing with a random walk or white noise model.")
      }
      
      
    })
    output$forecast <- renderPlot({
      
      #Regenerate time series data
      ts_data <- un_comtrade_yearly %>%
        filter(ReporterISO == input$country, FlowDesc == input$flow_type) %>%
        group_by(RefYear) %>%
        summarize(TotalTradeValue = sum(PrimaryValue, na.rm = TRUE))
      
      #Grab full country name from key-value pair
      country_name <- unique_countries[unique_countries$ReporterISO == input$country,]$ReporterDesc
      
      set.seed(0)
      
      #Fit ARIMA(p,d,q) model
      arima_model <- auto.arima(ts_data$TotalTradeValue)
      
      #Based on the ARIMA(p,d,q) model we crafted above, forecast the
      #next 3 years of trading data
      forecasts <- forecast(arima_model, h=3)
      
      #Create title based on inputs
      fc_title <- paste("3-Year Forecast of ", country_name, " ",
                        input$flow_type, "s", sep = "")
      
      fc_scale <- paste("Yearly Total ", input$flow_type, "s ($USD)", sep = "")
      
      #Plot final forecast
      plot_forecast <- autoplot(forecasts) +
        labs(title = fc_title, x = "Year", y = fc_scale) + 
        scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
        theme_minimal() +
        annotate("text", x = Inf, y = forecasts$lower[3, 2], label = "95% CI", 
                 hjust = 1.1, vjust = -1, color = "blue") +
        annotate("text", x = Inf, y = forecasts$lower[3, 1], label = "80% CI", 
                 hjust = 1.1, vjust = -1, color = "red") +
        theme(plot.title = element_text(size = 20))
      
      plot_forecast
      
    })
}


#
#
#     RUN SHINY APP
#
#


# Run the application 
shinyApp(ui = ui, server = server)