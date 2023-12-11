# install.packages('rsconnect')
#library(rsconnect)
#rsconnect::deployApp('Shiny App - Final Project .R')

library(gridExtra)
library(knitr)
library(tidyverse)
library(ggplot2)
library(kableExtra)
library(tidyverse)
library(stringr)
library(openxlsx)
library(shiny)
library(leaflet)






# Data used
sey <- read.xlsx("sey_data.xlsx")
transposed_data <- t(sey)
transposed_df <- as.data.frame(transposed_data)
colnames(transposed_df) <- transposed_df[1, ]
transposed_df <- transposed_df[-1, ]
transposed_df <- transposed_df %>%
  mutate_all(as.numeric)
sey_df <- transposed_df %>%
  mutate(year = rownames(transposed_df))
sey_df$year <- as.numeric(as.character(sey_df$year))
naomit <- na.omit(sey_df[, c("year", "migrant_popu_proportion")])
naomit2 <- na.omit(sey_df[, c("year", "Human_capital_index_upper_bound")])
naomit3 <- na.omit(sey_df[, c("year", "GDP")])
naomit4 <- na.omit(sey_df[, c("year", "GDP_growth_annual")])

Seychelles <- read.xlsx("Seychelles.xlsx")


# the shiny app
ui <- fluidPage(
  titlePanel("Exploring Seychelles: A Tropical Paradise Unveiled"),
  
  tabsetPanel(
    tabPanel("The icon and map of Seychelles",
             verbatimTextOutput("icon_note"),
             div(
               img(src = "seychelles-icon.png", height = 360, width = 360),
               style = "display: flex; justify-content: center;"
             ),
             verbatimTextOutput("sey_map_note"),
             div(
               img(src = "map.png", height = 700, width = 900),
               style = "display: flex; justify-content: center;"
             )
    ),
    
    tabPanel("Where are Seychelles and Its Neighbors",
             fluidPage(
               leafletOutput("seychelles_map"),
               verbatimTextOutput("map_note"),
               verbatimTextOutput("map_note2"),
               verbatimTextOutput("map_note3"),
               verbatimTextOutput("map_note4")
             )
    ),
    
    tabPanel("Development of Seychelles",
             fluidPage(
               selectInput("section", "Population, Economy, Society and Environment", choices = c("Population", "Economy", "Society and Environment")),
               uiOutput("plot_selector"),
               plotOutput("selected_plot")
             )
    ),
    
    tabPanel("SWOT Analysis for Seychelles",
             fluidPage(
               tableOutput("swot_table"),
               verbatimTextOutput("swot_note"),
               div(
                 img(src = "SWOT.png", height = 450, width = 900),
                 style = "display: flex; justify-content: center;"
               )
               
             )
    ),
    
    tabPanel("Travel to Seychelles",
             fluidPage(
               tableOutput("Seychelles_visit"),
               div(
                 img(src = "sey photo1.png", height = 400, width = 700),
                 style = "display: flex; justify-content: center;"
               ),
               div(
                 img(src = "sey photo2.png", height = 400, width = 700),
                 style = "display: flex; justify-content: center;"
               ),
               div(
                 img(src = "best time to visit.png", height = 400, width = 700),
                 style = "display: flex; justify-content: center;"
               ),
               div(
                 img(src = "climate.png", height = 250, width = 900),
                 style = "display: flex; justify-content: center;"
               )
             )
    ),
    
    tabPanel("Comparison Between Seychelles and Its Neighbors",
             fluidPage(
               tableOutput("country_comparison_table")
             )
    ),
    
    tabPanel("Main Data Origin",
             fluidPage(
               tableOutput("references_table"),
               verbatimTextOutput("data_origin_note"),
               div(
                 img(src = "xinxin1.png", height = 270, width = 300),
                 style = "display: flex; justify-content: center;"
               )
             )
    )
  )
)





server <- function(input, output) {
  
  output$icon_note <- renderPrint({
    cat("The icon of Seychelles:")
  })
  
  output$sey_map_note <- renderPrint({
    cat("The map of Seychelles:")
  })
  
  output$swot_note <- renderPrint({
    cat("SWOT Analysis Mind Map:")
  })
  
  output$seychelles_map <- renderLeaflet({
    leaflet() %>%
      setView(-4.6796, 55.4920, zoom = 2) %>%  
      addTiles() %>%  
      addMarkers(lng = 55.4920, lat = -4.6796, popup = "Seychelles")%>%
      addMarkers(lng = 43.3333, lat = -11.6455, popup = "Neighbor1: Comoros") %>%
      addMarkers(lng = 57.5522, lat = -20.3484, popup = "Neighbor2: Mauritius") %>%
      addMarkers(lng = 55.5364, lat = -21.1151, popup = "Neighbor3: Reunion")
  })
  
  output$map_note <- renderPrint({
    cat("Seychelles is located in the Atlantic, Indian Ocean, and South China Sea (AIS) (IOC) region.")
  })
  
  output$map_note2 <- renderPrint({
    cat("The main neighboring countries are Comoros, Mauritius, and Reunion.")
  })
  
  output$map_note3 <- renderPrint({
    cat("Please click on the markers to view the country names.")
  })
  
  output$map_note4 <- renderPrint({
    cat("Feel free to zoom in and explore the areas of interest.")
  })
  
  output$plot_selector <- renderUI({
    section <- input$section
    
    if (section == "Population") {
      choices <- c(
        "Total Population Change",
        "Female Percentage",
        "Old Population Proportion",
        "Migrant Population Proportion",
        "Human Capital Index"
      )
    } else if (section == "Economy") {
      choices <- c(
        "Adjusted Net National Income Per Capita (US$)",
        "GDP",
        "GDP Growth Annual",
        "central Government Debt",
        "Total Tax Revenue",
        "Consumer Price Index",
        "Export Value Index",
        "Import Value Index",
        "Exports of Goods and Services",
        "Imports of Goods and Services",
        "Foreign Direct Investment (% of GDP)"
      )
    } else if (section == "Society and Environment") {
      choices <- c(
        "CO2 Emissions",
        "Renewable Energy Consumption Proportion",
        "Individuals Access to Electricity Proportion",
        "Individuals Using the Internet Proportion",
        "Young Percentage of Working Age Population",
        "Old Percentage of Working Age Population",
        "Unemployment Proportion"
      )
    }
    
    selectInput("plot_selector", "Please choose the indicators that interest you", choices = choices)
  })
  
  selected_plot <- reactive({
    section <- input$section
    plot_selector <- input$plot_selector
    
    if (section == "Population") {
      switch(
        plot_selector,
        "Total Population Change" = ggplot(data = sey_df, aes(x = year, y = population)) +
          geom_line(aes(group = 1), color = "blue") +  
          geom_point(aes(color = "pink")) +
          labs(
            title = "The total population in Seychelles",
            x = "Year",
            y = "Total Population"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Female Percentage" = ggplot(data = sey_df, aes(x = year, y = female_percentage)) +
          geom_line(aes(group = 1), color = "blue") + 
          geom_point(aes(color = "pink")) +
          labs(
            title = "The annual changes in the proportion of females in the total population",
            x = "Year",
            y = "Proportion of Females (%)"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)) +
          geom_hline(yintercept = 50, linetype = "dashed", color = "red"),
        "Old Population Proportion" = ggplot(data = sey_df, aes(x = year, y = old_popu_proportion)) +
          geom_line(aes(group = 1), color = "blue") +
          geom_point(color = "pink") + 
          labs(
            title = "The changing trend of population aging over the years",
            x = "Year",
            y = "Proportion of the Population Aged 65 and Above"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Migrant Population Proportion" = ggplot(data = naomit, aes(x = year, y = migrant_popu_proportion)) +
          geom_line(aes(group = 1), color = "blue") +
          geom_point(color = "pink") +  
          labs(
            title = "The changes in the proportion of migrants",
            x = "Year",
            y = "Proportion of the Migrants (%)"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Human Capital Index" = ggplot(data = naomit2, aes(x = year, y = Human_capital_index_upper_bound)) +
          geom_line(aes(group = 1), color = "blue") +
          geom_point(color = "pink") +  
          labs(
            title = "The changes in the level of human capital development from 2010",
            x = "Year",
            y = "Human Capital Index (upper bound)"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5))
      )
    } else if (section == "Economy") {
      switch(
        plot_selector,
        "Adjusted Net National Income Per Capita (US$)" = ggplot(data = Seychelles, aes(x = year, y = adjusted_net_national_income_per_capita_Usdollar)) +
          geom_line(aes(group = 1), color = "purple") + 
          geom_point(aes(color = "pink")) +
          labs(
            title = "The annual changes in the adjusted net national income per capita (US$)",
            x = "Year",
            y = "Adjusted Net National Income Per Capita (US$))"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "GDP" = ggplot(data = naomit3, aes(x = year, y = GDP)) +
          geom_line(aes(group = 1), color = "blue") +
          geom_point(color = "pink") +  
          labs(
            title = "The value of GDP over years",
            x = "Year",
            y = "GDP"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "GDP Growth Annual" = ggplot(data = naomit4, aes(x = year, y = GDP_growth_annual)) +
          geom_line(aes(group = 1), color = "blue") +
          geom_point(color = "pink") +  
          labs(
            title = "The GDP annual growth over years",
            x = "Year",
            y = "GDP Annual Growth (%)"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5))+
          geom_hline(yintercept = 0, linetype = "dashed", color = "red"),
        "central Government Debt" = ggplot(data = sey_df, aes(x = year, y = central_government_debt_billions)) +
          geom_line(aes(group = 1), color = "purple") +
          geom_point(color = "pink") +
          labs(
            title = "The annual changes in the central government debt (billions)",
            x = "Year",
            y = "Central Government Debt (billions)"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Total Tax Revenue" = ggplot(data = sey_df, aes(x = year, y = total_tax_revenue)) +
          geom_line(aes(group = 1), color = "purple") +
          geom_point(color = "pink") +
          labs(
            title = "The annual changes in the total tax revenue",
            x = "Year",
            y = "Total Tax Revenue"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Consumer Price Index" = ggplot(data = sey_df, aes(x = year, y = Consumer_price_index)) +
          geom_line(aes(group = 1), color = "blue") +
          geom_point(color = "pink") +  
          labs(
            title = "The changes in the level of inflation over years",
            x = "Year",
            y = "Consumer Price Index"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Export Value Index" = ggplot(data = Seychelles, aes(x = year, y = export_value_index)) +
          geom_line(aes(group = 1), color = "purple") + 
          geom_point(aes(color = "pink")) +
          labs(
            title = "The annual changes in the export value index (2015=100)",
            x = "Year",
            y = "Export Value Index"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Import Value Index" = ggplot(data = Seychelles, aes(x = year, y = import_value_index)) +
          geom_line(aes(group = 1), color = "purple") + 
          geom_point(aes(color = "pink")) +
          labs(
            title = "The annual changes in the import value index (2015=100)",
            x = "Year",
            y = "Import Value Index"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Exports of Goods and Services" = ggplot(data = sey_df, aes(x = year, y = exports_of_goods_and_services)) +
          geom_line(aes(group = 1), color = "purple") +
          geom_point(color = "lightblue") +  
          labs(
            title = "The annual changes in the exports of goods and services",
            x = "Year",
            y = "Exports of Goods and Services"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Imports of Goods and Services" = ggplot(data = sey_df, aes(x = year, y = imports_of_goods_and_services)) +
          geom_line(aes(group = 1), color = "purple") +
          geom_point(color = "lightblue") +
          labs(
            title = "The annual changes in the imports of goods and services",
            x = "Year",
            y = "Imports of Goods and Services"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Foreign Direct Investment (% of GDP)" = ggplot(data = sey_df, aes(x = year, y = foreign_direct_investment_percentage_of_GDP)) +
          geom_line(aes(group = 1), color = "purple") +
          geom_point(color = "pink") +
          labs(
            title = "The annual changes in the foreign direct investment (% of GDP)",
            x = "Year",
            y = "Foreign Direct Investment (% of GDP)"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5))
      )
    } else if (section == "Society and Environment") {
      switch(
        plot_selector,
        "CO2 Emissions" = ggplot(data = sey_df, aes(x = year, y = CO2_emissions_kt)) +
          geom_line(aes(group = 1), color = "purple") +
          geom_point(color = "lightblue") +
          labs(
            title = "The annual changes in the CO2 emissions(kt)",
            x = "Year",
            y = "CO2 Emissions (kt)"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Renewable Energy Consumption Proportion"= ggplot(data = sey_df, aes(x = year, y = renewable_energy_consumption_proportion)) +
          geom_line(aes(group = 1), color = "purple") +
          geom_point(color = "lightblue") +
          labs(
            title = "The annual changes in the renewable energy consumption proportion",
            x = "Year",
            y = "Proportion (%)"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Individuals Access to Electricity Proportion" = ggplot(data = sey_df, aes(x = year, y = access_to_electricity_proportion)) +
          geom_line(aes(group = 1), color = "blue") +
          geom_point(color = "lightblue") +
          labs(
            title = "The annual changes in the access-to-electricity population proportion",
            x = "Year",
            y = "Proportion (%)"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Individuals Using the Internet Proportion" = ggplot(data = sey_df, aes(x = year, y = individuals_using_the_Internet_proportion)) +
          geom_line(aes(group = 1), color = "blue") +
          geom_point(color = "lightblue") +
          labs(
            title = "The annual changes in the individuals using the Internet proportion",
            x = "Year",
            y = "Proportion (%)"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Young Percentage of Working Age Population" = ggplot(data = sey_df, aes(x = year, y = young_percentage_of_working_age_popu)) +
          geom_line(aes(group = 1), color = "blue") +
          geom_point(color = "lightblue") + 
          labs(
            title = "The annual changes in the proportion of young laborers within the total labor force",
            x = "Year",
            y = "Proportion of Young Laborers (%)"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Old Percentage of Working Age Population" = ggplot(data = sey_df, aes(x = year, y = old_percentage_of_working_age_popu)) +
          geom_line(aes(group = 1), color = "blue") +
          geom_point(color = "lightblue") + 
          labs(
            title = "The annual changes in the proportion of old laborers within the total labor force",
            x = "Year",
            y = "Proportion of Old Laborers (%)"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5)),
        "Unemployment Proportion" = ggplot(data = sey_df, aes(x = year, y = unemployment_prop)) +
          geom_line(aes(group = 1), color = "blue") +
          geom_point(color = "lightblue") +
          labs(
            title = "The annual changes in the proportion of unemployment population",
            x = "Year",
            y = "Proportion of Unemployment Population (%)"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
          scale_x_continuous(breaks = seq(1960, 2022, by = 5))
      )
    }
  })
  
  output$selected_plot <- renderPlot({
    print(selected_plot())
  })
  output$country_comparison_table <- renderTable({
    
    country_comparison_data <- data.frame(
      Aspect = c("Geographical Location", "Area (sq km)", "Population", "Population Density", "GDP (USD)", "GDP per Capita (USD)", 
                 "Main Economic Sectors", "Government Type", "Capital", "Capital Longitude", "Capital Latitude", "Average Age", 
                 "Male-Female Ratio", "Language", "Climate", "Average Temperature (C)", "Average Precipitation (mm)", 
                 "International Trade", "Inflation Rate", "Environmental Pollution", "Unemployment Rate", "Crime Rate"),
      Seychelles = c("Indian Ocean West", 459, 98823, 215, 1.754e9, 17700, "Services, Tourism", "Republic", "Victoria", 55.45, -4.62, 33, 0.96, "Seychellois Creole, English, French", "Tropical", 27.5, 2100, "Tourism, Fishing", 2.5, "Low", 4.2, 20.5),
      Comoros = c("Northeast Africa", 2034, 896000, 440, 1.219e9, 1360, "Agriculture, Fishing, Services", "Federal Republic", "Moroni", 43.26, -11.69, 19, 1.02, "Comorian, Arabic, French", "Tropical", 26.7, 1000, "Agriculture, Fishing", 1.8, "Moderate", 8.81, 7.6),
      Mauritius = c("Indian Ocean West", 2040, 1268315, 623, 2.748e9, 21660, "Services, Industry, Agriculture", "Democracy", "Port Louis", 57.49, -20.09, 37, 1.02, "English, French, Creole", "Tropical", 23.6, 1200, "Textiles, Sugar, Tourism", 3.5, "Moderate", 6.9, 15.6),
      Reunion = c("Indian Ocean West", 2512, 859959, 343, 2.082e7, 27793, "Agriculture, Fishing, Tourism", "Metropolitan France", "Saint-Denis", 55.47, -20.87, 34.1, 1.07, "French", "Tropical", 24.5, 1541, "Sugar, Rum, Handicrafts, Flower oil extraction", 1.44, "Moderate", 29.68, 17.9)
    )
    return(country_comparison_data)
    
    
  }, rownames = FALSE)
  
  output$Seychelles_visit <- renderTable({
    Seychelles_visit <- data.frame(
      Tips = c("multi-ethnics", 
               "Languages", 
               "Airport", 
               "Climate",
               "Best Time to Visit",
               "Popular Destinations",
               "Popular Add-ons"),
      Content = c("The multi-ethnic roots of the Seychelles people stretch far and wide, including France, India, the UK, China, the Arab world, and other parts of Africa.",
                  "There are three official languages in the Seychelles: Creole, English, and French. The most common language spoken by the Seychellois population is their own Creole variant, Seychellois Creole, or Seselwa. ",
                  "All international flights arrive at Seychelles International Airport (SEZ), 8km/5mi south of Victoria on Mahé.",
                  "Seychelles has a tropical climate with high humidity. Temperatures don’t fluctuate much throughout the year. Afternoons are around 30°C/86°F and nights are cooler at around 24°C/75°F. Dry Season: May to September, and Wet Season: October to April.",
                  "March, April, October and November are recommended. (less windy, not too busy)",
                  "Main islands: (1) Mahé for experiencing the hustle and bustle of Victoria, walking in Morne Seychellois National Park and some beach time at Beau Vallon; (2) Praslin for seeing coco de mer palms at Vallée de Mai, snorkeling at Ile Cocos and sunbathing on Anse Lazio; (3) La Digue for spending time on picture-perfect beaches Anse Source d’Argent and Grand Anse.",
                  "(1) Bird Island for seeing colonies of sooty terns and nesting turtles; (2) Cousin Island for spotting Seychelles warblers and other endemic birds; (3) Curieuse Island for walking along coastal trails and seeing giant tortoises."
      )
    )
    
    return(Seychelles_visit)
  }, rownames = FALSE)
  
  output$swot_table <- renderTable({
    swot_data <- data.frame(
      SWOT = c("Strength", 
               "Weaknesses", 
               "Opportunities", 
               "Threats"),
      Description = c("(1) Thriving Tourism Industry: The scenery is beautiful, with exotic charm and natural landscapes that captivate people.   (2) High Economic Freedom: has a relatively open market economy, being friendly to foreign investments, promoting economic growth.  (3) Political Stability: The country has maintained political and social stability over the years, providing favorable conditions for development.   (4) Cultural Richness: The cultural heritage of Seychelles, influenced by African, European, and Asian traditions, contributes to a vibrant and diverse society.",
                      "(1) Dependency on Imported Energy: largely depends on imported fossil fuels for energy, making it susceptible to price fluctuations in the global energy market.  (2) Inconvenient Transportation: The geographical nature of being an island nation results in relatively inconvenient transportation, posing challenges to trade and logistics.   (3) Limited Resources: Despite abundant natural resources, the small land area of Seychelles imposes restrictions on resource utilization.",
                      "(1) Promotion of Technological Innovation: Investing in technology and innovation can enhance other sectors of the economy, reducing dependence on traditional industries.   (2) Marine Resource Utilization: Seychelles can explore sustainable ways to utilize its vast marine resources, such as fisheries and aquaculture.   (3) Sustainable Development: Seychelles can enhance its attractiveness by promoting sustainable tourism and environmental initiatives.   (4) Marine Resource Utilization: Seychelles can explore sustainable ways to utilize its vast marine resources, such as fisheries and aquaculture.",
                      "(1) Global Competitiveness: Increasing competition in the global tourism market may impact Seychelles' market share and revenue.  (2) Global Economic Uncertainty: Fluctuations and uncertainties in the global economy may adversely affect Seychelles' tourism industry and international trade.  (3) Impact of Climate Change: Climate change may result in rising sea levels and natural disasters, posing a threat to Seychelles' island ecosystems and economy.   (4) Geopolitical Risks: Tensions in geopolitics could negatively impact the security and stability of the region."
      )
    )
    
    return(swot_data)
  }, rownames = FALSE)
  
  output$references_table <- renderTable({
    references_data <- data.frame(
      DataOrigin = c("UN data", "World Bank data (finances)", "IMF data", "Seychelles Meteorological Authority", "Seychelles Climate | Weather & When to Go", "National Bureau of Statistics Seychelles"),  
      Link = c("https://data.un.org/", "https://data.worldbank.org/", "https://www.imf.org/en/Data", "https://www.meteo.gov.sc/#/", "https://www.seyvillas.com/en/guide/at-a-glance/climate", "https://www.nbs.gov.sc/")  
    ) 
    
    references_data$Link <- sprintf('<a href="%s" target="_blank">%s</a>', references_data$Link, references_data$Link)
    
    references_data
  }, sanitize.text.function = function(x) x, escape = FALSE, rownames = FALSE)
  
  
  
  output$data_origin_note <- renderPrint({
    cat("In the process of creating this shiny app, I would like to express gratitude not only to the aforementioned data references and the  Professor Haviland's course but also to the book 'R for Data Science' and Aidan's help.")
  })
  
  
}

shinyApp(ui, server)

