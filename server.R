
# Libraries ------------------------------
library(dplyr)
library(tidyr)
library(plotly)
library(readxl)
library(jsonlite) # json to df
library(lubridate)
library(stringr)
library(leaflet) # maps
library(leaflet.extras) # heatmap

server <- function(input, output, session) {
  # Total carbon emission in transportation -----------------------------------------------
  ## Emission by fuel type ------------------------------------------------------------
  totalEnergy <- read.csv('./최종에너지_부문별_소비_수송_9701_2105.csv', fileEncoding = 'EUC-KR') %>%
  pivot_longer(-1, names_to = 'year_month')

  # Reformatting data frame
  names(totalEnergy) <- c('category', 'year_month', 'toe1000')
  totalEnergy$year_month <- sub('X', '', totalEnergy$year_month)
  totalEnergy$year_month <- sub('..', '-', totalEnergy$year_month, fixed = T)

  # Energy use change
  plot_energyUse <-
    totalEnergy %>%
  filter(category != '합계') %>%
    plot_ly(x = ~year_month, y = ~toe1000, type = 'scatter', mode = 'lines', color = ~category) %>%
  add_annotations(text = "1000TOE", xref = "paper", yref = "paper",
                  x = 0.02, xanchor = "right",
                  y = 1.1, yanchor = "top",
                  legendtitle = TRUE, showarrow = FALSE) %>%
  layout(xaxis = list(title = ""),
         yaxis = list(title = ""))

  output$plot_energyUse <- renderPlotly({ plot_energyUse })
}