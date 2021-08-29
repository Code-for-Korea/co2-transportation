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

# Total carbon emission in transportation -----------------------------------------------
## Emission by fuel type ------------------------------------------------------------
totalEnergy <- read.csv('data/최종에너지_부문별_소비_수송_9701_2105.csv', fileEncoding = 'EUC-KR') %>%
  pivot_longer(-1, names_to = 'year_month')

# Reformatting data frame
names(totalEnergy) <- c('category', 'year_month', 'toe1000')
totalEnergy$year_month <- sub('X', '', totalEnergy$year_month)
totalEnergy$year_month <- sub('..', '-', totalEnergy$year_month, fixed=T)

# Energy use change
plot_energyUse <- totalEnergy %>%
  filter(category != '합계') %>%
  plot_ly(x=~year_month, y=~toe1000, type = 'scatter', mode = 'lines', color = ~category) %>%
  add_annotations(text="1000TOE", xref="paper", yref="paper", 
                  x=0.02, xanchor="right",
                  y=1.1, yanchor="top",
                  legendtitle=TRUE, showarrow=FALSE) %>%
  layout(xaxis = list(title=""),
         yaxis = list(title=""))
plot_energyUse

# Calculate carbon emission
coefficient <- c("휘발유" = 2.179/781, "경유" = 2.596/903, "LPG" = 3.737/1519, "CNG" = NA, "전력" = 0.04594/22900)
totalEmission <- data.frame()
for ( fuel in names(coefficient) ) {
  df <- totalEnergy %>% filter(category == fuel)
  df$milCO2 <- df$toe1000 * coefficient[fuel]
  
  totalEmission <- rbind(totalEmission, df)
}

totalEmission$year <- as.numeric(substr(totalEmission$year_month, 1, 4))

# Annual change
sumEmission_yr <- totalEmission %>% group_by(year) %>% summarize(milCO2 = sum(milCO2, na.rm=T))
plot_totalEmission_yr <- plot_ly(sumEmission_yr, type = 'bar') %>%
  add_trace(x=c(2018, 2050), y=c(sumEmission_yr$milCO2[which(sumEmission_yr==2018)], 2.8), 
            type = 'scatter', mode = 'lines', line=list(color = 'green')) %>%
  add_trace(x=~year, y=~milCO2, type = 'bar', color = 'orange') %>%
  add_trace(x=2050, y=2.8, type='bar', color = 'green') %>%
  add_annotations(x=2050, y=10, text=2.8, showarrow=F, font=list(size=25, color='green')) %>%
  add_annotations(text="백만CO2", xref="paper", yref="paper", 
                  x=0.03, xanchor="right",
                  y=1, yanchor="top",
                  legendtitle=TRUE, showarrow=FALSE) %>%
  layout(font = list(family = "Source Sans Pro", size = 20),
         xaxis = list(title=""),
         yaxis = list(title=""),
         showlegend=F)
plot_totalEmission_yr

# Monthly change
sumEmission_m <- totalEmission %>% group_by(year_month) %>% summarize(milCO2 = sum(milCO2, na.rm=T))

plot_totalEmission <- sumEmission_m %>%
  plot_ly(x=~year_month, y=~milCO2, type = 'scatter', mode = 'line+marker') %>%
  add_annotations(text="백만CO2", xref="paper", yref="paper", 
                  x=0.03, xanchor="right",
                  y=1, yanchor="top",
                  legendtitle=TRUE, showarrow=FALSE) %>%
  layout(xaxis = list(title=""),
         yaxis = list(title=""))
plot_totalEmission

## Emission by sector -------------------------------------------------------
sectorUse <- data.frame()
for (sector in c('철도', '도로', '항공', '해운')) {
  dt <- read_xls(paste0('data/petronet/제품별산업별소비_', sector, '_201701_202107.xls'))
  dt <- dt[grepl('월', dt$월),]
  dt$year <- c(rep(2017, 12), rep(2018, 12), rep(2019, 12), rep(2020, 12), rep(2021, 7))
  dt$month <- c(rep(1:12, 4), 1:7)
  
  dt <- dt %>%
    select(-월, -제품, -ends_with('계')) %>%
    pivot_longer(-c(year, month), names_to = 'fuel', values_to = 'qnt_1000bbl')
  dt$sector <- sector
  dt$qnt_1000bbl <- as.numeric(dt$qnt_1000bbl)
  dt <- na.omit(dt)
  
  sectorUse <- rbind(sectorUse, dt)
}

sectorUse$year_month <- paste0(sectorUse$year, '.', str_pad(sectorUse$month, 2, 'left', '0'))

sectorUse_m <- sectorUse %>%
  group_by(year_month, sector) %>%
  summarize(qnt_1000bbl = sum(qnt_1000bbl))

# plot v.1 - stacked area
plot_sector <- sectorUse_m %>%
  plot_ly(x=~year_month, y=~qnt_1000bbl, color = ~sector, type = 'scatter', mode = 'none', 
          fill = 'tonexty', stackgroup='one')
plot_sector

# plot v.2 - stacked bar
plot_sector <- sectorUse_m %>%
  plot_ly(x=~year_month, y=~qnt_1000bbl, color = ~sector, type = 'bar') %>%
  layout(barmode= 'stack')
plot_sector

# plot v.3 - line
plot_sector <- sectorUse_m %>%
  plot_ly(x=~year_month, y=~qnt_1000bbl, color = ~sector, type = 'scatter', mode = 'line') %>%
  layout(xaxis = list(title=""),
         yaxis = list(title="1000 배럴"))
plot_sector

# plot - pie
plot_sector_pie <- sectorUse_m %>%
  filter(year_month == max(sectorUse_m$year_month)) %>%
  plot_ly(labels = ~sector, values = ~qnt_1000bbl) %>%
  add_pie(hole = 0.7) %>%
  add_annotations(text = format(sum(filter(sectorUse_m, year_month == max(sectorUse_m$year_month))$qnt_1000bbl), big.mark=','), 
                  showarrow = F, font=list(size=30))
plot_sector_pie

# Case: Seoul ------------------------------------
## Road traffic ----------------------
# Compile data
traffic <- data.frame()
for (file in list.files('data/Seoul_traffic/')) {
  df <- read_excel(paste0('data/Seoul_traffic/', file), sheet = 2) %>%
    select("일자", "요일", "지점명", "지점번호", "방향", "구분", "0시", "1시", "2시", "3시", "4시", "5시", "6시",
           "7시", "8시", "9시", "10시", "11시", "12시", "13시", "14시", "15시", "16시", "17시", "18시", "19시", "20시",
           "21시", "22시", "23시")
  traffic <- rbind(traffic, df)
}
traffic <- pivot_longer(traffic, ends_with('시'), names_to = 'hour')
traffic$일자 <- as.Date(as.character(traffic$일자), '%Y%m%d')
traffic$year_month <- format(traffic$일자, "%Y-%m")
traffic_avg <- traffic %>% group_by(year_month) %>% summarize(avg = mean(value, na.rm=T))
traffic_avg$year <- substr(traffic_avg$year_month, 1, 4)

# Calculate 2018 average (cars/hour)
traffic_2018 <- filter(traffic_avg, year == 2018) %>% arrange(date)
traffic_2018_avg <- mean(traffic_2018$avg)

# Calculate relative volume to 2018 by month
traffic_avg$rel_to_2018 <- traffic_avg$avg/traffic_2018$avg[as.numeric(format(traffic_avg$date, '%m'))]

# Average traffic chart
latestMonth <- tail(traffic_avg, 1)
diff_perc <- (latestMonth$rel_to_2018-1)*100
if (diff_perc > 0) {
  change <- '증가'
} else {
  change <- '감소'
}
summary <- paste0(format(latestMonth$date, '%Y'), '년 ', format(latestMonth$date, '%m'), '월 통행량: ',
                  format(round(latestMonth$avg, 0), big.mark=','), '대/시간\n',
                  '2018년 ', format(latestMonth$date, '%m'), '월 대비 <b>', round(abs(diff_perc), 1), '% ', change, '</b>')
traffic_avg$date <- as.Date(paste0(traffic_avg$year_month, "-01"), '%Y-%m-%d')
plot_traffic <- traffic_avg %>%
  plot_ly(x=~date, y=~avg, type = 'scatter', mode = 'lines') %>%
  add_lines(x=c('2018-01-01', '2050-01-01'), y=c(traffic_2018_avg, traffic_2018_avg*0.83)) %>%
  add_markers(x=latestMonth$date, y=latestMonth$avg, size = 10) %>%
  add_annotations(text = summary, font = list(size = 20), color = 'green',
                  x=latestMonth$date, xanchor="left",
                  y=latestMonth$avg, yanchor="bottom",
                  arrowhead = 0, ax = 50, ay = -50, arrowcolor = "green",
                  legendtitle=TRUE, showarrow=T) %>%
  layout(showlegend = F,
         xaxis = list(title=""),
         yaxis = list(title="구간평균 통행량(차량수/시간)"))
plot_traffic

### Heat maps ---------------------------
roads <- read_excel('data/Seoul_traffic/서울시_지점별일자별교통량_202106.xlsx', sheet = 3, col_types = 'text')
dt <- traffic %>% filter(year_month == max(year_month)) %>% group_by(id = 지점번호) %>% 
  summarize(value = sum(value, na.rm=T)) %>%
  left_join(select(roads, id = 지점번호, name = 지점명칭, lat = 위도, long = 경도))
dt$lat <- as.numeric(dt$lat)
dt$long <- as.numeric(dt$long)
dt$scale <- scale(dt$value)

dt %>%
  leaflet() %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addTiles() %>%
  addHeatmap(lng=~long, lat=~lat, intensity = ~scale, 
             blur=20, max=2, radius=20, minOpacity = 0.5) %>%
  addCircleMarkers(radius = 10, stroke = F, popup = ~value, label = ~name, fillOpacity = 0) %>% # invisible
  addCircleMarkers(radius = 5) # visible
  
binpal <- colorBin("Spectral", dt$value, 6, pretty = FALSE, reverse = T)
dt %>%
  leaflet() %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addCircleMarkers(lng=~long, lat=~lat, color = ~binpal(value), stroke = T, 
                   radius = 5, opacity = 0.2, fillOpacity = 0.7,
                   popup = ~value, label = ~name) %>%
  addProviderTiles(providers$CartoDB.Positron) #Stadia.AlidadeSmooth

## Bus -----------------------------------------------
apikey <- '4c4b4a767377696c36316771766766'
date <- '20200101'
url <- paste0('http://openapi.seoul.go.kr:8088/', apikey, '/json/CardBusStatisticsServiceNew/1/5/', date)
dt <- fromJSON(url)

bus <- data.frame()
for (file in list.files('data/Seoul_bus/')) {
  df <- read.csv(paste0('data/Seoul_bus/', file), fileEncoding = 'EUC-KR')
  bus <- rbind(bus, df)
}
bus$사용일자 <- as.Date(as.character(bus$사용일자), '%Y%m%d')
sum(is.na(bus)) # check NA

bus$year_month <- format(bus$사용일자, '%Y-%m')
bus_sum <- bus %>% group_by(year_month) %>% summarize(on = sum(승차총승객수), days = length(unique(사용일자)))
bus_sum$on_day <- bus_sum$on/bus_sum$days

bus_2018 <- bus_sum %>% filter(year_month < '2019-01-01')

plot_bus <- bus_sum %>%
  plot_ly(x=~year_month, y=~on_day/10^6, type = 'scatter', mode= 'lines+markers',
          marker = list(size=10)) %>%
  layout(xaxis = list(title=""),
         yaxis = list(title="평균 일일승차수(백만)"))
plot_bus

## metro -------------------------------------
metro <- data.frame()
cols <- c("호선", "역번호", "역명", 'month', 'num')
for (file in list.files('data/Seoul_metro/')) {
  df <- read.csv(paste0('data/Seoul_metro/', file), fileEncoding = 'EUC-KR') %>%
    pivot_longer(starts_with('X'), names_to = 'month', values_to = 'num')
  names(df) <- cols
  
  df$year <- substr(file, 15, 18)
  metro <- rbind(metro, df)
}

metro$month <- metro$month %>%
  str_replace_all(c('X' = '', '월' = '')) %>%
  str_pad(2, side='left', 0)

metro_avg <- metro %>% group_by(year, month) %>% summarize(sum = sum(num, na.rm = T)) %>% ungroup()
metro_avg$year_month <- paste0(metro_avg$year, '-', metro_avg$month)


plot_metro <- metro_avg %>%
  plot_ly(x=~year_month, y=~sum/10^6, type = 'scatter', mode= 'lines+markers',
          marker = list(size=10)) %>%
  layout(xaxis = list(title=""),
         yaxis = list(title="월별수송인원(백만)"))
plot_metro

## Bicycle ------------------------
bike_raw <- data.frame()
for (file in list.files('data/Seoul_bike/')) {
  df <- read.csv(paste0('data/Seoul_bike/', file), fileEncoding = 'EUC-KR')
  bike_raw <- rbind(bike_raw, df)
}

# Data cleaning- later
ind <- which(str_length(bike_raw$대여일시) != 19)
bike_error <- bike_raw[ind,]

bike <- bike_raw %>% filter(str_length(대여일시) == 19)
bike$이용거리 <- as.numeric(bike$이용거리)
bike$이용시간 <- as.numeric(bike$이용시간)

bike <- bike %>% na.omit() %>% filter(이용거리 > 0, 이용시간 > 0)

# Calculate monthly sum
bike$대여일시 <- ymd_hms(bike$대여일시)
bike$year_month <- paste0(year(bike$대여일시), '-', str_pad(month(bike$대여일시), 2, 'left', 0))
bike_avg <- bike %>% group_by(year_month) %>% 
  summarize(count = n(), time_hour = sum(이용시간/60), distance_km = sum(이용거리/10^3))

plot_bike <- bike_avg %>%
  plot_ly(x=~year_month, y=~count, type = 'scatter', mode= 'lines+markers',
          marker = list(size=10)) %>%
  layout(xaxis = list(title=""),
         yaxis = list(title="이용건수"))
plot_bike

plot_bike_dist <- bike_avg %>%
  plot_ly(x=~year_month, y=~time_hour, name = '이용시간(시간)', type = 'bar', 
          marker = list(color = 'purple')) %>%
  add_trace(y=~distance_km, name = '이용거리(km)', type = 'scatter', mode = 'lines+markers', 
            marker = list(color = 'green', size = 10),
            line = list(color = 'green'),
            yaxis = "y2") %>%
  layout(xaxis = list(title=""),
         yaxis = list(title="<b>이용시간(시간)</b>", range = c(0, max(bike_avg$time_hour)),
                      titlefont = list(color = 'purple')),
         yaxis2 = list(title = "<b>이용거리(km)</b>", range = c(0, max(bike_avg$distance_km)), 
                       overlaying = "y", side = "right",
                       titlefont = list(color = 'green')))
plot_bike_dist

# Hot spot maps -------------------------------------------------------------------------

