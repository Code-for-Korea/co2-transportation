# Libraries ------------------------------
library(dplyr)
library(tidyr)
library(plotly)
library(readxl)
library(lubridate)
library(stringr)
library(leaflet) # maps
library(leaflet.extras) # heatmap

# Total carbon emission in transportation -----------------------------------------------
## Emission by fuel type ------------------------------------------------------------
totalEnergy <- read.csv('data/최종에너지_부문별_소비_수송_9701_2105.csv', fileEncoding = 'EUC-KR') %>%
  pivot_longer(-1, names_to = '연월')

# Reformatting data frame
names(totalEnergy) <- c('category', '연월', 'toe1000')
totalEnergy$연월 <- sub('X', '', totalEnergy$연월)
totalEnergy$연월 <- sub('..', '-', totalEnergy$연월, fixed=T)

# Energy use change
plot_energyUse <- totalEnergy %>%
  filter(category != '합계') %>%
  plot_ly(x=~연월, y=~toe1000, type = 'scatter', mode = 'lines', color = ~category) %>%
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

totalEmission$year <- as.numeric(substr(totalEmission$연월, 1, 4))
totalEmission$month <- as.numeric(substr(totalEmission$연월, 6, 7))

# Annual change
sumEmission_yr <- totalEmission %>% group_by(year) %>% summarize(milCO2 = sum(milCO2, na.rm=T))

# Calculate emission targets
ref <- sumEmission_yr$milCO2[which(sumEmission_yr==2018)]
target <- 2.8
emissionTargets <- data.frame(year = c(2018:2050), 
                              target_milCO2 = seq(from=ref, to=2.8, length.out= (2050-2017)))

# Monthly status
latestYear <- max(totalEmission$year)
latestMonth <- str_sub(max(totalEmission$연월), 6, 7)
targetThisYear <- emissionTargets$target_milCO2[which(emissionTargets$year == latestYear)]

dt <- totalEmission %>% filter(year == latestYear) %>%
  group_by(month) %>%
  summarize(milCO2 = sum(milCO2, na.rm=T))
dt$month <- as.character(dt$month)

emissionThisYear <- sum(dt$milCO2, na.rm=T)

plot_emissionNow <- plot_ly(dt, y='', x=~milCO2, color=~month, type='bar',
                            colors = 'Reds',
                            text=~month,textposition='inside',
                            insidetextfont = list(color = "black", size = 20)) %>%
  layout(barmode = "stack",
         showlegend = F,
         xaxis = list(showgrid=F, zeroline=F),
         shapes = list(type = "line", 
                        x0 = targetThisYear, 
                        x1 = targetThisYear, 
                        yref = "paper",
                        y0 = 0, 
                        y1 = 1, 
                        line = list(width=5, color = 'green')))
plot_emissionNow

# Compare emission with last year
emissionLastYear <- totalEmission %>% 
  filter(year == latestYear-1, month <= as.numeric(latestMonth)) %>%
  summarize(milCO2 = sum(milCO2, na.rm=T)) %>%
  pull()

emissionChange <- (emissionThisYear - emissionLastYear)/emissionLastYear*100

if(emissionChange > 0.5) {
  status <- 'bad'
} else if (emissionChange < -0.5) {
  status <- 'good'
} else {
  status <- 'medium'
}

## Check if the last year is complete
if (latestMonth != '12') {
  dt <- sumEmission_yr %>% filter(year < max(totalEmission$year))
} else {
  dt <- sumEmission_yr
}

plot_totalEmission_yr <- plot_ly(dt, type = 'bar') %>%
  add_trace(x=c(2018, 2050), y=c(sumEmission_yr$milCO2[which(sumEmission_yr==2018)], 2.8), 
            type = 'scatter', mode = 'lines', line=list(color = 'green')) %>%
  add_trace(x=~year, y=~milCO2, type = 'bar') %>%
  add_trace(x=2050, y=2.8, type='bar', color = 'green') %>%
  add_annotations(x=2050, y=10, text=2.8, showarrow=F, font=list(size=25, color='green')) %>%
  add_annotations(text="백만톤CO2", xref="paper", yref="paper", 
                  x=0.03, xanchor="right",
                  y=1, yanchor="top",
                  legendtitle=TRUE, showarrow=FALSE) %>%
  layout(font = list(family = "Source Sans Pro", size = 20),
         xaxis = list(title=""),
         yaxis = list(title=""),
         showlegend=F)
plot_totalEmission_yr

# Monthly change
sumEmission_m <- totalEmission %>% group_by(연월) %>% summarize(milCO2 = sum(milCO2, na.rm=T))

plot_totalEmission <- sumEmission_m %>%
  plot_ly(x=~연월, y=~milCO2, type = 'scatter', mode = 'line+marker') %>%
  add_annotations(text="백만톤CO2", xref="paper", yref="paper", 
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

sectorUse$연월 <- paste0(sectorUse$year, '.', str_pad(sectorUse$month, 2, 'left', '0'))

sectorUse_m <- sectorUse %>%
  group_by(연월, sector) %>%
  summarize(qnt_1000bbl = sum(qnt_1000bbl))

# plot v.1 - stacked area
plot_sector <- sectorUse_m %>%
  plot_ly(x=~연월, y=~qnt_1000bbl, color = ~sector, type = 'scatter', mode = 'none', 
          fill = 'tonexty', stackgroup='one')
plot_sector

# plot v.2 - stacked bar
plot_sector <- sectorUse_m %>%
  plot_ly(x=~연월, y=~qnt_1000bbl, color = ~sector, type = 'bar') %>%
  layout(barmode= 'stack')
plot_sector

# plot v.3 - line
plot_sector <- sectorUse_m %>%
  plot_ly(x=~연월, y=~qnt_1000bbl, color = ~sector, type = 'scatter', mode = 'line') %>%
  layout(xaxis = list(title=""),
         yaxis = list(title="1000 배럴"))
plot_sector

# plot - pie
plot_sector_pie <- sectorUse_m %>%
  filter(연월 == max(sectorUse_m$연월)) %>%
  plot_ly(labels = ~sector, values = ~qnt_1000bbl) %>%
  add_pie(hole = 0.7) %>%
  add_annotations(text = format(sum(filter(sectorUse_m, 연월 == max(sectorUse_m$연월))$qnt_1000bbl), big.mark=','), 
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
           "21시", "22시", "23시") %>%
    pivot_longer(ends_with('시'), names_to = '시') %>%
    group_by(일자, 지점번호, 방향) %>%
    summarize(avg_cars.h = mean(value, na.rm=T)) %>%
    group_by(일자, 지점번호) %>%
    summarize(avg_cars.h = sum(avg_cars.h, na.rm=T)) %>%
    filter(avg_cars.h > 0)
  
  df$일자 <- ymd(df$일자)
  df$년월 <- format(df$일자, "%Y-%m")
  df <- df %>%
    group_by(년월, 지점번호) %>%
    summarize(교통량_대.시 = mean(avg_cars.h, na.rm=T))
  
  traffic <- rbind(traffic, df)
}

# 서울시 월별 구간평균 통행량(대/시/지점)
traffic_avg <- traffic %>% group_by(년월) %>% 
  summarize(교통량_대.시 = mean(교통량_대.시, na.rm=T))

# 2018년 통행량 대비 증감률 계산
traffic_avg <- traffic_avg %>%
  separate(col='년월', into = c('연도', '월'), sep = '-', remove=F)

traffic_2018 <- filter(traffic_avg, 연도 == 2018) %>% arrange(월)
traffic_2018_avg <- mean(traffic_2018$교통량_대.시)

traffic_avg$변화량_perc <- (traffic_avg$교통량_대.시 / traffic_2018$교통량_대.시[as.numeric(traffic_avg$월)]-1)*100

# 최근 월 데이터 설명 텍스트
latestMonth <- traffic_avg %>% arrange(년월) %>% tail(1)

if (latestMonth$변화량_perc > 0) {
  change <- '증가'
} else {
  change <- '감소'
}

summary <- paste0(latestMonth$연도, '년 ', latestMonth$월, '월 통행량: ',
                  format(round(latestMonth$교통량_대.시, 0), big.mark=','), '대/시간\n',
                  '2018년 ', latestMonth$월, '월 대비 <b>', round(abs(latestMonth$변화량_perc), 1), '% ', change, '</b>')

traffic_avg$날짜 <- as.Date(paste0(traffic_avg$년월, "-01"), '%Y-%m-%d') # 시각화 위해 임의 날짜 설정

# 시각화
plot_traffic <- traffic_avg %>%
  plot_ly(x=~날짜, y=~교통량_대.시, type = 'scatter', mode = 'lines') %>%
  add_lines(x=c('2018-01-01', '2050-01-01'), y=c(traffic_2018_avg, traffic_2018_avg*0.85)) %>%
  add_markers(x=latestMonth$년월, y=latestMonth$교통량_대.시, size = 10) %>%
  add_annotations(text = summary, font = list(size = 20), color = 'green',
                  x=latestMonth$년월, xanchor="left",
                  y=latestMonth$교통량_대.시, yanchor="bottom",
                  arrowhead = 0, ax = 50, ay = -50, arrowcolor = "green",
                  legendtitle=TRUE, showarrow=T) %>%
  layout(showlegend = F,
         xaxis = list(title=""),
         yaxis = list(title="구간평균 통행량(차량수/시간)"))
plot_traffic

### Heat maps ---------------------------
# Load road data (name, location, etc.)
roads <- read_excel('data/Seoul_traffic/서울시_지점별일자별교통량_202106.xlsx', sheet = 3, col_types = 'text')

# Merge road and traffic data
dt <- traffic %>% 
  left_join(select(roads, 지점번호, 지점명칭, 위도, 경도))
dt$위도 <- as.numeric(dt$위도)
dt$경도 <- as.numeric(dt$경도)
dt$scale <- scale(dt$교통량_대.시)

# 지도에 표시하기
m_traffic <- dt %>%
  leaflet() %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addTiles() %>%
  addHeatmap(lng=~경도, lat=~위도, intensity = ~scale, 
             blur=20, max=2, radius=20, minOpacity = 0.5) %>%
  addCircleMarkers(lng=~경도, lat=~위도, radius = 10, stroke = F, label = ~지점명칭, fillOpacity = 0)
m_traffic

# 교통량이 가장 많이 감소한 도로와 가장 많이 증가한 지역 찾기

lastYear <- strftime(ym(max(traffic$년월))-years(1), '%Y-%m')
dt <- traffic %>%
    filter(년월 %in% c(max(traffic$년월), lastYear)) %>%
    pivot_wider(names_from=년월, values_from=교통량_대.시)

names(dt) <- c('지점번호', '작년교통량', '올해교통량')

dt <- dt %>%
    mutate(교통량변화_perc = (올해교통량 - 작년교통량) / 작년교통량 *100) %>% # 작년 동월 대비 교통량변화(%) 계산하기
    left_join(select(roads, 지점번호, 지점명칭, 위도, 경도)) %>% # 위치데이터와 통행량 데이터 합치기
    filter(!is.na(교통량변화_perc)) # 신규 교통수집지점 발생 등으로 인해 교통량변화가 계산되지 않은 도로 제외
    
dt$위도 <- as.numeric(dt$위도)
dt$경도 <- as.numeric(dt$경도)

# 교통량 최다증가 도로 top5
head(dt %>% arrange(교통량변화_perc), 5)

# 교통량 최다감소 도로 top5
head(dt %>% arrange(desc(교통량변화_perc)), 5)

m_traffic_change <- dt %>%
  leaflet() %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addTiles() %>%
  addHeatmap(lng=~경도, lat=~위도, intensity = ~교통량변화_perc, 
             blur=20, max=2, radius=20, minOpacity = 0.5) %>%
  addCircleMarkers(lng=~경도, lat=~위도, radius = 10, stroke = F, label = ~지점명칭, fillOpacity = 0)
m_traffic_change

## Bus -----------------------------------------------
#apikey <- '4c4b4a767377696c36316771766766'
#date <- '20200101'
#url <- paste0('http://openapi.seoul.go.kr:8088/', apikey, '/json/CardBusStatisticsServiceNew/1/5/', date)
#dt <- fromJSON(url)

# Load data
bus <- data.frame()
for (file in list.files('data/Seoul_bus/')) {
  df <- read.csv(paste0('data/Seoul_bus/', file), fileEncoding = 'EUC-KR',
                 col.names = c("일자", "노선번호", "노선명", "버스정류장ARS번호", "역명", 
                 "승차총승객수", "하차총승객수", "등록일자"))
  df$일자 <- ymd(df$일자)
  df$연월 <- format(df$일자, "%Y-%m")
  df <- df %>%
    group_by(연월, 일자, 버스정류장ARS번호) %>%
    summarize(승차총승객수 = sum(승차총승객수, na.rm=T),
              하차총승객수 = sum(하차총승객수, na.rm=T)) %>%
    group_by(연월, 버스정류장ARS번호) %>%
    summarize(일평균승차승객수 = mean(승차총승객수, na.rm=T),
              일평균하차승객수 = mean(하차총승객수, na.rm=T)) %>%
    ungroup()
  
  bus <- rbind(bus, df)
}
bus$버스정류장ARS번호 <- str_pad(bus$버스정류장ARS번호, 5, 'left', '0')

busstops <- read.csv('data/서울시버스정류소좌표데이터(2021.01.14.).csv', 
                     fileEncoding = 'EUC-KR')
busstops$ARS.ID <- str_pad(busstops$ARS.ID, 5, 'left', '0')

# Calculate monthly averages
bus_avg <- bus %>% group_by(연월) %>% 
  summarize(일평균승차승객수 = sum(일평균승차승객수, na.rm=T))

bus_2018 <- bus_avg %>% filter(연월 < '2019-01-01')

plot_bus <- bus_avg %>%
  plot_ly(x=~연월, y=~일평균승차승객수/10^6, type = 'scatter', mode= 'lines+markers',
          marker = list(size=10)) %>%
  layout(xaxis = list(title=""),
         yaxis = list(title="일평균 승차승객수(백만 명)"))
plot_bus

# Heatmap
## Prepare data
thisYear <- max(bus$연월)
lastYear <- strftime(ym(thisYear)-years(1), '%Y-%m')
dt <- bus %>% 
  filter(연월 %in% c(thisYear, lastYear)) %>%
  pivot_longer(starts_with('일평균'), names_to = '구분', values_to = '일평균승객수') %>%
  pivot_wider(names_from = 연월, values_from = 일평균승객수)
names(dt) <- c('버스정류장ARS번호', '구분', '작년', '올해')
dt <- dt %>% 
  filter(올해 > 0) %>%
  mutate(변화율 = (올해 - 작년)/작년*100) %>%
  left_join(busstops, by = c('버스정류장ARS번호' = 'ARS.ID'))

## Draw a heatmap - onboard
dt %>%
  filter(구분 == '일평균승차승객수') %>%
  na.omit() %>%
  leaflet() %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addTiles() %>%
  addHeatmap(lng=~X좌표, lat=~Y좌표, intensity = ~올해,
             blur=15, max=7000, radius=10, minOpacity = 0.5) %>%
  addCircleMarkers(lng=~X좌표, lat=~Y좌표, label = ~정류소명,
                   radius = 10, stroke = F,  fillOpacity = 0)

# Draw a heatmap - offboard
dt %>%
  filter(구분 == '일평균하차승객수') %>%
  na.omit() %>%
  leaflet() %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addTiles() %>%
  addHeatmap(lng=~X좌표, lat=~Y좌표, intensity = ~올해,
             blur=15, max=7000, radius=10, minOpacity = 0.5) %>%
  addCircleMarkers(lng=~X좌표, lat=~Y좌표, label = ~정류소명,
                   radius = 10, stroke = F,  fillOpacity = 0)

# 
# change
bus_rank <- dt %>%
    select(버스정류장ARS번호, 구분, 작년, 올해) %>%
    group_by(버스정류장ARS번호) %>%
    summarize(작년동월이용객수 = sum(작년, na.rm=T),
              이번달이용객수 = sum(올해, na.rm=T)) %>%
    mutate(변화율 = (이번달이용객수-작년동월이용객수)/작년동월이용객수*100,
          변화수 = 이번달이용객수-작년동월이용객수) %>%
    left_join(busstops, by = c('버스정류장ARS번호' = 'ARS.ID'))

# 버스 이용객 변화 지도에 나타내기
bus_rank %>%
  na.omit() %>% # 위치정보가 제공되지 않은 일부 버스정류장은 분석에서 제외
  filter(변화율 < Inf) %>%
  leaflet() %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addTiles() %>%
  addHeatmap(lng=~X좌표, lat=~Y좌표, intensity = ~변화수,
             blur=15, max=500, radius=10, minOpacity = 0.5) %>%
  addCircleMarkers(lng=~X좌표, lat=~Y좌표, label = ~정류소명,
                   radius = 10, stroke = F,  fillOpacity = 0)

## Metro -------------------------------------
metro <- read.csv('data/서울시 지하철 호선별 역별 시간대별 승하차 인원 정보.csv', 
                  fileEncoding = 'EUC-KR') %>%
  pivot_longer(starts_with('X'), names_to='시간', values_to='인원')
metro$구분 <- NA
metro$구분[grep('승차', metro$시간)] <- '승차'
metro$구분[grep('하차', metro$시간)] <- '하차'

metro <- metro %>%
  group_by(연월 = 사용월, 호선명, 지하철역, 구분) %>%
  summarize(인원 = sum(인원, na.rm=T)) %>%
  ungroup()
metro$연월 <- ym(metro$연월)

metro_avg <- metro %>%
  filter(구분 == '승차') %>%
  group_by(연월) %>%
  summarize(인원 = sum(인원, na.rm=T))

plot_metro <- metro_avg %>%
  plot_ly(x=~연월, y=~인원/10^6, type = 'scatter', mode= 'lines+markers',
          marker = list(size=10)) %>%
  layout(xaxis = list(title=""),
         yaxis = list(title="월별승차인원(백만 명)"))
plot_metro

# 지하철 승차인원 증감율 계산
thisYear <- max(metro$연월)
lastYear <- strftime(thisYear-years(1))

num_thisYear <- metro_avg$인원[which(metro_avg$연월 == thisYear)]
num_lastYear <- metro_avg$인원[which(metro_avg$연월 == lastYear)]

change <- (num_thisYear - num_lastYear)/num_lastYear*100
change

# Heatmap
metroAddress <- read.csv('data/전국도시철도역사정보표준데이터.csv',
                         fileEncoding = 'EUC-KR') %>%
  filter(운영기관명 %in% c("서울교통공사", "한국철도공사", "인천교통공사"))
metroAddress$역사명 <- paste0(metroAddress$역사명, ' ')
metroAddress$역사명 <- str_replace(metroAddress$역사명, '역 ', ' ')
metroAddress$역사명 <- str_replace(metroAddress$역사명, ' ', '')
ind <- which(metroAddress$역사명 == '서울')
metroAddress$역사명[ind] <- '서울역'
metroAddress <- separate(metroAddress, 역사명, c('역사명', 'etc'), '\\(')
metroAddress <- metroAddress %>%
  group_by(역사명) %>%
  summarize(역위도=mean(역위도), 역경도=mean(역경도))

# 최근 월 데이터 추출하기
metro_now <- metro %>% 
  filter(연월 %in% c(thisYear, lastYear)) %>%
  separate(지하철역, c('지하철역', 'etc'), '\\(') %>%
  group_by(연월, 지하철역, 구분) %>%
  summarize(인원 = sum(인원)) %>%
  pivot_wider(names_from = 연월, values_from = 인원)
names(metro_now) <- c('지하철역', '구분', '작년', '올해')
metro_now <- metro_now %>% 
  filter(올해 > 0) %>%
  mutate(변화율 = (올해 - 작년)/작년*100,
         변화수 = 올해 - 작년) %>%
  left_join(metroAddress, by = c('지하철역'='역사명'))

# 지하철역별 일평균 승차승객수 히트맵
m__on <- metro_now %>%
  filter(구분 == '승차') %>%
  na.omit() %>% # 위치정보가 제공되지 않은 일부 지하철역은 분석에서 제외
  leaflet() %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addTiles() %>%
  addHeatmap(lng=~역경도, lat=~역위도, intensity = ~올해,
             blur=15, max=10^6, radius=10, minOpacity = 0.5) %>%
  addCircleMarkers(lng=~역경도, lat=~역위도, label = ~지하철역,
                   radius = 10, stroke = F,  fillOpacity = 0)
m__on
# 지하철역별 일평균 하차승객수 히트맵
m__off <- metro_now %>%
  filter(구분 == '하차') %>%
  na.omit() %>% # 위치정보가 제공되지 않은 일부 지하철역은 분석에서 제외
  leaflet() %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addTiles() %>%
  addHeatmap(lng=~역경도, lat=~역위도, intensity = ~올해,
             blur=15, max=10^6, radius=10, minOpacity = 0.5) %>%
  addCircleMarkers(lng=~역경도, lat=~역위도, label = ~지하철역,
                   radius = 10, stroke = F,  fillOpacity = 0)
m__off
# 주피터 노트북에 interactive 지도를 나타낼 수 없어 png 이미지로 대신 불러왔습니다. 실제 지도에서는 마우스오버를 하면 도로정보가 나타납니다.

# 지하철 이용객이 가장 많이 증가하거나 감소한 지역 찾기
metro_rank <- metro_now %>%
    select(지하철역, 구분, 작년, 올해) %>%
    group_by(지하철역) %>%
    summarize(작년동월이용객수 = sum(작년, na.rm=T),
              이번달이용객수 = sum(올해, na.rm=T)) %>%
    mutate(변화율 = (이번달이용객수-작년동월이용객수)/작년동월이용객수*100,
           변화수 = 이번달이용객수-작년동월이용객수) %>%
    left_join(metroAddress, by = c('지하철역'='역사명'))
    
# 이용객수 최다 증가 정류장 top5
metro_rank %>%
    select(지하철역, 이번달이용객수, 작년동월이용객수, 변화수, 변화율) %>% 
    filter(변화율 < Inf) %>%
    arrange(desc(변화수)) %>%
    head(5)# 버스 이용객이 가장 많이 증가하거나 감소한 지역 찾기

# 이용객수 최다 감소감소 정류장 top5
metro_rank %>%
    select(지하철역, 이번달이용객수, 작년동월이용객수, 변화수, 변화율) %>% 
    filter(변화율 < Inf) %>%
    arrange(변화수) %>%
    head(5)

# 이용객 변화 지도에 나타내기
m <- metro_rank %>%
  na.omit() %>% # 위치정보가 제공되지 않은 일부 버스정류장은 분석에서 제외
  filter(변화율 < Inf) %>%
  leaflet() %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addTiles() %>%
  addHeatmap(lng=~역경도, lat=~역위도, intensity = ~변화수,
             blur=15, max=max(metro_rank$변화수), radius=10, minOpacity = 0.5) %>%
  addCircleMarkers(lng=~역경도, lat=~역위도, label = ~지하철역,
                   radius = 10, stroke = F,  fillOpacity = 0)
m


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

# Format data
bike$대여일시 <- ymd_hms(bike$대여일시)
bike$연월 <- paste0(year(bike$대여일시), '-', str_pad(month(bike$대여일시), 2, 'left', 0))

# Calculate monthly sum
bike_avg <- bike %>% group_by(연월) %>% 
  summarize(count = n(), time_hour = sum(이용시간/60), distance_km = sum(이용거리/10^3))

plot_bike <- bike_avg %>%
  plot_ly(x=~연월, y=~count, type = 'scatter', mode= 'lines+markers',
          marker = list(size=10)) %>%
  layout(xaxis = list(title=""),
         yaxis = list(title="이용건수"))
plot_bike

plot_bike_dist <- bike_avg %>%
  plot_ly(x=~연월, y=~time_hour, name = '이용시간(시간)', type = 'bar', 
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

# Heatmap
# Locations
bikeStations <- read.csv('data/공공자전거 대여소 정보(21.06월 기준).csv', 
                         fileEncoding = 'EUC-KR')
names(bikeStations )<- c('id', 'name', 'district', 'address', 'lat', 'long', 'install.date', 'num_LCD', 'num_QR', 'type')
bikeStations <- bikeStations[!is.na(bikeStations$id), ]
bikeStations$id <- str_pad(bikeStations$id, 5, 'left', 0)

# Rentals by station
dt_rent <- bike %>% 
  filter(연월 == max(bike$연월)) %>%
  group_by(id = 대여.대여소번호) %>% 
  summarize(num_rent = n())

dt_return <- bike %>% 
  filter(연월 == max(bike$연월)) %>%
  group_by(id = 반납대여소번호) %>% 
  summarize(num_return = n())

dt_all <- full_join(dt_rent, dt_return)
dt_all[is.na(dt_all)] <- 0

# Merge coordinates
dt <- left_join(dt_all, bikeStations)
dt$num_total <- dt$num_rent + dt$num_return
dt$lat <- as.numeric(dt$lat)
dt$long <- as.numeric(dt$long)

# Draw heatmap
dt %>%
  na.omit() %>%
  leaflet() %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addTiles() %>%
  addHeatmap(lng=~long, lat=~lat, intensity = ~num_total, 
             blur=20, max=0, radius=20, minOpacity = 0.5) %>%
  addCircleMarkers(lng=~long, lat=~lat, radius = 10, stroke = F, popup = ~num_rent, label = ~name, fillOpacity = 0)
