library(shinydashboard)
library(plotly)
ui <- dashboardPage(
  dashboardHeader(title = "온실가스지킴이 - 교통편"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("감축현황", tabName = "감축현황", icon = icon("dashboard")),
      menuItem("정책성과", tabName = "performance", icon = icon("th")),
      menuItem("작당모의", tabName = "discussion", icon = icon("th")),
      menuItem("더 알아보기", tabName = "more", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
# First tab content
      tabItem(tabName = "감축현황",
        h2("감축현황"),
        plotlyOutput("plot_energyUse")
      ),

# Second tab content
      tabItem(tabName = "performance",
        h2("정책성과")
      ),
      tabItem(tabName = "discussion",
        h2("작당모의")
      ),
      tabItem(tabName = "more",
        h2("더 알아보기")
      )
    )
  )
)