library(shiny)
library(shinymaterial)
library(readxl)
hrdata<-read_excel("Database 2008-2016 (as at 17 Jul 2017).xlsx")
attach(hrdata)
hrdata$Date<-as.Date(hrdata$Date)

ui<-fluidPage(
  titlePanel("2008-2016年度香港賽馬資料版"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      checkboxGroupInput("show_vars", "選擇顯示資料列:",names(hrdata), selected = names(hrdata))
    ),
    mainPanel(
      fluidRow(
        column(3, wellPanel(dateInput('date1',label = '開始日期: yyyy-mm-dd',value = "2008-09-01"))),
        column(3, wellPanel(dateInput('date2',label = '結束日期: yyyy-mm-dd',value = Sys.Date()))),
        column(3,material_dropdown("hrname","馬匹",c("All",unique(as.character(hrdata$Name))))),
        column(3,material_dropdown("jc","騎師",c("All",unique(as.character(hrdata$JC))))),
        column(3,material_dropdown("jt","練馬師",c("All",unique(as.character(hrdata$JT))))),
        column(3,material_dropdown("dist","途程",c("All",unique(as.character(hrdata$Dist))))),
        column(3,material_dropdown("loc","地點",c("All",unique(as.character(hrdata$Loc))))),
        column(3,material_dropdown("rw","場地",c("All",unique(as.character(hrdata$Runway)))))
      ),
      fluidRow(DT::dataTableOutput("table"))
    )
  )
)

server<-function(input, output) {
  output$table <- DT::renderDataTable(DT::datatable({
    data <- hrdata
    data2<-data[, input$show_vars, drop = FALSE]
    data3<-data2[data2$Date>=input$date1 & data2$Date<=input$date2,]
    if (input$jc != "All") {
      data3 <- data3[data3$JC == input$jc,]
    }
    if (input$jt != "All") {
      data3 <- data3[data3$JT == input$jt,]
    }
    if (input$dist != "All") {
      data3 <- data3[data3$Dist == input$dist,]
    }
    if (input$hrname != "All") {
      data3 <- data3[data3$Name == input$hrname,]
    }
    if (input$loc != "All") {
      data3 <- data3[data3$Loc == input$loc,]
    }
    if (input$rw != "All") {
      data3 <- data3[data3$Runway == input$rw,]
    }
    data3
  }))
}

shinyApp(ui = ui, server = server)
