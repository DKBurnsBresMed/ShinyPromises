# Example of promises

library(shiny)
library(future)
library(promises)
plan(multisession)


server <- function(input,output,session) {


  # evaluate the data for the plot
  PlotData <- eventReactive(input$DelayEvalStart,{
    runif(100000000)
  })

  # reactiveval tracking status of process
  PlotDatStat <- reactiveValues(stat = "Pending")

  # change to running when the user presses the button
  observeEvent(input$DelayEvalStart,{
    PlotDatStat$stat <- "Running"
  })

  observeEvent(PlotDatStat$stat,{
    future({PlotData}) %...>%
      {
        PlotDatStat$stat <- "Completed"
      }
  })

  observe({
    req(PlotDatStat$stat == "Completed")
    PlotDatStat$stat <- "Pending"
  })

  output$StatusText <- renderText({
    req(PlotDatStat$stat)
    paste0(
      "Processing is currently ",
      PlotDatStat$stat
    )
  })


  # using future/promise evaluation to wait until
  # data is ready before making the plot
  output$plot <- renderPlot({
    p <- PlotData()
    future({ p }) %...>%
      plot()
  })

}



ui <- fluidPage(
  fluidRow(
    column(4,actionButton("DelayEvalStart","Start process"),
           br(),
           textOutput("StatusText")),
    column(8,plotOutput("plot"))
  )
)

shinyApp(ui = ui,server = server)
