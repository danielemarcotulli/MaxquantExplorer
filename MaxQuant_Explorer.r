library(shiny)
library(plotly)
library(mygene)
library(ggplot2)
library(DT)
library(crosstalk)
library(colourpicker)

MQ <-read.table(file.choose(),header=T,sep="\t")

Gene_list_MQ <- MQ$Gene.names

GO <- queryMany(Gene_list_MQ, scopes='symbol', fields='go', species='mouse')

require(svDialogs)
GO.term <- dlgInput("Enter a GOterm, e.g. endocytosis or synaptic vesicle", Sys.info()[""])$res

userGO_MQ <- GO[grep(GO.term, GO$go.BP), ]
#userGO_MQ$query

userGO_MQ_proteins <- userGO_MQ[['query']]
GO_selected <- MQ[MQ$Gene.names %in% userGO_MQ_proteins, ]

x1 <- dlgInput("lower limit ratio to visualize", Sys.info()["0.8"])$res
x2 <- dlgInput("upper limit ratio to visualize", Sys.info()["1.2"])$res


lower_selected <- MQ[MQ$Ratio.H.L.normalized.ko01<x1, ]
upper_selected <- MQ[MQ$Ratio.H.L.normalized.ko01>x2, ]
both_selected <- rbind(lower_selected, upper_selected)
#MQ$Ratio.H.L.normalized.ko01  <- factor(MQ$Ratio.H.L.normalized.ko01, levels =c(<x1, >x2))

nms <- names(MQ)
ui <- fluidPage(

  headerPanel("Maxquant Explorer"),
  sidebarPanel(
    #sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(Sup_KO1),
    #value = 11, step = 1, round = 0),
    selectInput('df', 'Select dataframe', choices = c('MQ', 'GO_selected', 'both_selected', 'lower_selected', 'upper_selected'), selected = 'MQ'),
    selectInput('x', 'X', choices = nms, selected = "Ratio.H.L.normalized.ko01"),
    selectInput('y', 'Y', choices = nms, selected = "Intensity.H.ko01"),
    colourInput("col_low", "Select colour low", "red"),
    colourInput("col_up", "Select colour mid", "gray"),
    colourInput("col_mid", "Select colour up", "red")
  ),
  mainPanel(
    plotlyOutput('Plot')
  )
)

server <- function(input, output) {

  df <- reactive({
    dff <- get(input$df)
  })



  output$Plot <- renderPlotly({
    sd <- SharedData$new(df, ~Gene.names, group = "Choose a protein")

    new_plot <- plot_ly(sd, x = ~get(input$x), y = ~get(input$y),
                        color = ~Ratio.H.L.normalized.ko01, colors = c(input$col_low, input$col_up, input$col_mid),
                        hoverinfo = 'text',
                        text = ~paste('Protein: ', Protein.names )) %>%
      highlight(on = "plotly_hover", persistent = TRUE, selectize = TRUE) %>%
      layout(xaxis= list(title=input$x), yaxis = list(title=input$x)) #%>%
      #color=c(rep(0,0.8),rep(0,0.4))

  })

  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
}

shinyApp(ui, server)
