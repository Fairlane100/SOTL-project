library(LDAvis)
library(shiny)

# read the json from file.
library("rjson")
fileName <- 'json_VEM'
json_VEM <- readChar(fileName, file.info(fileName)$size)
fileName <- 'json_VEMf'
json_VEMf <- readChar(fileName, file.info(fileName)$size)
fileName <- 'json_Gib'
json_Gib <- readChar(fileName, file.info(fileName)$size)
fileName <- 'json_CTM'
json_CTM <- readChar(fileName, file.info(fileName)$size)

shinyServer(function(input, output, session) {
    output$myChartVEM <- renderVis(json_VEM)
    output$myChartVEMf <- renderVis(json_VEMf)
    output$myChartGib <- renderVis(json_Gib)
    output$myChartCTM <- renderVis(json_CTM)
})