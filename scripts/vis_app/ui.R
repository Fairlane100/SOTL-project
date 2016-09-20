library(LDAvis)
shinyUI(
    fluidPage(
        mainPanel(
            tabsetPanel(
                tabPanel("VEM", visOutput('myChartVEM')),
                tabPanel("VEMf", visOutput('myChartVEMf')),
                tabPanel("Gibbs", visOutput('myChartGib')),
                tabPanel("CTM", visOutput('myChartCTM'))
            )
        )
        # titlePanel("VEM"),
        # visOutput('myChartVEM'),
        # titlePanel("VEMf"),
        # visOutput('myChartVEMf'),
        # titlePanel("Gibbs"),
        # visOutput('myChartGib'),
        # titlePanel("CTM"),
        # visOutput('myChartCTM')
    )
)