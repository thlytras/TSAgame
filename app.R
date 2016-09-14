# This is the TSA game!
# Created by Theodore Lytras on Tuesday 13 Sep 2016

plotPeriodo <- function(x, col="navyblue", lwd=1) {
  Density <- Mod(fft(x - mean(x, na.rm=TRUE)))^2
  Period <- (1/((1:length(x))-1))*length(x)
  Period[Inf] <- NA
  plot(y=log(Density)[Period<100], x=Period[Period<100], 
       type="l", ylab=NA, xlab="Period", bty="l", col=col, lwd=lwd, main="Periodogram")
}


makeSeries <- function(serLen, trend, perAmp, noise, perAmpTrend=0, exp=FALSE) {
  x <- 1:(serLen*52)
  if (exp) return(trend*x + perAmp*(1/exp((abs((x%%52)-52/2))^1/4)) + rnorm(serLen*52, sd=noise))
  trend*x + perAmp*exp(perAmpTrend*x/52)*sin(2*pi*x/52) + rnorm(serLen*52, sd=noise)
  
}


library(shiny)

ui <- shinyUI(fluidPage(
   titlePanel("TSA - The game!"),
   sidebarLayout(
      sidebarPanel(
         sliderInput("serLen",
                     "Length of series (years):",
                     min = 1,
                     max = 10,
                     value = 4),
         sliderInput("perAmp",
                     "Periodicity amplitude:",
                     min = 0,
                     max = 100,
                     value = 10, step=0.1),
         sliderInput("trend",
                     "Trend (per week):",
                     min = -1,
                     max = 1,
                     value = 0, step=0.02),
         sliderInput("noise",
                     "Noise SD:",
                     min = 0,
                     max = 50,
                     value = 5, step=0.1),
         sliderInput("perAmpTrend",
                     "Exp. trend for periodicity amp:",
                     min = -1,
                     max = 1,
                     value = 0, step=0.02),
         radioButtons("dist", "Distribution type:",
                      c("Sine" = "sin",
                        "Exp" = "exp"))
      ),
      
      mainPanel(
         plotOutput("mainPlot", height="600px")
      )
   )
))


server <- shinyServer(function(input, output) {
   
   output$mainPlot <- renderPlot({
      x    <- makeSeries(input$serLen, input$trend, input$perAmp, input$noise, input$perAmpTrend, input$dist=="exp")
      par(mfrow=c(3,1))
      plot(x, type="l", main="Time series")
      acf(x, lag.max=100, main="Autocorrelation")
      plotPeriodo(x)
   })
})

shinyApp(ui = ui, server = server)

