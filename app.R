
library(shiny)   ## duh ?
library(plotly)  ## duh ?
library(xts)  ##  formed ts
library(zoo)  ##  same above
library(forecast)  ##  smoothing calculate
library(mgcv)      ## same above
library(smooth)    ## same above
library(ggplot2)   ##  shiny dependency


ui <- fluidPage(

  
  plotlyOutput("plot",width = "100%", height = "600")
 
  
)

server <- function(input, output) ({
  
  eth <-read.csv("https://etherscan.io/chart/etherprice?output=csv", sep=",", header = TRUE, encoding = 'UTF-8')
  


  
  da <- as.Date(eth$Date.UTC., format = "%m/%d/%Y")
  Price <- as.numeric(eth$Value)
  daily.ETH <- as.xts(Price, order.by = da)
  return <- diff(log(daily.ETH))
  
  
  ### handeling NaN, Na and INF data
  return[is.nan(return)] = 0
  return[is.na(return)] = 0
  return[is.infinite(return)] = 0
  ########
  
  ##### frame the data
  r.df <- data.frame(return)
  df <- data.frame(daily.ETH, r.df)
  #####

  ### name the data 
  x <- time(daily.ETH)
  y <- df$daily.ETH
  y2 <- r.df$return
  Date = time(daily.ETH)
  ###
  
  #### smoothing methods (lesser the span/band-width, more sensitive to Price)
  
  y3 = fitted(loess(y ~ as.numeric(Date), span = 0.095))   ## loess, you can change span = c
  y4 = ksmooth(x, y, "normal", 5, x.points=x)            ## kernel, you can change span = c
  y5 = stats::filter(y, rep(1/7, 7), side=2)             ##  7-day moving average for return, taking the mean of a fixed number of nearby points
  y6 = supsmu(x, y)                                      ## running line smoother, cross-validated
  y7 = smooth.spline(x,y)                                ## smoothing spline 
  y8 = ksmooth(x, y2, "normal", 5, x.points=x)          ### experiment, kernel the return. 5 as bandwidth
  ###
  
                                                     
  ## ploting
  output$plot <-  renderPlotly({
    ### first draft the Price Plot
    test.plot <- plot_ly() %>%
      layout(legend = list(bordercolor = "#E2E2E2",borderwidth = 0)) %>%
      
      add_lines(x = ~Date, y = ~y,mode = 'lines',name = 'Ether Price',
                hoverinfo = "text",
                line = list(color=rgb(0.8,0.8,0.8,0.8), width=2),
                text = ~paste('\nDate:', Date,
                              '\nPrice:', y))  %>%
      
      add_lines(x = ~Date,y = ~y3,mode = 'lines', name = "Loess Smoothing",
                hoverinfo = "text",
                text = ~paste('\nSmooth@:', round(y3, digits = 3)),
                visible = "legendonly") %>%
      
      add_lines(x=~Date, y=~y6$y, mode = 'lines',name="Cross-validated span",
                line = list(color = 'rgb(93, 84, 82)'),
                hoverinfo = "text",
                text = ~paste('\nSmooth@:', round(y6$y, digits = 3)),
                visible = "legendonly") %>%
      
      add_lines(x=~Date, y=~y5, mode = 'lines',name="7-day SMA",
                line = list(color = 'rgb(234, 68, 31)'), 
                hoverinfo = "text",
                text = ~paste('\nSmooth@:', round(y5, digits = 3)),
                visible = "legendonly") %>%
      
      add_lines(x = ~Date, y = ~predict(y7)$y,  name = 'Smoothing SPline',  mode = 'lines',
                line = list(color = '#B52FC7'),
                hoverinfo = "text",
                text = ~paste('\nSmooth@ :', round(predict(y7)$y, digits = 3)),
                visible = "legendonly") %>%
      
      add_lines(x = ~Date, y = ~y2,  name = 'Natrual Return', yaxis = 'y2', mode = 'lines',
                line = list(color = 'rgb(122, 209, 109)', width=1),
                hoverinfo = "text",
                text = ~paste('\nReturn:', round(y2, digits = 3))) %>%
      
      add_lines(x = ~Date, y = ~y8$y,  name = 'Kernel Smoothing', yaxis = 'y2', mode = 'lines',
                line = list(color = '#0522D8'),
                hoverinfo = "text",
                text = ~paste('\nSmooth@:', round(y8$y, digits = 3)),
                visible = "legendonly") %>%
      
     
      
      
      layout(title = 'Ethereum Price with Natrual Return',
             xaxis = list(title = "Date", showspikes = TRUE, spikemode = "across", spikedash = "solid", spikecolor = "black", 
                          spikethickness = "1", spikesnap ="cursor"), spikedistance = "-1",mode = 'markers',
             yaxis = list(side = 'left', title = 'Ether Price', showgrid = TRUE, zeroline = FALSE, overlaying = "y2"),
             yaxis2 = list(side = 'right', title = 'Return', showgrid = FALSE, 
                           zeroline = TRUE, range =c(-.8, 0.8))) %>%
      
      layout(
        title = "Price and Return (t-1)",
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(
                count = 1,
                label = "1 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 3,
                label = "3 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 6,
                label = "6 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 yr",
                step = "year",
                stepmode = "backward"),
              list(
                count = 1,
                label = "YTD",
                step = "year",
                stepmode = "todate"),
              list(step = "all"))),
          
          rangeslider = list(type = "date"), rangemode = "match")) %>%
          layout(hovermode = 'x')  

  })  
})
# Create Shiny app ----
shinyApp(ui = ui, server = server)

# # deploy to shiny.io
# rsconnect::setAccountInfo(name='kai-peng', token='2F57C4902543ECF0142207139371EF41', secret='')
# library(rsconnect)
# rsconnect::deployApp('C:/Users/nicoo/OneDrive/eng/ether-return')