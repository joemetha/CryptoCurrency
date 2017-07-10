library(shiny)
library(XML)
library(plotrix)
library(quantmod)
library(anytime)



urlBTC <- "http://coinmarketcap.com/currencies/bitcoin/historical-data/"
urlETH <- "http://coinmarketcap.com/currencies/ethereum/historical-data/"
urlLTC <- "http://coinmarketcap.com/currencies/litecoin/historical-data/"
urlZEC <- "http://coinmarketcap.com/currencies/zcash/historical-data/"
urlXRP <- "http://coinmarketcap.com/currencies/ripple/historical-data/"
urlETC <- "http://coinmarketcap.com/currencies/ethereum-classic/historical-data/"
urlDASH <- "http://coinmarketcap.com/currencies/dash/historical-data/"
urlXEM <- "http://coinmarketcap.com/currencies/nem/historical-data/"


popBTC <- readHTMLTable(urlBTC, which = 2)
popETH <- readHTMLTable(urlETH, which = 2)
popLTC <- readHTMLTable(urlLTC, which = 2)
popZEC <- readHTMLTable(urlZEC, which = 2)
popXRP <- readHTMLTable(urlXRP, which = 2)
popETC <- readHTMLTable(urlETC, which = 2)
popDASH <- readHTMLTable(urlDASH, which = 2)
popXEM <- readHTMLTable(urlXEM, which = 2)

BTC <-as.numeric(as.character(popBTC$Close[30:1]))
ETH <-as.numeric(as.character(popETH$Close[30:1]))
LTC <-as.numeric(as.character(popLTC$Close[30:1]))
ZEC <-as.numeric(as.character(popZEC$Close[30:1]))
XRP <-as.numeric(as.character(popXRP$Close[30:1]))
ETC <-as.numeric(as.character(popETC$Close[30:1]))
DASH <-as.numeric(as.character(popDASH$Close[30:1]))
XEM <-as.numeric(as.character(popXEM$Close[30:1]))

#plot(x=seq(1,30,1),y= XEM, type = 'l', xlab = "",ylab = "USD",xaxt="n")
#axis(1, at = 1:30, las=2, labels=as.character(popBTC$Date[30:1]))



marketCap <- c(as.numeric(as.character(gsub(",","",popBTC$`Market Cap`[1])))
               ,as.numeric(as.character(gsub(",","",popETH$`Market Cap`[1])))
               ,as.numeric(as.character(gsub(",","",popLTC$`Market Cap`[1])))
               ,as.numeric(as.character(gsub(",","",popZEC$`Market Cap`[1])))
               ,as.numeric(as.character(gsub(",","",popXRP$`Market Cap`[1])))
               ,as.numeric(as.character(gsub(",","",popETC$`Market Cap`[1])))
               ,as.numeric(as.character(gsub(",","",popDASH$`Market Cap`[1])))
               ,as.numeric(as.character(gsub(",","",popXEM$`Market Cap`[1])))
)



lbls <- c("Bitcoin", "Ethereum", "Litecoin", "Zcash", "Ripple", "Ethereum Classic", "Dash", "NEM")

CreateData <- function(){
  Name <- c("Bitcoin","Dash","Ethereum Classic","Ethereum", "Litecoin","NEM","Ripple","ZCash")
  Date <- c(as.character(popBTC$Date[1]),as.character(popDASH$Date[1]),as.character(popETC$Date[1]),as.character(popETH$Date[1]),as.character(popLTC$Date[1]),as.character(popXEM$Date[1]),as.character(popXRP$Date[1]),as.character(popZEC$Date[1]))
  Open <- c(as.character(popBTC$Open[1]),as.character(popDASH$Open[1]),as.character(popETC$Open[1]),as.character(popETH$Open[1]),as.character(popLTC$Open[1]),as.character(popXEM$Open[1]),as.character(popXRP$Open[1]),as.character(popZEC$Open[1]))
  CloseAt <- c(as.character(popBTC$Close[1]),as.character(popDASH$Close[1]),as.character(popETC$Close[1]),as.character(popETH$Close[1]),as.character(popLTC$Close[1]),as.character(popXEM$Close[1]),as.character(popXRP$Close[1]),as.character(popZEC$Close[1]))
  Change <- c(
    as.numeric(as.character(popBTC$Close[1]))-as.numeric(as.character(popBTC$Close[2])),
    as.numeric(as.character(popDASH$Close[1]))-as.numeric(as.character(popDASH$Close[2])),
    as.numeric(as.character(popETC$Close[1]))-as.numeric(as.character(popETC$Close[2])),
    as.numeric(as.character(popETH$Close[1]))-as.numeric(as.character(popETH$Close[2])),
    as.numeric(as.character(popLTC$Close[1]))-as.numeric(as.character(popLTC$Close[2])),
    as.numeric(as.character(popXEM$Close[1]))-as.numeric(as.character(popXEM$Close[2])),
    as.numeric(as.character(popXRP$Close[1]))-as.numeric(as.character(popXRP$Close[2])),
    as.numeric(as.character(popZEC$Close[1]))-as.numeric(as.character(popZEC$Close[2]))
  )
  Volume <-c(as.character(popBTC$Volume[1]),as.character(popDASH$Volume[1]),as.character(popETC$Volume[1]),as.character(popETH$Volume[1]),as.character(popLTC$Volume[1]),as.character(popXEM$Volume[1]),as.character(popXRP$Volume[1]),as.character(popZEC$Volume[1]))
  Market_Cap <- c(as.character(popBTC$`Market Cap`[1]),as.character(popDASH$`Market Cap`[1]),as.character(popETC$`Market Cap`[1]),as.character(popETH$`Market Cap`[1]),as.character(popLTC$`Market Cap`[1]),as.character(popXEM$`Market Cap`[1]),as.character(popXRP$`Market Cap`[1]),as.character(popZEC$`Market Cap`[1]))
  
  return(dataSum <- data.frame(Name, Date, Open, CloseAt, Change, Volume, Market_Cap))
}




function(input, output) {
  output$piePlot <- renderPlot({
    pie(marketCap, main="Market Capitalization", labels = lbls, col = c("#900c3f","#c70039","#ff8d1a","#ffc300","#add45c", "#57c785","#2a7b9b","#3d3d6b"))
  })
  
  output$mySum <- renderPrint({
    if(input$forecastType=='BTC'){
      tempX <- popBTC$Close[input$day:1]
    }
    else if(input$forecastType=='DASH'){
      tempX <- popDASH$Close[input$day:1]
    }
    else if(input$forecastType=='ETC'){
      tempX <- popETC$Close[input$day:1]
    }
    else if(input$forecastType=='ETH'){
      tempX <- popETH$Close[input$day:1]
    }
    else if(input$forecastType=='LTC'){
      tempX <- popLTC$Close[input$day:1]
    }
    else if(input$forecastType=='XEM'){
      tempX <- popXEM$Close[input$day:1]
    }
    else if(input$forecastType=='XRP'){
      tempX <- popXRP$Close[input$day:1]
    }
    else{
      tempX <- popZEC$Close[input$day:1]
    }
    
    summary(as.numeric(as.character(tempX)))
    
  })
  
  output$mytable = renderDataTable({
    CreateData()
    
  })
  
  output$plot <- renderPlot({    
    
    if(input$plotType=='BTC'){
      temp <- popBTC
    }
    else if(input$plotType=='DASH'){
      temp <- popDASH
    }
    else if(input$plotType=='ETC'){
      temp <- popETC
    }
    else if(input$plotType=='ETH'){
      temp <- popETH
    }
    else if(input$plotType=='LTC'){
      temp <- popLTC
    }
    else if(input$plotType=='XEM'){
      temp <- popXEM
    }
    else if(input$plotType=='XRP'){
      temp <- popXRP
    }
    else{
      temp <- popZEC
    }
    
    
    
    
    Open <- as.numeric(as.character(temp$Open))
    Close <- as.numeric(as.character(temp$Close))
    High <- as.numeric(as.character(temp$High))
    Low <- as.numeric(as.character(temp$Low))
    Volume <-as.numeric(as.character(gsub(",","",temp$Volume)))
    
    test1 <- data.frame(Open,Close,High,Low,Volume)
    
    Rgraph <- xts(test1, order.by=as.POSIXct(anytime(temp$Date)))
    
    chartSeries(Rgraph,name = "Y axis = $USD") 
  })
  
  output$forecastPlot <- renderPlot({
    if(input$forecastType=='BTC'){
      tempX <- popBTC$Close[input$day:1]
    }
    else if(input$forecastType=='DASH'){
      tempX <- popDASH$Close[input$day:1]
    }
    else if(input$forecastType=='ETC'){
      tempX <- popETC$Close[input$day:1]
    }
    else if(input$forecastType=='ETH'){
      tempX <- popETH$Close[input$day:1]
    }
    else if(input$forecastType=='LTC'){
      tempX <- popLTC$Close[input$day:1]
    }
    else if(input$forecastType=='XEM'){
      tempX <- popXEM$Close[input$day:1]
    }
    else if(input$forecastType=='XRP'){
      tempX <- popXRP$Close[input$day:1]
    }
    else{
      tempX <- popZEC$Close[input$day:1]
    }
    
    BTC.ts<- ts(as.numeric(as.character(tempX)))
    
    fit <- arima(BTC.ts, order=c(3,0,0), list(order=c(0,0,1), period=input$day))
    fore <- predict(fit, n.ahead=input$forcastDay)
    U <- fore$pred + fore$se
    L <- fore$pred - fore$se
    ts.plot(BTC.ts, fore$pred, U,L, col=c(1,2,4,4), lty = c(1,1,2,2), gpars = list(xlab="", ylab="USD",xaxt="n",main="Forecasting graph"))
    axis(1, at = 1:input$day, las=1, labels=c(as.character(popBTC$Date[input$day:1])))
    legend("top", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4), lty=c(1,1,2))
  })
  
  output$forecastTable = renderDataTable({
    if(input$forecastType=='BTC'){
      temp2 <- popBTC
    }
    else if(input$forecastType=='DASH'){
      temp2 <- popDASH
    }
    else if(input$forecastType=='ETC'){
      temp2 <- popETC
    }
    else if(input$forecastType=='ETH'){
      temp2 <- popETH
    }
    else if(input$forecastType=='LTC'){
      temp2 <- popLTC
    }
    else if(input$forecastType=='XEM'){
      temp2 <- popXEM
    }
    else if(input$forecastType=='XRP'){
      temp2 <- popXRP
    }
    else{
      temp2 <- popZEC
    }
    
    temp2
  })
  
}