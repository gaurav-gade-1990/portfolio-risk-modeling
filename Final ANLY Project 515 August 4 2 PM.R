install.packages("xts")
install.packages("quantmod")
install.packages("dplyr")
install.packages("zoo")
install.packages("haven")
install.packages("MOTE")

 library(dplyr)
library(xts)
library(quantmod)
library(zoo)
 library(haven)
 library(MOTE)
#Data preparation


getwd()

setwd("C:/Users/nagar/Documents/ANLY 515 -FI Risk modeling and portfolio optimization/Project")

#Creating the data

#Comparing Capital Gains of Multiple Securities Over Time                  

#========DATA PREP==========================

    
#Import data for each of the four securities - AMGN
    data.AMGN <- read.csv("AMGN.csv",header=TRUE)
    date <- as.Date(data.AMGN$Date,format="%Y-%m-%d")  
    data.AMGN <- cbind(date, data.AMGN[,-1])
    data.AMGN <- data.AMGN[order(data.AMGN$date),]
    data.AMGN <- xts(data.AMGN[,2:7],order.by=data.AMGN[,1])
    names(data.AMGN) <- paste(c("AMGN.Open","AMGN.High","AMGN.Low","AMGN.Close","AMGN.Adjusted","AMGN.Volume"))
    data.AMGN[c(1:3,nrow(data.AMGN)),]
    

#Import data for each of the four securities - JNJ
    data.JNJ <- read.csv("JNJ.csv",header=TRUE)
    date <- as.Date(data.JNJ$Date,format="%Y-%m-%d")  
    data.JNJ <- cbind(date, data.JNJ[,-1])
    data.JNJ <- data.JNJ[order(data.JNJ$date),]
    data.JNJ <- xts(data.JNJ[,2:7],order.by=data.JNJ[,1])
    names(data.JNJ) <- paste(c("JNJ.Open","JNJ.High","JNJ.Low","JNJ.Close","JNJ.Adjusted","JNJ.Volume"))
    data.JNJ[c(1:3,nrow(data.JNJ)),]

#Import data for each of the four securities - CISCO
    data.CSCO <- read.csv("CSCO.csv",header=TRUE)
    date <- as.Date(data.CSCO$Date,format="%Y-%m-%d")  
    data.CSCO <- cbind(date, data.CSCO[,-1])
    data.CSCO <- data.CSCO[order(data.CSCO$date),]
    data.CSCO <- xts(data.CSCO[,2:7],order.by=data.CSCO[,1])
    names(data.CSCO) <- paste(c("CSCO.Open","CSCO.High","CSCO.Low","CSCO.Close","CSCO.Adjusted","CSCO.Volume"))
    data.CSCO[c(1:3,nrow(data.CSCO)),]

#Import data for each of the four securities - Microsoft (MSFT)
    data.MSFT <- read.csv("MSFT.csv",header=TRUE)
    date <- as.Date(data.MSFT$Date,format="%Y-%m-%d")  
    data.MSFT <- cbind(date, data.MSFT[,-1])
    data.MSFT <- data.MSFT[order(data.MSFT$date),]
    data.MSFT <- xts(data.MSFT[,2:7],order.by=data.MSFT[,1])
    names(data.MSFT) <- paste(c("MSFT.Open","MSFT.High","MSFT.Low","MSFT.Close","MSFT.Adjusted","MSFT.Volume"))
    data.MSFT[c(1:3,nrow(data.MSFT)),]

#Import data for each of the four securities - Exxon Mobil (XOM)
    data.XOM <- read.csv("XOM.csv",header=TRUE)
    date <- as.Date(data.XOM$Date,format="%Y-%m-%d")  
    data.XOM <- cbind(date, data.XOM[,-1])
    data.XOM <- data.XOM[order(data.XOM$date),]
    data.XOM <- xts(data.XOM[,2:7],order.by=data.XOM[,1])
    names(data.XOM) <- paste(c("XOM.Open","XOM.High","XOM.Low","XOM.Close","XOM.Adjusted","XOM.Volume"))
    data.XOM[c(1:3,nrow(data.XOM)),]
    
#Import data for each of the four securities - Chevron (CVX)
    data.CVX <- read.csv("CVX.csv",header=TRUE)
    date <- as.Date(data.CVX$Date,format="%Y-%m-%d")  
    data.CVX <- cbind(date, data.CVX[,-1])
    data.CVX <- data.CVX[order(data.CVX$date),]
    data.CVX <- xts(data.CVX[,2:7],order.by=data.CVX[,1])
    names(data.CVX) <- paste(c("CVX.Open","CVX.High","CVX.Low","CVX.Close","CVX.Adjusted","CVX.Volume"))
    data.CVX[c(1:3,nrow(data.CVX)),]
    
    
#Combine Data Into one Data Object
    Close.Prices <- cbind(data.AMGN$AMGN.Close,data.JNJ$JNJ.Close, data.CSCO$CSCO.Close,data.MSFT$MSFT.Close,data.CVX$CVX.Close, data.XOM$XOM.Close)
    Close.Prices[c(1:3,nrow(Close.Prices)),]


#Convert Data into a data.frame
    
    multi.df <- cbind(index(Close.Prices),data.frame(Close.Prices))
    names(multi.df) <- paste(c("date","AMGN","JNJ","CSCO","MSFT","CVX","XOM"))
    rownames(multi.df) <- seq(1,nrow(multi.df),1)
    multi.df[c(1:3,nrow(multi.df)),]

#Calculate Normalized Values for Each Security
#Create an index for each security with values that equal the price of the 	security on each day divided by the security's price on December 31, 	2010.

    multi.df$AMGN.idx <- multi.df$AMGN/multi.df$AMGN[1]
    multi.df$JNJ.idx <- multi.df$JNJ/multi.df$JNJ[1]
    

    
    multi.df$CSCO.idx <- multi.df$CSCO/multi.df$CSCO[1]
    multi.df$MSFT.idx <- multi.df$MSFT/multi.df$MSFT[1]
    
    multi.df$CVX.idx <- multi.df$CVX/multi.df$CVX[1]
    multi.df$XOM.idx <- multi.df$XOM/multi.df$XOM[1]
    
    options(digits=5)   # this option remains in place until end of session
    multi.df[c(1:3,nrow(multi.df)),]    

#Plot the Capital Appreciation of Each Security
#For the x variable use date, for y use Qualcomm Index

    
    multi <- data.AMGN[,5]
    head(multi)
    multi <- merge(multi,data.JNJ[,5])
    multi <- merge(multi,data.CSCO[,5])
    multi <- merge(multi,data.MSFT[,5])
    multi <- merge(multi,data.CVX[,5])
    multi <- merge(multi,data.XOM[,5])
    
    
    multi[c(1:3,nrow(multi)),]

                #============PLOTS======================
                
                    plot(x=multi.df$date,
                         y=multi.df$AMGN.idx,
                         type="l",
                         xlab="Date",
                         ylab="Value of Investment ($)",
                         col="green",
                         lty=1,
                         lwd=1,
                         main="Value of $1 Investment in
                         AMGN, JNJ, CSCO, MSFT, CVX and XOM from 07-2010 - 07-2020")
                
                    #Adding the lines for the other 5 securities
                    
                       lines(x=multi.df$date,
                          y=multi.df$JNJ.idx,
                          col="dark green",
                          lty=1,
                          lwd=1)
                    
                    lines(x=multi.df$date,
                          y=multi.df$CSCO.idx,
                          col="red",
                          lty=1,
                          lwd=1)
                    
                    lines(x=multi.df$date,
                          y=multi.df$MSFT.idx,
                          col="dark red",
                          lty=1,
                          lwd=1)
                    
                    lines(x=multi.df$date,
                          y=multi.df$CVX.idx,
                          col="blue",
                          lty=1,
                          lwd=1)
                    
                    lines(x=multi.df$date,
                          y=multi.df$XOM.idx,
                          col="dark blue",
                          lty=1,
                          lwd=1)
                    #Adding a line to denote the starting investment value of $1
                    
                    abline(h=1,lty=2,col="black")
                    
                    #Adding a legend
                    legend("topleft",
                           c("AMGN","JNJ","CSCO","MSFT","CVX","XOM"),
                           col=c("green","dark green","red","dark red","blue","dark blue"),
                           lty=c(1,1,1,1,1,1),
                           lwd=c(1,1,1,1,1,1))
                    
                  #The plot cuts off the values of some securities.
                    y.range <- range(multi.df[,6:9])  # find minimum/maximum of all 6 securities
                    y.range
                    
                #Trend: Simple Moving Average Crossover
                    #Obtain Closing Prices for AMGN Stock
                    
                        AMGN.sma <- data.AMGN[,4]
                        AMGN.sma[c(1:3,nrow(AMGN.sma)),]
                        #Trend: Simple Moving Average Crossover
                        #Calculate the Rolling 50-Day and 200-Day Average Price
                        AMGN.sma$sma50 <- rollmeanr(AMGN.sma$AMGN.Close,k=50)
                        AMGN.sma$sma200 <- rollmeanr(AMGN.sma$AMGN.Close,k=200)
                        AMGN.sma[c(1:3,nrow(AMGN.sma)),]                    
                    
                    #Subset to 2019 data
                    
                        AMGN.sma2019 <- subset(AMGN.sma,
                                           index(AMGN.sma)>"2019-01-01")
                    
                    #Plot the SMA
                        y.range <- range(AMGN.sma,na.rm=TRUE)
                        y.range
                        par(mfrow=c(1,1))
                        plot(x=index(AMGN.sma),
                         xlab="Date",
                         y=AMGN.sma$AMGN.Close,
                         ylim=y.range,
                         ylab="Price ($)",
                         type="l",
                         col="dark green",
                         lty=1,
                         lwd=1,
                         main="Amgen - Simple Moving Average")
                    
                         lines(x=index(AMGN.sma),y=AMGN.sma$sma50)
                         lines(x=index(AMGN.sma),y=AMGN.sma$sma200,lty=2)
                          legend("topleft",
                           c("Amgen Price","50-Day Moving Average", "200-Day Moving Average"),
                           lty=c(1,1,2))
                
                #----------------------
                          
                          JNJ.sma <- data.JNJ[,4]
                          JNJ.sma[c(1:3,nrow(JNJ.sma)),]
                          #Trend: Simple Moving Average Crossover
                          #Calculate the Rolling 50-Day and 200-Day Average Price
                          JNJ.sma$sma50 <- rollmeanr(JNJ.sma$JNJ.Close,k=50)
                          JNJ.sma$sma200 <- rollmeanr(JNJ.sma$JNJ.Close,k=200)
                          JNJ.sma[c(1:3,nrow(JNJ.sma)),]                    
                          
                          #Subset to  data
                          
                          JNJ.sma <- subset(JNJ.sma,
                                                 index(JNJ.sma)>"-01-01")
                          
                          #Plot the SMA
                          y.range <- range(JNJ.sma,na.rm=TRUE)
                          y.range
                          par(mfrow=c(1,1))
                          plot(x=index(JNJ.sma),
                               xlab="Date",
                               y=JNJ.sma$JNJ.Close,
                               ylim=y.range,
                               ylab="Price ($)",
                               type="l",
                               col="dark green",
                               lty=1,
                               lwd=1,
                               main="JNJ - Simple Moving Average")
                          
                          lines(x=index(JNJ.sma),y=JNJ.sma$sma50)
                          lines(x=index(JNJ.sma),y=JNJ.sma$sma200,lty=2)
                          legend("topleft",
                                 c("JNJ Price","50-Day Moving Average", "200-Day Moving Average"),
                                 lty=c(1,1,2))
                 
                          
                          #_________________
                          
                          #----------------------
                          
                          CSCO.sma <- data.CSCO[,4]
                          CSCO.sma[c(1:3,nrow(CSCO.sma)),]
                          #Trend: Simple Moving Average Crossover
                          #Calculate the Rolling 50-Day and 200-Day Average Price
                          CSCO.sma$sma50 <- rollmeanr(CSCO.sma$CSCO.Close,k=50)
                          CSCO.sma$sma200 <- rollmeanr(CSCO.sma$CSCO.Close,k=200)
                          CSCO.sma[c(1:3,nrow(CSCO.sma)),]                    
                          
                          #Subset to  data
                          
                          CSCO.sma <- subset(CSCO.sma,
                                                index(CSCO.sma)>"-01-01")
                          
                          #Plot the SMA
                          y.range <- range(CSCO.sma,na.rm=TRUE)
                          y.range
                          par(mfrow=c(1,1))
                          plot(x=index(CSCO.sma),
                               xlab="Date",
                               y=CSCO.sma$CSCO.Close,
                               ylim=y.range,
                               ylab="Price ($)",
                               type="l",
                               col="red",
                               lty=1,
                               lwd=1,
                               main="CSCO - Simple Moving Average")
                          
                          lines(x=index(CSCO.sma),y=CSCO.sma$sma50)
                          lines(x=index(CSCO.sma),y=CSCO.sma$sma200,lty=2)
                          legend("topleft",
                                 c("CSCO Price","50-Day Moving Average", "200-Day Moving Average"),
                                 lty=c(1,1,2))
                          
                          
                          #_________________         
                
                          #----------------------
                          
                          MSFT.sma <- data.MSFT[,4]
                          MSFT.sma[c(1:3,nrow(MSFT.sma)),]
                          #Trend: Simple Moving Average Crossover
                          #Calculate the Rolling 50-Day and 200-Day Average Price
                          MSFT.sma$sma50 <- rollmeanr(MSFT.sma$MSFT.Close,k=50)
                          MSFT.sma$sma200 <- rollmeanr(MSFT.sma$MSFT.Close,k=200)
                          MSFT.sma[c(1:3,nrow(MSFT.sma)),]                    
                          
                          #Subset to  data
                          
                          MSFT.sma <- subset(MSFT.sma,
                                                 index(MSFT.sma)>"-01-01")
                          
                          #Plot the SMA
                          y.range <- range(MSFT.sma,na.rm=TRUE)
                          y.range
                          par(mfrow=c(1,1))
                          plot(x=index(MSFT.sma),
                               xlab="Date",
                               y=MSFT.sma$MSFT.Close,
                               ylim=y.range,
                               ylab="Price ($)",
                               type="l",
                               col="dark red",
                               lty=1,
                               lwd=1,
                               main="MSFT - Simple Moving Average")
                          
                          lines(x=index(MSFT.sma),y=MSFT.sma$sma50)
                          lines(x=index(MSFT.sma),y=MSFT.sma$sma200,lty=2)
                          legend("topleft",
                                 c("MSFT Price","50-Day Moving Average", "200-Day Moving Average"),
                                 lty=c(1,1,2))
                          
                          
                          #_________________         
                          
                          #----------------------
                          
                          CVX.sma <- data.CVX[,4]
                          CVX.sma[c(1:3,nrow(CVX.sma)),]
                          #Trend: Simple Moving Average Crossover
                          #Calculate the Rolling 50-Day and 200-Day Average Price
                          CVX.sma$sma50 <- rollmeanr(CVX.sma$CVX.Close,k=50)
                          CVX.sma$sma200 <- rollmeanr(CVX.sma$CVX.Close,k=200)
                          CVX.sma[c(1:3,nrow(CVX.sma)),]                    
                          
                          #Subset to  data
                          
                          CVX.sma <- subset(CVX.sma,
                                                 index(CVX.sma)>"-01-01")
                          
                          #Plot the SMA
                          y.range <- range(CVX.sma,na.rm=TRUE)
                          y.range
                          par(mfrow=c(1,1))
                          plot(x=index(CVX.sma),
                               xlab="Date",
                               y=CVX.sma$CVX.Close,
                               ylim=y.range,
                               ylab="Price ($)",
                               type="l",
                               col="blue",
                               lty=1,
                               lwd=1,
                               main="CVX - Simple Moving Average")
                          
                          lines(x=index(CVX.sma),y=CVX.sma$sma50)
                          lines(x=index(CVX.sma),y=CVX.sma$sma200,lty=2)
                          legend("topleft",
                                 c("CVX Price","50-Day Moving Average", "200-Day Moving Average"),
                                 lty=c(1,1,2))
                          
                          
                          #_________________         
                          
                          #----------------------
                          
                          XOM.sma <- data.XOM[,4]
                          XOM.sma[c(1:3,nrow(XOM.sma)),]
                          #Trend: Simple Moving Average Crossover
                          #Calculate the Rolling 50-Day and 200-Day Average Price
                          XOM.sma$sma50 <- rollmeanr(XOM.sma$XOM.Close,k=50)
                          XOM.sma$sma200 <- rollmeanr(XOM.sma$XOM.Close,k=200)
                          XOM.sma[c(1:3,nrow(XOM.sma)),]                    
                          
                          #Subset to  data
                          
                          XOM.sma <- subset(XOM.sma,
                                                 index(XOM.sma)>"-01-01")
                          
                          #Plot the SMA
                          y.range <- range(XOM.sma,na.rm=TRUE)
                          y.range
                          par(mfrow=c(1,1))
                          plot(x=index(XOM.sma),
                               xlab="Date",
                               y=XOM.sma$XOM.Close,
                               ylim=y.range,
                               ylab="Price ($)",
                               type="l",
                               col="dark blue",
                               lty=1,
                               lwd=1,
                               main="XOM - Simple Moving Average")
                          
                          lines(x=index(XOM.sma),y=XOM.sma$sma50)
                          lines(x=index(XOM.sma),y=XOM.sma$sma200,lty=2)
                          legend("topleft",
                                 c("XOM Price","50-Day Moving Average", "200-Day Moving Average"),
                                 lty=c(1,1,2))
                          
                          
                          #_________________         
                
                #Bollinger Bands (a measure of volatility) - Amgen 
                #20-day simple moving average (SMA)
                #upper band: two std.dev above the 20-day SMA
                #lower band: two std.dev below the 20-day SMA
                
                #Obtain Closing Price for AMGN
                    AMGN.bb <- data.AMGN[,4]
                    AMGN.bb[c(1:3,nrow(AMGN.bb)),]
                
                #Calculate Rolling 20-Day Mean and Std.Dev
                    AMGN.bb$avg <- rollmeanr(AMGN.bb$AMGN.Close,k=20)
                    AMGN.bb$sd <- rollapply(AMGN.bb$AMGN.Close,width = 20,FUN = sd,fill = NA)
                    AMGN.bb[c(1:3,nrow(AMGN.bb)),]
                #Subset to 2019 Data only
                    AMGN.bb2019 <- subset(AMGN.bb,
                                      index(AMGN.bb)>="2019-07-01")
                    AMGN.bb2019[c(1:3,nrow(AMGN.bb2019)),]
                
                #Calculate the Bollinger Bands
                    AMGN.bb2019$sd2up <- AMGN.bb2019$avg+2*AMGN.bb2019$sd
                    AMGN.bb2019$sd2down <- AMGN.bb2019$avg-2*AMGN.bb2019$sd
                    AMGN.bb2019[c(1:3,nrow(AMGN.bb2019)),]
                #Plot the Bollinger Bands
                    y.range <- range(AMGN.bb2019[,-3],na.rm=TRUE)
                    plot(x=index(AMGN.bb2019),
                     xlab="Date",
                     y=AMGN.bb2019$AMGN.Close,
                     ylim = y.range,
                     ylab = "Price ($)",
                     type = "l",
                     col = "blue",
                     lwd=3,
                     main="AMGN - Bollinger Bands (20 days, 2 deviations)
                     July 1, 2019 - July 1,2020")
                        lines(x=index(AMGN.bb2019),y=AMGN.bb2019$avg,lty=2, col= "black")
                        lines(x=index(AMGN.bb2019),y=AMGN.bb2019$sd2up,col="sky blue")
                        lines(x=index(AMGN.bb2019),y=AMGN.bb2019$sd2down,col="dark blue")
                        legend("topleft",
                        c("AMGN Price","20-Day Moving Average","Upper Band","Lower Band"),
                        lty=c(1,2,1,1),
                       lwd=c(3,1,1,1),
                       col=c("blue","black","sky blue","dark blue"))
                
                #-----------------
                        #Bollinger Bands (a measure of volatility)
                        #20-day simple moving average (SMA)
                        #upper band: two std.dev above the 20-day SMA
                        #lower band: two std.dev below the 20-day SMA
                        
                        #Obtain Closing Price for JNJ
                        JNJ.bb <- data.JNJ[,4]
                        JNJ.bb[c(1:3,nrow(JNJ.bb)),]
                        
                        #Calculate Rolling 20-Day Mean and Std.Dev
                        JNJ.bb$avg <- rollmeanr(JNJ.bb$JNJ.Close,k=20)
                        JNJ.bb$sd <- rollapply(JNJ.bb$JNJ.Close,width = 20,FUN = sd,fill = NA)
                        JNJ.bb[c(1:3,nrow(JNJ.bb)),]
                        #Subset to 2019 Data only
                        JNJ.bb2019 <- subset(JNJ.bb,
                                              index(JNJ.bb)>="2019-07-01")
                        JNJ.bb2019[c(1:3,nrow(JNJ.bb2019)),]
                        
                        #Calculate the Bollinger Bands
                        JNJ.bb2019$sd2up <- JNJ.bb2019$avg+2*JNJ.bb2019$sd
                        JNJ.bb2019$sd2down <- JNJ.bb2019$avg-2*JNJ.bb2019$sd
                        JNJ.bb2019[c(1:3,nrow(JNJ.bb2019)),]
                        #Plot the Bollinger Bands
                        y.range <- range(JNJ.bb2019[,-3],na.rm=TRUE)
                        plot(x=index(JNJ.bb2019),
                             xlab="Date",
                             y=JNJ.bb2019$JNJ.Close,
                             ylim = y.range,
                             ylab = "Price ($)",
                             type = "l",
                             col = "blue",
                             lwd=3,
                             main="JNJ - Bollinger Bands (20 days, 2 deviations)
                             July 1, 2019 - July 1,2020")
                        lines(x=index(JNJ.bb2019),y=JNJ.bb2019$avg,lty=2, col= "black")
                        lines(x=index(JNJ.bb2019),y=JNJ.bb2019$sd2up,col="sky blue")
                        lines(x=index(JNJ.bb2019),y=JNJ.bb2019$sd2down,col="dark blue")
                        legend("topleft",
                               c("JNJ Price","20-Day Moving Average","Upper Band","Lower Band"),
                               lty=c(1,2,1,1),
                               lwd=c(3,1,1,1),
                               col=c("blue","black","sky blue","dark blue"))        
                #--------------------        
                
                        #-----------------
                        #Bollinger Bands (a measure of volatility)
                        #20-day simple moving average (SMA)
                        #upper band: two std.dev above the 20-day SMA
                        #lower band: two std.dev below the 20-day SMA
                        
                        #Obtain Closing Price for CSCO
                        CSCO.bb <- data.CSCO[,4]
                        CSCO.bb[c(1:3,nrow(CSCO.bb)),]
                        
                        #Calculate Rolling 20-Day Mean and Std.Dev
                        CSCO.bb$avg <- rollmeanr(CSCO.bb$CSCO.Close,k=20)
                        CSCO.bb$sd <- rollapply(CSCO.bb$CSCO.Close,width = 20,FUN = sd,fill = NA)
                        CSCO.bb[c(1:3,nrow(CSCO.bb)),]
                        #Subset to 2019 Data only
                        CSCO.bb2019 <- subset(CSCO.bb,
                                             index(CSCO.bb)>="2019-07-01")
                        CSCO.bb2019[c(1:3,nrow(CSCO.bb2019)),]
                        
                        #Calculate the Bollinger Bands
                        CSCO.bb2019$sd2up <- CSCO.bb2019$avg+2*CSCO.bb2019$sd
                        CSCO.bb2019$sd2down <- CSCO.bb2019$avg-2*CSCO.bb2019$sd
                        CSCO.bb2019[c(1:3,nrow(CSCO.bb2019)),]
                        #Plot the Bollinger Bands
                        y.range <- range(CSCO.bb2019[,-3],na.rm=TRUE)
                        plot(x=index(CSCO.bb2019),
                             xlab="Date",
                             y=CSCO.bb2019$CSCO.Close,
                             ylim = y.range,
                             ylab = "Price ($)",
                             type = "l",
                             col = "blue",
                             lwd=3,
                             main="CSCO - Bollinger Bands (20 days, 2 deviations)
                             July 1, 2019 - July 1,2020")
                        lines(x=index(CSCO.bb2019),y=CSCO.bb2019$avg,lty=2, col= "black")
                        lines(x=index(CSCO.bb2019),y=CSCO.bb2019$sd2up,col="sky blue")
                        lines(x=index(CSCO.bb2019),y=CSCO.bb2019$sd2down,col="dark blue")
                        legend("topleft",
                               c("CSCO Price","20-Day Moving Average","Upper Band","Lower Band"),
                               lty=c(1,2,1,1),
                               lwd=c(3,1,1,1),
                               col=c("blue","black","sky blue","dark blue"))        
                        #--------------------     
                        
                        #-----------------
                        #Bollinger Bands (a measure of volatility)
                        #20-day simple moving average (SMA)
                        #upper band: two std.dev above the 20-day SMA
                        #lower band: two std.dev below the 20-day SMA
                        
                        #Obtain Closing Price for MSFT
                        MSFT.bb <- data.MSFT[,4]
                        MSFT.bb[c(1:3,nrow(MSFT.bb)),]
                        
                        #Calculate Rolling 20-Day Mean and Std.Dev
                        MSFT.bb$avg <- rollmeanr(MSFT.bb$MSFT.Close,k=20)
                        MSFT.bb$sd <- rollapply(MSFT.bb$MSFT.Close,width = 20,FUN = sd,fill = NA)
                        MSFT.bb[c(1:3,nrow(MSFT.bb)),]
                        #Subset to 2019 Data only
                        MSFT.bb2019 <- subset(MSFT.bb,
                                             index(MSFT.bb)>="2019-07-01")
                        MSFT.bb2019[c(1:3,nrow(MSFT.bb2019)),]
                        
                        #Calculate the Bollinger Bands
                        MSFT.bb2019$sd2up <- MSFT.bb2019$avg+2*MSFT.bb2019$sd
                        MSFT.bb2019$sd2down <- MSFT.bb2019$avg-2*MSFT.bb2019$sd
                        MSFT.bb2019[c(1:3,nrow(MSFT.bb2019)),]
                        #Plot the Bollinger Bands
                        y.range <- range(MSFT.bb2019[,-3],na.rm=TRUE)
                        plot(x=index(MSFT.bb2019),
                             xlab="Date",
                             y=MSFT.bb2019$MSFT.Close,
                             ylim = y.range,
                             ylab = "Price ($)",
                             type = "l",
                             col = "blue",
                             lwd=3,
                             main="MSFT - Bollinger Bands (20 days, 2 deviations)
                             July 1, 2019 - July 1,2020")
                        lines(x=index(MSFT.bb2019),y=MSFT.bb2019$avg,lty=2, col= "black")
                        lines(x=index(MSFT.bb2019),y=MSFT.bb2019$sd2up,col="sky blue")
                        lines(x=index(MSFT.bb2019),y=MSFT.bb2019$sd2down,col="dark blue")
                        legend("topleft",
                               c("MSFT Price","20-Day Moving Average","Upper Band","Lower Band"),
                               lty=c(1,2,1,1),
                               lwd=c(3,1,1,1),
                               col=c("blue","black","sky blue","dark blue"))        
                        #--------------------     
                        
                        #-----------------
                        #Bollinger Bands (a measure of volatility)
                        #20-day simple moving average (SMA)
                        #upper band: two std.dev above the 20-day SMA
                        #lower band: two std.dev below the 20-day SMA
                        
                        #Obtain Closing Price for CVX
                        CVX.bb <- data.CVX[,4]
                        CVX.bb[c(1:3,nrow(CVX.bb)),]
                        
                        #Calculate Rolling 20-Day Mean and Std.Dev
                        CVX.bb$avg <- rollmeanr(CVX.bb$CVX.Close,k=20)
                        CVX.bb$sd <- rollapply(CVX.bb$CVX.Close,width = 20,FUN = sd,fill = NA)
                        CVX.bb[c(1:3,nrow(CVX.bb)),]
                        #Subset to 2019 Data only
                        CVX.bb2019 <- subset(CVX.bb,
                                             index(CVX.bb)>="2019-07-01")
                        CVX.bb2019[c(1:3,nrow(CVX.bb2019)),]
                        
                        #Calculate the Bollinger Bands
                        CVX.bb2019$sd2up <- CVX.bb2019$avg+2*CVX.bb2019$sd
                        CVX.bb2019$sd2down <- CVX.bb2019$avg-2*CVX.bb2019$sd
                        CVX.bb2019[c(1:3,nrow(CVX.bb2019)),]
                        #Plot the Bollinger Bands
                        y.range <- range(CVX.bb2019[,-3],na.rm=TRUE)
                        plot(x=index(CVX.bb2019),
                             xlab="Date",
                             y=CVX.bb2019$CVX.Close,
                             ylim = y.range,
                             ylab = "Price ($)",
                             type = "l",
                             col = "blue",
                             lwd=3,
                             main="CVX - Bollinger Bands (20 days, 2 deviations)
                             July 1, 2019 - July 1,2020")
                        lines(x=index(CVX.bb2019),y=CVX.bb2019$avg,lty=2, col= "black")
                        lines(x=index(CVX.bb2019),y=CVX.bb2019$sd2up,col="sky blue")
                        lines(x=index(CVX.bb2019),y=CVX.bb2019$sd2down,col="dark blue")
                        legend("topleft",
                               c("CVX Price","20-Day Moving Average","Upper Band","Lower Band"),
                               lty=c(1,2,1,1),
                               lwd=c(3,1,1,1),
                               col=c("blue","black","sky blue","dark blue"))        
                        #--------------------     
                        
                        
                        #-----------------
                        #Bollinger Bands (a measure of volatility)
                        #20-day simple moving average (SMA)
                        #upper band: two std.dev above the 20-day SMA
                        #lower band: two std.dev below the 20-day SMA
                        
                        #Obtain Closing Price for XOM
                        XOM.bb <- data.XOM[,4]
                        XOM.bb[c(1:3,nrow(XOM.bb)),]
                        
                        #Calculate Rolling 20-Day Mean and Std.Dev
                        XOM.bb$avg <- rollmeanr(XOM.bb$XOM.Close,k=20)
                        XOM.bb$sd <- rollapply(XOM.bb$XOM.Close,width = 20,FUN = sd,fill = NA)
                        XOM.bb[c(1:3,nrow(XOM.bb)),]
                        #Subset to 2019 Data only
                        XOM.bb2019 <- subset(XOM.bb,
                                             index(XOM.bb)>="2019-07-01")
                        XOM.bb2019[c(1:3,nrow(XOM.bb2019)),]
                        
                        #Calculate the Bollinger Bands
                        XOM.bb2019$sd2up <- XOM.bb2019$avg+2*XOM.bb2019$sd
                        XOM.bb2019$sd2down <- XOM.bb2019$avg-2*XOM.bb2019$sd
                        XOM.bb2019[c(1:3,nrow(XOM.bb2019)),]
                        #Plot the Bollinger Bands
                        y.range <- range(XOM.bb2019[,-3],na.rm=TRUE)
                        plot(x=index(XOM.bb2019),
                             xlab="Date",
                             y=XOM.bb2019$XOM.Close,
                             ylim = y.range,
                             ylab = "Price ($)",
                             type = "l",
                             col = "blue",
                             lwd=3,
                             main="XOM - Bollinger Bands (20 days, 2 deviations)
                             July 1, 2019 - July 1,2020")
                        lines(x=index(XOM.bb2019),y=XOM.bb2019$avg,lty=2, col= "black")
                        lines(x=index(XOM.bb2019),y=XOM.bb2019$sd2up,col="sky blue")
                        lines(x=index(XOM.bb2019),y=XOM.bb2019$sd2down,col="dark blue")
                        legend("topleft",
                               c("XOM Price","20-Day Moving Average","Upper Band","Lower Band"),
                               lty=c(1,2,1,1),
                               lwd=c(3,1,1,1),
                               col=c("blue","black","sky blue","dark blue"))        
                        #--------------------     
                        
                
#-------------------HOMEWORK 2 QUESTION 1 Starts here------------------#
#------------------------------------------------------
#Calculating individual security returns - HW2 Question 1



#Import AMGN Data from Yahoo Finance

#In this dataset, we need the data only for the year 2019. 

    data.AMGN <- subset(data.AMGN,
                        index(data.AMGN)>"2019-07-01")
    
    data.JNJ <- subset(data.JNJ,
                       index(data.JNJ)>"2019-07-01")
    
    data.CSCO <- subset(data.CSCO,
                        index(data.CSCO)>"2019-07-01")
    
    data.MSFT <- subset(data.MSFT,
                        index(data.MSFT)>"2019-07-01")
    data.CVX <- subset(data.CVX,
                        index(data.CVX)>"2019-07-01")
    data.XOM <- subset(data.XOM,
                        index(data.XOM)>"2019-07-01")

# Calculate EW portfolio values for 1Q 2019

#Subset to include only closing price

    AMGN.prc.ret <- data.AMGN[,4]
    AMGN.prc.ret[c(1:3,nrow(AMGN.prc.ret)),]
    
    JNJ.prc.ret <- data.JNJ[,4]
    JNJ.prc.ret[c(1:3,nrow(JNJ.prc.ret)),]
    
    CSCO.prc.ret <- data.CSCO[,4]
    CSCO.prc.ret[c(1:3,nrow(CSCO.prc.ret)),]
    
    MSFT.prc.ret <- data.MSFT[,4]
    MSFT.prc.ret[c(1:3,nrow(MSFT.prc.ret)),]
    
    CVX.prc.ret <- data.CVX[,4]
    CVX.prc.ret[c(1:3,nrow(CVX.prc.ret)),]
    
    XOM.prc.ret <- data.XOM[,4]
    XOM.prc.ret[c(1:3,nrow(XOM.prc.ret)),]
    
    
#Calculate AMGN, JNJ, CSCO, MSFT, XOM and CVX Price Return (using Delt command)

    library(quantmod)
    
    AMGN.prc.ret$AMGN.prc.ret <- Delt(AMGN.prc.ret$AMGN.Close)
    AMGN.prc.ret[c(1:3,nrow(AMGN.prc.ret)),]

    JNJ.prc.ret$JNJ.prc.ret <-  Delt(JNJ.prc.ret$JNJ.Close)
    JNJ.prc.ret[c(1:3,nrow(JNJ.prc.ret)),]
    
    CSCO.prc.ret$CSCO.prc.ret <-  Delt(CSCO.prc.ret$CSCO.Close)
    CSCO.prc.ret[c(1:3,nrow(CSCO.prc.ret)),]
    
    MSFT.prc.ret$MSFT.prc.ret <-  Delt(MSFT.prc.ret$MSFT.Close)
    MSFT.prc.ret[c(1:3,nrow(MSFT.prc.ret)),]
    
    CVX.prc.ret$CVX.prc.ret <-  Delt(CVX.prc.ret$CVX.Close)
    CVX.prc.ret[c(1:3,nrow(CVX.prc.ret)),]
    
    XOM.prc.ret$XOM.prc.ret <-  Delt(XOM.prc.ret$XOM.Close)
    XOM.prc.ret[c(1:3,nrow(XOM.prc.ret)),]
    
#Clean up the data object (remove the NA and
#[Stock].close - only need returns column)
    options(digits = 5)
    
    head(AMGN.prc.ret)
    
    AMGN.prc.ret<-AMGN.prc.ret[-1,2]
    AMGN.prc.ret[c(1:3,nrow(AMGN.prc.ret)),]
    
    JNJ.prc.ret<-JNJ.prc.ret[-1,2]
    JNJ.prc.ret[c(1:3,nrow(JNJ.prc.ret)),]
    
    CSCO.prc.ret<-CSCO.prc.ret[-1,2]
    CSCO.prc.ret[c(1:3,nrow(CSCO.prc.ret)),]
    
    MSFT.prc.ret<-MSFT.prc.ret[-1,2]
    MSFT.prc.ret[c(1:3,nrow(MSFT.prc.ret)),]
    
    CVX.prc.ret<-CVX.prc.ret[-1,2]
    CVX.prc.ret[c(1:3,nrow(CVX.prc.ret)),]
    
    XOM.prc.ret<-XOM.prc.ret[-1,2]
    XOM.prc.ret[c(1:3,nrow(XOM.prc.ret)),]

#Calculate Daily Total Return
#Import adjusted closing price data
    AMGN.ret <- data.AMGN[,5]
    AMGN.ret[c(1:3,nrow(AMGN.ret)),]
    
    JNJ.ret <- data.JNJ[,5]
    JNJ.ret[c(1:3,nrow(JNJ.ret)),]
    
    CSCO.ret <- data.CSCO[,5]
    CSCO.ret[c(1:3,nrow(CSCO.ret)),]
    
    MSFT.ret <- data.MSFT[,5]
    MSFT.ret[c(1:3,nrow(MSFT.ret)),]
    
    CVX.ret <- data.CVX[,5]
    CVX.ret[c(1:3,nrow(CVX.ret)),]
    
    XOM.ret <- data.XOM[,5]
    XOM.ret[c(1:3,nrow(XOM.ret)),]
    
#Calculate Total Return (should be at least as
#large as the price return)

    AMGN.ret$AMGN.tot.ret=Delt(AMGN.ret$AMGN.Adjusted)
    AMGN.ret[c(1:3,nrow(AMGN.ret)),]
    
    JNJ.ret$JNJ.tot.ret=Delt(JNJ.ret$JNJ.Adjusted)
    JNJ.ret[c(1:3,nrow(JNJ.ret)),]
    
    CSCO.ret$CSCO.tot.ret=Delt(CSCO.ret$CSCO.Adjusted)
    CSCO.ret[c(1:3,nrow(CSCO.ret)),]
    
    MSFT.ret$MSFT.tot.ret=Delt(MSFT.ret$MSFT.Adjusted)
    MSFT.ret[c(1:3,nrow(MSFT.ret)),]

    CVX.ret$CVX.tot.ret=Delt(CVX.ret$CVX.Adjusted)
    CVX.ret[c(1:3,nrow(CVX.ret)),]
    
    XOM.ret$XOM.tot.ret=Delt(XOM.ret$XOM.Adjusted)
    XOM.ret[c(1:3,nrow(XOM.ret)),]
    
    
    #Clean up the data
    options(digits = 3)
    AMGN.tot.ret <- AMGN.ret[-1,2]
    AMGN.tot.ret[c(1:3,nrow(AMGN.tot.ret)),]
    
    options(digits = 3)
    JNJ.tot.ret <- JNJ.ret[-1,2]
    JNJ.tot.ret[c(1:3,nrow(JNJ.tot.ret)),]
    
    options(digits = 3)
    CSCO.tot.ret <- CSCO.ret[-1,2]
    CSCO.tot.ret[c(1:3,nrow(CSCO.tot.ret)),]
    
    options(digits = 3)
    MSFT.tot.ret <- MSFT.ret[-1,2]
    MSFT.tot.ret[c(1:3,nrow(MSFT.tot.ret)),]

    options(digits = 3)
    CVX.tot.ret <- CVX.ret[-1,2]
    CVX.tot.ret[c(1:3,nrow(CVX.tot.ret)),]
    
    options(digits = 3)
    XOM.tot.ret <- XOM.ret[-1,2]
    XOM.tot.ret[c(1:3,nrow(XOM.tot.ret)),]
    
    
#Logarithmic Total Returns


#Logarithmic returns will be used for cumulative returns
# calculations

    # Import adjusted closing price data
    options(digits = 5) #to include decimals
    AMGN.ret <- data.AMGN[,5]
    AMGN.ret[c(1:3,nrow(AMGN.ret)),]
    #Logarithmic Total Returns
    AMGN.log.ret <- data.AMGN[,5]
    AMGN.log.ret[c(1:3,nrow(AMGN.log.ret)),]
    AMGN.log.ret$AMGN.log.ret <- diff(log(AMGN.log.ret))
    AMGN.log.ret[c(1:3,nrow(AMGN.log.ret)),]
    #Logarithmic Total Returns
    #Clean up the data
    options(digits=3)
    AMGN.log.ret <- AMGN.log.ret[,2]
    AMGN.log.ret[c(1:3,nrow(AMGN.log.ret)),]

    #==Candlestick charts
                #Converting Daily prices to weekly/weekly
                #to.weekly command - covert into weekly data
                
                    AMGN.log.df <- data.frame(AMGN.log.ret)
                    wk <- AMGN.log.df
                    data.weekly <- to.weekly(wk)
                    data.weekly[c(1:3,nrow(data.weekly)),]        
                    
                    #to.monthly command - covert into weekly data
                    mo <- AMGN.log.df
                    
                    data.monthly <- to.monthly(mo)
                    data.monthly[c(1:3,nrow(data.monthly)),]
                    
                
                #Candlestick Chart using Weekly Data                 
                #Before plotting, first create a open-high-low-close (OHLC) object. 
                #Use quantmod package
                    #install.packages(quantmod)
                    library(quantmod)
                    OHLC <- data.weekly[-1,-6]
                    AMGN.ohlc <- as.quantmod.OHLC(OHLC,col.names=c("wk.Open", "wk.High", "wk.Low", "wk.Close"))
                    AMGN.ohlc[c(1:3,nrow(AMGN.ohlc)),]	
                
                #Candlestick Chart using weekly Data                 
                    chartSeries(AMGN.ohlc,theme="white.mono",name="AMGN OHLC")
                
                #Candlestick Chart using monthly Data                 
                
                    oHLC <- data.monthly[-1,-6]
                    AMGN.ohlc <- as.quantmod.OHLC(OHLC,col.names=c("mo.Open", "mo.High", "mo.Low", "mo.Close"))
                    AMGN.ohlc[c(1:3,nrow(AMGN.ohlc)),]	
                
                #Candlestick Chart using monthly Data                 
                    
                    chartSeries(AMGN.ohlc,theme="white.mono",name="AMGN OHLC")
                
#JNJ

    options(digits = 5) #to include decimals
    JNJ.ret <- data.JNJ[,5]
    JNJ.ret[c(1:3,nrow(JNJ.ret)),]

#Logarithmic Total Returns

    JNJ.log.ret <- data.JNJ[,5]
    JNJ.log.ret[c(1:3,nrow(JNJ.log.ret)),]
    JNJ.log.ret$JNJ.log.ret <- diff(log(JNJ.log.ret))
    JNJ.log.ret[c(1:3,nrow(JNJ.log.ret)),]

    #Logarithmic Total Returns

#Clean up the data
    options(digits=3)
    JNJ.log.ret <- JNJ.log.ret[,2]
    JNJ.log.ret[c(1:3,nrow(JNJ.log.ret)),]
    
#CSCO
    options(digits = 5) #to include decimals
    CSCO.ret <- data.CSCO[,5]
    CSCO.ret[c(1:3,nrow(CSCO.ret)),]

#Logarithmic Total Returns
    CSCO.log.ret <- data.CSCO[,5]
    CSCO.log.ret[c(1:3,nrow(CSCO.log.ret)),]
    CSCO.log.ret$CSCO.log.ret <- diff(log(CSCO.log.ret))
    CSCO.log.ret[c(1:3,nrow(CSCO.log.ret)),]

#Logarithmic Total Returns
#Clean up the data

    options(digits=3)
    CSCO.log.ret <- CSCO.log.ret[,2]
    CSCO.log.ret[c(1:3,nrow(CSCO.log.ret)),]


    #----
    
    #MSFT
    
    options(digits = 5) #to include decimals
    MSFT.ret <- data.MSFT[,5]
    MSFT.ret[c(1:3,nrow(MSFT.ret)),]
    
    #Logarithmic Total Returns
    
    MSFT.log.ret <- data.MSFT[,5]
    MSFT.log.ret[c(1:3,nrow(MSFT.log.ret)),]
    MSFT.log.ret$MSFT.log.ret <- diff(log(MSFT.log.ret))
    MSFT.log.ret[c(1:3,nrow(MSFT.log.ret)),]
    
    #Logarithmic Total Returns
    
    #Clean up the data
    options(digits=3)
    MSFT.log.ret <- MSFT.log.ret[,2]
    MSFT.log.ret[c(1:3,nrow(MSFT.log.ret)),]
    
    #-------
    
    
    #----
    
    #MSFT
    
    options(digits = 5) #to include decimals
    MSFT.ret <- data.MSFT[,5]
    MSFT.ret[c(1:3,nrow(MSFT.ret)),]
    
    #Logarithmic Total Returns
    
    MSFT.log.ret <- data.MSFT[,5]
    MSFT.log.ret[c(1:3,nrow(MSFT.log.ret)),]
    MSFT.log.ret$MSFT.log.ret <- diff(log(MSFT.log.ret))
    MSFT.log.ret[c(1:3,nrow(MSFT.log.ret)),]
    
    #Logarithmic Total Returns
    
    #Clean up the data
    options(digits=3)
    MSFT.log.ret <- MSFT.log.ret[,2]
    MSFT.log.ret[c(1:3,nrow(MSFT.log.ret)),]
    
    #-------
    
    #----
    
    #XOM
    
    options(digits = 5) #to include decimals
    XOM.ret <- data.XOM[,5]
    XOM.ret[c(1:3,nrow(XOM.ret)),]
    
    #Logarithmic Total Returns
    
    XOM.log.ret <- data.XOM[,5]
    XOM.log.ret[c(1:3,nrow(XOM.log.ret)),]
    XOM.log.ret$XOM.log.ret <- diff(log(XOM.log.ret))
    XOM.log.ret[c(1:3,nrow(XOM.log.ret)),]
    
    #Logarithmic Total Returns
    
    #Clean up the data
    options(digits=3)
    XOM.log.ret <- XOM.log.ret[,2]
    XOM.log.ret[c(1:3,nrow(XOM.log.ret)),]
    
    #-------
    
                #Converting Daily prices to weekly/weekly
            #to.weekly command - covert into weekly data
            
            JNJ.log.df <- data.frame(JNJ.log.ret)
            wk <- JNJ.log.df
            data.weekly <- to.weekly(wk)
            data.weekly[c(1:3,nrow(data.weekly)),]        
            
            #to.monthly command - covert into weekly data
            mo <- JNJ.log.df
            
            data.monthly <- to.monthly(mo)
            data.monthly[c(1:3,nrow(data.monthly)),]
            
            
            #Candlestick Chart using Weekly Data                 
            #Before plotting, first create a open-high-low-close (OHLC) object. 
            #Use quantmod package
            #install.packages(quantmod)
            library(quantmod)
            OHLC <- data.weekly[-1,-6]
            JNJ.ohlc <- as.quantmod.OHLC(OHLC,col.names=c("wk.Open", "wk.High", "wk.Low", "wk.Close"))
            JNJ.ohlc[c(1:3,nrow(JNJ.ohlc)),]	
            
            #Candlestick Chart using weekly Data                 
            chartSeries(JNJ.ohlc,theme="white.mono",name="JNJ OHLC")
            
            #Candlestick Chart using monthly Data                 
            
            oHLC <- data.monthly[-1,-6]
            JNJ.ohlc <- as.quantmod.OHLC(OHLC,col.names=c("mo.Open", "mo.High", "mo.Low", "mo.Close"))
            JNJ.ohlc[c(1:3,nrow(JNJ.ohlc)),]	
            
            #Candlestick Chart using monthly Data                 
            
            chartSeries(JNJ.ohlc,theme="white.mono",name="JNJ OHLC")
    

            #----
            
            #Converting Daily prices to weekly/weekly
            #to.weekly command - covert into weekly data
            
            CSCO.log.df <- data.frame(CSCO.log.ret)
            wk <- CSCO.log.df
            data.weekly <- to.weekly(wk)
            data.weekly[c(1:3,nrow(data.weekly)),]        
            
            #to.monthly command - covert into weekly data
            mo <- CSCO.log.df
            
            data.monthly <- to.monthly(mo)
            data.monthly[c(1:3,nrow(data.monthly)),]
            
            
            #Candlestick Chart using Weekly Data                 
            #Before plotting, first create a open-high-low-close (OHLC) object. 
            #Use quantmod package
            #install.packages(quantmod)
            library(quantmod)
            OHLC <- data.weekly[-1,-6]
            CSCO.ohlc <- as.quantmod.OHLC(OHLC,col.names=c("wk.Open", "wk.High", "wk.Low", "wk.Close"))
            CSCO.ohlc[c(1:3,nrow(CSCO.ohlc)),]	
            
            #Candlestick Chart using weekly Data                 
            chartSeries(CSCO.ohlc,theme="white.mono",name="CSCO OHLC")
            
            #Candlestick Chart using monthly Data                 
            
            oHLC <- data.monthly[-1,-6]
            CSCO.ohlc <- as.quantmod.OHLC(OHLC,col.names=c("mo.Open", "mo.High", "mo.Low", "mo.Close"))
            CSCO.ohlc[c(1:3,nrow(CSCO.ohlc)),]	
            
            #Candlestick Chart using monthly Data                 
            
            chartSeries(CSCO.ohlc,theme="white.mono",name="CSCO OHLC")
            
            #---------
              
              
              #Converting Daily prices to weekly/weekly
              #to.weekly command - covert into weekly data
              
              MSFT.log.df <- data.frame(MSFT.log.ret)
            wk <- MSFT.log.df
            data.weekly <- to.weekly(wk)
            data.weekly[c(1:3,nrow(data.weekly)),]        
            
            #to.monthly command - covert into weekly data
            mo <- MSFT.log.df
            
            data.monthly <- to.monthly(mo)
            data.monthly[c(1:3,nrow(data.monthly)),]
            
            
            #Candlestick Chart using Weekly Data                 
            #Before plotting, first create a open-high-low-close (OHLC) object. 
            #Use quantmod package
            #install.packages(quantmod)
            library(quantmod)
            OHLC <- data.weekly[-1,-6]
            MSFT.ohlc <- as.quantmod.OHLC(OHLC,col.names=c("wk.Open", "wk.High", "wk.Low", "wk.Close"))
            MSFT.ohlc[c(1:3,nrow(MSFT.ohlc)),]	
            
            #Candlestick Chart using weekly Data                 
            chartSeries(MSFT.ohlc,theme="white.mono",name="MSFT OHLC")
            
            #Candlestick Chart using monthly Data                 
            
            oHLC <- data.monthly[-1,-6]
            MSFT.ohlc <- as.quantmod.OHLC(OHLC,col.names=c("mo.Open", "mo.High", "mo.Low", "mo.Close"))
            MSFT.ohlc[c(1:3,nrow(MSFT.ohlc)),]	
            
            #Candlestick Chart using monthly Data                 
            
            chartSeries(MSFT.ohlc,theme="white.mono",name="MSFT OHLC")
            
            
            #-------
              
              
              #Converting Daily prices to weekly/weekly
              #to.weekly command - covert into weekly data
              
              CVX.log.df <- data.frame(CVX.log.ret)
            wk <- CVX.log.df
            data.weekly <- to.weekly(wk)
            data.weekly[c(1:3,nrow(data.weekly)),]        
            
            #to.monthly command - covert into weekly data
            mo <- CVX.log.df
            
            data.monthly <- to.monthly(mo)
            data.monthly[c(1:3,nrow(data.monthly)),]
            
            
            #Candlestick Chart using Weekly Data                 
            #Before plotting, first create a open-high-low-close (OHLC) object. 
            #Use quantmod package
            #install.packages(quantmod)
            library(quantmod)
            OHLC <- data.weekly[-1,-6]
            CVX.ohlc <- as.quantmod.OHLC(OHLC,col.names=c("wk.Open", "wk.High", "wk.Low", "wk.Close"))
            CVX.ohlc[c(1:3,nrow(CVX.ohlc)),]	
            
            #Candlestick Chart using weekly Data                 
            chartSeries(CVX.ohlc,theme="white.mono",name="CVX OHLC")
            
            #Candlestick Chart using monthly Data                 
            
            oHLC <- data.monthly[-1,-6]
            CVX.ohlc <- as.quantmod.OHLC(OHLC,col.names=c("mo.Open", "mo.High", "mo.Low", "mo.Close"))
            CVX.ohlc[c(1:3,nrow(CVX.ohlc)),]	
            
            #Candlestick Chart using monthly Data                 
            
            chartSeries(CVX.ohlc,theme="white.mono",name="CVX OHLC")
            
            
            #--------
            #Converting Daily prices to weekly/weekly
            #to.weekly command - covert into weekly data
            
            XOM.log.df <- data.frame(XOM.log.ret)
            wk <- XOM.log.df
            data.weekly <- to.weekly(wk)
            data.weekly[c(1:3,nrow(data.weekly)),]        
            
            #to.monthly command - covert into weekly data
            mo <- XOM.log.df
            
            data.monthly <- to.monthly(mo)
            data.monthly[c(1:3,nrow(data.monthly)),]
            
            
            #Candlestick Chart using Weekly Data                 
            #Before plotting, first create a open-high-low-close (OHLC) object. 
            #Use quantmod package
            #install.packages(quantmod)
            library(quantmod)
            OHLC <- data.weekly[-1,-6]
            XOM.ohlc <- as.quantmod.OHLC(OHLC,col.names=c("wk.Open", "wk.High", "wk.Low", "wk.Close"))
            XOM.ohlc[c(1:3,nrow(XOM.ohlc)),]	
            
            #Candlestick Chart using weekly Data                 
            chartSeries(XOM.ohlc,theme="white.mono",name="XOM OHLC")
            
            #Candlestick Chart using monthly Data                 
            
            oHLC <- data.monthly[-1,-6]
            XOM.ohlc <- as.quantmod.OHLC(OHLC,col.names=c("mo.Open", "mo.High", "mo.Low", "mo.Close"))
            XOM.ohlc[c(1:3,nrow(XOM.ohlc)),]	
            
            #Candlestick Chart using monthly Data                 
            
            chartSeries(XOM.ohlc,theme="white.mono",name="XOM OHLC")
            #MSFT

    options(digits = 5) #to include decimals
    MSFT.ret <- data.MSFT[,5]
    MSFT.ret[c(1:3,nrow(MSFT.ret)),]

#Logarithmic Total Returns
    MSFT.log.ret <- data.MSFT[,5]
    MSFT.log.ret[c(1:3,nrow(MSFT.log.ret)),]
    MSFT.log.ret$MSFT.log.ret <- diff(log(MSFT.log.ret))
    MSFT.log.ret[c(1:3,nrow(MSFT.log.ret)),]

#Logarithmic Total Returns
#Clean up the data
    options(digits=3)
    MSFT.log.ret <- MSFT.log.ret[,2]
    MSFT.log.ret[c(1:3,nrow(MSFT.log.ret)),]

#Compare Log Returns with Arithmetic
#Returns

#AMgen
    options(digits=3,scipen=100) #increase the threshold to allow reading the small differences
    tot.rets <- cbind(AMGN.tot.ret,AMGN.log.ret)
    tot.rets[c(1:3,nrow(tot.rets)),]
    max(abs(tot.rets$AMGN.tot.ret - tot.rets$AMGN.log.ret),na.rm=TRUE)
    min(abs(tot.rets$AMGN.tot.ret - tot.rets$AMGN.log.ret),na.rm=TRUE)
    options(digits=7,scipen=0)
#JNJ
    options(digits=3,scipen=100) #increase the threshold to allow reading the small differences
    tot.rets <- cbind(JNJ.tot.ret,JNJ.log.ret)
    tot.rets[c(1:3,nrow(tot.rets)),]
    max(abs(tot.rets$JNJ.tot.ret - tot.rets$JNJ.log.ret),na.rm=TRUE)
    min(abs(tot.rets$JNJ.tot.ret - tot.rets$JNJ.log.ret),na.rm=TRUE)
    options(digits=7,scipen=0)

#CSCO
    options(digits=3,scipen=100) #increase the
    #threshold to allow reading the small differences
    tot.rets <- cbind(CSCO.tot.ret,CSCO.log.ret)
    tot.rets[c(1:3,nrow(tot.rets)),]
    max(abs(tot.rets$CSCO.tot.ret - tot.rets$CSCO.log.ret),na.rm=TRUE)
    min(abs(tot.rets$CSCO.tot.ret - tot.rets$CSCO.log.ret),na.rm=TRUE)
    options(digits=7,scipen=0)

#MSFT
    options(digits=3,scipen=100) #increase the
    #threshold to allow reading the small differences
    tot.rets <- cbind(MSFT.tot.ret,MSFT.log.ret)
    tot.rets[c(1:3,nrow(tot.rets)),]
    max(abs(tot.rets$MSFT.tot.ret - tot.rets$MSFT.log.ret),na.rm=TRUE)
    min(abs(tot.rets$MSFT.tot.ret - tot.rets$MSFT.log.ret),na.rm=TRUE)
    options(digits=7,scipen=0)

#Cumulating Multi-Day Returns

  AMGN.acum <- AMGN.tot.ret
  AMGN.acum[c(1:3,nrow(AMGN.acum)),]
  AMGN.acum[1,1] <-0
  AMGN.acum[c(1:3,nrow(AMGN.acum)),]
  AMGN.acum$GrossRet <- 1+AMGN.acum$AMGN.tot.ret
  AMGN.acum[c(1:3,nrow(AMGN.acum)),]
  AMGN.acum$GrossCum <- cumprod(AMGN.acum$GrossRet)
  AMGN.acum[c(1:3,nrow(AMGN.acum)),]
  AMGN.acum$NetCum <- AMGN.acum$GrossCum-1
  AMGN.acum[c(1:3,nrow(AMGN.acum)),]

                    #PLOT
                        plot(AMGN.acum$AMGN.tot.ret,
                             type="l",
                             xlab="Date",
                             ylab="Multi Day Returns Investment ($)",
                             ylim=c(-1,1.5),
                             col = "black",
                             main="
                    
                            AMGN Stocks Performance Based on 
                             Total Returns - BLACK 
                             Gross returns - RED 
                             Gross Cumulative - GREEN 
                             Net Cumulative Price Returns - BLUE July 2019 -
                             July 2020",
                             legend(x = "topleft",
                                    col = c("black", "red","green", "blue"), lty = 1, lwd = 1,
                                    legend = c('Total Ret', 'Gross Ret','Gross Cumulative','Net Cumulative')),
                             abline(h=1,col="red")
                             
                             )
                        
                        lines(AMGN.acum$GrossRet,
                              type="l", col = "red"
                        )
                        lines(AMGN.acum$GrossCum,
                              type="l",col = "green"
                        )
                        lines(AMGN.acum$NetCum,
                              type="l", col = "blue"
                        )
                        
                    
                    
                    #JNJ
                        JNJ.acum <- JNJ.tot.ret
                        JNJ.acum[c(1:3,nrow(JNJ.acum)),]
                        JNJ.acum[1,1] <-0
                        JNJ.acum[c(1:3,nrow(JNJ.acum)),]
                        JNJ.acum$GrossRet <- 1+JNJ.acum$JNJ.tot.ret
                        JNJ.acum[c(1:3,nrow(JNJ.acum)),]
                        JNJ.acum$GrossCum <- cumprod(JNJ.acum$GrossRet)
                        JNJ.acum[c(1:3,nrow(JNJ.acum)),]
                        JNJ.acum$NetCum <- JNJ.acum$GrossCum-1
                        JNJ.acum[c(1:3,nrow(JNJ.acum)),]
                        
                    
                    #PLOT
                    plot(JNJ.acum$JNJ.tot.ret,
                         type="l",
                         xlab="Date",
                         ylab="Multi Day Returns Investment ($)",
                         ylim=c(-1,1.5),
                         col = "black",
                         main="
                    
                          JNJ Stocks Performance Based on Total Returns - BLACK 
                         Gross returns - RED 
                         Gross Cumulative - GREEN 
                         Net Cumulative Price Returns - BLUE June 31, 2019 -June 31, 2020")
                    
                        lines(JNJ.acum$GrossRet,
                              type="l", col = "red"
                        )
                        lines(JNJ.acum$GrossCum,
                              type="l",col = "green"
                        )
                        lines(JNJ.acum$NetCum,
                              type="l", col = "blue"
                        )
                        legend(x = "topleft",
                               col = c("black", "red","green", "blue"), lty = 1, lwd = 1,
                               legend = c('Total Ret', 'Gross Ret','Gross Cumulative','Net Cumulative'))
                        abline(h=1,col="red")
                    
                    #CSCO
                        CSCO.acum <- CSCO.tot.ret
                        CSCO.acum[c(1:3,nrow(CSCO.acum)),]
                        CSCO.acum[1,1] <-0
                        CSCO.acum[c(1:3,nrow(CSCO.acum)),]
                        CSCO.acum$GrossRet <- 1+CSCO.acum$CSCO.tot.ret
                        CSCO.acum[c(1:3,nrow(CSCO.acum)),]
                        CSCO.acum$GrossCum <- cumprod(CSCO.acum$GrossRet)
                        CSCO.acum[c(1:3,nrow(CSCO.acum)),]
                        CSCO.acum$NetCum <- CSCO.acum$GrossCum-1
                        CSCO.acum[c(1:3,nrow(CSCO.acum)),]
                        
                    
                    #PLOT
                        plot(CSCO.acum$CSCO.tot.ret,
                         type="l",
                         xlab="Date",
                         ylab="Multi Day Returns Investment ($)",
                         ylim=c(-1,1.5),
                         col = "black",
                         main="
                    
                    CSCO Stocks Performance Based on Total Returns 
                         Gross returns - 
                         Gross Cumulative -  
                         Net Cumulative Price Returns -  June 31, 2019 - June 31, 2020")
                    
                        lines(CSCO.acum$GrossRet,
                        type="l", col = "red",lty=c(1,3))
                        lines(CSCO.acum$GrossCum,
                          type="l",col = "green",lty=c(1,3))
                        lines(CSCO.acum$NetCum,
                          type="l", col = "blue",lty=c(1,3))
                    
                        legend("topleft", c('Total Ret', 'Gross Ret','Gross Cumulative','Net Cumulative'),
                           col = c("black", "red","green", "blue"), lty = C(1,3), lwd = 1,
                            abline(h=1,col="red"))
                    
                    
                    #MSFT
                        MSFT.acum <- MSFT.tot.ret
                        MSFT.acum[c(1:3,nrow(MSFT.acum)),]
                        MSFT.acum[1,1] <-0
                        MSFT.acum[c(1:3,nrow(MSFT.acum)),]
                        MSFT.acum$GrossRet <- 1+MSFT.acum$MSFT.tot.ret
                        MSFT.acum[c(1:3,nrow(MSFT.acum)),]
                        MSFT.acum$GrossCum <- cumprod(MSFT.acum$GrossRet)
                        MSFT.acum[c(1:3,nrow(MSFT.acum)),]
                        MSFT.acum$NetCum <- MSFT.acum$GrossCum-1
                        MSFT.acum[c(1:3,nrow(MSFT.acum)),]
                    
                    #PLOT
                        plot(MSFT.acum$MSFT.tot.ret,
                             type="l",
                             xlab="Date",
                             ylab="Multi Day Returns Investment ($)",
                             ylim=c(-1,1.5),
                             col = "black",
                             main="
                    
                            MSFT Stocks Performance Based on Total Returns - BLACK 
                             Gross returns - RED 
                             Gross Cumulative - GREEN 
                             Net Cumulative Price Returns - BLUE December 31, 2006 -
                             December 31, 2009")
                        
                        lines(MSFT.acum$GrossRet,
                              type="l", col = "red"
                        )
                        lines(MSFT.acum$GrossCum,
                              type="l",col = "green"
                        )
                        lines(MSFT.acum$NetCum,
                              type="l", col = "blue"
                        )
                        legend(x = "topleft",
                               col = c("black", "red","green", "blue"), lty = 1, lwd = 1,
                               legend = c('Total Ret', 'Gross Ret','Gross Cumulative','Net Cumulative'))
                        abline(h=1,col="red")
                    
                        #CVX
                        CVX.acum <- CVX.tot.ret
                        CVX.acum[c(1:3,nrow(CVX.acum)),]
                        CVX.acum[1,1] <-0
                        CVX.acum[c(1:3,nrow(CVX.acum)),]
                        CVX.acum$GrossRet <- 1+CVX.acum$CVX.tot.ret
                        CVX.acum[c(1:3,nrow(CVX.acum)),]
                        CVX.acum$GrossCum <- cumprod(CVX.acum$GrossRet)
                        CVX.acum[c(1:3,nrow(CVX.acum)),]
                        CVX.acum$NetCum <- CVX.acum$GrossCum-1
                        CVX.acum[c(1:3,nrow(CVX.acum)),]
                        
                        #PLOT
                        plot(CVX.acum$CVX.tot.ret,
                             type="l",
                             xlab="Date",
                             ylab="Multi Day Returns Investment ($)",
                             ylim=c(-1,1.5),
                             col = "black",
                             main="
                    
                    
                    CVX Stocks Performance Based on Total Returns - BLACK 
                             Gross returns - RED 
                             Gross Cumulative - GREEN 
                             Net Cumulative Price Returns - BLUE December 31, 2006 -
                             December 31, 2009")
                        
                        lines(CVX.acum$GrossRet,
                              type="l", col = "red"
                        )
                        lines(CVX.acum$GrossCum,
                              type="l",col = "green"
                        )
                        lines(CVX.acum$NetCum,
                              type="l", col = "blue"
                        )
                        legend(x = "topleft",
                               col = c("black", "red","green", "blue"), lty = 1, lwd = 1,
                               legend = c('Total Ret', 'Gross Ret','Gross Cumulative','Net Cumulative'))
                        abline(h=1,col="red")
                        
                        
                        #XOM
                        XOM.acum <- XOM.tot.ret
                        XOM.acum[c(1:3,nrow(XOM.acum)),]
                        XOM.acum[1,1] <-0
                        XOM.acum[c(1:3,nrow(XOM.acum)),]
                        XOM.acum$GrossRet <- 1+XOM.acum$XOM.tot.ret
                        XOM.acum[c(1:3,nrow(XOM.acum)),]
                        XOM.acum$GrossCum <- cumprod(XOM.acum$GrossRet)
                        XOM.acum[c(1:3,nrow(XOM.acum)),]
                        XOM.acum$NetCum <- XOM.acum$GrossCum-1
                        XOM.acum[c(1:3,nrow(XOM.acum)),]
                        
                        #PLOT
                        plot(XOM.acum$XOM.tot.ret,
                             type="l",
                             xlab="Date",
                             ylab="Multi Day Returns Investment ($)",
                             ylim=c(-1,1.5),
                             col = "black",
                             main="
                              
                             XOM Stocks Performance Based on Total Returns - BLACK 
                             Gross returns - RED 
                             Gross Cumulative - GREEN 
                             Net Cumulative Price Returns - BLUE December 31, 2006 -
                             December 31, 2009")
                        
                        lines(XOM.acum$GrossRet,
                              type="l", col = "red"
                        )
                        lines(XOM.acum$GrossCum,
                              type="l",col = "green"
                        )
                        lines(XOM.acum$NetCum,
                              type="l", col = "blue"
                        )
                        legend(x = "topleft",
                               col = c("black", "red","green", "blue"), lty = 1, lwd = 1,
                               legend = c('Total Ret', 'Gross Ret','Gross Cumulative','Net Cumulative'))
                        abline(h=1,col="red")
                        
    
# Cumulating Logarithmic Returns

    AMGN.logcum <- AMGN.log.ret
    AMGN.logcum[c(1:3,nrow(AMGN.logcum)),]
    AMGN.logcum[1,1] <- 0
    AMGN.logcum[c(1:3,nrow(AMGN.logcum)),]
    logcumret=sum(AMGN.logcum$AMGN.log.ret)
    logcumret
    AMGN.cumret=exp(logcumret)-1
    AMGN.cumret

#JNJ
    JNJ.logcum <- JNJ.log.ret
    JNJ.logcum[c(1:3,nrow(JNJ.logcum)),]
    JNJ.logcum[1,1] <- 0
    JNJ.logcum[c(1:3,nrow(JNJ.logcum)),]
    logcumret=sum(JNJ.logcum$JNJ.log.ret)
    logcumret
    JNJ.cumret=exp(logcumret)-1
    JNJ.cumret
    
#CSCO
    CSCO.logcum <- CSCO.log.ret
    CSCO.logcum[c(1:3,nrow(CSCO.logcum)),]
    CSCO.logcum[1,1] <- 0
    CSCO.logcum[c(1:3,nrow(CSCO.logcum)),]
    logcumret=sum(CSCO.logcum$CSCO.log.ret)
    logcumret
    CSCO.cumret=exp(logcumret)-1
    CSCO.cumret
    
#MSFT
    MSFT.logcum <- MSFT.log.ret
    MSFT.logcum[c(1:3,nrow(MSFT.logcum)),]
    MSFT.logcum[1,1] <- 0
    MSFT.logcum[c(1:3,nrow(MSFT.logcum)),]
    logcumret=sum(MSFT.logcum$MSFT.log.ret)
    logcumret
    MSFT.cumret=exp(logcumret)-1
    MSFT.cumret

#Comparing Price Returns and Total Returns

    AMGN.Ret <- cbind(AMGN.prc.ret,AMGN.tot.ret)
    names(AMGN.Ret) <- c("prc.ret","tot.ret")
    AMGN.Ret[c(1:3,nrow(AMGN.Ret)),]
    AMGN.Ret$prc.ret[1] <- 0
    AMGN.Ret$tot.ret[1] <- 0
    AMGN.Ret[c(1:3,nrow(AMGN.Ret)),]
    AMGN.Ret$gross.prc <- 1+AMGN.Ret$prc.ret
    AMGN.Ret$gross.tot <- 1+AMGN.Ret$tot.ret
    AMGN.Ret[c(1:3,nrow(AMGN.Ret)),]
    AMGN.Ret$cum.prc <- cumprod(AMGN.Ret$gross.prc)
    AMGN.Ret$cum.tot <- cumprod(AMGN.Ret$gross.tot)
    AMGN.Ret[c(1:3,nrow(AMGN.Ret)),]
    AMGN.y.range <- range(AMGN.Ret[,5:6])
    AMGN.y.range

#JNJ

    JNJ.Ret <- cbind(JNJ.prc.ret,JNJ.tot.ret)
    names(JNJ.Ret) <- c("prc.ret","tot.ret")
    JNJ.Ret[c(1:3,nrow(JNJ.Ret)),]
    JNJ.Ret$prc.ret[1] <- 0
    JNJ.Ret$tot.ret[1] <- 0
    JNJ.Ret[c(1:3,nrow(JNJ.Ret)),]
    JNJ.Ret$gross.prc <- 1+JNJ.Ret$prc.ret
    JNJ.Ret$gross.tot <- 1+JNJ.Ret$tot.ret
    JNJ.Ret[c(1:3,nrow(JNJ.Ret)),]
    JNJ.Ret$cum.prc <- cumprod(JNJ.Ret$gross.prc)
    JNJ.Ret$cum.tot <- cumprod(JNJ.Ret$gross.tot)
    JNJ.Ret[c(1:3,nrow(JNJ.Ret)),]
    JNJ.y.range <- range(JNJ.Ret[,5:6])
    JNJ.y.range

#CSCO
    CSCO.Ret <- cbind(CSCO.prc.ret,CSCO.tot.ret)
    names(CSCO.Ret) <- c("prc.ret","tot.ret")
    CSCO.Ret[c(1:3,nrow(CSCO.Ret)),]
    CSCO.Ret$prc.ret[1] <- 0
    CSCO.Ret$tot.ret[1] <- 0
    CSCO.Ret[c(1:3,nrow(CSCO.Ret)),]
    CSCO.Ret$gross.prc <- 1+CSCO.Ret$prc.ret
    CSCO.Ret$gross.tot <- 1+CSCO.Ret$tot.ret
    CSCO.Ret[c(1:3,nrow(CSCO.Ret)),]
    CSCO.Ret$cum.prc <- cumprod(CSCO.Ret$gross.prc)
    CSCO.Ret$cum.tot <- cumprod(CSCO.Ret$gross.tot)
    CSCO.Ret[c(1:3,nrow(CSCO.Ret)),]
    CSCO.y.range <- range(CSCO.Ret[,5:6])
    CSCO.y.range

#MSFT
    MSFT.Ret <- cbind(MSFT.prc.ret,MSFT.tot.ret)
    names(MSFT.Ret) <- c("prc.ret","tot.ret")
    MSFT.Ret[c(1:3,nrow(MSFT.Ret)),]
    MSFT.Ret$prc.ret[1] <- 0
    MSFT.Ret$tot.ret[1] <- 0
    MSFT.Ret[c(1:3,nrow(MSFT.Ret)),]
    MSFT.Ret$gross.prc <- 1+MSFT.Ret$prc.ret
    MSFT.Ret$gross.tot <- 1+MSFT.Ret$tot.ret
    MSFT.Ret[c(1:3,nrow(MSFT.Ret)),]
    MSFT.Ret$cum.prc <- cumprod(MSFT.Ret$gross.prc)
    MSFT.Ret$cum.tot <- cumprod(MSFT.Ret$gross.tot)
    MSFT.Ret[c(1:3,nrow(MSFT.Ret)),]
    MSFT.y.range <- range(MSFT.Ret[,5:6])
    MSFT.y.range
    
    #CVX
    CVX.Ret <- cbind(CVX.prc.ret,CVX.tot.ret)
    names(CVX.Ret) <- c("prc.ret","tot.ret")
    CVX.Ret[c(1:3,nrow(CVX.Ret)),]
    CVX.Ret$prc.ret[1] <- 0
    CVX.Ret$tot.ret[1] <- 0
    CVX.Ret[c(1:3,nrow(CVX.Ret)),]
    CVX.Ret$gross.prc <- 1+CVX.Ret$prc.ret
    CVX.Ret$gross.tot <- 1+CVX.Ret$tot.ret
    CVX.Ret[c(1:3,nrow(CVX.Ret)),]
    CVX.Ret$cum.prc <- cumprod(CVX.Ret$gross.prc)
    CVX.Ret$cum.tot <- cumprod(CVX.Ret$gross.tot)
    CVX.Ret[c(1:3,nrow(CVX.Ret)),]
    CVX.y.range <- range(CVX.Ret[,5:6])
    CVX.y.range
    
    
    #XOM
    XOM.Ret <- cbind(XOM.prc.ret,XOM.tot.ret)
    names(XOM.Ret) <- c("prc.ret","tot.ret")
    XOM.Ret[c(1:3,nrow(XOM.Ret)),]
    XOM.Ret$prc.ret[1] <- 0
    XOM.Ret$tot.ret[1] <- 0
    XOM.Ret[c(1:3,nrow(XOM.Ret)),]
    XOM.Ret$gross.prc <- 1+XOM.Ret$prc.ret
    XOM.Ret$gross.tot <- 1+XOM.Ret$tot.ret
    XOM.Ret[c(1:3,nrow(XOM.Ret)),]
    XOM.Ret$cum.prc <- cumprod(XOM.Ret$gross.prc)
    XOM.Ret$cum.tot <- cumprod(XOM.Ret$gross.tot)
    XOM.Ret[c(1:3,nrow(XOM.Ret)),]
    XOM.y.range <- range(XOM.Ret[,5:6])
    XOM.y.range

                          #PLOT
                          plot(AMGN.Ret$cum.tot,
                               type="l",
                               auto.grid=FALSE,
                               xlab="Date",
                               ylab="Value of Investment ($)",
                               ylim=y.range,
                               minor.ticks=FALSE,
                               main="AMGN Stocks Performance Based on Total
                               Returns and Price Returns")
                          lines(AMGN.Ret$cum.prc,
                                type="l",
                                lty=3)
                          abline(h=1,col="red")
                          legend("topleft",
                                 c("Value Based on Total Return",
                                   "Value Based on Price Return"),
                                 col=c("black","black"),
                                 lty=c(1,3))
                          #AMGN
                          plot(AMGN.Ret$cum.tot,
                               type="l",
                               auto.grid=FALSE,
                               xlab="Date",
                               ylab="Value of Investment ($)",
                               #ylim=y.range,
                               minor.ticks=FALSE,
                               main="AMGN Stocks Performance Based on Total
                               Returns and Price Return 2019-20")
                          lines(AMGN.Ret$cum.prc,
                                type="l",
                                lty=3)
                          abline(h=1,col=2)
                          legend("topleft",
                                 c("Value Based on Total Return",
                                   "Value Based on Price Return"),
                                 col=c("black","black"),
                                 lty=c(2,1))
                              
                      #JNJ
                          plot(JNJ.Ret$cum.tot,
                               type="l",
                               auto.grid=FALSE,
                               xlab="Date",
                               ylab="Value of Investment ($)",
                               #ylim=y.range,
                               minor.ticks=FALSE,
                               main="JNJ Stocks Performance Based on Total
                               Returns and Price Returns 2019-20")
                          lines(JNJ.Ret$cum.prc,
                                type="l",
                                lty=3)
                          abline(h=1,col=2)
                          legend("topleft",
                                 c("Value Based on Total Return",
                                   "Value Based on Price Return"),
                                 col=c("black","black"),
                                 lty=c(2,1))
                      
                      #CSCO
                          plot(CSCO.Ret$cum.tot,
                               type="l",
                               auto.grid=FALSE,
                               xlab="Date",
                               ylab="Value of Investment ($)",
                               ylim= c(0,2),
                               minor.ticks=FALSE,
                               main="CSCO Stocks Performance Based on Total
                               Returns and Price Returns 2019-20")
                          lines(CSCO.Ret$cum.prc,
                                type="l",
                                lty=3)
                          abline(h=1,col="red")
                          legend("topleft",
                                 c("Value Based on Total Return",
                                   "Value Based on Price Return"),
                                 col=c("black","black"),
                                 lty=c(1,3))
                      
                      #MSFT
                      
                          plot(MSFT.Ret$cum.tot,
                               type="l",
                               auto.grid=FALSE,
                               xlab="Date",
                               ylab="Value of Investment ($)",
                               ylim= c(0,2),
                               minor.ticks=FALSE,
                               main="MSFT Stocks Performance Based on Total
                               Returns and Price Returns 2019-20")
                          lines(MSFT.Ret$cum.prc,
                                type="l",
                                lty=3)
                          abline(h=1,col="red")
                          legend("topleft",
                                 c("Value Based on Total Return",
                                   "Value Based on Price Return"),
                                 col=c("black","black"),
                                 lty=c(1,3))
                          
                          #CVX
                          
                          plot(CVX.Ret$cum.tot,
                               type="l",
                               auto.grid=FALSE,
                               xlab="Date",
                               ylab="Value of Investment ($)",
                               ylim= c(0,2),
                               minor.ticks=FALSE,
                               main="CVX Stocks Performance Based on Total
                               Returns and Price Returns 2019-20")
                          lines(CVX.Ret$cum.prc,
                                type="l",
                                lty=3)
                          abline(h=1,col="red")
                          legend("topleft",
                                 c("Value Based on Total Return",
                                   "Value Based on Price Return"),
                                 col=c("black","black"),
                                 lty=c(1,3))
                          
                          #XOM
                          
                          plot(XOM.Ret$cum.tot,
                               type="l",
                               auto.grid=FALSE,
                               xlab="Date",
                               ylab="Value of Investment ($)",
                               ylim= c(0,2),
                               minor.ticks=FALSE,
                               main="XOM Stocks Performance Based on Total
                               Returns and Price Returns 2019-20")
                          lines(XOM.Ret$cum.prc,
                                type="l",
                                lty=3)
                          abline(h=1,col="red")
                          legend("topleft",
                                 c("Value Based on Total Return",
                                   "Value Based on Price Return"),
                                 col=c("black","black"),
                                 lty=c(1,3))
#Calculating Portfolio return

#Comparing Performance of Multiple
#Securities: Total Returns 

    multi <- data.AMGN[,5]
    multi <- merge(multi,data.MSFT[,5])
    multi <- merge(multi,data.CSCO[,5])
    multi <- merge(multi,data.JNJ[,5])
    multi <- merge(multi,data.CVX[,5])
    multi <- merge(multi,data.XOM[,5])
    
    multi[c(1:3,nrow(multi)),]
    
    multi.df <- cbind(data.frame(index(multi)),
                      data.frame(multi))
    names(multi.df) <-
      paste(c("date","AMGN","MSFT","CSCO","JNJ","CVX","XOM"))
    multi.df[c(1:3,nrow(multi.df)),]
    
    multi.df$AMGN.idx <-
      multi.df$AMGN/multi.df$AMGN[1]
    multi.df$MSFT.idx <-
      multi.df$MSFT/multi.df$MSFT[1]
    multi.df$CSCO.idx <-
      multi.df$CSCO/multi.df$CSCO[1]
    multi.df$JNJ.idx <- multi.df$JNJ/multi.df$JNJ[1]
    options(digits=5)
    multi.df$CVX.idx <-
      multi.df$CVX/multi.df$CVX[1]
    multi.df$XOM.idx <- multi.df$XOM/multi.df$XOM[1]
    options(digits=5)
    
    multi.df[c(1:3,nrow(multi.df)),6:9]
    
    
    y.range <- range(multi.df[,6:9]) 
                              # find  minimum/maximum of all 6 securities
                                  y.range
                              
                              #Plot
                                  plot(x=multi.df$date,
                                       y=multi.df$MSFT.idx,
                                       ylim=y.range,
                                       type="l",
                                       xlab="Date",
                                       ylab="Value of Investment ($)",
                                       col="blue",
                                       lty=1,
                                       lwd=2,
                                       main="Value of $1 Investment in
                                       AMGN, JNJ, CSCO, and MSFT")
                                  lines(x=multi.df$date,
                                        y=multi.df$AMGN.idx,
                                        col="green",
                                        lty=1,
                                        lwd=2)
                                  lines(x=multi.df$date,y=multi.df$JNJ.idx,
                                        col="red",
                                        lty=1,
                                        lwd=2)
                                  lines(x=multi.df$date,
                                        y=multi.df$CSCO.idx,
                                        col="black",
                                        lty=1,
                                        lwd=2)
                                  lines(x=multi.df$date,
                                        y=multi.df$CVX.idx,
                                        col="dark blue",
                                        lty=1,
                                        lwd=2)
                                  lines(x=multi.df$date,
                                        y=multi.df$XOM.idx,
                                        col="dark red",
                                        lty=1,
                                        lwd=2)
                                  abline(h=1,lty=1,col="black")
                                  legend("topleft",
                                         c("AMGN","JNJ","CSCO","MSFT","CVX","XOM"),
                                         col=c("green","red","gray","blue","dark blue","dark red"),
                                         lty=c(1,1,1,1,1,1),
                                         lwd=c(2,2,2,2,2,2))
                              

#--------------------------------------------------

#EW----------------------------------------------



#Find First and Last Adjusted Closing Price for each security over
#the investment period (use data from previous lectures: AMGN, 
#                       CSCO, MSFT, JNJ)
#Investment period: July 1 2019 - July 1 2020. 

    period.ret <- multi[c(1,nrow(multi)),]
    period.ret

#Calculate returns for each security over the investment period
#Since there are only two observations in "rets", the Delt
#command can be used.
#"lapply" is used to apply "delt" to all four securities

    rets <- lapply(period.ret,Delt)
    rets

#Convert to a data.frame and clean up data (since is currently a
#                                           list object)
#Remove NA's and convert to percentages

    rets <- data.frame(rets)
    rets
    
    rets <- rets[2,]*100
    names(rets) <- paste(c("AMGN","JNJ","CSCO","MSFT","CVX","XOM"))
    rets

# 
# Calculate weight of each security in the portfolio
# Create variables representing the dollar amount of
# investment in each security
# Calculate the weight by dividing the amount invested by the
# total investment ($100k) 
# Here the split in investment is AMGN 30K + JNJ 10K+ CSCO 10K + MSFT 30K + CVX 10k + XOM - 10k total 100K)

    
    i.AMGN <- 16667
    i.JNJ  <- 16667
    i.CSCO <- 16667
    i.MSFT <- 16667
    i.CVX  <- 16666
    i.XOM  <- 16666


# Calculate weight of each security in the portfolio (cont.)
    w.AMGN <- i.AMGN/(i.AMGN+i.JNJ + i.CSCO +i.MSFT + i.CVX + i.XOM)
    w.AMGN
    
    
    w.JNJ <- i.JNJ/(i.AMGN+i.JNJ + i.CSCO +i.MSFT + i.CVX + i.XOM)
    w.JNJ
    
    
    w.CSCO <- i.CSCO/(i.AMGN+i.JNJ + i.CSCO +i.MSFT + i.CVX + i.XOM)
    w.CSCO
    
    w.MSFT <- i.MSFT/(i.AMGN+i.JNJ + i.CSCO +i.MSFT + i.CVX + i.XOM)
    w.MSFT
    
    w.CVX <- i.CVX/(i.AMGN+i.JNJ + i.CSCO +i.MSFT + i.CVX + i.XOM)
    w.CVX
    
    w.XOM <- i.XOM/(i.AMGN+i.JNJ + i.CSCO +i.MSFT + i.CVX + i.XOM)
    w.XOM

# Calculate portfolio return (weights x return)
  port.ret.4asset <-
  w.AMGN*rets$AMGN+ w.JNJ*rets$JNJ + w.CSCO*rets$CSCO + w.MSFT*rets$MSFT+w.CVX*rets$CVX+w.XOM*rets$XOM
  port.ret.4asset

# 
# The calculation requires two vectors (used for larger number of
#                                       securities):
#   Vector of weights - matrix with one row (16.667% for all)

  # Column vector of returns (using c(.) and matrix command)
    wgt <- c(0.1666666666666667,0.1666666666666667,0.1666666666666667,0.1666666666666667,0.1666666666666667,0.1666666666666667)
    mat.wgt <- matrix(wgt,1)
    mat.wgt


  ret <- c(rets$AMGN,rets$JNJ,rets$CSCO,rets$MSFT, rets$CVX, rets$XOM)
  mat.ret <- matrix(ret,6)
  mat.ret

# Calculate portfolio returns by multiplying the row vector times
# the column vector using the matrix mutiplication operator
  port.ret <- mat.wgt %*% mat.ret
  port.ret


# Constructing Benchmark Portfolio Returns
# The benchmark is used to compare against a portfolio in order to
# measure performance.
# Typically, a performance index is used as the benchmark (S&P
#                                                          500).
# This section describes how to create an equally-weighted (EW)
# index and value-weighted (VW) index.

# The objective is to create a hypothetical EW and VW portfolio


# Create object with only the relevant data
    port <- data.AMGN[,c(4,5)]
    port <- merge(port,data.JNJ[,c(4,5)])
    
    port <- merge(port,data.CSCO[,c(4,5)])
    port <- merge(port,data.MSFT[,c(4,5)])
    
    port <- merge(port,data.CVX[,c(4,5)])
    port <- merge(port,data.XOM[,c(4,5)])
    
    
    port[c(1:3,nrow(port)),]


# Calculate total returns of each security using the adjusted close
# prices
    port$AMGN.ret <- Delt(port$AMGN.Adjusted)
    port$JNJ.ret <- Delt(port$JNJ.Adjusted)
    port$CSCO.ret <- Delt(port$CSCO.Adjusted)
    port$MSFT.ret <- Delt(port$MSFT.Adjusted)
    port$CVX.ret <- Delt(port$CVX.Adjusted) 
    port$XOM.ret <- Delt(port$XOM.Adjusted)

    
    port[c(1:3,nrow(port)),]

# Convert to data.frame object and subset data from Dec 31, 2006 (to include data from 3rd yr of portfolio)
# to Dec 31, 2019 leaving the Dec 31, 2009 data in because it will
# be used for later calculations.
    port <- cbind(data.frame(index(port)),
                  data.frame(port))
    names(port)[1] <- paste("date")
    port[c(1:3,nrow(port)),]
    
    #Selecting data from 2019 and 2020
    
    port <- subset(port,
                    port$date >= "2019-07-03")
    port[c(1:3,nrow(port)),] 
    

#This will be the bechmark portfolio


# Equal-Weighted Portfolio
# Equal-weight to small firms and large firms
# Example: Qualcomm Equal Weight Index
# The indexes are balanced quarterly which means that in
# between quarters the constituent weights are allowed to
# fluctuate based on their performance.
# On each rebalancing date, the weights of each firm is set to reset
# to 1/N (number of securities in the portfolio)
# Keep only variables we need to construct EW portfolio
# (AMGN,CSCO, JNJ, MSFT)

# To make calling in the variables easier, rename the return
# variables to only be the tickers. Also, renumber the index to
# denote the observation number.
    
  port <- port[,c(1,14:19)]
  port[c(1:3,nrow(port)),] 
    
  ewport <- port

    ewport[c(1:3,nrow(ewport)),]
    names(ewport) <-
    paste(c("date","AMGN","JNJ","CSCO","MSFT","CVX","XOM"))
    rownames(ewport) <- seq(1:nrow(ewport))
    ewport[c(1:3,nrow(ewport)),]

# Convert the data to gross returns, by adding one to each
# security's returns.
# To reduce the number of variables overwrite the net return
# series
      ewport$AMGN <- 1+ewport$AMGN
      ewport$JNJ <- 1+ewport$JNJ
      ewport$CSCO <- 1+ewport$CSCO
      ewport$MSFT <- 1+ewport$MSFT
      ewport$CVX <- 1+ewport$CVX
      ewport$XOM <- 1+ewport$XOM
      
      
      
      ewport[c(1:3,nrow(ewport)),]

      #======================= July 2019 Q3 2019========================

# Calculate EW portfolio values for 3Q 2019
ew.q3 <- subset(ewport,
                ewport$date >= as.Date("2019-07-01") &
                  ewport$date <= as.Date("2019-09-30"))
ew.q3[c(1:3,nrow(ew.q3)),]


# Calculate EW portfolio values for 1Q 2019 (cont.)
# The return on Dec. 31, 2006 is not used (it is used as a
#                                          placeholder used later for indexing for each security at $1)
# Overwrite return on Dec. 31, 2019 (for each security) to 1.00
ew.q3[1,2:7]<-1
ew.q3[c(1:3,nrow(ew.q3)),]
ew.q3$AMGN <- cumprod(ew.q3$AMGN)
ew.q3$JNJ <- cumprod(ew.q3$JNJ)
ew.q3$CSCO <- cumprod(ew.q3$CSCO)
ew.q3$MSFT <- cumprod(ew.q3$MSFT)
ew.q3$CVX <- cumprod(ew.q3$CVX)
ew.q3$XOM <- cumprod(ew.q3$XOM)



ew.q3[c(1:3,nrow(ew.q3)),]


# Calculate EW portfolio values for 1Q 2019 (cont.)
# Create a variable to denote the number of securities in the
# portfoliothat can be used for each quarterly calculation.
  num.sec <- 6

# Calculate the index value for each security during the quarter
ew.q3$AMGN.idx <- (1/num.sec)*ew.q3$AMGN
ew.q3$CSCO.idx <- (1/num.sec)*ew.q3$CSCO
ew.q3$JNJ.idx <- (1/num.sec)*ew.q3$JNJ
ew.q3$MSFT.idx <- (1/num.sec)*ew.q3$MSFT
ew.q3$XOM.idx <- (1/num.sec)*ew.q3$XOM
ew.q3$CVX.idx <- (1/num.sec)*ew.q3$CVX


ew.q3 <- ew.q3[,c(1,8:19)]


ew.q3[c(1:3,nrow(ew.q3)),]


# Calculate the index value for each security during the quarter
# The value of a portfolio is equal to the value of the components.
# To calculate the value of the EW portfolio on each day, sum the
# values of the three index variables.
    q3.val <- data.frame(rowSums(ew.q3[,8:13]))
    q3.val[c(1:3,nrow(q3.val)),]
    names(q3.val) <- paste("port.val")
    q3.val$date <- ew.q3$date
    q3.val[c(1:3,nrow(q3.val)),]


# Calculate the index value for each security during the quarter
# q3.val holds the portfolio values for the third quarter
# q4.inv holds the aggregate portfolio value at the end of Q3
# q4.inv will be redistributed equally among six stocks at the
# beginning of the fourth quarter
# The value of 1.9156 is used at the beginning of the second quarter
# instead of 1.00
      q4.inv <- q3.val[nrow(q3.val),1]
      q4.inv

      
      #======================= Oct 2019 Q4 2019========================
      
      #---------------------------------
      
      
      #Quarter 4 2019
      
      # Calculate EW portfolio values for 4th Quarter 2019
      ew.q4 <- subset(ewport,
                      ewport$date >= as.Date("2019-10-01") &
                        ewport$date <= as.Date("2019-12-31"))
      ew.q4[c(1:3,nrow(ew.q4)),]
      
      
      # Calculate EW portfolio values for 4Q 2019 (cont.)
      # The return on Dec. 31, 2006 is not used (it is used as a
      #                                          placeholder used later for indexing for each security at $1)
      
      
      
      # Overwrite return on Oct 1, 2019 (for each security) to 0.95778
      ew.q4[1,8:13]<-0.95778
      
      ew.q4$AMGN <- cumprod(ew.q4$AMGN)
      ew.q4$JNJ <- cumprod(ew.q4$JNJ)
      ew.q4$CSCO <- cumprod(ew.q4$CSCO)
      ew.q4$MSFT <- cumprod(ew.q4$MSFT)
      ew.q4$CVX <- cumprod(ew.q4$CVX)
      ew.q4$XOM <- cumprod(ew.q4$XOM)
      
      
      
      ew.q4[c(1:3,nrow(ew.q4)),]
      
      
      # Calculate EW portfolio values for 4Q 2019 (cont.)
      # Create a variable to denote the number of securities in the
      # portfoliothat can be used for each quarterly calculation.
      num.sec <- 6
      
      # Calculate the index value for each security during the quarter
      ew.q4$AMGN.idx <- (1/num.sec)*ew.q4$AMGN
      ew.q4$CSCO.idx <- (1/num.sec)*ew.q4$CSCO
      ew.q4$JNJ.idx <- (1/num.sec)*ew.q4$JNJ
      ew.q4$MSFT.idx <- (1/num.sec)*ew.q4$MSFT
      ew.q4$XOM.idx <- (1/num.sec)*ew.q4$XOM
      ew.q4$CVX.idx <- (1/num.sec)*ew.q4$CVX
      
      
      ew.q4 <- ew.q4[,c(1,8:19)]
      
      
      ew.q4[c(1:3,nrow(ew.q4)),]
      
      
      # Calculate the index value for each security during the quarter
      # The value of a portfolio is equal to the value of the components.
      # To calculate the value of the EW portfolio on each day, sum the
      # values of the three index variables.
      q4.val <- data.frame(rowSums(ew.q4[,8:13]))
      q4.val[c(1:3,nrow(q4.val)),]
      names(q4.val) <- paste("port.val")
      q4.val$date <- ew.q4$date
      q4.val[c(1:3,nrow(q4.val)),]
      
      
      # Calculate the index value for each security during the quarter
      # q1.val holds the portfolio values for the first quarter of 2020
      # q1.val holds the aggregate portfolio value at the end of q4
      # q1.val will be redistributed equally among six stocks at the
      # beginning of the first quarter
      # The value of 4.2309 is used at the beginning of the first quarter
      # instead of 1.9156
      
      q1.inv <- q4.val[nrow(q4.val),1]
      q1.inv
      
      
      #======================= Jan 2020 Q1 2020========================

           # Calculate EW portfolio values Q1 2020
      ew.q1 <- subset(ewport,
                      ewport$date >= as.Date("2020-01-01") &
                        ewport$date <= as.Date("2020-03-31"))
      ew.q1[c(1:3,nrow(ew.q1)),]
      
      
      # Calculate EW portfolio values for 4Q 2019 (cont.)
     
      ew.q1[1,8:13]<-1.0886
      
      ew.q1$AMGN <- cumprod(ew.q1$AMGN)
      ew.q1$JNJ <- cumprod(ew.q1$JNJ)
      ew.q1$CSCO <- cumprod(ew.q1$CSCO)
      ew.q1$MSFT <- cumprod(ew.q1$MSFT)
      ew.q1$CVX <- cumprod(ew.q1$CVX)
      ew.q1$XOM <- cumprod(ew.q1$XOM)
      
      
      
      ew.q1[c(1:3,nrow(ew.q1)),]
      
      
      # Calculate EW portfolio values for 4Q 2019 (cont.)
      # Create a variable to denote the number of securities in the
      # portfoliothat can be used for each quarterly calculation.
      num.sec <- 6
      
      # Calculate the index value for each security during the quarter
      ew.q1$AMGN.idx <- (1/num.sec)*ew.q1$AMGN
      ew.q1$CSCO.idx <- (1/num.sec)*ew.q1$CSCO
      ew.q1$JNJ.idx <- (1/num.sec)*ew.q1$JNJ
      ew.q1$MSFT.idx <- (1/num.sec)*ew.q1$MSFT
      ew.q1$XOM.idx <- (1/num.sec)*ew.q1$XOM
      ew.q1$CVX.idx <- (1/num.sec)*ew.q1$CVX
      
      
      ew.q1 <- ew.q1[,c(1,8:19)]
      
      
      ew.q1[c(1:3,nrow(ew.q1)),]
      
      
      # Calculate the index value for each security during the quarter
      # The value of a portfolio is equal to the value of the components.
      # To calculate the value of the EW portfolio on each day, sum the
      # values of the three index variables.
      q1.val <- data.frame(rowSums(ew.q1[,8:13]))
      q1.val[c(1:3,nrow(q1.val)),]
      names(q1.val) <- paste("port.val")
      q1.val$date <- ew.q1$date
      q1.val[c(1:3,nrow(q1.val)),]
      
      
      # Calculate the index value for each security during the quarter
      # q1.val holds the portfolio values for the first quarter of 2020
      # q1.val holds the aggregate portfolio value at the end of q1
      # q1.val will be redistributed equally among six stocks at the
      # beginning of the first quarter
      # The value of 4.2309 is used at the beginning of the first quarter
      # instead of 1.9156
      
      q2.inv <- q1.val[nrow(q1.val),1]
      q2.inv
      
      
      #======================= Apr 2020 Q1 2020========================
      
      # Calculate EW portfolio values for 2nd Quarter 2020
      ew.q2 <- subset(ewport,
                      ewport$date >= as.Date("2020-04-01") &
                        ewport$date <= as.Date("2020-06-30"))
      ew.q2[c(1:3,nrow(ew.q2)),]
      
      

      # The return on Dec. 31, 2006 is not used (it is used as a
      #                                          placeholder used later for indexing for each security at $1)
      
      
      
      # Overwrite return on Jan 2 2020 (for each security) to 6.6253
      
      ew.q2[1,8:13]<-0.79006
      
      ew.q2$AMGN <- cumprod(ew.q2$AMGN)
      ew.q2$JNJ <- cumprod(ew.q2$JNJ)
      ew.q2$CSCO <- cumprod(ew.q2$CSCO)
      ew.q2$MSFT <- cumprod(ew.q2$MSFT)
      ew.q2$CVX <- cumprod(ew.q2$CVX)
      ew.q2$XOM <- cumprod(ew.q2$XOM)
      
      
      
      ew.q2[c(1:3,nrow(ew.q2)),]
      
      
      # Calculate EW portfolio values for 2Q 2020 (cont.)
      # Create a variable to denote the number of securities in the
      # portfoliothat can be used for each quarterly calculation.
      num.sec <- 6
      
      # Calculate the index value for each security during the quarter
      ew.q2$AMGN.idx <- (1/num.sec)*ew.q2$AMGN
      ew.q2$CSCO.idx <- (1/num.sec)*ew.q2$CSCO
      ew.q2$JNJ.idx <- (1/num.sec)*ew.q2$JNJ
      ew.q2$MSFT.idx <- (1/num.sec)*ew.q2$MSFT
      ew.q2$XOM.idx <- (1/num.sec)*ew.q2$XOM
      ew.q2$CVX.idx <- (1/num.sec)*ew.q2$CVX
      
      
      ew.q2 <- ew.q2[,c(1,8:19)]
      
      
      ew.q2[c(1:3,nrow(ew.q2)),]
      
      
      # Calculate the index value for each security during the quarter
      # The value of a portfolio is equal to the value of the components.
      # To calculate the value of the EW portfolio on each day, sum the
      # values of the three index variables.
      q2.val <- data.frame(rowSums(ew.q2[,8:13]))
      q2.val[c(1:3,nrow(q2.val)),]
      names(q2.val) <- paste("port.val")
      q2.val$date <- ew.q2$date
      q2.val[c(1:3,nrow(q2.val)),]
      
      
      # Calculate the index value for each security during the quarter
      # q2.val holds the portfolio values for the first quarter of 2020
      # q2.val holds the aggregate portfolio value at the end of q2
      # q2.val will be redistributed equally among six stocks at the
      # beginning of the first quarter
      # The value of 6.6253 is used at the beginning of the second quarter
   
      
      q3.inv <- q2.val[nrow(q2.val),1]
      q3.inv
      
      
# Combine quarterly EW portfolio values into one data object
# (ew.portval)
# Please note that this calculations do not include transactions
# costs incurred during each Q rebalancing.
    ew.portval <- rbind(q3.val,q4.val,q1.val,q2.val)
    ew.portval[c(1:3,nrow(ew.portval)),]
    
    
    ew.portval


q4.inv    
q1.inv
q2.inv
q3.inv

#------- -EQUAL PORTFOLIO ENDS HERE*********************#

#----------------# Value-Weighted Portfolio--------------
# Some of the major indexes use some form of value-weighting,
# such as the Qualcomm Index.
# In a VW portfolio the returns of larger firms are given more
# weight.
# Rebalance at the start of each quarter using the prior quarter
# end's capitalization data.
# Value-Weighted Portfolio
# Keep only the variables needed to construct the VW portfolio


# Create object with only the relevant data



      vport <- data.AMGN[,c(4,5)]
      vport <- merge(vport, data.JNJ[,c(4,5)])
      
      vport <- merge(vport,data.CSCO[,c(4,5)])
      vport <- merge(vport,data.MSFT[,c(4,5)])
      
      vport <- merge(vport,data.CVX[,c(4,5)])
      vport <- merge(vport,data.XOM[,c(4,5)])

vport[c(1:3,nrow(vport)),]

# Calculate total returns of each security using the adjusted close
# prices
      vport$AMGN.ret <- Delt(vport$AMGN.Adjusted)
      vport$JNJ.ret <- Delt(vport$JNJ.Adjusted)
      vport$CSCO.ret <- Delt(vport$CSCO.Adjusted)
      vport$MSFT.ret <- Delt(vport$MSFT.Adjusted)
      vport$CVX.ret <- Delt(vport$CVX.Adjusted) 
      vport$XOM.ret <- Delt(vport$XOM.Adjusted)


vport[c(1:3,nrow(vport)),]
#Considering the values as of July 1 2019.

vport <- cbind(data.frame(index(vport)),
               data.frame(vport))
names(vport)[1] <- paste("date")
vport[c(1:3,nrow(vport)),]
# Calculate the market capitalization of each security in the
# portfolio
# Market cap is equal to the price multiplied by the outstanding
# shares
# Construct a series of calendar days



date <- seq(as.Date("2019-07-1"),as.Date("2020-06-30"),by=1)
date <- data.frame(date)
date[c(1:3,nrow(date)),]

    vwport <- vport
    vwport[c(1:3,nrow(vwport)),]



# Date already available, the index of vwport can be changed to an
# indicator of the observation number
    rownames(vwport) <- seq(1:nrow(vwport))
    vwport[c(1:3,nrow(vwport)),]


# Converting Net Returns to Gross Returns
    vwport$AMGN.ret <- 1+vwport$AMGN.ret
    vwport$JNJ.ret <- 1+vwport$JNJ.ret
    
    vwport$CSCO.ret <- 1+vwport$CSCO.ret
    vwport$MSFT.ret <- 1+vwport$MSFT.ret
    
    vwport$CVX.ret <- 1+vwport$CVX.ret
    vwport$XOM.ret <- 1+vwport$XOM.ret
    
    vwport[c(1:3,nrow(vwport)),]


  
# Calculate the market capitalization of each security in the
# portfolio
# Market cap is equal to the price multiplied by the outstanding
# shares
# Create data object with daily prices, filling in last avialable price
# on non-trading days
    

    PRICE.qtr <- vwport[,c(1,2,4,6,8,10,12)]
    PRICE.qtr[c(1:3,nrow(PRICE.qtr)),]
    
    
    # Value-Weighted Portfolio
# Calculate the market capitalization of each security in the
# portfolio
# Market cap is equal to the price multiplied by the outstanding
# shares
# Create data object with daily prices, filling in last avialable price
# on non-trading days
# Merge date and PRICE.qtr using a combination of the na.locf and
# merge commands.
# The merge command combines two data objects. Using the
# all.x=TRUE option tells R that when merging we should keep all
# the data in the "by" variable that is available in the x=date data
# object.

    
    PRICE.qtr <-  na.locf(merge(x=date,y=PRICE.qtr,by="date",all.x=TRUE))
    PRICE.qtr[c(1:3,nrow(PRICE.qtr)),]


# Calculate the market capitalization of each security in the
# portfolio
# Market cap is equal to the price multiplied by the outstanding
# shares
# Keep only prices at the end of each calendar quarter

    install.packages("dplyr")
    library(dplyr)

PRICE.qtr <- filter(PRICE.qtr, (PRICE.qtr$date == as.Date("2019-07-02") | PRICE.qtr$date==as.Date("2019-10-01") |
                                  PRICE.qtr$date==as.Date("2020-01-02") |
                                  PRICE.qtr$date==as.Date("2020-04-01")))
PRICE.qtr[c(1:3,nrow(PRICE.qtr)),]



# Value-Weighted Portfolio
# Calculate the market capitalization of each security in the
# portfolio
# Market cap is equal to the price multiplied by the outstanding mcap = price of 1 share * total outstanding shares
# shares
# Obtain shares outstanding data from SEC filings
# Available from the SEC EDGAR database (Form 10k - Form
#                                        10-Q)
# Enter the data manually

    PRICE.qtr$AMGN.shout <- c(0.6021, 0.5962, 0.5914, 0.588)
    
    PRICE.qtr$JNJ.shout <- c(2.642, 2.631, 2.633, 2.632) 
    
    PRICE.qtr$CSCO.shout <- c(4.25, 4.244, 4.242, 4.222)
    
    PRICE.qtr$MSFT.shout <-c(7.643, 7.634, 7.611, 7.59)
    
    PRICE.qtr$CVX.shout <-c(1.898, 1.891, 1.882, 1.867)
    
    PRICE.qtr$XOM.shout <-  c(4.231, 4.231, 4.234, 4.228)

    
# Value-Weighted Portfolio
# Calculate the market capitalization of each security in the
# portfolio
# Market cap is equal to the price multiplied by the outstanding
# shares
# Calculate market capitalization for each security -check that
# date and closing price are read-in as character variables

    str(PRICE.qtr)

# Calculate the market capitalization of each security in the
# portfolio
# Market cap is equal to the price multiplied by the outstanding
# shares
# To make the data object name more sensible to hold the weights
# of the securities, copy the data in PRICE.qtr into weights.
# Calculate the market cap for each security by multiplying that
# security's closing price with its shares outstanding.
      weights <- PRICE.qtr
      
      weights$AMGN.mcap <- weights$AMGN.Close*weights$AMGN.shout
      
      
      weights$CSCO.mcap <-
        weights$CSCO.Close*weights$CSCO.shout
      
      weights$JNJ.mcap <-
        weights$JNJ.Close*weights$JNJ.shout
      
      weights$MSFT.mcap <-
        weights$MSFT.Close*weights$MSFT.shout
      
      weights$CVX.mcap <-
        weights$CVX.Close*weights$CVX.shout
      
      weights$XOM.mcap <-
        weights$XOM.Close*weights$XOM.shout
      
      head(weights)


# Calculate the market capitalization of each security in the
# portfolio
# Market cap is equal to the price multiplied by the outstanding
# shares
# Calculate quarter-end aggregate market capitalization -Apply
# the rowSums command to the three market capitalization
# variables (8-10)
    weights$tot.mcap <- rowSums(weights[14:19])
    weights

# Calculate the quarter-end weights of each security in the
# portfolio
# Divide each security's market cap by the combined market cap
# of the six  securities
      weights$AMGN.wgt <-weights$AMGN.mcap/weights$tot.mcap
      weights$CSCO.wgt <-weights$CSCO.mcap/weights$tot.mcap
      weights$JNJ.wgt <-weights$JNJ.mcap/weights$tot.mcap
      weights$MSFT.wgt <-weights$MSFT.mcap/weights$tot.mcap
      weights$CVX.wgt <-weights$CVX.mcap/weights$tot.mcap
      weights$XOM.wgt <-weights$XOM.mcap/weights$tot.mcap
      weights

# Calculate the quarter-end weights of each security in the portfolio
# Create an object WEIGHT to keep only the date and the six weight variables
    WEIGHT <- weights[,c(1,21:26)]
    WEIGHT

# Calculate the quarter-end weights of each security in the
# portfolio
# To apply the weights to the start of the following quarter, add
# one to all dates
    WEIGHT$date <- WEIGHT$date+1
    WEIGHT




#Merge weight into date
vwret <- na.locf(merge(date,WEIGHT,by="date",all.x=TRUE))
vwret[c(1:3,nrow(vwret)),]


# Calculating the quarterly VW portfolio values (similar to EW
#                                                portfolio)
# Check date as.date and wgt as.num
str(vwret)


#===============Quarter 3 July 2019 - Oct 2019

# Calculating the quarterly VW portfolio values (similar to EW portfolio)
# Extract the beginning of the quarter weights from vwret

q3.vw.wgt <-  subset(vwret,vwret$date == as.Date("2019-07-03"))
q3.vw.wgt

q4.vw.wgt <-  subset(vwret,vwret$date==as.Date("2019-10-01"))
q4.vw.wgt

q1.vw.wgt <-
  subset(vwret,vwret$date==as.Date("2020-01-01"))
q1.vw.wgt

q2.vw.wgt <-
  subset(vwret,vwret$date==as.Date("2020-04-01"))
q2.vw.wgt


# Value-Weighted Portfolio

      #Create pie charts of the weights Q3 2019
      q3.pie.values <- as.numeric(q3.vw.wgt[,-1])
      q3.pie.labels <- c("AMGN","JNJ","CSCO","MSFT","CVX","XOM")
      pct <- round(q3.pie.values*100)
      q3.pie.labels <- paste(q3.pie.labels,pct) 
      #Add pct
      q3.pie.labels <- paste(q3.pie.labels,"%",sep="")
      #Add % sign
      pie(q3.pie.values,
          labels=q3.pie.labels,
          col=c("red","blue","green","yellow","gray","pink"),
          main="q3-2019 Value Weighting")
      
      #Create pie charts of the weights Q4
      q4.pie.values <- as.numeric(q4.vw.wgt[,-1])
      q4.pie.labels <- c("AMGN","JNJ","CSCO","MSFT","CVX","XOM")
      pct <- round(q4.pie.values*100)
      q4.pie.labels <- paste(q4.pie.labels,pct) 
      #Add pct
      q4.pie.labels <- paste(q4.pie.labels,"%",sep="")
      #Add % sign
      pie(q4.pie.values,
          labels=q4.pie.labels,
          col=c("red","blue","green","yellow","gray","pink"),
          main="q4 Value Weigthing")
      
      
      #Create pie charts of the weights Q1 -2020
      q1.pie.values <- as.numeric(q1.vw.wgt[,-1])
      q1.pie.labels <- c("AMGN","JNJ","CSCO","MSFT","CVX","XOM")
      pct <- round(q1.pie.values*100)
      q1.pie.labels <- paste(q1.pie.labels,pct) 
      #Add pct
      q1.pie.labels <- paste(q1.pie.labels,"%",sep="")
      #Add % sign
      pie(q1.pie.values,
          labels=q1.pie.labels,
          col=c("red","blue","green","yellow","gray","pink"),
          main="q1 2020 Value Weighting")
      
      
      #Create pie charts of the weights q2
      q2.pie.values <- as.numeric(q2.vw.wgt[,-1])
      q2.pie.labels <- c("AMGN","JNJ","CSCO","MSFT","CVX","XOM")
      pct <- round(q2.pie.values*100)
      q2.pie.labels <- paste(q2.pie.labels,pct) 
      #Add pct
      q2.pie.labels <- paste(q2.pie.labels,"%",sep="")
      #Add % sign
      pie(q2.pie.values,
          labels=q2.pie.labels,
          col=c("red","blue","green","yellow","gray","pink"),
          main="q2 2020 Value Weighting")
      
 #=========================Start Q1 2020     
      # Calculating VW portfolio values for QUARTER1 2020 (similar to the EW
      #                                              portfolio)
      # Subset vwport to the data from Jan 1 2020 - March 31 2020
      
      vw.q1 <- subset(vwport[,c(1,14:19)],
                      vwport$date >= as.Date("2020-01-01") &
                        vwport$date <= as.Date("2020-03-31"))
      names(vw.q1) <-  paste(c("date","AMGN","JNJ","CSCO","MSFT","CVX","XOM")) 
      # shortenthe names
      vw.q1[c(1:3,nrow(vw.q1)),]
      
      # Calculating VW portfolio values for 1Q 2020 (similar to the EW
      #                                              portfolio)
      # Calculate the cumulative gross return for each security
      vw.q1[1,2:7] <-1.0892
      vw.q1$AMGN <- cumprod(vw.q1$AMGN)
      
      vw.q1$JNJ <- cumprod(vw.q1$JNJ)
      vw.q1$CSCO <- cumprod(vw.q1$CSCO)
      vw.q1$MSFT <- cumprod(vw.q1$MSFT)
      
      vw.q1$CVX <- cumprod(vw.q1$CVX)
      vw.q1$XOM <- cumprod(vw.q1$XOM)
      
      vw.q1[c(1:3,nrow(vw.q1)),]
      
      # Apply quarter-end weights
      vw.q1$AMGN.idx <-   (1*q1.vw.wgt$AMGN.wgt)*vw.q1$AMGN
      
      vw.q1$JNJ.idx <-    (1*q1.vw.wgt$JNJ.wgt)*vw.q1$JNJ
      vw.q1$CSCO.idx <-   (1*q1.vw.wgt$CSCO.wgt)*vw.q1$CSCO
      vw.q1$MSFT.idx <-   (1*q1.vw.wgt$MSFT.wgt)*vw.q1$MSFT
      
      vw.q1$CVX.idx <-   (1*q1.vw.wgt$CVX.wgt)*vw.q1$CVX
      vw.q1$XOM.idx <-   (1*q1.vw.wgt$XOM.wgt)*vw.q1$XOM
      
      vw.q1[c(1:3,nrow(vw.q1)),]

# Calculating VW portfolio values for 1Q 2008 (similar to the EW
#                                              portfolio)
# Calculate daily portfolio values
      q1.vw.val <- data.frame(rowSums(vw.q1[,8:13]))
      q1.vw.val[c(1:3,nrow(q1.vw.val)),]
      names(q1.vw.val) <- paste("port.val")
      q1.vw.val$date <- vw.q1$date
      q1.vw.val[c(1:3,nrow(q1.vw.val)),]
      
      q2.vw.inv <- q1.vw.val[nrow(q1.vw.val),1]
      q2.vw.inv

#End of Q1 Value

#===================================================End Q1
      
      
      #=========================Start Q2 2020     
      # Calculating VW portfolio values for QUARTER2 2020 (similar to the EW
      #                                              portfolio)
      # Subset vwport to the data from Apr 1 2020 - June 30 2020
      
      vw.q2 <- subset(vwport[,c(1,14:19)],
                      vwport$date >= as.Date("2020-04-01") &
                        vwport$date <= as.Date("2020-07-30"))
      names(vw.q2) <-  paste(c("date","AMGN","JNJ","CSCO","MSFT","CVX","XOM")) 
      # shortenthe names
      vw.q2[c(1:3,nrow(vw.q2)),]
      
      # Calculating VW portfolio values for 1Q 2020 (similar to the EW
      #                                              portfolio)
      # Calculate the cumulative gross return for each security
      vw.q2[1,2:7] <-0.92979
      vw.q2$AMGN <- cumprod(vw.q2$AMGN)
      
      vw.q2$JNJ <- cumprod(vw.q2$JNJ)
      vw.q2$CSCO <- cumprod(vw.q2$CSCO)
      vw.q2$MSFT <- cumprod(vw.q2$MSFT)
      
      vw.q2$CVX <- cumprod(vw.q2$CVX)
      vw.q2$XOM <- cumprod(vw.q2$XOM)
      
      vw.q2[c(1:3,nrow(vw.q2)),]
      
      # Apply quarter-end weights
      vw.q2$AMGN.idx <-   (1*q2.vw.wgt$AMGN.wgt)*vw.q2$AMGN
      
      vw.q2$JNJ.idx <-    (1*q2.vw.wgt$JNJ.wgt)*vw.q2$JNJ
      vw.q2$CSCO.idx <-   (1*q2.vw.wgt$CSCO.wgt)*vw.q2$CSCO
      vw.q2$MSFT.idx <-   (1*q2.vw.wgt$MSFT.wgt)*vw.q2$MSFT
      
      vw.q2$CVX.idx <-   (1*q2.vw.wgt$CVX.wgt)*vw.q2$CVX
      vw.q2$XOM.idx <-   (1*q2.vw.wgt$XOM.wgt)*vw.q2$XOM
      
      vw.q2[c(1:3,nrow(vw.q2)),]
      
      # Calculating VW portfolio values for 2Q 2008 (similar to the EW
      #                                              portfolio)
      # Calculate daily portfolio values
      q2.vw.val <- data.frame(rowSums(vw.q2[,8:13]))
      q2.vw.val[c(1:3,nrow(q2.vw.val)),]
      names(q2.vw.val) <- paste("port.val")
      q2.vw.val$date <- vw.q2$date
      q2.vw.val[c(1:3,nrow(q2.vw.val)),]
      
      q3.vw.inv <- q2.vw.val[nrow(q2.vw.val),1]
      q3.vw.inv
      
      #End of q2 Value
      
      #===================================================End q2     

      #=========================Start q3 2019     
      # Calculating VW portfolio values for QUARTER3 2019 (similar to the EW
      #                                              portfolio)
      # Subset vwport to the data from Apr 1 2019 - June 30 2019
      
      vw.q3 <- subset(vwport[,c(1,14:19)],
                      vwport$date >= as.Date("2019-07-01") &
                        vwport$date <= as.Date("2019-09-30"))
      names(vw.q3) <-  paste(c("date","AMGN","JNJ","CSCO","MSFT","CVX","XOM")) 
      # shortenthe names
      vw.q3[c(1:3,nrow(vw.q3)),]
      
      # Calculating VW portfolio values for 1Q 2019 (similar to the EW
      #                                              portfolio)
      # Calculate the cumulative gross return for each security
      vw.q3[1,2:7] <-1
      vw.q3$AMGN <- cumprod(vw.q3$AMGN)
      
      vw.q3$JNJ <- cumprod(vw.q3$JNJ)
      vw.q3$CSCO <- cumprod(vw.q3$CSCO)
      vw.q3$MSFT <- cumprod(vw.q3$MSFT)
      
      vw.q3$CVX <- cumprod(vw.q3$CVX)
      vw.q3$XOM <- cumprod(vw.q3$XOM)
      
      vw.q3[c(1:3,nrow(vw.q3)),]
      
      # Apply quarter-end weights
      vw.q3$AMGN.idx <-   (1*q3.vw.wgt$AMGN.wgt)*vw.q3$AMGN
      
      vw.q3$JNJ.idx <-    (1*q3.vw.wgt$JNJ.wgt)*vw.q3$JNJ
      vw.q3$CSCO.idx <-   (1*q3.vw.wgt$CSCO.wgt)*vw.q3$CSCO
      vw.q3$MSFT.idx <-   (1*q3.vw.wgt$MSFT.wgt)*vw.q3$MSFT
      
      vw.q3$CVX.idx <-   (1*q3.vw.wgt$CVX.wgt)*vw.q3$CVX
      vw.q3$XOM.idx <-   (1*q3.vw.wgt$XOM.wgt)*vw.q3$XOM
      
      vw.q3[c(1:3,nrow(vw.q3)),]
      
      # Calculating VW portfolio values for 2Q 2008 (similar to the EW
      #                                              portfolio)
      # Calculate daily portfolio values
      q3.vw.val <- data.frame(rowSums(vw.q3[,8:13]))
      q3.vw.val[c(1:3,nrow(q3.vw.val)),]
      names(q3.vw.val) <- paste("port.val")
      q3.vw.val$date <- vw.q3$date
      q3.vw.val[c(1:3,nrow(q3.vw.val)),]
      
      q4.vw.inv <- q3.vw.val[nrow(q3.vw.val),1]
      q4.vw.inv
      
      #End of q3 Value
      
      #===================================================End q3
      
      
      #=========================Start q4 2019     
      # Calculating VW portfolio values for QUARTER4 2019 (similar to the EW
      #                                              portfolio)
      # Subset vwport to the data from Apr 1 2019 - June 30 2019
      
      vw.q4 <- subset(vwport[,c(1,14:19)],
                      vwport$date >= as.Date("2019-10-01") &
                        vwport$date <= as.Date("2019-12-31"))
      names(vw.q4) <-  paste(c("date","AMGN","JNJ","CSCO","MSFT","CVX","XOM")) 
      # shortenthe names
      vw.q4[c(1:3,nrow(vw.q4)),]
      
      # Calculating VW portfolio values for 1Q 2019 (similar to the EW
      #                                              portfolio)
      # Calculate the cumulative gross return for each security
      vw.q4[1,2:7] <-0.97927
      vw.q4$AMGN <- cumprod(vw.q4$AMGN)
      
      vw.q4$JNJ <- cumprod(vw.q4$JNJ)
      vw.q4$CSCO <- cumprod(vw.q4$CSCO)
      vw.q4$MSFT <- cumprod(vw.q4$MSFT)
      
      vw.q4$CVX <- cumprod(vw.q4$CVX)
      vw.q4$XOM <- cumprod(vw.q4$XOM)
      
      vw.q4[c(1:3,nrow(vw.q4)),]
      
      # Apply quarter-end weights
      vw.q4$AMGN.idx <-   (1*q4.vw.wgt$AMGN.wgt)*vw.q4$AMGN
      
      vw.q4$JNJ.idx <-    (1*q4.vw.wgt$JNJ.wgt)*vw.q4$JNJ
      vw.q4$CSCO.idx <-   (1*q4.vw.wgt$CSCO.wgt)*vw.q4$CSCO
      vw.q4$MSFT.idx <-   (1*q4.vw.wgt$MSFT.wgt)*vw.q4$MSFT
      
      vw.q4$CVX.idx <-   (1*q4.vw.wgt$CVX.wgt)*vw.q4$CVX
      vw.q4$XOM.idx <-   (1*q4.vw.wgt$XOM.wgt)*vw.q4$XOM
      
      vw.q4[c(1:3,nrow(vw.q4)),]
      
      # Calculating VW portfolio values for 2Q 2008 (similar to the EW
      #                                              portfolio)
      # Calculate daily portfolio values
      q4.vw.val <- data.frame(rowSums(vw.q4[,8:13]))
      q4.vw.val[c(1:3,nrow(q4.vw.val)),]
      names(q4.vw.val) <- paste("port.val")
      q4.vw.val$date <- vw.q4$date
      q4.vw.val[c(1:3,nrow(q4.vw.val)),]
      
      q12020.vw.inv <- q4.vw.val[nrow(q4.vw.val),1]
      q12020.vw.inv
      
      #End of q4 Value
      
      #===================================================End q4      
      

# Value-Weighted Portfolio
# Combining quarterly VW portfolio values into one data object
vw.portval <-
  rbind(q3.vw.val,q4.vw.val,q1.vw.val,q2.vw.val)
vw.portval[c(1:3,nrow(vw.portval)),]


# Normalized EW and VW portfolio price chart
# Combine the data
port.val <-
  merge(vw.portval,ew.portval,by="date")
names(port.val) <-
  paste(c("date","VW.cum","EW.cum"))
port.val[c(1:3,nrow(port.val)),]



# Value-Weighted Portfolio
# Plot the data
y.range <-range(port.val[,2:3])
plot(port.val$EW.cum,
     type="l",
     xlab="Date",
     ylab="Value of Investment",
     ylim=y.range,
     lty=1,
     col = "blue",
     main="Value of $1 Investment in Equal-
                  Weighted and Value-Weighted Portfolios of AMGN,
                  JNJn CSCO MSFT, CVX and XOM July 1 2019 - July 1 2020
                  2019")
lines(port.val$VW.cum,lty=2,col = "red")
abline(h=1,lty=1)
legend("topleft",
       c("Equal-Weighted Portfolio","Value-Weighted Portfolio"), col = c("blue","red"),
       lty=c(1,2))


#===========================================================================================



install.packages("xts")
install.packages("quantmod")
install.packages("dplyr")
install.packages("zoo")
install.packages("fBasics")
install.packages("FRAPO")
install.packages("ghyp")
install.packages("delt")
library("ghyp")
library(fBasics)
library("FRAPO")
library(dplyr)
library(xts)
library(quantmod)
library(zoo)
library(delt)
#========DATA PREP==========================


#Import data for each of the four securities - AMGN
data.Amgen <- read.csv("AMGN.csv",header=TRUE)
date <- as.Date(data.Amgen$Date,format="%Y-%m-%d")  
data.Amgen <- cbind(date, data.Amgen[,-1])
data.Amgen <- data.Amgen[order(data.Amgen$date),]
data.Amgen <- xts(data.Amgen[,2:7],order.by=data.Amgen[,1])
names(data.Amgen) <- paste(c("AMGN.Open","AMGN.High","AMGN.Low","AMGN.Close","AMGN.Adjusted","AMGN.Volume"))
data.Amgen[c(1:3,nrow(data.Amgen)),]


#Import data for each of the four securities - JNJ
data.JandJ <- read.csv("JNJ.csv",header=TRUE)
date <- as.Date(data.JandJ$Date,format="%Y-%m-%d")  
data.JandJ <- cbind(date, data.JandJ[,-1])
data.JandJ <- data.JandJ[order(data.JandJ$date),]
data.JandJ <- xts(data.JandJ[,2:7],order.by=data.JandJ[,1])
names(data.JandJ) <- paste(c("JNJ.Open","JNJ.High","JNJ.Low","JNJ.Close","JNJ.Adjusted","JNJ.Volume"))
data.JandJ[c(1:3,nrow(data.JandJ)),]

#Import data for each of the four securities - CISCO
data.Cisco <- read.csv("CSCO.csv",header=TRUE)
date <- as.Date(data.Cisco$Date,format="%Y-%m-%d")  
data.Cisco <- cbind(date, data.Cisco[,-1])
data.Cisco <- data.Cisco[order(data.Cisco$date),]
data.Cisco <- xts(data.Cisco[,2:7],order.by=data.Cisco[,1])
names(data.Cisco) <- paste(c("CSCO.Open","CSCO.High","CSCO.Low","CSCO.Close","CSCO.Adjusted","CSCO.Volume"))
data.Cisco[c(1:3,nrow(data.Cisco)),]

#Import data for each of the four securities - Microsoft (MSFT)
data.Microsoft <- read.csv("MSFT.csv",header=TRUE)
date <- as.Date(data.Microsoft$Date,format="%Y-%m-%d")  
data.Microsoft <- cbind(date, data.Microsoft[,-1])
data.Microsoft <- data.Microsoft[order(data.Microsoft$date),]
data.Microsoft <- xts(data.Microsoft[,2:7],order.by=data.Microsoft[,1])
names(data.Microsoft) <- paste(c("MSFT.Open","MSFT.High","MSFT.Low","MSFT.Close","MSFT.Adjusted","MSFT.Volume"))
data.Microsoft[c(1:3,nrow(data.Microsoft)),]

#Import data for each of the four securities - Exxon Mobil (XOM)
data.Exxon <- read.csv("XOM.csv",header=TRUE)
date <- as.Date(data.Exxon$Date,format="%Y-%m-%d")  
data.Exxon <- cbind(date, data.Exxon[,-1])
data.Exxon <- data.Exxon[order(data.Exxon$date),]
data.Exxon <- xts(data.Exxon[,2:7],order.by=data.Exxon[,1])
names(data.Exxon) <- paste(c("XOM.Open","XOM.High","XOM.Low","XOM.Close","XOM.Adjusted","XOM.Volume"))
data.Exxon[c(1:3,nrow(data.Exxon)),]

#Import data for each of the four securities - Chevron (CVX)
data.Chevron <- read.csv("CVX.csv",header=TRUE)
date <- as.Date(data.Chevron$Date,format="%Y-%m-%d")  
data.Chevron <- cbind(date, data.Chevron[,-1])
data.Chevron <- data.Chevron[order(data.Chevron$date),]
data.Chevron <- xts(data.Chevron[,2:7],order.by=data.Chevron[,1])
names(data.Chevron) <- paste(c("CVX.Open","CVX.High","CVX.Low","CVX.Close","CVX.Adjusted","CVX.Volume"))
data.Chevron[c(1:3,nrow(data.Chevron)),]


#Combine Data Into one Data Object
Close.Prices <- cbind(data.Amgen$AMGN.Close,data.JandJ$JNJ.Close, data.Cisco$CSCO.Close,data.Microsoft$MSFT.Close,data.Chevron$CVX.Close, data.Exxon$XOM.Close)
Close.Prices[c(1:3,nrow(Close.Prices)),]


#Convert Data into a data.frame

multi.df <- cbind(index(Close.Prices),data.frame(Close.Prices))
names(multi.df) <- paste(c("date","AMGN","JNJ","CSCO","MSFT","CVX","XOM"))
rownames(multi.df) <- seq(1,nrow(multi.df),1)
multi.df[c(1:3,nrow(multi.df)),]

#Calculate Normalized Values for Each Security
#Create an index for each security with values that equal the price of the 	security on each day divided by the security's price on December 31, 	2010.

multi.df$AMGN.idx <- multi.df$AMGN/multi.df$AMGN[1]
multi.df$JNJ.idx <- multi.df$JNJ/multi.df$JNJ[1]



multi.df$CSCO.idx <- multi.df$CSCO/multi.df$CSCO[1]
multi.df$MSFT.idx <- multi.df$MSFT/multi.df$MSFT[1]

multi.df$CVX.idx <- multi.df$CVX/multi.df$CVX[1]
multi.df$XOM.idx <- multi.df$XOM/multi.df$XOM[1]

options(digits=5)   # this option remains in place until end of session
multi.df[c(1:3,nrow(multi.df)),]    

#Plot the Capital Appreciation of Each Security
#For the x variable use date, for y use Qualcomm Index


multi <- data.Amgen[,5]
head(multi)
multi <- merge(multi,data.JandJ[,5])
multi <- merge(multi,data.Cisco[,5])
multi <- merge(multi,data.Microsoft[,5])
multi <- merge(multi,data.Chevron[,5])
multi <- merge(multi,data.Exxon[,5])


multi[c(1:3,nrow(multi)),]


#Porfolio Risk -#Combine the two return series

data.Amgen <- subset(data.Amgen,
                     index(data.Amgen) >= as.Date("2019-07-02"))

data.JandJ <- subset(data.JandJ,
                     index(data.JandJ) >= as.Date("2019-07-02"))
data.Cisco <- subset(data.Cisco,
                     index(data.Cisco) >= as.Date("2019-07-02"))
data.Microsoft <- subset(data.Microsoft,
                         index(data.Microsoft) >= as.Date("2019-07-02"))
data.Chevron <- subset(data.Chevron,
                       index(data.Chevron) >= as.Date("2019-07-02"))
data.Exxon <- subset(data.Exxon,
                     index(data.Exxon) >= as.Date("2019-07-02"))

data.Amgen[c(1:3,nrow(data.Amgen)),]

library(quantmod)
Amgen.ret <- Delt(data.Amgen$AMGN.Adjusted)
Amgen.ret[c(1:3,nrow(Amgen.ret)),]
data.Amgen[c(1:3,nrow(data.Amgen)),]


JandJ.ret <- Delt(data.JandJ$JNJ.Adjusted)
JandJ.ret[c(1:3,nrow(JandJ.ret)),]
data.JandJ[c(1:3,nrow(data.JandJ)),]


Cisco.ret <- Delt(data.Cisco$CSCO.Adjusted)
Cisco.ret[c(1:3,nrow(Cisco.ret)),]
data.Cisco[c(1:3,nrow(data.Cisco)),]

Microsoft.ret <- Delt(data.Microsoft$MSFT.Adjusted)
Microsoft.ret[c(1:3,nrow(Microsoft.ret)),]
data.Microsoft[c(1:3,nrow(data.Microsoft)),]

Chevron.ret <- Delt(data.Chevron$CVX.Adjusted)
Chevron.ret[c(1:3,nrow(Chevron.ret)),]
data.Chevron[c(1:3,nrow(data.Chevron)),]

Exxon.ret <- Delt(data.Exxon$XOM.Adjusted)
Exxon.ret[c(1:3,nrow(Exxon.ret)),]
data.Exxon[c(1:3,nrow(data.Exxon)),]


returns <- cbind(Amgen.ret,JandJ.ret, Cisco.ret, Microsoft.ret, Chevron.ret, Exxon.ret)
names(returns) <- paste(c("AMGN.Ret","JNJ.Ret","CSCO.ret","MSFT.ret","CVX.ret","XOM.ret"))


#returns <- returns[-1,]
returns[c(1:3,nrow(returns)),]

#Create a vector of weights - All 16.667%

WGT.2asset <- c(0.1666666666666667,0.1666666666666667,0.1666666666666667,0.1666666666666667,0.1666666666666667,0.1666666666666667)
WGT.2asset <- matrix(WGT.2asset,1)
WGT.2asset


#Create a transposed vector of weights (command: t)

tWGT.2asset <- t(WGT.2asset)
tWGT.2asset

#Construct Variance-Covariance Matrix Must convert returns into a matrix using
#as.matrix command

#To annualize the variances and covariances multiply the matrix by 252
## AMGN.Ret JNJ.Ret

mat.Ret <- as.matrix(returns)
head(mat.Ret)
mat.Ret[1,1:6]<- 0
# Calculate the covariance using cov command (scipen argument to 100 to allow
#                         for higher threshold before R converts the number into scientific notation)
options(scipen = "100")
cov(mat.Ret)

#To annualize the variances and covariances multiply the matrix by 252
## AMGN.Ret JNJ.Ret
## AMGN.Ret 0.10721389 0.02327632
## JNJ.Ret 0.02327632 0.03703578
#AMGN variance: 0.1072 JNJ variance: 0.0370
cov(mat.Ret)
VCOV.2asset <- cov(mat.Ret)*252
VCOV.2asset


# Calculate the portfolio risk (remmeber %*% is used to multiply a matrix)


mat.var2asset <- WGT.2asset %*% VCOV.2asset %*% tWGT.2asset
mat.var2asset

# To calculate the portfolio stndard deviation, use the sqrt command.

mat.sd2asset <- sqrt(mat.var2asset)
mat.sd2asset

q4.vw.inv
q12020.vw.inv
q2.vw.inv
q3.vw.inv

#--------------------------------------------------#



#Part B - Suitable distributions for returns
# 1. Fit the data using GHD, HYP and NIG
# 2. Plot the combine density functions
# 3. Create a Q-Q plot
# 4. Make a model recommendation using lik.ratio.test
# 5. Calculate and plot the VaR (using all models)
# 6. Calculate and plot the ES (using all models)


# Distributions
# Generalized hyperbolic distribution (GHD) and its special cases namely
# the hyperbolic (HYP) and normal inverse Gaussian (NIG) distributions
# Generalized lambda distribution (GLD)
# Time series data of returns, in particular daily return series, are in
# general not independent and identically distributed (iid).
# The volatility of return processes is not constant with respect to
# time.
# The absolute or squared returns are highly autocorrelated
# When analyzing historical returns, kurtosis can help an investor gauge an
# asset's level of risk. A leptokurtic distribution (more than 3 Kurtosis) means that the investor can
# experience broader fluctuations (e.g. three or more standard deviations
#                                  from the mean) resulting in greater potential for extremely low or high
# returns.
#


# The generalized hyperbolic distribution (GHD) was introduced by
# Barndorff-Nielsen to model the distribution of sand grain sizes and can
# account for heavy tails. It has since been applied to turbulence theory,
# geomorphology, financial mathematics
#





# The shape of the density for the GHD, HYP, and NIG can be plotted
# interactively for various parameter combinations.
# The functions that relate to the GHD have gh in their names, while those
# for the generalized hyperbolic Student's t have ght, those for the HYP have
# hyp, and those for the NIG have nig.




set.seed(10)
library(conflicted)
conflict_prefer("rghyp", "ghyp")
library("ghyp")
ghypsim<-rghyp(1000)
head(ghypsim)


# Generate datasets using hyperbolic, normal and normal iverse Gaussian
# simultions
# Check the data
## Min. 1st Qu. Median Mean 3rd Qu. Max.
## -7.653033 -0.936917 -0.003669 -0.035814 0.919887 8.707241
## Min. 1st Qu. Median Mean 3rd Qu. Max.
## -6.7015 -0.8388 0.0211 0.0275 0.9486 6.8417
## Min. 1st Qu. Median Mean 3rd Qu. Max.
## -4.793141 -0.527910 -0.000153 0.001417 0.499228 3.919666
## Min. 1st Qu. Median Mean 3rd Qu. Max.
## -3.531563 -0.658255 0.009352 0.009428 0.702029 2.989967
conflict_prefer("rnig", "GeneralizedHyperbolic")
library(fBasics)

hypsim<-rhyp(1000)
normsim<-rnorm(1000)
nigsim<-rnig(1000)
summary(ghypsim)
summary(hypsim)
summary(nigsim)
summary(normsim)

# Create histograms foreach data set
par(mfrow=c(2,2))
hist(nigsim, breaks = 50, col = "yellow", xlim = c(-10,10),ylim = c(0,150))
hist(ghypsim, breaks=50, col="red", xlim=c(-10,10), ylim = c(0,150))
hist(hypsim, breaks=50, col="green", xlim=c(-10,10),ylim = c(0,150))
hist(normsim, breaks=50, col="blue", xlim=c(-10,10),ylim = c(0,150))
box()



#==========================================================================================================================                          

#Part 2 starts here


date <- subset(index(data.Amgen),
               + index(data.Amgen) >= "2019-07-02")      

#date
# Create two variables: date (represents dates of observation), AMGNPrice
# (price of AMGN stock)

AMGNPrice <-data.frame(data.Amgen$AMGN.Open)

# Use date variable to create attribute "time" for Amgen Price

attr(AMGNPrice, 'time')<-date
head(AMGNPrice)

AMGNPrice
# Create a variable that represent daily returns by using returnseries()(part
#                                                                        of FRAPO package) function.
# Call this variable BTCRet
# The function "returnseries" from the FRAPO package is used to compute
# financial returns from prices or indexes.
# 

AMGNRet<-returnseries(AMGNPrice)
summary(AMGNRet)
head(AMGNRet)
AMGNRet[1,] <- 0

attr(AMGNRet, 'time')<-date
str(AMGNRet)



library(ghyp)
library(timeSeries)
library(fBasics)



# Fitting the data set returns to the
# GHD
# This example is based on Example 1 of Ch6 but it uses the data set created
# in the previous example
# Upload the packages

# Create the timeseries using AMGNRet

datets<-as.character(date)
AMGNTimeS<-timeSeries(AMGNRet)
str(AMGNTimeS)
head(AMGNTimeS)


# Define and plot the density function for the timeseries AMGNTimes
# Review A probability density function (PDF), or density of a continuous
# random variable, is a function whose value at any given sample (or point) in
# the sample space (the set of possible values taken by the random variable)
# can be interpreted as providing a relative likelihood that the value of the
# random variable would equal that sample.


ef<-density(AMGNTimeS, na.rm=TRUE)
plot(ef)

library("FRAPO")
library(ghyp)
# Fit the Generalized Hyperbolic Distribution
ghdfit <- fit.ghypuv(AMGNTimeS, symmetric = FALSE, control = list(maxit =
                                                                    1000), na.rm = TRUE)
# Fit the Hyperbolic Distribution
hypfit<- fit.hypuv(AMGNTimeS, symmetric = FALSE, control = list(maxit =
                                                                  1000), na.rm = TRUE)
# Fit the Normal Inverse Gaussian Distribution
nigfit<- fit.NIGuv(AMGNTimeS, symmetric = FALSE, control = list(maxit =
                                                                  1000), na.rm = TRUE)                  
#install.packages("HyperbolicDist")
library("HyperbolicDist")

#install.packages("GeneralizedHyperbolic")
library(GeneralizedHyperbolic)


#as.numeric(unlist(ef$x))
conflict_prefer("dghyp", "ghyp")
ghddens <- dghyp(ef$x, ghdfit)
hypdens <- dghyp(ef$x, hypfit)
nigdens <- dghyp(ef$x, nigfit)
nordens <- dnorm(ef$x, mean = mean(AMGNTimeS, na.rm=TRUE), sd = sd(c(AMGNTimeS[, 1]), na.rm=TRUE))
col.def <- c("black", "red", "blue", "green", "orange")
plot(ef, xlab = "", ylab = expression(f(x)), ylim = c(0, 0.50))
lines(ef$x, ghddens, col = "red")
lines(ef$x, hypdens, col = "blue")
lines(ef$x, nigdens, col = "green")
lines(ef$x, nordens, col = "orange")
legend("topleft",
       legend = c("empirical", "GHD", "HYP", "NIG", "NORM"),
       col = col.def, lty = 1)



#Fitting the data set returns to the
# GHD


# Create a Q-Q Plot
# "A Q-Q plot is a scatterplot created by plotting two sets of quantiles
# against one another. If both sets of quantiles came from the same
# distribution, we should see the points forming a line that's roughly straight."
# cex: number indicating the amount by which plotting text and symbols
# should be scaled relative to the default. 1=default, 1.5 is 50% larger, 0.5 is
# 50% smaller, etc
# ghyp.pch: a plotting character, i.e., symbol to use for quantiles of the
# generalized hyperbolic distribution


conflict_prefer("qqghyp", "ghyp")
qqghyp(ghdfit, line = TRUE, ghyp.col = "red", plot.legend = FALSE,
       gaussian = FALSE, main = "", cex = 0.8)
qqghyp(hypfit, add = TRUE, ghyp.pch = 2, ghyp.col = "blue",
       gaussian = FALSE, line = FALSE, cex = 0.8)
qqghyp(nigfit, add = TRUE, ghyp.pch = 3, ghyp.col = "green",
       gaussian = FALSE, line = FALSE, cex = 0.8)
legend("topleft", legend = c("GHD", "HYP", "NIG"),
       col = col.def[-c(1,5)], pch = 1:3)

# Fitting the data set returns to the
# GHD
# Determine which model works best
# Use the function "stepAIC.ghyp"
# This function performs a model selection in the scope of the generalized
# hyperbolic distribution class based on the Akaike information criterion.
# stepAIC.ghyp can be used for the univariate as well as for the multivariate
# case.
# Akaike information criterion: an index used in a number of areas as an aid
# to choosing between competing models
# the smaller the AIC value the better is the model
# the smaller the LLM value the better is the model
str(AIC <- stepAIC.ghyp(AMGNTimeS, control = list(maxit = 1000)))    
head(AIC$fit.table)                    


# Determine which model works best
# Use the function "lik.ratio.test" to perform a likelihood-ratio test on fitted
# generalized hyperbolic distribution objects of class mle.ghyp.
# The likelihood-ratio test can be used to check whether a special case of
# the generalized hyperbolic distribution is the "true" underlying distribution.
# statistic: the value of the L-statistic.
# p.value: the p-value for the test (the p-value is less than 0.05, then there
#                                    is evidence against the null hypothesis)
# df: the degrees of freedom for the L-statistic
# H0: a boolean stating whether the null hypothesis is TRUE or FALSE (if
#                                                                     TRUE there is no relationship between the data sets                        
# 


LRghdnig <- lik.ratio.test(ghdfit, nigfit)
LRghdnig


LRghdhyp <- lik.ratio.test(ghdfit, hypfit)
LRghdhyp                                    



# Risk assessment using GHD
#       The behavior of the VaR and ES risk measures according to each of the
#       models is investigated.
#       The two risks (VaR, ES) measures are derived from the fitted GHD, HYP,
#       and NIG distributions for the AMGN returns from the previous subsection.
#       These measures are calculated over a span from the 95.0% to the 99.9%
#         levels. The resulting trajectories of the VaR and ES are then compared to
#       their empirical counterparts.
#       For the ES the mean of the lower quantile values is used (the relevant code
#                                                                 is provided in Listing 6.2)                        

# 
# First, a sequence of probabilities is created for which the VaR and ES are to
# be computed.
# Because we are dealing with returns instead of losses, these are defined for
# the left tail of the distribution. In the next lines the VaR for these levels is
# computed by utilizing the quantile function for the GHD.
# By convention, losses are expressed as positive numbers, and hence the
# absolute values of the quantiles returned by the function are used.
# The VaR based on the normal distribution can be computed by providing
# the necessary estimates for the location and scale. The VaR values thus
# determined are compared to their empirical counterparts, which are
# determined by the quantile() function.
# Probabilities


p <- seq(0.001, 0.05, 0.001)


# VaR calculation (using quantiles)

conflict_prefer("qghyp", "ghyp")
ghd.VaR <- abs(qghyp(p, ghdfit))
hyp.VaR <- abs(qghyp(p, hypfit))
nig.VaR <- abs(qghyp(p, nigfit))
nor.VaR <- abs(qnorm(p, mean = mean(AMGNTimeS, na.rm=TRUE), sd = sd(c(AMGNTimeS[, 1]), na.rm = TRUE)))
emp.VaR <- abs(quantile(x = AMGNTimeS, probs = p, na.rm=TRUE))                        


# Plot the VaR

plot(emp.VaR, type = "l", xlab = "", ylab = "VaR", axes = FALSE,
     ylim = range(c(hyp.VaR, nig.VaR, ghd.VaR, nor.VaR, emp.VaR)))
box()
axis(1, at = seq(along = p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.VaR, ghd.VaR, hyp.VaR,
                          nig.VaR, nor.VaR)))
lines(seq(along = p), ghd.VaR, col = "red")
lines(seq(along = p), hyp.VaR, col = "blue")
lines(seq(along = p), nig.VaR, col = "green")
lines(seq(along = p), nor.VaR, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)


# Risk assessment using GHD
# ES calculation
ghd.ES <- abs(ESghyp(p, ghdfit))
hyp.ES <- abs(ESghyp(p, hypfit))
nig.ES <- abs(ESghyp(p, nigfit))
nor.ES <- abs(mean(AMGNTimeS, na.rm=TRUE) - sd(c(AMGNTimeS[, 1]), na.rm = TRUE) *
                dnorm(qnorm(1 - p)) / p)
obs.p <- ceiling(p * length(AMGNTimeS))
emp.ES <- sapply(obs.p, function(x) abs(mean(sort(c(AMGNTimeS))[1:x])))


# Plot the ES
plot(emp.ES, type = "l", xlab = "", ylab = "ES", axes = FALSE,
     ylim = range(c(hyp.ES, nig.ES, ghd.ES, nor.ES, emp.ES), na.rm = TRUE))
box()
axis(1, at = 1:length(p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.ES, ghd.ES, hyp.ES, nig.ES, nor.ES)))
lines(1:length(p), ghd.ES, col = "red")
lines(1:length(p), hyp.ES, col = "blue")
lines(1:length(p), nig.ES, col = "green")
lines(1:length(p), nor.ES, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)

#===========================End of Amgen - Distributions, VaR and ES

#==========================================================================================================================                          

#Part 2 starts here


date <- subset(index(data.Amgen),
               + index(data.Amgen) >= "2019-07-02")      

#date
# Create two variables: date (represents dates of observation), AMGNPrice
# (price of AMGN stock)

AMGNPrice <-data.frame(data.Amgen$AMGN.Open)

# Use date variable to create attribute "time" for Amgen Price

attr(AMGNPrice, 'time')<-date
head(AMGNPrice)

AMGNPrice
# Create a variable that represent daily returns by using returnseries()(part
#                                                                        of FRAPO package) function.
# Call this variable BTCRet
# The function "returnseries" from the FRAPO package is used to compute
# financial returns from prices or indexes.
# 

AMGNRet<-returnseries(AMGNPrice)
summary(AMGNRet)
head(AMGNRet)
AMGNRet[1,] <- 0

attr(AMGNRet, 'time')<-date
str(AMGNRet)



library(ghyp)
library(timeSeries)
library(fBasics)



# Fitting the data set returns to the
# GHD
# This example is based on Example 1 of Ch6 but it uses the data set created
# in the previous example
# Upload the packages

# Create the timeseries using AMGNRet

datets<-as.character(date)
AMGNTimeS<-timeSeries(AMGNRet)
str(AMGNTimeS)
head(AMGNTimeS)


# Define and plot the density function for the timeseries AMGNTimes
# Review A probability density function (PDF), or density of a continuous
# random variable, is a function whose value at any given sample (or point) in
# the sample space (the set of possible values taken by the random variable)
# can be interpreted as providing a relative likelihood that the value of the
# random variable would equal that sample.


ef<-density(AMGNTimeS, na.rm=TRUE)
plot(ef)

library("FRAPO")
library(ghyp)
# Fit the Generalized Hyperbolic Distribution
ghdfit <- fit.ghypuv(AMGNTimeS, symmetric = FALSE, control = list(maxit =
                                                                    1000), na.rm = TRUE)
# Fit the Hyperbolic Distribution
hypfit<- fit.hypuv(AMGNTimeS, symmetric = FALSE, control = list(maxit =
                                                                  1000), na.rm = TRUE)
# Fit the Normal Inverse Gaussian Distribution
nigfit<- fit.NIGuv(AMGNTimeS, symmetric = FALSE, control = list(maxit =
                                                                  1000), na.rm = TRUE)                  
#install.packages("HyperbolicDist")
library("HyperbolicDist")

#install.packages("GeneralizedHyperbolic")
library(GeneralizedHyperbolic)


#as.numeric(unlist(ef$x))
conflict_prefer("dghyp", "ghyp")
ghddens <- dghyp(ef$x, ghdfit)
hypdens <- dghyp(ef$x, hypfit)
nigdens <- dghyp(ef$x, nigfit)
nordens <- dnorm(ef$x, mean = mean(AMGNTimeS, na.rm=TRUE), sd = sd(c(AMGNTimeS[, 1]), na.rm=TRUE))
col.def <- c("black", "red", "blue", "green", "orange")
plot(ef, xlab = "", ylab = expression(f(x)), ylim = c(0, 0.50))
lines(ef$x, ghddens, col = "red")
lines(ef$x, hypdens, col = "blue")
lines(ef$x, nigdens, col = "green")
lines(ef$x, nordens, col = "orange")
legend("topleft",
       legend = c("empirical", "GHD", "HYP", "NIG", "NORM"),
       col = col.def, lty = 1)



#Fitting the data set returns to the
# GHD


# Create a Q-Q Plot
# "A Q-Q plot is a scatterplot created by plotting two sets of quantiles
# against one another. If both sets of quantiles came from the same
# distribution, we should see the points forming a line that's roughly straight."
# cex: number indicating the amount by which plotting text and symbols
# should be scaled relative to the default. 1=default, 1.5 is 50% larger, 0.5 is
# 50% smaller, etc
# ghyp.pch: a plotting character, i.e., symbol to use for quantiles of the
# generalized hyperbolic distribution


conflict_prefer("qqghyp", "ghyp")
qqghyp(ghdfit, line = TRUE, ghyp.col = "red", plot.legend = FALSE,
       gaussian = FALSE, main = "", cex = 0.8)
qqghyp(hypfit, add = TRUE, ghyp.pch = 2, ghyp.col = "blue",
       gaussian = FALSE, line = FALSE, cex = 0.8)
qqghyp(nigfit, add = TRUE, ghyp.pch = 3, ghyp.col = "green",
       gaussian = FALSE, line = FALSE, cex = 0.8)
legend("topleft", legend = c("GHD", "HYP", "NIG"),
       col = col.def[-c(1,5)], pch = 1:3)

# Fitting the data set returns to the
# GHD
# Determine which model works best
# Use the function "stepAIC.ghyp"
# This function performs a model selection in the scope of the generalized
# hyperbolic distribution class based on the Akaike information criterion.
# stepAIC.ghyp can be used for the univariate as well as for the multivariate
# case.
# Akaike information criterion: an index used in a number of areas as an aid
# to choosing between competing models
# the smaller the AIC value the better is the model
# the smaller the LLM value the better is the model
str(AIC <- stepAIC.ghyp(AMGNTimeS, control = list(maxit = 1000)))    
head(AIC$fit.table)                    


# Determine which model works best
# Use the function "lik.ratio.test" to perform a likelihood-ratio test on fitted
# generalized hyperbolic distribution objects of class mle.ghyp.
# The likelihood-ratio test can be used to check whether a special case of
# the generalized hyperbolic distribution is the "true" underlying distribution.
# statistic: the value of the L-statistic.
# p.value: the p-value for the test (the p-value is less than 0.05, then there
#                                    is evidence against the null hypothesis)
# df: the degrees of freedom for the L-statistic
# H0: a boolean stating whether the null hypothesis is TRUE or FALSE (if
#                                                                     TRUE there is no relationship between the data sets                        
# 


LRghdnig <- lik.ratio.test(ghdfit, nigfit)
LRghdnig


LRghdhyp <- lik.ratio.test(ghdfit, hypfit)
LRghdhyp                                    



# Risk assessment using GHD
#       The behavior of the VaR and ES risk measures according to each of the
#       models is investigated.
#       The two risks (VaR, ES) measures are derived from the fitted GHD, HYP,
#       and NIG distributions for the AMGN returns from the previous subsection.
#       These measures are calculated over a span from the 95.0% to the 99.9%
#         levels. The resulting trajectories of the VaR and ES are then compared to
#       their empirical counterparts.
#       For the ES the mean of the lower quantile values is used (the relevant code
#                                                                 is provided in Listing 6.2)                        

# 
# First, a sequence of probabilities is created for which the VaR and ES are to
# be computed.
# Because we are dealing with returns instead of losses, these are defined for
# the left tail of the distribution. In the next lines the VaR for these levels is
# computed by utilizing the quantile function for the GHD.
# By convention, losses are expressed as positive numbers, and hence the
# absolute values of the quantiles returned by the function are used.
# The VaR based on the normal distribution can be computed by providing
# the necessary estimates for the location and scale. The VaR values thus
# determined are compared to their empirical counterparts, which are
# determined by the quantile() function.
# Probabilities


p <- seq(0.001, 0.05, 0.001)


# VaR calculation (using quantiles)

conflict_prefer("qghyp", "ghyp")
ghd.VaR <- abs(qghyp(p, ghdfit))
hyp.VaR <- abs(qghyp(p, hypfit))
nig.VaR <- abs(qghyp(p, nigfit))
nor.VaR <- abs(qnorm(p, mean = mean(AMGNTimeS, na.rm=TRUE), sd = sd(c(AMGNTimeS[, 1]), na.rm = TRUE)))
emp.VaR <- abs(quantile(x = AMGNTimeS, probs = p, na.rm=TRUE))                        


# Plot the VaR

plot(emp.VaR, type = "l", xlab = "", ylab = "VaR", axes = FALSE,
     ylim = range(c(hyp.VaR, nig.VaR, ghd.VaR, nor.VaR, emp.VaR)))
box()
axis(1, at = seq(along = p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.VaR, ghd.VaR, hyp.VaR,
                          nig.VaR, nor.VaR)))
lines(seq(along = p), ghd.VaR, col = "red")
lines(seq(along = p), hyp.VaR, col = "blue")
lines(seq(along = p), nig.VaR, col = "green")
lines(seq(along = p), nor.VaR, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)


# Risk assessment using GHD
# ES calculation
ghd.ES <- abs(ESghyp(p, ghdfit))
hyp.ES <- abs(ESghyp(p, hypfit))
nig.ES <- abs(ESghyp(p, nigfit))
nor.ES <- abs(mean(AMGNTimeS, na.rm=TRUE) - sd(c(AMGNTimeS[, 1]), na.rm = TRUE) *
                dnorm(qnorm(1 - p)) / p)
obs.p <- ceiling(p * length(AMGNTimeS))
emp.ES <- sapply(obs.p, function(x) abs(mean(sort(c(AMGNTimeS))[1:x])))


# Plot the ES
plot(emp.ES, type = "l", xlab = "", ylab = "ES", axes = FALSE,
     ylim = range(c(hyp.ES, nig.ES, ghd.ES, nor.ES, emp.ES), na.rm = TRUE))
box()
axis(1, at = 1:length(p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.ES, ghd.ES, hyp.ES, nig.ES, nor.ES)))
lines(1:length(p), ghd.ES, col = "red")
lines(1:length(p), hyp.ES, col = "blue")
lines(1:length(p), nig.ES, col = "green")
lines(1:length(p), nor.ES, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)

#===========================End of Amgen - Distributions, VaR and ES

#===========================Start of Johnson & Johnson=======================================================                          


date <- subset(index(data.JandJ),
               + index(data.JandJ) >= "2019-07-02")      

#date
# Create two variables: date (represents dates of observation), JandJPrice
# (price of JandJ stock)

JandJPrice <-data.frame(data.JandJ$JNJ.Open)

# Use date variable to create attribute "time" for JandJ Price

attr(JandJPrice, 'time')<-date
head(JandJPrice)

#JandJPrice
# Create a variable that represent daily returns by using returnseries()(part
#                                                                        of FRAPO package) function.
# The function "returnseries" from the FRAPO package is used to compute
# financial returns from prices or indexes.
# 

JandJRet<-returnseries(JandJPrice)
summary(JandJRet)
head(JandJRet)
JandJRet[1,] <- 0

attr(JandJRet, 'time')<-date
str(JandJRet)


# Fitting the data set returns to the
# GHD
# This example is based on Example 1 of Ch6 but it uses the data set created
# in the previous example
# Upload the packages

# Create the timeseries using JandJRet

datets<-as.character(date)
JandJTimeS<-timeSeries(JandJRet)
str(JandJTimeS)
head(JandJTimeS)


# Define and plot the density function for the timeseries JandJTimes
# Review A probability density function (PDF), or density of a continuous
# random variable, is a function whose value at any given sample (or point) in
# the sample space (the set of possible values taken by the random variable)
# can be interpreted as providing a relative likelihood that the value of the
# random variable would equal that sample.


ef<-density(JandJTimeS, na.rm=TRUE)
plot(ef)


# Fit the Generalized Hyperbolic Distribution
ghdfit <- fit.ghypuv(JandJTimeS, symmetric = FALSE, control = list(maxit =
                                                                     1000), na.rm = TRUE)
# Fit the Hyperbolic Distribution
hypfit<- fit.hypuv(JandJTimeS, symmetric = FALSE, control = list(maxit =
                                                                   1000), na.rm = TRUE)
# Fit the Normal Inverse Gaussian Distribution
nigfit<- fit.NIGuv(JandJTimeS, symmetric = FALSE, control = list(maxit =
                                                                   1000), na.rm = TRUE)                  
#install.packages("HyperbolicDist")
library("HyperbolicDist")

#install.packages("GeneralizedHyperbolic")
library(GeneralizedHyperbolic)


#as.numeric(unlist(ef$x))
conflict_prefer("dghyp", "ghyp")
ghddens <- dghyp(ef$x, ghdfit)
hypdens <- dghyp(ef$x, hypfit)
nigdens <- dghyp(ef$x, nigfit)
nordens <- dnorm(ef$x, mean = mean(JandJTimeS, na.rm=TRUE), sd = sd(c(JandJTimeS[, 1]), na.rm=TRUE))
col.def <- c("black", "red", "blue", "green", "orange")
plot(ef, xlab = "", ylab = expression(f(x)), ylim = c(0, 0.50))
lines(ef$x, ghddens, col = "red")
lines(ef$x, hypdens, col = "blue")
lines(ef$x, nigdens, col = "green")
lines(ef$x, nordens, col = "orange")
legend("topleft",
       legend = c("empirical", "GHD", "HYP", "NIG", "NORM"),
       col = col.def, lty = 1)



#Fitting the data set returns to the
# GHD


# Create a Q-Q Plot
# "A Q-Q plot is a scatterplot created by plotting two sets of quantiles
# against one another. If both sets of quantiles came from the same
# distribution, we should see the points forming a line that's roughly straight."
# cex: number indicating the amount by which plotting text and symbols
# should be scaled relative to the default. 1=default, 1.5 is 50% larger, 0.5 is
# 50% smaller, etc
# ghyp.pch: a plotting character, i.e., symbol to use for quantiles of the
# generalized hyperbolic distribution


conflict_prefer("qqghyp", "ghyp")
qqghyp(ghdfit, line = TRUE, ghyp.col = "red", plot.legend = FALSE,
       gaussian = FALSE, main = "", cex = 0.8)
qqghyp(hypfit, add = TRUE, ghyp.pch = 2, ghyp.col = "blue",
       gaussian = FALSE, line = FALSE, cex = 0.8)
qqghyp(nigfit, add = TRUE, ghyp.pch = 3, ghyp.col = "green",
       gaussian = FALSE, line = FALSE, cex = 0.8)
legend("topleft", legend = c("GHD", "HYP", "NIG"),
       col = col.def[-c(1,5)], pch = 1:3)

# Fitting the data set returns to the
# GHD
# Determine which model works best
# Use the function "stepAIC.ghyp"
# This function performs a model selection in the scope of the generalized
# hyperbolic distribution class based on the Akaike information criterion.
# stepAIC.ghyp can be used for the univariate as well as for the multivariate
# case.
# Akaike information criterion: an index used in a number of areas as an aid
# to choosing between competing models
# the smaller the AIC value the better is the model
# the smaller the LLM value the better is the model
str(AIC <- stepAIC.ghyp(JandJTimeS, control = list(maxit = 1000)))    
head(AIC$fit.table)                    


# Determine which model works best
# Use the function "lik.ratio.test" to perform a likelihood-ratio test on fitted
# generalized hyperbolic distribution objects of class mle.ghyp.
# The likelihood-ratio test can be used to check whether a special case of
# the generalized hyperbolic distribution is the "true" underlying distribution.
# statistic: the value of the L-statistic.
# p.value: the p-value for the test (the p-value is less than 0.05, then there
#                                    is evidence against the null hypothesis)
# df: the degrees of freedom for the L-statistic
# H0: a boolean stating whether the null hypothesis is TRUE or FALSE (if
#                                                                     TRUE there is no relationship between the data sets                        
# 


LRghdnig <- lik.ratio.test(ghdfit, nigfit)
LRghdnig


LRghdhyp <- lik.ratio.test(ghdfit, hypfit)
LRghdhyp                                    


# Risk assessment using GHD
#       The behavior of the VaR and ES risk measures according to each of the
#       models is investigated.
#       The two risks (VaR, ES) measures are derived from the fitted GHD, HYP,
#       and NIG distributions for the JandJ returns from the previous subsection.
#       These measures are calculated over a span from the 95.0% to the 99.9%
#         levels. The resulting trajectories of the VaR and ES are then compared to
#       their empirical counterparts.
#       For the ES the mean of the lower quantile values is used (the relevant code
#                                                                 is provided in Listing 6.2)                        

# 
# First, a sequence of probabilities is created for which the VaR and ES are to
# be computed.
# Because we are dealing with returns instead of losses, these are defined for
# the left tail of the distribution. In the next lines the VaR for these levels is
# computed by utilizing the quantile function for the GHD.
# By convention, losses are expressed as positive numbers, and hence the
# absolute values of the quantiles returned by the function are used.
# The VaR based on the normal distribution can be computed by providing
# the necessary estimates for the location and scale. The VaR values thus
# determined are compared to their empirical counterparts, which are
# determined by the quantile() function.
# Probabilities


p <- seq(0.001, 0.05, 0.001)


# VaR calculation (using quantiles)

conflict_prefer("qghyp", "ghyp")
ghd.VaR <- abs(qghyp(p, ghdfit))
hyp.VaR <- abs(qghyp(p, hypfit))
nig.VaR <- abs(qghyp(p, nigfit))
nor.VaR <- abs(qnorm(p, mean = mean(JandJTimeS, na.rm=TRUE), sd = sd(c(JandJTimeS[, 1]), na.rm = TRUE)))
emp.VaR <- abs(quantile(x = JandJTimeS, probs = p, na.rm=TRUE))                        


# Plot the VaR

plot(emp.VaR, type = "l", xlab = "", ylab = "VaR", axes = FALSE,
     ylim = range(c(hyp.VaR, nig.VaR, ghd.VaR, nor.VaR, emp.VaR)))
box()
axis(1, at = seq(along = p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.VaR, ghd.VaR, hyp.VaR,
                          nig.VaR, nor.VaR)))
lines(seq(along = p), ghd.VaR, col = "red")
lines(seq(along = p), hyp.VaR, col = "blue")
lines(seq(along = p), nig.VaR, col = "green")
lines(seq(along = p), nor.VaR, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)


# Risk assessment using GHD
# ES calculation
ghd.ES <- abs(ESghyp(p, ghdfit))
hyp.ES <- abs(ESghyp(p, hypfit))
nig.ES <- abs(ESghyp(p, nigfit))
nor.ES <- abs(mean(JandJTimeS, na.rm=TRUE) - sd(c(JandJTimeS[, 1]), na.rm = TRUE) *
                dnorm(qnorm(1 - p)) / p)
obs.p <- ceiling(p * length(JandJTimeS))
emp.ES <- sapply(obs.p, function(x) abs(mean(sort(c(JandJTimeS))[1:x])))


# Plot the ES
plot(emp.ES, type = "l", xlab = "", ylab = "ES", axes = FALSE,
     ylim = range(c(hyp.ES, nig.ES, ghd.ES, nor.ES, emp.ES), na.rm = TRUE))
box()
axis(1, at = 1:length(p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.ES, ghd.ES, hyp.ES, nig.ES, nor.ES)))
lines(1:length(p), ghd.ES, col = "red")
lines(1:length(p), hyp.ES, col = "blue")
lines(1:length(p), nig.ES, col = "green")
lines(1:length(p), nor.ES, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)

#===========================End of Johnson and Johnson - Distributions, VaR and ES

#===========================Start of CISCO=======================================================                          


date <- subset(index(data.Cisco),
               + index(data.Cisco) >= "2019-07-02")      

#date
# Create two variables: date (represents dates of observation), CiscoPrice
# (price of Cisco stock)

CiscoPrice <-data.frame(data.Cisco$CSCO.Open)

# Use date variable to create attribute "time" for Cisco Price

attr(CiscoPrice, 'time')<-date
head(CiscoPrice)

#CiscoPrice
# Create a variable that represent daily returns by using returnseries()(part
#                                                                        of FRAPO package) function.
# The function "returnseries" from the FRAPO package is used to compute
# financial returns from prices or indexes.
# 

CiscoRet<-returnseries(CiscoPrice)
summary(CiscoRet)
CiscoRet[1,] <- 0
head(CiscoRet)

attr(CiscoRet, 'time')<-date
str(CiscoRet)


# Fitting the data set returns to the
# GHD
# This example is based on Example 1 of Ch6 but it uses the data set created
# in the previous example
# Upload the packages

# Create the timeseries using CiscoRet

datets<-as.character(date)
CiscoTimeS<-timeSeries(CiscoRet)
str(CiscoTimeS)
head(CiscoTimeS)


# Define and plot the density function for the timeseries CiscoTimes
# Review A probability density function (PDF), or density of a continuous
# random variable, is a function whose value at any given sample (or point) in
# the sample space (the set of possible values taken by the random variable)
# can be interpreted as providing a relative likelihood that the value of the
# random variable would equal that sample.


ef<-density(CiscoTimeS, na.rm=TRUE)
plot(ef)


# Fit the Generalized Hyperbolic Distribution
ghdfit <- fit.ghypuv(CiscoTimeS, symmetric = FALSE, control = list(maxit =
                                                                     1000), na.rm = TRUE)
# Fit the Hyperbolic Distribution
hypfit<- fit.hypuv(CiscoTimeS, symmetric = FALSE, control = list(maxit =
                                                                   1000), na.rm = TRUE)
# Fit the Normal Inverse Gaussian Distribution
nigfit<- fit.NIGuv(CiscoTimeS, symmetric = FALSE, control = list(maxit =
                                                                   1000), na.rm = TRUE)                  
#install.packages("HyperbolicDist")
library("HyperbolicDist")

#install.packages("GeneralizedHyperbolic")
library(GeneralizedHyperbolic)


#as.numeric(unlist(ef$x))
conflict_prefer("dghyp", "ghyp")
ghddens <- dghyp(ef$x, ghdfit)
hypdens <- dghyp(ef$x, hypfit)
nigdens <- dghyp(ef$x, nigfit)
nordens <- dnorm(ef$x, mean = mean(CiscoTimeS, na.rm=TRUE), sd = sd(c(CiscoTimeS[, 1]), na.rm=TRUE))
col.def <- c("black", "red", "blue", "green", "orange")
plot(ef, xlab = "", ylab = expression(f(x)), ylim = c(0, 0.50))
lines(ef$x, ghddens, col = "red")
lines(ef$x, hypdens, col = "blue")
lines(ef$x, nigdens, col = "green")
lines(ef$x, nordens, col = "orange")
legend("topleft",
       legend = c("empirical", "GHD", "HYP", "NIG", "NORM"),
       col = col.def, lty = 1)



#Fitting the data set returns to the
# GHD


# Create a Q-Q Plot
# "A Q-Q plot is a scatterplot created by plotting two sets of quantiles
# against one another. If both sets of quantiles came from the same
# distribution, we should see the points forming a line that's roughly straight."
# cex: number indicating the amount by which plotting text and symbols
# should be scaled relative to the default. 1=default, 1.5 is 50% larger, 0.5 is
# 50% smaller, etc
# ghyp.pch: a plotting character, i.e., symbol to use for quantiles of the
# generalized hyperbolic distribution


conflict_prefer("qqghyp", "ghyp")
qqghyp(ghdfit, line = TRUE, ghyp.col = "red", plot.legend = FALSE,
       gaussian = FALSE, main = "", cex = 0.8)
qqghyp(hypfit, add = TRUE, ghyp.pch = 2, ghyp.col = "blue",
       gaussian = FALSE, line = FALSE, cex = 0.8)
qqghyp(nigfit, add = TRUE, ghyp.pch = 3, ghyp.col = "green",
       gaussian = FALSE, line = FALSE, cex = 0.8)
legend("topleft", legend = c("GHD", "HYP", "NIG"),
       col = col.def[-c(1,5)], pch = 1:3)

# Fitting the data set returns to the
# GHD
# Determine which model works best
# Use the function "stepAIC.ghyp"
# This function performs a model selection in the scope of the generalized
# hyperbolic distribution class based on the Akaike information criterion.
# stepAIC.ghyp can be used for the univariate as well as for the multivariate
# case.
# Akaike information criterion: an index used in a number of areas as an aid
# to choosing between competing models
# the smaller the AIC value the better is the model
# the smaller the LLM value the better is the model
str(AIC <- stepAIC.ghyp(CiscoTimeS, control = list(maxit = 1000)))    
summary(AIC$fit.table)                    


# Determine which model works best
# Use the function "lik.ratio.test" to perform a likelihood-ratio test on fitted
# generalized hyperbolic distribution objects of class mle.ghyp.
# The likelihood-ratio test can be used to check whether a special case of
# the generalized hyperbolic distribution is the "true" underlying distribution.
# statistic: the value of the L-statistic.
# p.value: the p-value for the test (the p-value is less than 0.05, then there
#                                    is evidence against the null hypothesis)
# df: the degrees of freedom for the L-statistic
# H0: a boolean stating whether the null hypothesis is TRUE or FALSE (if
#                                                                     TRUE there is no relationship between the data sets                        
# 


LRghdnig <- lik.ratio.test(ghdfit, nigfit)
LRghdhyp <- lik.ratio.test(ghdfit, hypfit)
LRghdnig
LRghdhyp                                    


# Risk assessment using GHD
#       The behavior of the VaR and ES risk measures according to each of the
#       models is investigated.
#       The two risks (VaR, ES) measures are derived from the fitted GHD, HYP,
#       and NIG distributions for the Cisco returns from the previous subsection.
#       These measures are calculated over a span from the 95.0% to the 99.9%
#         levels. The resulting trajectories of the VaR and ES are then compared to
#       their empirical counterparts.
#       For the ES the mean of the lower quantile values is used (the relevant code
#                                                                 is provided in Listing 6.2)                        

# 
# First, a sequence of probabilities is created for which the VaR and ES are to
# be computed.
# Because we are dealing with returns instead of losses, these are defined for
# the left tail of the distribution. In the next lines the VaR for these levels is
# computed by utilizing the quantile function for the GHD.
# By convention, losses are expressed as positive numbers, and hence the
# absolute values of the quantiles returned by the function are used.
# The VaR based on the normal distribution can be computed by providing
# the necessary estimates for the location and scale. The VaR values thus
# determined are compared to their empirical counterparts, which are
# determined by the quantile() function.
# Probabilities


p <- seq(0.001, 0.05, 0.001)


# VaR calculation (using quantiles)

conflict_prefer("qghyp", "ghyp")
ghd.VaR <- abs(qghyp(p, ghdfit))
hyp.VaR <- abs(qghyp(p, hypfit))
nig.VaR <- abs(qghyp(p, nigfit))
nor.VaR <- abs(qnorm(p, mean = mean(CiscoTimeS, na.rm=TRUE), sd = sd(c(CiscoTimeS[, 1]), na.rm = TRUE)))
emp.VaR <- abs(quantile(x = CiscoTimeS, probs = p, na.rm=TRUE))                        


# Plot the VaR

plot(emp.VaR, type = "l", xlab = "", ylab = "VaR", axes = FALSE,
     ylim = range(c(hyp.VaR, nig.VaR, ghd.VaR, nor.VaR, emp.VaR)))
box()
axis(1, at = seq(along = p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.VaR, ghd.VaR, hyp.VaR,
                          nig.VaR, nor.VaR)))
lines(seq(along = p), ghd.VaR, col = "red")
lines(seq(along = p), hyp.VaR, col = "blue")
lines(seq(along = p), nig.VaR, col = "green")
lines(seq(along = p), nor.VaR, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)


# Risk assessment using GHD
# ES calculation
ghd.ES <- abs(ESghyp(p, ghdfit))
hyp.ES <- abs(ESghyp(p, hypfit))
nig.ES <- abs(ESghyp(p, nigfit))
nor.ES <- abs(mean(CiscoTimeS, na.rm=TRUE) - sd(c(CiscoTimeS[, 1]), na.rm = TRUE) *
                dnorm(qnorm(1 - p)) / p)
obs.p <- ceiling(p * length(CiscoTimeS))
emp.ES <- sapply(obs.p, function(x) abs(mean(sort(c(CiscoTimeS))[1:x])))


# Plot the ES
plot(emp.ES, type = "l", xlab = "", ylab = "ES", axes = FALSE,
     ylim = range(c(hyp.ES, nig.ES, ghd.ES, nor.ES, emp.ES), na.rm = TRUE))
box()
axis(1, at = 1:length(p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.ES, ghd.ES, hyp.ES, nig.ES, nor.ES)))
lines(1:length(p), ghd.ES, col = "red")
lines(1:length(p), hyp.ES, col = "blue")
lines(1:length(p), nig.ES, col = "green")
lines(1:length(p), nor.ES, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)

#===========================End of Cisco - Distributions, VaR and ES          



#===========================Start of Microsoft=======================================================                          


date <- subset(index(data.Microsoft),
               + index(data.Microsoft) >= "2019-07-02")      

#date
# Create two variables: date (represents dates of observation), MicrosoftPrice
# (price of Microsoft stock)

MicrosoftPrice <-data.frame(data.Microsoft$MSFT.Open)

# Use date variable to create attribute "time" for Microsoft Price

attr(MicrosoftPrice, 'time')<-date
head(MicrosoftPrice)

#MicrosoftPrice
# Create a variable that represent daily returns by using returnseries()(part
#                                                                        of FRAPO package) function.
# The function "returnseries" from the FRAPO package is used to compute
# financial returns from prices or indexes.
# 

MicrosoftRet<-returnseries(MicrosoftPrice)

MicrosoftRet[1,] <- 0
head(MicrosoftRet)
summary(MicrosoftRet)

attr(MicrosoftRet, 'time')<-date
str(MicrosoftRet)


# Fitting the data set returns to the
# GHD
# This example is based on Example 1 of Ch6 but it uses the data set created
# in the previous example
# Upload the packages

# Create the timeseries using MicrosoftRet

datets<-as.character(date)
MicrosoftTimeS<-timeSeries(MicrosoftRet)
str(MicrosoftTimeS)
head(MicrosoftTimeS)


# Define and plot the density function for the timeseries MicrosoftTimes
# Review A probability density function (PDF), or density of a continuous
# random variable, is a function whose value at any given sample (or point) in
# the sample space (the set of possible values taken by the random variable)
# can be interpreted as providing a relative likelihood that the value of the
# random variable would equal that sample.


ef<-density(MicrosoftTimeS, na.rm=TRUE)
plot(ef)


# Fit the Generalized Hyperbolic Distribution
ghdfit <- fit.ghypuv(MicrosoftTimeS, symmetric = FALSE, control = list(maxit =
                                                                         1000), na.rm = TRUE)
# Fit the Hyperbolic Distribution
hypfit<- fit.hypuv(MicrosoftTimeS, symmetric = FALSE, control = list(maxit =
                                                                       1000), na.rm = TRUE)
# Fit the Normal Inverse Gaussian Distribution
nigfit<- fit.NIGuv(MicrosoftTimeS, symmetric = FALSE, control = list(maxit =
                                                                       1000), na.rm = TRUE)                  
#install.packages("HyperbolicDist")
library("HyperbolicDist")

#install.packages("GeneralizedHyperbolic")
library(GeneralizedHyperbolic)


#as.numeric(unlist(ef$x))
conflict_prefer("dghyp", "ghyp")
ghddens <- dghyp(ef$x, ghdfit)
hypdens <- dghyp(ef$x, hypfit)
nigdens <- dghyp(ef$x, nigfit)
nordens <- dnorm(ef$x, mean = mean(MicrosoftTimeS, na.rm=TRUE), sd = sd(c(MicrosoftTimeS[, 1]), na.rm=TRUE))
col.def <- c("black", "red", "blue", "green", "orange")
plot(ef, xlab = "", ylab = expression(f(x)), ylim = c(0, 0.50))
lines(ef$x, ghddens, col = "red")
lines(ef$x, hypdens, col = "blue")
lines(ef$x, nigdens, col = "green")
lines(ef$x, nordens, col = "orange")
legend("topleft",
       legend = c("empirical", "GHD", "HYP", "NIG", "NORM"),
       col = col.def, lty = 1)



#Fitting the data set returns to the
# GHD


# Create a Q-Q Plot
# "A Q-Q plot is a scatterplot created by plotting two sets of quantiles
# against one another. If both sets of quantiles came from the same
# distribution, we should see the points forming a line that's roughly straight."
# cex: number indicating the amount by which plotting text and symbols
# should be scaled relative to the default. 1=default, 1.5 is 50% larger, 0.5 is
# 50% smaller, etc
# ghyp.pch: a plotting character, i.e., symbol to use for quantiles of the
# generalized hyperbolic distribution


conflict_prefer("qqghyp", "ghyp")
qqghyp(ghdfit, line = TRUE, ghyp.col = "red", plot.legend = FALSE,
       gaussian = FALSE, main = "", cex = 0.8)
qqghyp(hypfit, add = TRUE, ghyp.pch = 2, ghyp.col = "blue",
       gaussian = FALSE, line = FALSE, cex = 0.8)
qqghyp(nigfit, add = TRUE, ghyp.pch = 3, ghyp.col = "green",
       gaussian = FALSE, line = FALSE, cex = 0.8)
legend("topleft", legend = c("GHD", "HYP", "NIG"),
       col = col.def[-c(1,5)], pch = 1:3)

# Fitting the data set returns to the
# GHD
# Determine which model works best
# Use the function "stepAIC.ghyp"
# This function performs a model selection in the scope of the generalized
# hyperbolic distribution class based on the Akaike information criterion.
# stepAIC.ghyp can be used for the univariate as well as for the multivariate
# case.
# Akaike information criterion: an index used in a number of areas as an aid
# to choosing between competing models
# the smaller the AIC value the better is the model
# the smaller the LLM value the better is the model
str(AIC <- stepAIC.ghyp(MicrosoftTimeS, control = list(maxit = 1000)))    
summary(AIC$fit.table)                    


# Determine which model works best
# Use the function "lik.ratio.test" to perform a likelihood-ratio test on fitted
# generalized hyperbolic distribution objects of class mle.ghyp.
# The likelihood-ratio test can be used to check whether a special case of
# the generalized hyperbolic distribution is the "true" underlying distribution.
# statistic: the value of the L-statistic.
# p.value: the p-value for the test (the p-value is less than 0.05, then there
#                                    is evidence against the null hypothesis)
# df: the degrees of freedom for the L-statistic
# H0: a boolean stating whether the null hypothesis is TRUE or FALSE (if
#                                                                     TRUE there is no relationship between the data sets                        
# 


LRghdnig <- lik.ratio.test(ghdfit, nigfit)
LRghdhyp <- lik.ratio.test(ghdfit, hypfit)
LRghdnig
LRghdhyp                                    


# Risk assessment using GHD
#       The behavior of the VaR and ES risk measures according to each of the
#       models is investigated.
#       The two risks (VaR, ES) measures are derived from the fitted GHD, HYP,
#       and NIG distributions for the Microsoft returns from the previous subsection.
#       These measures are calculated over a span from the 95.0% to the 99.9%
#         levels. The resulting trajectories of the VaR and ES are then compared to
#       their empirical counterparts.
#       For the ES the mean of the lower quantile values is used (the relevant code
#                                                                 is provided in Listing 6.2)                        

# 
# First, a sequence of probabilities is created for which the VaR and ES are to
# be computed.
# Because we are dealing with returns instead of losses, these are defined for
# the left tail of the distribution. In the next lines the VaR for these levels is
# computed by utilizing the quantile function for the GHD.
# By convention, losses are expressed as positive numbers, and hence the
# absolute values of the quantiles returned by the function are used.
# The VaR based on the normal distribution can be computed by providing
# the necessary estimates for the location and scale. The VaR values thus
# determined are compared to their empirical counterparts, which are
# determined by the quantile() function.
# Probabilities


p <- seq(0.001, 0.05, 0.001)


# VaR calculation (using quantiles)

conflict_prefer("qghyp", "ghyp")
ghd.VaR <- abs(qghyp(p, ghdfit))
hyp.VaR <- abs(qghyp(p, hypfit))
nig.VaR <- abs(qghyp(p, nigfit))
nor.VaR <- abs(qnorm(p, mean = mean(MicrosoftTimeS, na.rm=TRUE), sd = sd(c(MicrosoftTimeS[, 1]), na.rm = TRUE)))
emp.VaR <- abs(quantile(x = MicrosoftTimeS, probs = p, na.rm=TRUE))                        


# Plot the VaR

plot(emp.VaR, type = "l", xlab = "", ylab = "VaR", axes = FALSE,
     ylim = range(c(hyp.VaR, nig.VaR, ghd.VaR, nor.VaR, emp.VaR)))
box()
axis(1, at = seq(along = p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.VaR, ghd.VaR, hyp.VaR,
                          nig.VaR, nor.VaR)))
lines(seq(along = p), ghd.VaR, col = "red")
lines(seq(along = p), hyp.VaR, col = "blue")
lines(seq(along = p), nig.VaR, col = "green")
lines(seq(along = p), nor.VaR, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)


# Risk assessment using GHD
# ES calculation
ghd.ES <- abs(ESghyp(p, ghdfit))
hyp.ES <- abs(ESghyp(p, hypfit))
nig.ES <- abs(ESghyp(p, nigfit))
nor.ES <- abs(mean(MicrosoftTimeS, na.rm=TRUE) - sd(c(MicrosoftTimeS[, 1]), na.rm = TRUE) *
                dnorm(qnorm(1 - p)) / p)
obs.p <- ceiling(p * length(MicrosoftTimeS))
emp.ES <- sapply(obs.p, function(x) abs(mean(sort(c(MicrosoftTimeS))[1:x])))


# Plot the ES
plot(emp.ES, type = "l", xlab = "", ylab = "ES", axes = FALSE,
     ylim = range(c(hyp.ES, nig.ES, ghd.ES, nor.ES, emp.ES), na.rm = TRUE))
box()
axis(1, at = 1:length(p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.ES, ghd.ES, hyp.ES, nig.ES, nor.ES)))
lines(1:length(p), ghd.ES, col = "red")
lines(1:length(p), hyp.ES, col = "blue")
lines(1:length(p), nig.ES, col = "green")
lines(1:length(p), nor.ES, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)

#===========================End of Microsoft - Distributions, VaR and ES


#===========================Start of Chevron=======================================================                          


date <- subset(index(data.Chevron),
               + index(data.Chevron) >= "2019-07-02")      

#date
# Create two variables: date (represents dates of observation), ChevronPrice
# (price of Chevron stock)

ChevronPrice <-data.frame(data.Chevron$CVX.Open)

# Use date variable to create attribute "time" for Chevron Price

attr(ChevronPrice, 'time')<-date
head(ChevronPrice)

#ChevronPrice
# Create a variable that represent daily returns by using returnseries()(part
#                                                                        of FRAPO package) function.
# The function "returnseries" from the FRAPO package is used to compute
# financial returns from prices or indexes.
# 

ChevronRet<-returnseries(ChevronPrice)

ChevronRet[1,] <- 0
head(ChevronRet)
summary(ChevronRet)

attr(ChevronRet, 'time')<-date
str(ChevronRet)


# Fitting the data set returns to the
# GHD
# This example is based on Example 1 of Ch6 but it uses the data set created
# in the previous example
# Upload the packages

# Create the timeseries using ChevronRet

datets<-as.character(date)
ChevronTimeS<-timeSeries(ChevronRet)
str(ChevronTimeS)
head(ChevronTimeS)


# Define and plot the density function for the timeseries ChevronTimes
# Review A probability density function (PDF), or density of a continuous
# random variable, is a function whose value at any given sample (or point) in
# the sample space (the set of possible values taken by the random variable)
# can be interpreted as providing a relative likelihood that the value of the
# random variable would equal that sample.


ef<-density(ChevronTimeS, na.rm=TRUE)
plot(ef)


# Fit the Generalized Hyperbolic Distribution
ghdfit <- fit.ghypuv(ChevronTimeS, symmetric = FALSE, control = list(maxit =
                                                                       1000), na.rm = TRUE)
# Fit the Hyperbolic Distribution
hypfit<- fit.hypuv(ChevronTimeS, symmetric = FALSE, control = list(maxit =
                                                                     1000), na.rm = TRUE)
# Fit the Normal Inverse Gaussian Distribution
nigfit<- fit.NIGuv(ChevronTimeS, symmetric = FALSE, control = list(maxit =
                                                                     1000), na.rm = TRUE)                  
#install.packages("HyperbolicDist")
library("HyperbolicDist")

#install.packages("GeneralizedHyperbolic")
library(GeneralizedHyperbolic)


#as.numeric(unlist(ef$x))
conflict_prefer("dghyp", "ghyp")
ghddens <- dghyp(ef$x, ghdfit)
hypdens <- dghyp(ef$x, hypfit)
nigdens <- dghyp(ef$x, nigfit)
nordens <- dnorm(ef$x, mean = mean(ChevronTimeS, na.rm=TRUE), sd = sd(c(ChevronTimeS[, 1]), na.rm=TRUE))
col.def <- c("black", "red", "blue", "green", "orange")
plot(ef, xlab = "", ylab = expression(f(x)), ylim = c(0, 0.50))
lines(ef$x, ghddens, col = "red")
lines(ef$x, hypdens, col = "blue")
lines(ef$x, nigdens, col = "green")
lines(ef$x, nordens, col = "orange")
legend("topleft",
       legend = c("empirical", "GHD", "HYP", "NIG", "NORM"),
       col = col.def, lty = 1)



#Fitting the data set returns to the
# GHD


# Create a Q-Q Plot
# "A Q-Q plot is a scatterplot created by plotting two sets of quantiles
# against one another. If both sets of quantiles came from the same
# distribution, we should see the points forming a line that's roughly straight."
# cex: number indicating the amount by which plotting text and symbols
# should be scaled relative to the default. 1=default, 1.5 is 50% larger, 0.5 is
# 50% smaller, etc
# ghyp.pch: a plotting character, i.e., symbol to use for quantiles of the
# generalized hyperbolic distribution


conflict_prefer("qqghyp", "ghyp")
qqghyp(ghdfit, line = TRUE, ghyp.col = "red", plot.legend = FALSE,
       gaussian = FALSE, main = "", cex = 0.8)
qqghyp(hypfit, add = TRUE, ghyp.pch = 2, ghyp.col = "blue",
       gaussian = FALSE, line = FALSE, cex = 0.8)
qqghyp(nigfit, add = TRUE, ghyp.pch = 3, ghyp.col = "green",
       gaussian = FALSE, line = FALSE, cex = 0.8)
legend("topleft", legend = c("GHD", "HYP", "NIG"),
       col = col.def[-c(1,5)], pch = 1:3)

# Fitting the data set returns to the
# GHD
# Determine which model works best
# Use the function "stepAIC.ghyp"
# This function performs a model selection in the scope of the generalized
# hyperbolic distribution class based on the Akaike information criterion.
# stepAIC.ghyp can be used for the univariate as well as for the multivariate
# case.
# Akaike information criterion: an index used in a number of areas as an aid
# to choosing between competing models
# the smaller the AIC value the better is the model
# the smaller the LLM value the better is the model
str(AIC <- stepAIC.ghyp(ChevronTimeS, control = list(maxit = 1000)))    
summary(AIC$fit.table)                    


# Determine which model works best
# Use the function "lik.ratio.test" to perform a likelihood-ratio test on fitted
# generalized hyperbolic distribution objects of class mle.ghyp.
# The likelihood-ratio test can be used to check whether a special case of
# the generalized hyperbolic distribution is the "true" underlying distribution.
# statistic: the value of the L-statistic.
# p.value: the p-value for the test (the p-value is less than 0.05, then there
#                                    is evidence against the null hypothesis)
# df: the degrees of freedom for the L-statistic
# H0: a boolean stating whether the null hypothesis is TRUE or FALSE (if
#                                                                     TRUE there is no relationship between the data sets                        
# 


LRghdnig <- lik.ratio.test(ghdfit, nigfit)
LRghdhyp <- lik.ratio.test(ghdfit, hypfit)
LRghdnig
LRghdhyp                                    


# Risk assessment using GHD
#       The behavior of the VaR and ES risk measures according to each of the
#       models is investigated.
#       The two risks (VaR, ES) measures are derived from the fitted GHD, HYP,
#       and NIG distributions for the Chevron returns from the previous subsection.
#       These measures are calculated over a span from the 95.0% to the 99.9%
#         levels. The resulting trajectories of the VaR and ES are then compared to
#       their empirical counterparts.
#       For the ES the mean of the lower quantile values is used (the relevant code
#                                                                 is provided in Listing 6.2)                        

# 
# First, a sequence of probabilities is created for which the VaR and ES are to
# be computed.
# Because we are dealing with returns instead of losses, these are defined for
# the left tail of the distribution. In the next lines the VaR for these levels is
# computed by utilizing the quantile function for the GHD.
# By convention, losses are expressed as positive numbers, and hence the
# absolute values of the quantiles returned by the function are used.
# The VaR based on the normal distribution can be computed by providing
# the necessary estimates for the location and scale. The VaR values thus
# determined are compared to their empirical counterparts, which are
# determined by the quantile() function.
# Probabilities


p <- seq(0.001, 0.05, 0.001)


# VaR calculation (using quantiles)

conflict_prefer("qghyp", "ghyp")
ghd.VaR <- abs(qghyp(p, ghdfit))
hyp.VaR <- abs(qghyp(p, hypfit))
nig.VaR <- abs(qghyp(p, nigfit))
nor.VaR <- abs(qnorm(p, mean = mean(ChevronTimeS, na.rm=TRUE), sd = sd(c(ChevronTimeS[, 1]), na.rm = TRUE)))
emp.VaR <- abs(quantile(x = ChevronTimeS, probs = p, na.rm=TRUE))                        


# Plot the VaR

plot(emp.VaR, type = "l", xlab = "", ylab = "VaR", axes = FALSE,
     ylim = range(c(hyp.VaR, nig.VaR, ghd.VaR, nor.VaR, emp.VaR)))
box()
axis(1, at = seq(along = p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.VaR, ghd.VaR, hyp.VaR,
                          nig.VaR, nor.VaR)))
lines(seq(along = p), ghd.VaR, col = "red")
lines(seq(along = p), hyp.VaR, col = "blue")
lines(seq(along = p), nig.VaR, col = "green")
lines(seq(along = p), nor.VaR, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)


# Risk assessment using GHD
# ES calculation
ghd.ES <- abs(ESghyp(p, ghdfit))
hyp.ES <- abs(ESghyp(p, hypfit))
nig.ES <- abs(ESghyp(p, nigfit))
nor.ES <- abs(mean(ChevronTimeS, na.rm=TRUE) - sd(c(ChevronTimeS[, 1]), na.rm = TRUE) *
                dnorm(qnorm(1 - p)) / p)
obs.p <- ceiling(p * length(ChevronTimeS))
emp.ES <- sapply(obs.p, function(x) abs(mean(sort(c(ChevronTimeS))[1:x])))


# Plot the ES
plot(emp.ES, type = "l", xlab = "", ylab = "ES", axes = FALSE,
     ylim = range(c(hyp.ES, nig.ES, ghd.ES, nor.ES, emp.ES), na.rm = TRUE))
box()
axis(1, at = 1:length(p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.ES, ghd.ES, hyp.ES, nig.ES, nor.ES)))
lines(1:length(p), ghd.ES, col = "red")
lines(1:length(p), hyp.ES, col = "blue")
lines(1:length(p), nig.ES, col = "green")
lines(1:length(p), nor.ES, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)

#===========================End of Chevron - Distributions, VaR and ES 


#===========================Start of Exxon=======================================================                          


date <- subset(index(data.Exxon),
               + index(data.Exxon) >= "2019-07-02")      

#date
# Create two variables: date (represents dates of observation), ExxonPrice
# (price of Exxon stock)

ExxonPrice <-data.frame(data.Exxon$XOM.Open)

# Use date variable to create attribute "time" for Exxon Price

attr(ExxonPrice, 'time')<-date
head(ExxonPrice)

#ExxonPrice
# Create a variable that represent daily returns by using returnseries()(part
#                                                                        of FRAPO package) function.
# The function "returnseries" from the FRAPO package is used to compute
# financial returns from prices or indexes.
# 

ExxonRet<-returnseries(ExxonPrice)

ExxonRet[1,] <- 0
head(ExxonRet)
summary(ExxonRet)

attr(ExxonRet, 'time')<-date
str(ExxonRet)


# Fitting the data set returns to the
# GHD
# This example is based on Example 1 of Ch6 but it uses the data set created
# in the previous example
# Upload the packages

# Create the timeseries using ExxonRet

datets<-as.character(date)
ExxonTimeS<-timeSeries(ExxonRet)
str(ExxonTimeS)
head(ExxonTimeS)


# Define and plot the density function for the timeseries ExxonTimes
# Review A probability density function (PDF), or density of a continuous
# random variable, is a function whose value at any given sample (or point) in
# the sample space (the set of possible values taken by the random variable)
# can be interpreted as providing a relative likelihood that the value of the
# random variable would equal that sample.


ef<-density(ExxonTimeS, na.rm=TRUE)
plot(ef)


# Fit the Generalized Hyperbolic Distribution
ghdfit <- fit.ghypuv(ExxonTimeS, symmetric = FALSE, control = list(maxit =
                                                                     1000), na.rm = TRUE)
# Fit the Hyperbolic Distribution
hypfit<- fit.hypuv(ExxonTimeS, symmetric = FALSE, control = list(maxit =
                                                                   1000), na.rm = TRUE)
# Fit the Normal Inverse Gaussian Distribution
nigfit<- fit.NIGuv(ExxonTimeS, symmetric = FALSE, control = list(maxit =
                                                                   1000), na.rm = TRUE)                  
#install.packages("HyperbolicDist")
library("HyperbolicDist")

#install.packages("GeneralizedHyperbolic")
library(GeneralizedHyperbolic)


#as.numeric(unlist(ef$x))
conflict_prefer("dghyp", "ghyp")
ghddens <- dghyp(ef$x, ghdfit)
hypdens <- dghyp(ef$x, hypfit)
nigdens <- dghyp(ef$x, nigfit)
nordens <- dnorm(ef$x, mean = mean(ExxonTimeS, na.rm=TRUE), sd = sd(c(ExxonTimeS[, 1]), na.rm=TRUE))
col.def <- c("black", "red", "blue", "green", "orange")
plot(ef, xlab = "", ylab = expression(f(x)), ylim = c(0, 0.50))
lines(ef$x, ghddens, col = "red")
lines(ef$x, hypdens, col = "blue")
lines(ef$x, nigdens, col = "green")
lines(ef$x, nordens, col = "orange")
legend("topleft",
       legend = c("empirical", "GHD", "HYP", "NIG", "NORM"),
       col = col.def, lty = 1)



#Fitting the data set returns to the
# GHD


# Create a Q-Q Plot
# "A Q-Q plot is a scatterplot created by plotting two sets of quantiles
# against one another. If both sets of quantiles came from the same
# distribution, we should see the points forming a line that's roughly straight."
# cex: number indicating the amount by which plotting text and symbols
# should be scaled relative to the default. 1=default, 1.5 is 50% larger, 0.5 is
# 50% smaller, etc
# ghyp.pch: a plotting character, i.e., symbol to use for quantiles of the
# generalized hyperbolic distribution


conflict_prefer("qqghyp", "ghyp")
qqghyp(ghdfit, line = TRUE, ghyp.col = "red", plot.legend = FALSE,
       gaussian = FALSE, main = "", cex = 0.8)
qqghyp(hypfit, add = TRUE, ghyp.pch = 2, ghyp.col = "blue",
       gaussian = FALSE, line = FALSE, cex = 0.8)
qqghyp(nigfit, add = TRUE, ghyp.pch = 3, ghyp.col = "green",
       gaussian = FALSE, line = FALSE, cex = 0.8)
legend("topleft", legend = c("GHD", "HYP", "NIG"),
       col = col.def[-c(1,5)], pch = 1:3)

# Fitting the data set returns to the
# GHD
# Determine which model works best
# Use the function "stepAIC.ghyp"
# This function performs a model selection in the scope of the generalized
# hyperbolic distribution class based on the Akaike information criterion.
# stepAIC.ghyp can be used for the univariate as well as for the multivariate
# case.
# Akaike information criterion: an index used in a number of areas as an aid
# to choosing between competing models
# the smaller the AIC value the better is the model
# the smaller the LLM value the better is the model
str(AIC <- stepAIC.ghyp(ExxonTimeS, control = list(maxit = 1000)))    
summary(AIC$fit.table)                    


# Determine which model works best
# Use the function "lik.ratio.test" to perform a likelihood-ratio test on fitted
# generalized hyperbolic distribution objects of class mle.ghyp.
# The likelihood-ratio test can be used to check whether a special case of
# the generalized hyperbolic distribution is the "true" underlying distribution.
# statistic: the value of the L-statistic.
# p.value: the p-value for the test (the p-value is less than 0.05, then there
#                                    is evidence against the null hypothesis)
# df: the degrees of freedom for the L-statistic
# H0: a boolean stating whether the null hypothesis is TRUE or FALSE (if
#                                                                     TRUE there is no relationship between the data sets                        
# 


LRghdnig <- lik.ratio.test(ghdfit, nigfit)
LRghdhyp <- lik.ratio.test(ghdfit, hypfit)
LRghdnig
LRghdhyp                                    


# Risk assessment using GHD
#       The behavior of the VaR and ES risk measures according to each of the
#       models is investigated.
#       The two risks (VaR, ES) measures are derived from the fitted GHD, HYP,
#       and NIG distributions for the Exxon returns from the previous subsection.
#       These measures are calculated over a span from the 95.0% to the 99.9%
#         levels. The resulting trajectories of the VaR and ES are then compared to
#       their empirical counterparts.
#       For the ES the mean of the lower quantile values is used (the relevant code
#                                                                 is provided in Listing 6.2)                        

# 
# First, a sequence of probabilities is created for which the VaR and ES are to
# be computed.
# Because we are dealing with returns instead of losses, these are defined for
# the left tail of the distribution. In the next lines the VaR for these levels is
# computed by utilizing the quantile function for the GHD.
# By convention, losses are expressed as positive numbers, and hence the
# absolute values of the quantiles returned by the function are used.
# The VaR based on the normal distribution can be computed by providing
# the necessary estimates for the location and scale. The VaR values thus
# determined are compared to their empirical counterparts, which are
# determined by the quantile() function.
# Probabilities


p <- seq(0.001, 0.05, 0.001)


# VaR calculation (using quantiles)

conflict_prefer("qghyp", "ghyp")
ghd.VaR <- abs(qghyp(p, ghdfit))
hyp.VaR <- abs(qghyp(p, hypfit))
nig.VaR <- abs(qghyp(p, nigfit))
nor.VaR <- abs(qnorm(p, mean = mean(ExxonTimeS, na.rm=TRUE), sd = sd(c(ExxonTimeS[, 1]), na.rm = TRUE)))
emp.VaR <- abs(quantile(x = ExxonTimeS, probs = p, na.rm=TRUE))                        


# Plot the VaR

plot(emp.VaR, type = "l", xlab = "", ylab = "VaR", axes = FALSE,
     ylim = range(c(hyp.VaR, nig.VaR, ghd.VaR, nor.VaR, emp.VaR)))
box()
axis(1, at = seq(along = p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.VaR, ghd.VaR, hyp.VaR,
                          nig.VaR, nor.VaR)))
lines(seq(along = p), ghd.VaR, col = "red")
lines(seq(along = p), hyp.VaR, col = "blue")
lines(seq(along = p), nig.VaR, col = "green")
lines(seq(along = p), nor.VaR, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)


# Risk assessment using GHD
# ES calculation
ghd.ES <- abs(ESghyp(p, ghdfit))
hyp.ES <- abs(ESghyp(p, hypfit))
nig.ES <- abs(ESghyp(p, nigfit))
nor.ES <- abs(mean(ExxonTimeS, na.rm=TRUE) - sd(c(ExxonTimeS[, 1]), na.rm = TRUE) *
                dnorm(qnorm(1 - p)) / p)
obs.p <- ceiling(p * length(ExxonTimeS))
emp.ES <- sapply(obs.p, function(x) abs(mean(sort(c(ExxonTimeS))[1:x])))


# Plot the ES
plot(emp.ES, type = "l", xlab = "", ylab = "ES", axes = FALSE,
     ylim = range(c(hyp.ES, nig.ES, ghd.ES, nor.ES, emp.ES), na.rm = TRUE))
box()
axis(1, at = 1:length(p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.ES, ghd.ES, hyp.ES, nig.ES, nor.ES)))
lines(1:length(p), ghd.ES, col = "red")
lines(1:length(p), hyp.ES, col = "blue")
lines(1:length(p), nig.ES, col = "green")
lines(1:length(p), nor.ES, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)

#===========================End of Exxon Mobil - Distributions, VaR and ES 


#===========================================================================

#======

#Portfolio OPtimization

##Large-Cap Equity ETF Data
## AMGN.Ret
## Dec 2010 NA
## Jan 2011 0.63616531
## Feb 2011 -0.05134835
## Dec 2013 0.01764962

library("xts")
library("ghyp")
library(fBasics)
library("FRAPO")
library(dplyr)
library(xts)
library(quantmod)
library(zoo)
library(DELT)   

data.AMGN <- read.csv ("AMGN.csv", header = TRUE)
date <-as.Date(data.AMGN$Date,format="%Y-%m-%d")
data.AMGN <- cbind(date, data.AMGN[,-1])
data.AMGN <- data.AMGN[order(data.AMGN$date),]
data.AMGN <- xts(data.AMGN[,2:7],order.by=data.AMGN[,1])
names(data.AMGN) <- paste(c("AMGN.Open","AMGN.High","AMGN.Low","AMGN.Close","AMGN.Volume","AMGN.Adjusted"))
AMGN.monthly <- to.monthly(data.AMGN)
AMGN.monthly <- AMGN.monthly[,6]
AMGN.ret <- Delt(AMGN.monthly$data.AMGN.Adjusted)
names(AMGN.ret) <- paste("AMGN.Ret")
AMGN.ret[c(1:3,nrow(AMGN.ret)),]

#JNJregate Bond Market ETF Data
## JNJ.Ret
## Dec 2010 NA
## Jan 2011 2.0665337
## Feb 2011 -0.6699970
## Dec 2013 0.2822791
data.JNJ <- read.csv ("JNJ.csv", header = TRUE)
date <-as.Date(data.JNJ$Date,format="%Y-%m-%d")
data.JNJ <- cbind(date, data.JNJ[,-1])
data.JNJ <- data.JNJ[order(data.JNJ$date),]
data.JNJ <- xts(data.JNJ[,2:7],order.by=data.JNJ[,1])
names(data.JNJ) <- paste(c("JNJ.Open","JNJ.High","JNJ.Low","JNJ.Close","JNJ.Volume","JNJ.Adjusted"))
JNJ.monthly <- to.monthly(data.JNJ)
JNJ.monthly <- JNJ.monthly[,6]
JNJ.ret <- Delt(JNJ.monthly$data.JNJ.Adjusted)
names(JNJ.ret) <- paste("JNJ.Ret")
JNJ.ret[c(1:3,nrow(JNJ.ret)),]


AMGN <- read.csv("AMGN.csv",header=TRUE)
date <- as.Date(AMGN$Date,format="%Y-%m-%d")
AMGN <- cbind(date,AMGN[,-1])
AMGN <- AMGN[order(AMGN$date),]
AMGN <- xts(AMGN[,2:7],order.by=AMGN[,1])
names(AMGN) <- paste(c("AMGN.Open","AMGN.High","AMGN.Low","AMGN.Close","AMGN.Volume","AMGN.Adjusted"))
AMGN.monthly <- to.monthly(AMGN)
AMGN.ret <- Delt(AMGN.monthly$AMGN.Adjusted)
names(AMGN.ret) <- paste("AMGN.Ret")
AMGN.ret[c(1:3,nrow(AMGN.ret)),]


JNJ <- read.csv("JNJ.csv",header=TRUE)
date <- as.Date(JNJ$Date,format="%Y-%m-%d")
JNJ <- cbind(date,JNJ[,-1])
JNJ <- JNJ[order(JNJ$date),]
JNJ <- xts(JNJ[,2:7],order.by=JNJ[,1])
names(JNJ) <- paste(c("JNJ.Open","JNJ.High","JNJ.Low","JNJ.Close","JNJ.Volume","JNJ.Adjusted"))
JNJ.monthly <- to.monthly(JNJ)
JNJ.ret <- Delt(JNJ.monthly$JNJ.Adjusted)
names(JNJ.ret) <- paste("JNJ.Ret")
JNJ.ret[c(1:3,nrow(JNJ.ret)),]


CSCO <- read.csv("CSCO.csv",header=TRUE)
date <- as.Date(CSCO$Date,format="%Y-%m-%d")
CSCO <- cbind(date,CSCO[,-1])
CSCO <- CSCO[order(CSCO$date),]
CSCO <- xts(CSCO[,2:7],order.by=CSCO[,1])
names(CSCO) <- paste(c("CSCO.Open","CSCO.High","CSCO.Low","CSCO.Close","CSCO.Volume","CSCO.Adjusted"))
CSCO.monthly <- to.monthly(CSCO)
CSCO.ret <- Delt(CSCO.monthly$CSCO.Adjusted)
names(CSCO.ret) <- paste("CSCO.Ret")
CSCO.ret[c(1:3,nrow(CSCO.ret)),]


MSFT <- read.csv("MSFT.csv",header=TRUE)
date <- as.Date(MSFT$Date,format="%Y-%m-%d")
MSFT <- cbind(date,MSFT[,-1])
MSFT <- MSFT[order(MSFT$date),]
MSFT <- xts(MSFT[,2:7],order.by=MSFT[,1])
names(MSFT) <- paste(c("MSFT.Open","MSFT.High","MSFT.Low","MSFT.Close","MSFT.Volume","MSFT.Adjusted"))
MSFT.monthly <- to.monthly(MSFT)
MSFT.ret <- Delt(MSFT.monthly$MSFT.Adjusted)
names(MSFT.ret) <- paste("MSFT.Ret")
MSFT.ret[c(1:3,nrow(MSFT.ret)),]

CVX <- read.csv("CVX.csv",header=TRUE)
date <- as.Date(CVX$Date,format="%Y-%m-%d")
CVX <- cbind(date,CVX[,-1])
CVX <- CVX[order(CVX$date),]
CVX <- xts(CVX[,2:7],order.by=CVX[,1])
names(CVX) <- paste(c("CVX.Open","CVX.High","CVX.Low","CVX.Close","CVX.Volume","CVX.Adjusted"))
CVX.monthly <- to.monthly(CVX)
CVX.ret <- Delt(CVX.monthly$CVX.Adjusted)
names(CVX.ret) <- paste("CVX.Ret")
CVX.ret[c(1:3,nrow(CVX.ret)),]


XOM <- read.csv("XOM.csv",header=TRUE)
date <- as.Date(XOM$Date,format="%Y-%m-%d")
XOM <- cbind(date,XOM[,-1])
XOM <- XOM[order(XOM$date),]
XOM <- xts(XOM[,2:7],order.by=XOM[,1])
names(XOM) <- paste(c("XOM.Open","XOM.High","XOM.Low","XOM.Close","XOM.Volume","XOM.Adjusted"))
XOM.monthly <- to.monthly(XOM)
XOM.ret <- Delt(XOM.monthly$XOM.Adjusted)
names(XOM.ret) <- paste("XOM.Ret")
XOM.ret[c(1:3,nrow(XOM.ret)),]

# Combine Monthly Return Data for
# AMGN and SCHZ
# Combine the two returns data into one object name Ret.monthly
# Use digits=3 option to reduce the number of decimals on display.
## AMGN.Ret JNJ.Ret
## Jan 2011 0.6362 2.067
## Feb 2011 -0.0513 -0.670
## Mar 2011 -0.0639 0.817
## Dec 2013 0.0176 0.282
options(digits=3)
Ret.monthly <- cbind(AMGN.ret[-1,],JNJ.ret[-1])
Ret.monthly[c(1:3,nrow(Ret.monthly)),]

# Calculate the Mean and Standard
# Deviation of AMGN and JNJ Returns
AMGN.Avg <- mean(Ret.monthly$AMGN.Ret)
AMGN.Avg
AMGN.sd <- sd(Ret.monthly$AMGN.Ret)
AMGN.sd
JNJ.Avg <- mean(Ret.monthly$JNJ.Ret)
JNJ.Avg
JNJ.sd <- sd(Ret.monthly$JNJ.Ret)
JNJ.sd

CSCO.Avg <- mean(Ret.monthly$CSCO.Ret)
CSCO.Avg
CSCO.sd <- sd(Ret.monthly$CSCO.Ret)
CSCO.sd
MSFT.Avg <- mean(Ret.monthly$MSFT.Ret)
MSFT.Avg
MSFT.sd <- sd(Ret.monthly$MSFT.Ret)
MSFT.sd

CVX.Avg <- mean(Ret.monthly$CVX.Ret)
CVX.Avg
CVX.sd <- sd(Ret.monthly$CVX.Ret)
CVX.sd
XOM.Avg <- mean(Ret.monthly$XOM.Ret)
XOM.Avg
XOM.sd <- sd(Ret.monthly$XOM.Ret)
XOM.sd



## JNJ.Ret
## AMGN.Ret
# Note that the output of the cov command has labels that may end up to
# be more misleading than helpful. To avoid confusion, clean up the covar
# result and make it a single number with no labels. In addition, use
# scipen=100 option in order for the value not to be reported in scientific
# notation.
## [1] 0.106
covar <- cov(Ret.monthly$AMGN.Ret, Ret.monthly$JNJ.Ret)

covar
options(scipen = 100)
covar <- covar[1,1]
covar

# Create a Series of Weights for AMGN
# Stock
# To find all the possible combinations of AMGN and JNJ stock, create a
# series with all possible weight combinations. Weights are restricted from
# 100% AMGN / 0% JNJ to 0% AMGN/ 100% JNJ.
# Since the weights must sum to one, create a series of weights for AMGN
# between 0 and 100% by 1% increments. Note that the number of
# increments is a trade-off between speed and accuracy. Using 1% would be
# sufficiently accurate for this example and would not affect the speed
#substantially.
## [1] 0.00 0.01 0.02 0.03 0.04
## [1] 0.95 0.96 0.97 0.98 0.99 1.00
Portfolio <- data.frame(seq(0,1,by=.01))
names(Portfolio) <- paste("AMGN.wgt")
Portfolio[1:5,]
Portfolio[(nrow(Portfolio)-5):nrow(Portfolio),]

# 
# Create a Series of Weights for JNJ
# Stock
# Since there are only two stock and they must add to one, the JNJ
# weights are created by substracting one from the AMGN weights.
## AMGN.wgt JNJ.wgt
## 1 0.00 1.00
## 2 0.01 0.99
## 3 0.02 0.98
## 101 1.00 0.00
Portfolio$JNJ.wgt <- 1-Portfolio$AMGN.wgt
Portfolio[c(1:3,nrow(Portfolio)),]



# 
# The portfolio return is the weighted average of the returns of the
# securities in the portfolio.
## AMGN.wgt JNJ.wgt PortRet
## 1 0.00 1.00 0.2356
## 2 0.01 0.99 0.2338
## 3 0.02 0.98 0.2320
## 101 1.00 0.00 0.0569

Portfolio$PortRet <- Portfolio$AMGN.wgt*AMGN.Avg+Portfolio$JNJ.wgt*JNJ.Avg
Portfolio[c(1:3,nrow(Portfolio)),]

# Caclulate the Portfolio Weight for
# Each Weight Combination
## AMGN.wgt JNJ.wgt PortRet PortRisk
## 1 0.00 1.00 0.2356 0.799
## 2 0.01 0.99 0.2338 0.792
## 3 0.02 0.98 0.2320 0.786
## 101 1.00 0.00 0.0569 0.364
Portfolio$PortRisk <- sqrt((Portfolio$AMGN.wgt^2*AMGN.sd^2)+(Portfolio$JNJ.wgt^2*JNJ.sd^2)+
                             (2*covar*Portfolio$AMGN.wgt*Portfolio$JNJ.wgt))
Portfolio[c(1:3,nrow(Portfolio)),]

# Identify the Tangency Portfolio

# The tangency portfolio is the portfolio that yields the highest Sharpe
# Ratio.
# The Sharpe Ratio requires a risk-free rate and we use the 3-Month
# Constant Maturity Treasury as a proxy for that rate. Since the yield as of
# December 31, 2013 of 0.07% is an annual yield, we convert this to a
# monthly yield by dividing the rate by 12.

minvar.port <- subset(Portfolio,Portfolio$PortRisk==min(Portfolio$PortRisk))
minvar.port


## AMGN.wgt JNJ.wgt PortRet PortRisk Sharpe
## 1 0.00 1.00 0.2356 0.799 0.295
## 2 0.01 0.99 0.2338 0.792 0.295
## 3 0.02 0.98 0.2320 0.786 0.295
## 101 1.00 0.00 0.0569 0.364 0.156
# To identify the portfolio with the highest Sharpe Ratio, we use a
# combination of the subset command and the max command. The output
# below shows that the tangency portfolio is attained if we invest
## AMGN.wgt JNJ.wgt PortRet PortRisk Sharpe
## 32 0.31 0.69 0.18 0.602 0.299

riskfree = .0007/12
Portfolio$Sharpe <- (Portfolio$PortRet-riskfree)/Portfolio$PortRisk
Portfolio[c(1:3,nrow(Portfolio)),]
tangency.port <- subset(Portfolio,Portfolio$Sharpe==max(Portfolio$Sharpe))
tangency.port

# Identify Efficient Portfolios
# Not all combinations of AMGN and JNJ can be classified as efficient
# portfolios. Efficient portfolios are only those portfolios that yield the
# highest return for a given level of risk. Extract these efficient portfolios by
# using the subset command and only keep those portfolios with returns
# greater than the return of the minimum variance portfolio.

## AMGN.wgt JNJ.wgt PortRet PortRisk Sharpe
## 1 0.00 1.00 0.2356 0.799 0.295
## 2 0.01 0.99 0.2338 0.792 0.295
## 3 0.02 0.98 0.2320 0.786 0.295
## 96 0.95 0.05 0.0659 0.362 0.182
eff.frontier <- subset(Portfolio,Portfolio$PortRet >= minvar.port$PortRet)
eff.frontier[c(1:3,nrow(eff.frontier)),]


# Plot the MV Efficient Frontier
# Visualize the MV Efficient Frontier using the plot command. Plot in gray
# circles all the portfolios formed from all combinations of AMGN and JNJ.
# Add a solid triangular point using the points command and pch=17 option
# to denote the minimum variance portfolio.
# The cex=3 options increases the size of the triangle, to make it easier to
# see the minimum variance portfolio on the frontier.
# Add a solid circle using the points command and pch=19 option to denote
# the tangency portfolio. Also use the cex=3 option to increase the size of
# the solid circle to facilitate the identification of the tangency portfolio on
# the frontier.
# Plot the efficient frontier using the points command and color the circles
# black using the col = "black" option.
plot (x=Portfolio$PortRisk,
      xlab="Portfolio Risk",
      y=Portfolio$PortRet,
      ylab = "Portfolio Return",
      col="gray40",
      main='Mean-Variance Efficient Frontier of the Two Assets Based on the "Long Way"')
abline(h=0,lty=1)
points(x=minvar.port$PortRisk,y=minvar.port$PortRet,pch=17,cex=3)
points(x=tangency.port$PortRisk,y=tangency.port$PortRet,pch=19,cex=3)
points(x=eff.frontier$PortRisk,y=eff.frontier$PortRet)


# Two-Assets Using Quadratic
# Programming
# The "long Way" is feasible for two assets but most portfolios have more
# than two assets. Given N securities portfolio, the variancecovariancematrix
# would have N(N+1)/2 elements with N variance terms
# and N(N-1)/2 covariance terms.
# This approach is called quadratic programming because the objective is to
# use a quadratic function for optimization. A quadratic function includes a
# variable (in this case weight) that is squared.
# Quadratic programming finds the combination of securities that yield the
# minimim risk for a specific level of return.


# Import Returns Data and Convert it
# Into a Matrix
# The data is available from the previous example. Just check that the
# Ret.monthly data object is still in the R memory.
# Remane the column headers of the matrix in order to know the first
# column are AMGN returns and the second are JNJ returns.
## AMGN.Ret JNJ.Ret
## Jan 2011 0.6362 2.067
## Feb 2011 -0.0513 -0.670
## Mar 2011 -0.0639 0.817
## Dec 2013 0.0176 0.282

# Convert the returns data into a matrix using the matrix command. The
# matrix command takes on two arguments. The first argument is tha data
# object that will be converted and the second argument is how many rows
# should the matrix have.

## [,1] [,2]
## [1,] 0.6362 2.067
## [2,] -0.0513 -0.670
## [3,] -0.0639 0.817
# Remane the column headers of the matrix in order to know the first
# column are AMGN returns and the second are JNJ returns.
## AMGN JNJ
## [1,] 0.6362 2.067
## [2,] -0.0513 -0.670
## [3,] -0.0639 0.817
## [4,] -0.1316 -0.508
## [5,] 0.4313 1.099
## [6,] 0.3557 0.237
options(digits=3)
Ret.monthly <- cbind(CSCO.ret[-1,],MSFT.ret[-1], CVX.ret[-1], XOM.ret[-1])
Ret.monthly[c(1:3,nrow(Ret.monthly)),]

Ret.monthly[c(1:3,nrow(Ret.monthly)),]
mat.ret <- matrix(Ret.monthly,nrow(Ret.monthly))
mat.ret[1:3,]
colnames(mat.ret) <- c("CSCO","MSFT","CVX","XOM")
head(mat.ret)


# Calculate Variance-Covariance
# (VCOV) Matrix of Returns
# Applying the cov command to a matrix creates a VCOV matrix. The
# terms on the main diagonal are the variance terms. The numbers oof the
# main diagonal are the covariance terms.
# ## AMGN JNJ
## AMGN 0.133 0.106
## JNJ 0.106 0.638

VCOV <- cov(mat.ret)
VCOV

# Construct the Target Portfolio
# Return Vector
# The first step is to find the average return for AMGN and JNJ over the
# relevant time period.
## Avg.Ret
## AMGN 0.0569
## JNJ 0.2356
# Set the smaller of the average returns as the minimum return min.ret and
# the larger of the average returns as the maximum return max.ret.
# ## [1] 0.0569
# ## [1] 0.236
# Create a sequence that begins with min.ret and ends with max.ret with
# 100 increments in between.
## [1] 0.0569 0.0587 0.0605 0.0624 0.0642 0.0660
## [1] 0.227 0.228 0.230 0.232 0.234 0.236
avg.ret <- matrix(apply(mat.ret,2,mean))
colnames(avg.ret) <- paste("Avg.Ret")
rownames(avg.ret) <- paste(c("AMGN","JNJ"))
avg.ret
min.ret <- min(avg.ret)
min.ret
max.ret <- max(avg.ret)
max.ret
increments = 100
tgt.ret <- seq(min.ret,max.ret,length=increments)
head(tgt.ret)
tail(tgt.ret)

# 
# Construct Dummy Portfolio
# Standard Deviation Vector
# An output of the quadratic program are portfolio standard deviations.
# However, the output needs to overwrite the values of an existing vector
# of standard deviations after each iteration. Since the number of iterations
# equal the number of increments, we create a series of 100 zeroes for the
# dummy portfolio standard deviation vector.
## [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [38] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [75] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

tgt.sd <- rep(0,length=increments)
tgt.sd

# Construct Dummy Portfolio Weights
# Vector
# Similarly, an output of the quadratic programming are portfolio weights.
# However, the output needs to overwrite the values of an existing vector
# of weigths after each iteration. Since the number of iterations equals the
# number of increments, create a series of 100 zeroes for the dummy
# portfolio weights vector.
## [,1] [,2]
## [1,] 0 0
## [2,] 0 0
## [3,] 0 0
## [4,] 0 0
## [5,] 0 0
## [6,] 0 0
## [,1] [,2]
## [95,] 0 0
## [96,] 0 0
## [97,] 0 0
## [98,] 0 0
## [99,] 0 0
## [100,] 0 0

wgt <- matrix(0,nrow=increments,ncol=length(avg.ret))
head(wgt)
tail(wgt)

# Run the quadprog Optimizer
# To run the optimizer, use the solve.QP function in the quadprog package.
# The optimizer loops through the calculations 100 times (increments).
# During each iteration, the optimizer minimizes portfolio risk subject to
# three constrains:
#   the weights in the portfolio must equal to one
# the portfolio return has to equal a target return
# the weights for each security has to be greater than or equal to
# zero
# The output of the optimizer for the portfolio standard deviation is below.
# It corresponds to each target portfolio return level constructed.
## [1] 0.364 0.363 0.363 0.363 0.362 0.362
## [1] 0.766 0.772 0.779 0.786 0.792 0.799
# These are the weights of AMGN and JNJ for each target return level.
# The output of the optimizer for the portfolio standard deviation is below.
# It corresponds to each target portfolio return level constructed.
## [1] 0.364 0.363 0.363 0.363 0.362 0.362
## [1] 0.766 0.772 0.779 0.786 0.792 0.799
# These are the weights of AMGN and JNJ for each target return level.
install.packages("quadprog")
library(quadprog)
for (i in 1:increments) {
  Dmat <- 2*VCOV
  dvec <- c(rep(0,length(avg.ret)))
  Amat <- cbind(rep(1,length(avg.ret)),avg.ret,
                diag(1,nrow=ncol(Ret.monthly)))
  bvec <- c(1,tgt.ret[i],rep(0,ncol(Ret.monthly)))
  soln <- solve.QP(Dmat,dvec,Amat,bvec=bvec,meq=2)
  tgt.sd[i] <- sqrt(soln$value)
  wgt[i,] <- soln$solution}
head(tgt.sd)
tail(tgt.sd)
options(scipen=100)
head(wgt)


# Overwrite the first entries, thaty are not zero due to a calculation error.
# Also rename the column headers.
## wgt.AMGN wgt.JNJ
## [1,] 0.000 0.0000000000000000347
## [2,] 0.990 0.0101010101010101383
## [3,] 0.980 0.0202020202020202419
## [4,] 0.970 0.0303030303030303490
## [5,] 0.960 0.0404040404040404560
## [6,] 0.949 0.0505050505050505180
## wgt.AMGN wgt.JNJ
## [95,] 0.0505 0.949
## [96,] 0.0404 0.960
## [97,] 0.0303 0.970
## [98,] 0.0202 0.980
## [99,] 0.0101 0.990
## [100,] 0.0000 0.000

colnames(wgt) <- paste(c("wgt.CSCO","wgt.MSFT","wgt.CVX","wgt.XOM"))
wgt[1,1] <- 0
wgt[nrow(wgt),2] <- 0
head(wgt)
tail(wgt)

# Combine Portfolio Returns, Portfolio
# Standard Deviations, and Portfolio
# Weights
## tgt.ret tgt.sd wgt.AMGN wgt.JNJ
## 1 0.0569 0.364 0.000 0.0000000000000000347
## 2 0.0587 0.363 0.990 0.0101010101010101383
## 3 0.0605 0.363 0.980 0.0202020202020202419
## 4 0.0624 0.363 0.970 0.0303030303030303490
## 5 0.0642 0.362 0.960 0.0404040404040404560
## 6 0.0660 0.362 0.949 0.0505050505050505180


tgt.port <- data.frame(cbind(tgt.ret,tgt.sd,wgt))
head(tgt.port)

# Identify the Minimum Variance
# Portfolio
# The minimum variance portfolio has the smallest risk. To identify the
# portfolio, use a combination of the subset command and min command.
## tgt.ret tgt.sd wgt.AMGN wgt.JNJ
## 6 0.066 0.362 0.949 0.0505

minvar.port <- subset(tgt.port,tgt.port$tgt.sd==min(tgt.port$tgt.sd))
minvar.port


# Identify the Tangency portfolio
# The tangency portfolio is the portfolio that yields the highest Sharpe
# Ratio. The Sharpe Ratio requires a risk-free rate and we use the 3-Month
# Constant Maturity Treasury as a proxy for that rate. (We already
#                                                       calcualted the risk free variable)
## [1] 0.0000583
## tgt.ret tgt.sd wgt.AMGN wgt.JNJ Sharpe
## 1 0.0569 0.364 0.000 0.0000000000000000347 0.156
## 2 0.0587 0.363 0.990 0.0101010101010101383 0.161
## 3 0.0605 0.363 0.980 0.0202020202020202419 0.167
## 4 0.0624 0.363 0.970 0.0303030303030303490 0.172
## 5 0.0642 0.362 0.960 0.0404040404040404560 0.177
## 6 0.0660 0.362 0.949 0.0505050505050505180 0.182
# Indentify the portfolio with the highest Sharpe Ratio using a combination
# of subset and max commands.
# ## tgt.ret tgt.sd wgt.AMGN wgt.JNJ Sharpe
## 69 0.18 0.6 0.313 0.687 0.299
riskfree
tgt.port$Sharpe <- (tgt.port$tgt.ret-riskfree)/tgt.port$tgt.sd
head(tgt.port)
tangency.port <- subset(tgt.port,tgt.port$Sharpe==max(tgt.port$Sharpe))
tangency.port

# Identify Efficient Portfolios
# Not all combinations of AMGN and JNJ ca be classified as efficient
# portfolios. Efficient portfolios are those that yield the highest return for a
# given level of risk. Extract these efficient portfolios by using the subset
# command and only keep those portfolios with returns greater than the
# return of the minimum variance portfolio.
## tgt.ret tgt.sd wgt.AMGN wgt.JNJ Sharpe
## 6 0.0660 0.362 0.949 0.0505 0.182
## 7 0.0678 0.363 0.939 0.0606 0.187
## 8 0.0696 0.363 0.929 0.0707 0.192
## 100 0.2356 0.799 0.000 0.0000 0.295

eff.frontier <- subset(tgt.port,tgt.port$tgt.ret>=minvar.port$tgt.ret)
eff.frontier[c(1:3,nrow(eff.frontier)),]

# Multiple Assets Using Quadratic
# Programming
# Import the data (continue using AMGN and JNJ, add AMZN and AAPL)
## AMZN.Ret
## Dec 2010 NA
## Jan 2011 0.94620
## Feb 2011 0.00969
## Dec 2013 0.03371
## AAPL.Ret
## Dec 2010 NA
## Jan 2011 0.9495
## Feb 2011 0.0685
## Dec 2013 -0.2027


# Multiple Assets Using Quadratic
# Programming
# Import the data (continue using AMGN and JNJ, add CSCO and MSFT)
## CSCO.Ret
## Dec 2010 NA
## Jan 2011 0.94620
## Feb 2011 0.00969
## Dec 2013 0.03371
## MSFT.Ret
## Dec 2010 NA
## Jan 2011 0.9495
## Feb 2011 0.0685
## Dec 2013 -0.2027


# Calculate Variance-Covariance
# (VCOV) Matrix of Returns
## AMGN JNJ CSCO APPL
## AMGN 0.13260 0.1061 0.00729 0.0545
## JNJ 0.10614 0.6384 0.13199 0.0264
## CSCO 0.00729 0.1320 0.31306 0.0892
## APPL 0.05449 0.0264 0.08925 0.4336

Ret.monthly <- cbind(AMGN.ret[-1,],JNJ.ret[-1,], CSCO.ret[-1], MSFT.ret[-1])
Ret.monthly[c(1:3,nrow(Ret.monthly)),]

Ret.monthly[c(1:3,nrow(Ret.monthly)),]
mat.ret <- matrix(Ret.monthly,nrow(Ret.monthly))
mat.ret[1:3,]
colnames(mat.ret) <- c("AMGN","JNJ","CSCO","MSFT")
head(mat.ret)


# Calculate Variance-Covariance
# (VCOV) Matrix of Returns
# Applying the cov command to a matrix creates a VCOV matrix. The
# terms on the main diagonal are the variance terms. The numbers oof the
# main diagonal are the covariance terms.
# ## AMGN JNJ
## AMGN 0.133 0.106
## JNJ 0.106 0.638

VCOV <- cov(mat.ret)
VCOV



# Construct the Target Portfolio
# Return Vector
# Calculate the average return of each security in the portfolio. Use the
# apply command witha second argument of two to tell R to apply the
# function mean to each column. This returns a column vector, where each
# element is the average monthly return for the security.
# Rename the row names and column names to make describing the output
# easier.
# ## Avg.Ret
## AMGN 0.0569
## JNJ 0.2356
## CSCO 0.1054
## MSFT 0.1434

avg.ret <- matrix(apply(mat.ret,2,mean))
rownames(avg.ret) <- c("AMGN","JNJ","CSCO","MSFT")
colnames(avg.ret) <- c("Avg.Ret")
avg.ret


# Construct the Target Portfolio
# Return Vector
# The next step is to define the bounds of the target vector. The smallest
# possible return is the minimum average return amonng the four securities
# in the portfolio and the same idea for the largest return.
# It is not possible to do worse than investing 100% in the security with the
# smalles return and no better than investing 100% in the security with the
# largest return.
# ## [1] 0.0997
# ## [1] 0.151
min.ret <- min(avg.ret)
min.ret
max.ret <- max(avg.ret)
max.ret


# Construct the Target Portfolio
# Return Vector
# Use 100 increments between the minimum and maximum returns to
# generate the target returns. For each run of the optimizer, the optimizer
# finds the combination of weights of the four securities that generates the
# target return with the lowest risk.
## [1] 0.0569 0.0587 0.0605 0.0624 0.0642 0.0660

increments=100
tgt.ret <- seq(min.ret,max.ret,length=increments)
head(tgt.ret)



# Construct Dummy Portfolio Weights Vector
# Placeholder for 100 observations in order for the optimizer to overwrite
# the entries with the appropriate standard deviation after each loop.
## [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [38] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [75] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

tgt.sd <- rep(0,length=increments)
tgt.sd


# Consdtruct Dummy Portfolio
# Weights Vector
# The optimizer also outputs portfolio weights and needs to overwrite a
# placeholder for those weights.
## [,1] [,2] [,3] [,4]
## [1,] 0 0 0 0
## [2,] 0 0 0 0
## [3,] 0 0 0 0
## [4,] 0 0 0 0
## [5,] 0 0 0 0
## [6,] 0 0 0 0

wgt <- matrix(0,nrow=increments,ncol=length(avg.ret))
head(wgt)


# Run the quadrpog Optimizer

for (i in 1:increments){
  Dmat <- 2*VCOV
  dvec <- c(rep(0,length(avg.ret)))
  Amat <- cbind(rep(1,length(avg.ret)),avg.ret,
                diag(1,nrow=ncol(Ret.monthly)))
  bvec <- c(1,tgt.ret[i],rep(0,ncol(Ret.monthly)))
  soln <- solve.QP(Dmat,dvec,Amat,bvec=bvec,meq=2)
  tgt.sd[i] <- sqrt(soln$value)
  wgt[i,] <- soln$solution
}
head(tgt.sd)

head(wgt)

## [1] 0.364 0.352 0.341 0.332 0.324 0.318
## [,1] [,2] [,3] [,4]
## [1,] 1.000 0.00000000000000002037 -0.000000000000000167 0.00000000000000000000
## [2,] 0.963 0.00000000000000001720 0.037246742648141234 0.00000000000000000000
## [3,] 0.926 0.00000000000000001402 0.074493485296282663 0.00000000000000000000
## [4,] 0.888 0.00000000000000001084 0.111740227944424161 0.00000000000000000694
## [5,] 0.851 0.00000000000000000767 0.148986970592565604 0.00000000000000000000
## [6,] 0.814 0.00000000000000002239 0.186233713240707005 0.00000000000000000000

# Some observations might look like they have negative weights, those
# values are very small and considered zero.
colnames(wgt) <- c("wgt.AMGN","wgt.JNJ","wgt.CSCO","wgt.MSFT")
head(wgt)
CHECK <- rowSums(wgt)
CHECK

## wgt.AMGN wgt.JNJ wgt.CSCO
## [1,] 1.000 0.00000000000000002037 -0.000000000000000167
## [2,] 0.963 0.00000000000000001720 0.037246742648141234
## [3,] 0.926 0.00000000000000001402 0.074493485296282663
## [4,] 0.888 0.00000000000000001084 0.111740227944424161
## [5,] 0.851 0.00000000000000000767 0.148986970592565604
## [6,] 0.814 0.00000000000000002239 0.186233713240707005
## wgt.MSFT
## [1,] 0.00000000000000000000
## [2,] 0.00000000000000000000
## [3,] 0.00000000000000000000
## [4,] 0.00000000000000000694
## [5,] 0.00000000000000000000
## [6,] 0.00000000000000000000


# Combine Portfolio Returns, Portfolio
# Standard Deviations, and Portfolio
# Weights
# ## tgt.ret tgt.sd wgt.AMGN wgt.JNJ wgt.CSCO
## 1 0.0569 0.364 1.000 0.00000000000000002037 -0.000000000000000167
## 2 0.0587 0.352 0.963 0.00000000000000001720 0.037246742648141234
## 3 0.0605 0.341 0.926 0.00000000000000001402 0.074493485296282663
## 4 0.0624 0.332 0.888 0.00000000000000001084 0.111740227944424161
## 5 0.0642 0.324 0.851 0.00000000000000000767 0.148986970592565604
## 6 0.0660 0.318 0.814 0.00000000000000002239 0.186233713240707005
## wgt.MSFT
## 1 0.00000000000000000000
## 2 0.00000000000000000000
## 3 0.00000000000000000000
## 4 0.00000000000000000694
## 5 0.00000000000000000000
## 6 0.00000000000000000000

tgt.port <- data.frame(cbind(tgt.ret,tgt.sd,wgt))
head(tgt.port)

# Identify the Minimum Variance
# Portfolio
## tgt.ret tgt.sd wgt.AMGN wgt.JNJ wgt.CSCO wgt.MSFT
## 12 0.0768 0.306 0.655 -0.000000000000000000459 0.262 0.0827

minvar.port <- subset(tgt.port,tgt.port$tgt.sd==min(tgt.port$tgt.sd))
minvar.port

# Identify Tangency Portfolio
# Check that the riskfree variable is still in R's memory. Then calculate the
# Sharpe Ratio for each of the portfolios constructed.
## [1] 0.0000583
## tgt.ret tgt.sd wgt.AMGN wgt.JNJ wgt.CSCO
## 1 0.0569 0.364 1.000 0.00000000000000002037 -0.000000000000000167
## 2 0.0587 0.352 0.963 0.00000000000000001720 0.037246742648141234
## 3 0.0605 0.341 0.926 0.00000000000000001402 0.074493485296282663
## 4 0.0624 0.332 0.888 0.00000000000000001084 0.111740227944424161
## 5 0.0642 0.324 0.851 0.00000000000000000767 0.148986970592565604
## 6 0.0660 0.318 0.814 0.00000000000000002239 0.186233713240707005
## wgt.MSFT Sharpe
## 1 0.00000000000000000000 0.156
## 2 0.00000000000000000000 0.167
## 3 0.00000000000000000000 0.177
## 4 0.00000000000000000694 0.188
## 5 0.00000000000000000000 0.198
# ## 6 0.00000000000000000000 0.207
# Identify the portfolio with the highest Sharpe Ratio and that is the
# tangency portfolio.
## tgt.ret tgt.sd wgt.AMGN wgt.JNJ wgt.CSCO wgt.MSFT Sharpe
## 64 0.171 0.47 0.0644 0.419 0.154 0.362 0.363

riskfree

tgt.port$Sharpe <- (tgt.port$tgt.ret-riskfree)/tgt.port$tgt.sd
head(tgt.port)

tangency.port <- subset(tgt.port,tgt.port$Sharpe==max(tgt.port$Sharpe))
tangency.port


#Identify Efficient Portfolios
#Portfolios that have returns higher than the return of the minimum
#variance portfolio.
## tgt.ret tgt.sd wgt.AMGN wgt.JNJ wgt.CSCO
## 12 0.0768 0.306 0.655 -0.000000000000000000459 0.262
## 13 0.0786 0.306 0.638 0.003763846816500298308 0.264
## 14 0.0804 0.307 0.626 0.011907344204225161349 0.262
## 100 0.2356 0.799 0.000 0.999999999999999777955 0.000
## wgt.MSFT Sharpe
## 12 0.082651327122755830 0.251
## 13 0.094769109101700597 0.256
## 14 0.100016485016852175 0.262
## 100 0.000000000000000333 0.295

eff.frontier <- subset(tgt.port,tgt.port$tgt.ret>=minvar.port$tgt.ret)
eff.frontier[c(1:3,nrow(eff.frontier)),]

#Plot the MV Efficient Frontier
plot(x=tgt.sd,
     y=tgt.ret,
     col="gray40",
     xlab="Portfolio Risk",
     ylab="Portfolio Return",
     main="Mean-Variance Efficient Frontier of Four Assets Based on the Quadratic Programming Approach")
abline(h=0,lty=1)
points(x=minvar.port$tgt.sd,y=minvar.port$tgt.ret,pch=17,cex=3)
points(x=tangency.port$tgt.sd,y=tangency.port$tgt.ret,pch=18,cex=3)
points(x=eff.frontier$tgt.sd,y=eff.frontier$tgt.ret)



#End of Project========================================================================================







