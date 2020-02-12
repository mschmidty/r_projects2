library(tidyverse)
library(quantmod)
library(lubridate)
library(TTR)
library(RcppRoll)
library(caret)



#' Calculates daily indicators for given stock
#' 
#' @param ticker a stock symbol.  Example: "TSLA". 
#' @param st_date the start of daily indicator calculations "2019-01-20". 
#' @param e_date the end of daily indicator calculations "1970-01-20". 
get_stock<-function(ticker, st_date, e_date, calc_ind=TRUE){
  
  
  
  print(ticker)
  
  ticker_data<-tryCatch(
    {
      
      ticker_data<-getSymbols(ticker, src="yahoo", auto.assign = FALSE, from=st_date, to=e_date)
      data.frame(date=index(ticker_data), coredata(ticker_data))%>%
        rename(open=2, high=3, low=4, close=5, volume=6, adjusted=7)%>%
        mutate(id = row_number())%>%
        mutate(symbol = ticker)
      
    },
    error = function(cond){
      print(paste(ticker, ":Was not able to download. Returned as Null"))
      return(NULL)
    }
  )
  
  if(!is.null(ticker_data)){
    if(calc_ind==TRUE){
      rsi<-as.data.frame(RSI(ticker_data$close))%>%
        mutate(id=ticker_data$id)%>%
        rename(rsi=1)
      
      stoch<- as.data.frame(stoch(ticker_data[,c("high", "low", "close")]))%>%
        mutate(id=ticker_data$id)
      
      wpr<-as.data.frame(WPR(ticker_data[,c("high", "low", "close")]))%>%
        mutate(id=ticker_data$id)
      
      wpr<-rename(wpr, WPR = 1)
      
      obv<-as.data.frame(OBV(ticker_data$close, ticker_data$volume))%>%
        mutate(id=ticker_data$id)%>%
        rename(obv=1)
      
      macd<-as.data.frame(MACD(ticker_data$close))%>%
        mutate(id=ticker_data$id)
      
      roc<-as.data.frame(ROC(ticker_data$close))%>%
        mutate(id=ticker_data$id)%>%
        rename(roc=1)
      
      data_joined<-ticker_data%>%
        left_join(obv, by="id")%>%
        left_join(roc, by="id")%>%
        left_join(macd, by="id")%>%
        left_join(rsi, by="id")%>%
        left_join(stoch, by="id")
      
      ind<-data_joined%>%
        ##get yesterdays close
        mutate(yesterday_close = head(c(NA, close), -1))%>%
        ## calculate the gain for the day
        mutate(gain_loss=close-yesterday_close)%>%
        
        ## calculate the percent gain/loss for the day
        mutate(percent_gain_loss=gain_loss/yesterday_close*100)%>%
        
        ## 52 week calculations
        ### - calculate the 52 week high
        mutate(max52week=roll_max(high, n=252, align="right", fill=NA, na.rm=FALSE))%>%
        ### - calculate a 1 for when the 52 week high is, all other times are 0 even NAs (very important to make NAs 0 or else the count from last high will not work.)
        mutate(max52occured=as.integer(if_else(max52week==high,1,0, 0)))%>%
        ### - the difference between the 52 week max and the close
        mutate(difPerc52max=(max52week-close)/max52week)%>%
        ### - count sequentially how many days it has been since the 52 week high. 
        mutate(panel="a")%>%
        group_by(panel, idx=cumsum(max52occured==1L))%>%
        mutate(since52weekHigh=row_number())%>%
        ungroup%>%
        select(-idx)%>%
        
        ## Calculate the 52 week min (same variables were calculated for min as max)
        mutate(min52week=roll_min(low, n=252, align="right", fill=NA, na.rm=FALSE))%>%
        mutate(min52occured=as.integer(if_else(min52week==low,1,0,0)))%>%
        mutate(difPerc52min=(close-min52week)/min52week)%>%
        group_by(panel, idx=cumsum(min52occured==1L))%>%
        mutate(since52weekLow=row_number())%>%
        ungroup%>%
        select(-idx, -panel)%>%
        
        ## calculate the classifyers used for randomForests
        ### - make 15 and 30 moving windows. 
        mutate(period_price_diff = c(tail(close, -30), rep(NA, 30)))%>%
        mutate(period_window_perc=(period_price_diff-close)/close*100)%>%
        mutate( test_variable = if_else(period_window_perc<10, 0 , 1, NA_real_ ))
      
      
      return(ind)
      
    }else{
      
      return(ticker_data)
      
    }
    
  }else{
    
    return(NULL)
    
  }
  
}



## Run random forests on stocks
stock_price_rf<-function(ticker, st_date, e_date, is_regression = FALSE, number_trees = 50){
  
  
    mtry <- 4
    stock_data<-get_stock(ticker, st_date, e_date)
    if(!is.null(stock_data)){
    
    if(is_regression == TRUE){
      
      
      
      
        ind<-stock_data%>%
          select(date, rsi:obv, since52weekHigh, difPerc52max, difPerc52min, since52weekLow, period_window_perc)%>%
          rename(test_variable = period_window_perc)%>%
          select(-roc)
        
        final_data<-head(ind, -30)%>%
          select(-date)%>%
          drop_na()
        
        predict_data<-tail(ind, 5)%>%
          select(-test_variable)%>%
          drop_na()
        
        train_control <- trainControl(	method="cv",
                                       number=5,
                                       classProbs = TRUE,
                                       savePredictions = TRUE, 
                                       sampling = "down")
        
        rf<- train(
          test_variable~ ., 
          data = final_data, 
          method="rf", 
          importance=T,
          tuneGrid = expand.grid(.mtry=11),
          trCrontrol = train_control,
          ntree=number_trees, 
          na.action = na.pass
        )
        
      }else{
        
        ind<-get_stock(ticker, st_date, e_date)%>%
          select(date, rsi:obv, since52weekHigh, difPerc52max, difPerc52min, since52weekLow, test_variable)%>%
          select(-roc) ##roc is the least predictive variable. 
        
        final_data<-head(ind, -10)%>%
          select(-date)%>%
          drop_na()
        
        predict_data<-tail(ind, 5)%>%
          select(-test_variable)%>%
          drop_na()
        
        rf<-train(
          as.factor(test_variable)~ .,
          data = final_data,
          method="rf",
          metric = "Accuracy",
          tuneGrid = expand.grid(.mtry=mtry),
          trControl = trainControl(method="repeatedcv", number=5, repeats=3, sampling="down"), 
          ntree=100
        )
      }
      
      predicted<-predict(rf, predict_data)
      predicted_df<-predict_data%>%
        mutate(prediction = predicted, ticker=ticker)%>%
        mutate(end_date = e_date)
      
      return(predicted_df)
    }else{
      return(NULL)
    }
 
}
