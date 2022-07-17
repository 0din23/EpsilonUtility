cumRet <- function (RET) {
  tmp = as.vector(RET)
  tmp[is.na(tmp)] = 0
  tmp = tmp + 1
  for (k in 2:length(tmp)) {
    tmp[k] = tmp[k] * tmp[k - 1]
  }
  tmp
}
################################################################################
# FILL TS Dates #
################################################################################
dateSplicer <- function(df, dates){
  df %>%
    full_join(.,data.frame(
      "date"=dates
    )) %>%
    arrange(as.Date(date)) %>%
    na_last_fill()
}



dateSplicer_long <- function(DF, dates){
  symbols <- DF %>% pull(symbol) %>% unique()
  lapply(1:length(symbols), function(x){

    temp <- DF %>%
      filter(symbol == symbols[x]) %>%
    temp <- dateSplicer(df=temp, dates)
    return(temp)
  }) %>%
    rbindlist()
}

################################################################################
# FILL NA with last #
################################################################################
na_last_fill <- function(df){
  for(k in 1:ncol(df)){
    for(j in 1:(nrow(df)-1)){
      if(is.na(df[j+1,k])){
        df[j+1,k] <- df[j,k]
      }
    }
  }
  df
}

## Normalize equity LIne
normEL <- function(base, el){
  if(base == 0){
    cumRet(as.numeric(Delt(el)))-1
  } else{
    cumRet(as.numeric(Delt(el)))*base
  }
}
################################################################################
# rolling Indicators
################################################################################
# Rolling Correlation ##########################################################
rollCorr <- function(bench, ts, lag){
  temp <-data.frame(
    "bench"=bench,
    "ts"=as.numeric(ts),
    "erg"=NA
  )
  for(k in lag:nrow(temp)){
    w <- c((k-(lag-1)):k)
    temp$erg[k] <- cor(temp$bench[w], temp$ts[w])
  }
  temp$erg
}
# Rolling hVol #################################################################
rollVol <- function(ts,lag, forward = F){
  # Create df
  temp <-data.frame(
    "ts"=as.numeric(ts),
    "erg"=NA
  )
  if(forward){
    for(k in 1:(nrow(temp)-lag)){
      w <- c(k:(k+lag))
      temp$erg[k] <- sqrt(sum((temp$ts[w] - mean(temp$ts[w]))^2))

    }
  }else{
    for(k in (lag+1):nrow(temp)){
      w <- c((k-lag):k)
      temp$erg[k] <- sqrt(sum((temp$ts[w] - mean(temp$ts[w]))^2))
    }
  }
  temp %>% pull(erg)
}



# Rolling Beta #################################################################
rollBeta <- function(bench, ts, lag){
  temp <-data.frame(
    "bench"=bench,
    "ts"=as.numeric(ts),
    "erg"=NA
  )
  for(k in (lag+1):nrow(temp)){
    w <- c((k-(lag-1)):k)
    temp$erg[k] <- lm(as.numeric(temp[w,1]) ~ as.numeric(temp[w,2]))$coefficients[2]
  }
  temp$erg
}

# rolling Z Score ##############################################################
rollZ <- function(ts, lag){
  temp <-data.frame(
    "ts"=as.numeric(ts),
    "erg"=NA
  )
  for(k in (lag+1):length(ts)){
    w <- k-lag
    temp$erg[k] <- (ts[k]- mean(ts[w:k], na.rm = T)) / sd(ts[w:k], na.rm = T)
  }
  temp$erg
}

## rolling Drawdowns
rollDrawdown <- function(ts,lag, forward = F){
  # Create df
  temp <-data.frame(
    "ts"=as.numeric(ts),
    "erg"=NA
  )
  if(forward){
    for(k in 1:(nrow(temp)-lag)){
      w <- c(k:(k+lag))
      temp$erg[k] <- (min(temp$ts[w]) / temp$ts[k])-1

    }
  }else{
    for(k in (lag+1):nrow(temp)){
      w <- c((k-lag):k)
      temp$erg[k] <- (min(temp$ts[w]) / temp$ts[k-lag])-1
    }
  }
  temp %>% pull(erg)
}

## Net Change
rollNetChange <- function(ts, lag, forward = FALSE){

  # Create df
  print(length(df))
  temp <-data.frame(
    "ts"=as.numeric(ts)
  )
  temp$erg <- NA
  if(forward){
    for(k in 1:(nrow(temp)-lag)){
      temp$erg[k] <- (temp$ts[k+lag] - temp$ts[k])
    }
  }else{
    for(k in (lag+1):nrow(temp)){
      temp$erg[k] <- (temp$ts[k] - temp$ts[k-lag])
    }
  }
  temp %>% pull(erg)
}

## Returns
rollReturns <- function(ts, lag, forward = FALSE){

  # Create df
  temp <-data.frame(
    "ts"=as.numeric(ts),
    "erg"=NA
  )
  if(forward){
    for(k in 1:(nrow(temp)-lag)){
      temp$erg[k] <- (temp$ts[k+lag] / temp$ts[k])-1
    }
  }else{
    for(k in (lag+1):nrow(temp)){
      temp$erg[k] <- (temp$ts[k] / temp$ts[k-lag])-1
    }
  }
  temp %>% pull(erg)
}

###############################################################################
ConstructPairSet <- function(s1, s2, from, to){

  ## Load TS
  ts_1 <- tq_get(input$PAIR_Symbol_1,
                 from=input$PAIR_From,to=input$PAIR_To)
  ts_2 <- tq_get(input$PAIR_Symbol_2,
                 from=input$PAIR_From,to=input$PAIR_To)

  ## Construct Data
  df <- data.frame(
    "date"= ts_1$date,
    "Stock_1_ret"=as.numeric(Delt(ts_1$adjusted)),
    "Stock_1_el"=cumRet(as.numeric(Delt(ts_1$adjusted))),
    "Stock_1_atr"=((ts_1$high - ts_1$low) / ts_1$close),
    "Stock_1_vol"=ts_1$volume,
    "Stock_2_ret"=as.numeric(Delt(ts_2$adjusted)),
    "Stock_2_el"=cumRet(as.numeric(Delt(ts_2$adjusted))),
    "Stock_2_atr"=((ts_2$high - ts_2$low) / ts_2$close),
    "Stock_2_vol"=ts_2$volume
  )

  # result
  df
}










