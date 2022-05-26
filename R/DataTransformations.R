# A bunch of functions to use the epsilon datbase outputs.

#' Calculates a bunch of transformations on OHLC long format data
#'
#' @param DF a dataframe with columns date; symbol; open; high; low; close; volume; adjusted
#' @param PCT takes a vector of integers that specify the lookback windows for percentage change time series
#' @param NET takes a vector of integers that specify the lookback windows for net change time series
#' @param SMA_DISTANCE takes a vector of integers that specify the lookback windows for sma_distance time series
#' @param SMA takes a vector of integers that specify the lookback windows for sma time series
#' @param LAG takes a vector of integers that specify the lookback windows for lagged time series
#' @param ATR takes a vector of integers that specify the lookback windows for atr time series
#' @return The same df with the transformations added to it
transformHLOC <- function(DF, PCT=NULL, NET=NULL, SMA_DISTANCE = NULL,
                          SMA=NULL, LAG=NULL, ATR=NULL){

  ## innit
  symbols <- DF %>% pull(symbol) %>% unique()

  lapply(1:length(symbols), function(k){
    df <- DF %>%
      filter(symbol == symbols[k])

    ### Percentage Change
    if(!is.null(PCT)){
      for(j in 1:length(PCT)){
        df <- df %>%
          mutate(!!paste0("PCT", PCT[j]) :=  as.numeric(Delt(adjusted, k = PCT[j])))
      }
    }

    ### Percentage Change
    if(!is.null(NET)){
      for(j in 1:length(NET)){
        df <- df %>%
          mutate(!!paste0("NET", NET[j]) :=  CHANGE(adjusted, n = NET[j]))
      }
    }

    ### ATR and Range
    if(!is.null(ATR)){
      df <- df %>%
        mutate(range_B = (high - low) / close)
      for(j in 1:length(ATR)){
        df <- df %>%
          mutate(!!paste0("ATR", ATR[j]) :=  SMA(df %>% pull(range_B), n = ATR[j]))
      }
    }

    ### SMA
    N <- ncol(df)
    if(!is.null(SMA)){
      for(j in 1:length(SMA)){
        for(i in 8:N){
          df <- df %>%
            mutate(!!paste0(colnames(df)[i], "_SMA", SMA[j]) :=  SMA(x = df %>% pull(colnames(df)[i]), n = SMA[j]))
        }
      }
    }

    ### LAG
    N <- ncol(df)
    if(!is.null(LAG)){
      for(j in 1:length(LAG)){
        for(i in 8:N){
          df <- df %>%
            mutate(!!paste0(colnames(df)[i], "_LAG", LAG[j]) :=  LAG(x = df %>% pull(colnames(df)[i]), LAG[j]))
        }
      }
    }
    df
  }) %>%
    rbindlist() %>%
    data.frame()
}

#-------------------------------------------------------------------------------
#' Calculates a bunch of transformations on price only long format data
#'
#' @param DF a dataframe with columns date; symbol; price
#' @param PCT takes a vector of integers that specify the lookback windows for percentage change time series
#' @param NET takes a vector of integers that specify the lookback windows for net change time series
#' @param SMA_DISTANCE takes a vector of integers that specify the lookback windows for sma_distance time series
#' @param SMA takes a vector of integers that specify the lookback windows for sma time series
#' @param LAG takes a vector of integers that specify the lookback windows for lagged time series
#' @return The same df with the transformations added to it
transformPrice <- function(DF, PCT=NULL, NET=NULL, SMA_DISTANCE = NULL,
                           SMA=NULL, LAG=NULL){

  ## innit
  symbols <- DF %>% pull(symbol) %>% unique()

  lapply(1:length(symbols), function(k){
    df <- DF %>%
      filter(symbol == symbols[k])

    ### Percentage Change
    if(!is.null(PCT)){
      for(j in 1:length(PCT)){
        df <- df %>%
          mutate(!!paste0("PCT", PCT[j]) :=  as.numeric(Delt(price, k = PCT[j])))
      }
    }

    ### Net Change
    if(!is.null(NET)){
      for(j in 1:length(NET)){
        df <- df %>%
          mutate(!!paste0("NET", NET[j]) :=  CHANGE(price, n = NET[j]))
      }
    }

    ### SMA
    N <- ncol(df)
    if(!is.null(SMA)){
      for(j in 1:length(SMA)){
        for(i in 3:N){
          df <- df %>%
            mutate(!!paste0(colnames(df)[i], "_SMA", SMA[j]) :=  SMA(x = df %>% pull(colnames(df)[i]), n = SMA[j]))
        }
      }
    }

    ### LAG
    N <- ncol(df)
    if(!is.null(LAG)){
      for(j in 1:length(LAG)){
        for(i in 3:N){
          df <- df %>%
            mutate(!!paste0(colnames(df)[i], "_LAG", LAG[j]) :=  LAG(x = df %>% pull(colnames(df)[i]), LAG[j]))
        }
      }
    }
    df
  }) %>%
    rbindlist() %>%
    data.frame()

}
