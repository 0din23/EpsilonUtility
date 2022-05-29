# Correlation Filter ###########################################################
COR_Filter <- function(data, label, th, features){

  cutout <- NULL
  for(k in 1:ncol(data)){

    catcher <- tryCatch({
      COR <- abs(cor(cbind(label,data[,k]) %>% na.omit())[1,2])
      FALSE
    }, error = function(cond){
      TRUE
    })
    if(is.na(COR)){COR <- 0}
    if(COR < th | catcher){
      cutout <- append(cutout, colnames(data)[k])
    }
    print(paste0(k, " out of ", ncol(data)))
  }
  return(features[!(features %in% cutout)])
  #return(cutout)
}

# Rolling Corr Filter ##########################################################
rollCOR_Filter <- function(data, label, quant){

  cutout <- NULL
  for(k in 1:ncol(data)){

    catcher <- tryCatch({

      rc <- rollCorr(as.numeric(label), pull(data[,k]), lag = 260)
      COR <- cor(cbind(label,data[,k]) %>% na.omit())[1,2]
      rCOR <- quantile(rc, quant, na.rm=T) %>% as.numeric()
      if(is.null(rCOR) | is.na(rCOR)){rCOR <- 0}

      FALSE
    }, error = function(cond){
      TRUE
    })

    if(COR > 0 & rCOR <= 0 | COR < 0 & rCOR >= 0 | catcher){
      cutout <- append(cutout, colnames(data)[k])
    }
  }
  cutout
}
# Inter Corr Filter ############################################################
# interCOR_Filter <- function(data, label, th){
#
#   cutout <- NULL
#   for(k in 1:ncol(data)){
#     a <- pull(data[,k])
#     J <- c(1:ncol(data))[c(1:ncol(data))!=k]
#
#     for(j in J){
#       b <- pull(data[,j])
#       ab <- cor(cbind(data[,k], data[,j]) %>% na.omit())[1,2]
#       if(abs(ab) > th){
#
#         ca <- cor(cbind(label,data[,k]) %>% na.omit())[1,2]
#         cb <- cor(cbind(label,data[,j]) %>% na.omit())[1,2]
#
#         if(abs(ca) > abs(cb)){
#           cutout <- append(cutout, colnames(data)[j])
#         } else{
#           cutout <- append(cutout, colnames(data)[k])
#         }
#       }
#     }
#   }
#   cutout %>% unique()
# }

interCOR_Filter <- function(data, label, th, features){

  ## Get Combis to check
  corr <- abs(cor(data[,-1]))
  diag(corr) <- 0
  corr[lower.tri(corr)] <- 0
  indi <- which(corr >= th, arr.ind=TRUE)

  cutout <- NULL
  for(k in 1:nrow(indi)){

    a <- pull(data[,indi[k,1]])
    b <- pull(data[,indi[k,2]])
    ab <- cor(cbind(a, b) %>% na.omit())[1,2] %>% abs()

    if(abs(ab) > th){
      ca <- cor(cbind(label,a) %>% na.omit())[1,2]
      cb <- cor(cbind(label,b) %>% na.omit())[1,2]

      if(abs(ca) > abs(cb)){
        cutout <- append(cutout, colnames(data)[indi[k,1]])
      } else{
        cutout <- append(cutout, colnames(data)[indi[k,2]])
      }
    }
  }
  cutout <- cutout %>% unique()
  return(features[!(features %in% cutout)])
}
# Predictivness Filter #########################################################
Pred_Filter <- function(data, label, th, features){

  cutout <- NULL
  for(k in 1:ncol(data)){

    catcher <- tryCatch({
      reg <- lm(label~as.matrix(data[,k]))
      val <- summary(reg)$r.squared
      FALSE
    }, error = function(cond){
      TRUE
    })
    if(is.na(val)){val <- 0}
    if(val < th | catcher){
      cutout <- append(cutout, colnames(data)[k])
    }
    print(paste0(k, " out of ", ncol(data)))
  }
  return(features[!(features %in% cutout)])
}

# Stationarity Filter ##########################################################
ADF_Filter <- function(data, th, drift = F, features){

  cutout <- NULL
  for(k in 1:ncol(data)){

    catcher <- tryCatch({
      p_val <- adf.test(pull(data[,k]), output = F)
      if(drift){p_val <- p_val$type2[,3] %>% mean() } else{p_val <- p_val$type1[,3] %>% mean() }

      FALSE
    }, error = function(cond){
      TRUE
    })

    if(p_val > th | catcher){
      cutout <- append(cutout, colnames(data)[k])
    }
    print(paste0(k, " out of ", ncol(data)))
  }
  return(features[!(features %in% cutout)])
}
