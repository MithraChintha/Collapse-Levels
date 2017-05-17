convertDateTime <- function(inputData,columnNames){
  for(i in seq(1:length(columnNames))){
    
    if(is.Date(as.Date(inputData[,columnNames[i]]))){
      colweekday <- paste(columnNames[i],'day',sep = "")
      colHour <- paste(columnNames[i],'Hour',sep = "")
      inputData[,colweekday] <- weekdays(as.Date(inputData[,columnNames[i]]))
      inputData[,colHour] <-  format(as.POSIXct(inputData[,columnNames[i]], format="%Y-%m-%d %H:%M"), format="%H")
      inputData[,columnNames[i]] <- NULL
    }
  }
  return(inputData)
}


collapse=function(inputData,collapseLevels=6,defaultLevel='others'){
  
  totRows=nrow(inputData)
  for(i in seq(1:ncol(inputData))){
    
    if(is.factor(inputData[,i]) | is.character(inputData[,i])){
      
      levels(inputData[,i])
      x <- data.frame(table(inputData[,i]))
      y <- as.vector(head(x %>% arrange(desc(Freq)),collapseLevels)[,1])
      inputData[,i] <- as.character(inputData[,i])
      inputData[!(inputData[,i] %in% y),i] <- defaultLevel
      inputData[,i] <- as.factor(inputData[,i])
    }
   }
  return(inputData)
}