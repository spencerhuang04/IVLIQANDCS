#读取文件夹下的所有文件
readXLSXFromPath = function(excelPath) {
  partCorpBondData = NULL
  for(singleFile in list.files(excelPath)){
    oneFileData = read.xlsx(xlsxFile=paste(excelPath,singleFile,sep = ""),sheet=1,colName = FALSE,rowName=FALSE)
    if(length(partCorpBondData) == 0) {
      partCorpBondData = oneFileData
    } else {
      partCorpBondData = rbind(partCorpBondData,oneFileData)
    }
    print(paste(excelPath, singleFile,sep = ""))
  }
  partCorpBondData
}

data_outline <- function(x){
  n <- length(x)
  max <- max(x,na.rm = TRUE)
  min <- min(x,na.rm = TRUE)
  m <- mean(x)
  v <- var(x)
  s <- sd(x)
  me <- median(x)
  cv <- 100*s/m
  css <- sum((x-m)^2)
  uss <- sum(x^2)
  R <- max(x)-min(x)
  R1 <- quantile(x,3/4)-quantile(x,1/4)
  sm <- s/sqrt(n)
  g1 <- n/((n-1)*(n-2))*sum((x-m)^3)/s^3
  g2 <- ((n*(n+1))/((n-1)*(n-2)*(n-3))*sum((x-m)^4)/s^4- (3*(n-1)^2)/((n-2)*(n-3)))
  data.frame(N=n, Mean=m, Var=v, std_dev=s,max=max,min=min,
             Median=me, std_mean=sm, CV=cv, CSS=css, USS=uss,
             R=R, R1=R1, Skewness=g1, Kurtosis=g2, row.names=1)
}

data_outlineNew <- function(x,nameSpace){
  n <- length(x)
  max <- max(x,na.rm = TRUE)
  min <- min(x,na.rm = TRUE)
  m <- mean(x)
  v <- var(x)
  s <- sd(x)
  me <- median(x)
  cv <- 100*s/m
  css <- sum((x-m)^2)
  uss <- sum(x^2)
  R <- max(x)-min(x)
  R1 <- quantile(x,3/4)-quantile(x,1/4)
  sm <- s/sqrt(n)
  g1 <- n/((n-1)*(n-2))*sum((x-m)^3)/s^3
  g2 <- ((n*(n+1))/((n-1)*(n-2)*(n-3))*sum((x-m)^4)/s^4- (3*(n-1)^2)/((n-2)*(n-3)))
  data.frame( nameSpace=nameSpace,N=n, Mean=m, Median=me, max=max,min=min, std_dev=s, Skewness=g1, Kurtosis=g2)
}

bindFrame = function(frame,target) {
  if(is.na(frame)){
    frame = target
  } else {
    frame = rbind(frame,target)
  }
  frame
}

saveDataFrameToFile = function(resultFrame,path){
  resultWb <- createWorkbook()
  addWorksheet(resultWb, "result")
  writeDataTable(resultWb, "result", resultFrame, startCol = 1, startRow = 1, rowNames = FALSE,colNames = TRUE)
  saveWorkbook(resultWb, path, overwrite = TRUE)
}

library(stringr)
library(openxlsx)
library(ggplot2)
