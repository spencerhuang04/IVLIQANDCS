# 债券与国债的同期可比利差计算

library(stringr)
library(openxlsx)

stockDayFrame = readXLSXFromPath('债券利差/债券交易信息/')
nationalFrame = readXLSXFromPath('债券利差/国债交易信息/')

corpBondList = read.xlsx(xlsxFile=BONDREADYLIST,sheet=1,colName = FALSE,rowName=FALSE)
stockDayResult = NA

# 数据框rbind在一起
bindFrame = function(frame,target) {
  if(is.na(frame)){
    frame = target
  } else {
    frame = rbind(frame,target)
  }
  frame
}

# 将结果保存到xlsx文件
saveDataFrameToFile = function(resultFrame,path){
  resultWb <- createWorkbook()
  addWorksheet(resultWb, "result")
  writeDataTable(resultWb, "result", resultFrame, startCol = 1, startRow = 1, rowNames = FALSE,colNames = TRUE)
  saveWorkbook(resultWb, path, overwrite = TRUE)
}

# 找出相似日期，然后进行利差计算
findSameDateTrade = function(corpFrame, nationalFrame) {
  begin = Sys.time()
  difference = c()
  countNum = 0
  resultFrame = NA
  for(i in 1:length(corpFrame[,1])){
    corpTradeDate = strftime(corpFrame[i,2],format='%Y-%m-%d')
    corpRate = as.numeric(corpFrame[i,9])
    nationalRate = 0
    
    tmpFrame = nationalFrame[which(as.numeric(difftime(strftime(nationalFrame$X2),corpTradeDate)) == 0),]
    nationalRate = as.numeric(tmpFrame[1,3])
    if(!is.na(nationalRate)){
      countNum = countNum + 1
      diff = corpRate - nationalRate

      corpFrame[i,10] = diff

      resultFrame = bindFrame(resultFrame, corpFrame[i,])
    }
  }
  resultFrame
}

# 循环计算所有实验用债券
for(corpIndex in 3:length(corpBondList[,1])){
  corpCode = corpBondList[corpIndex,1]
  nationalCode = corpBondList[corpIndex,13]
  tmpCorpFrame = stockDayFrame[which(stockDayFrame$X1 == corpCode),]
  tmpNationalFrame = nationalFrame[which(nationalFrame$X1 == nationalCode),]
  if(length(tmpCorpFrame[,1]) != 0 && length(tmpNationalFrame[,1]) != 0){
    stockDayResult = bindFrame(stockDayResult,findSameDateTrade(tmpCorpFrame,tmpNationalFrame)) 
  } else {
      print('missing data !')
  }
  print(corpIndex)
}

saveDataFrameToFile(stockDayResult, BONDDAYMARGINRESULT)


