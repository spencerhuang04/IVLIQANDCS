# 第一步，计算月度的 换手率、Liq2、Liq3、Liq4

library(openxlsx)

# get all corpanies trade data
excelPath = "债券交易信息/"
corpBondData = NULL
for(singleFile in list.files(excelPath)){
  oneFileData = read.xlsx(xlsxFile=paste(excelPath,singleFile,sep = ""),sheet=1,colName = FALSE,rowName=FALSE)
  if(length(corpBondData) == 0) {
    corpBondData = oneFileData
  } else {
    corpBondData = rbind(corpBondData,oneFileData)
  }
  print(paste(excelPath, singleFile,sep = ""))
}

# get trade code
countReadyListPath = "countReadyListfinal.xlsx"
countReadyList = read.xlsx(xlsxFile=countReadyListPath,sheet=1,colName = FALSE,rowName=FALSE)

bondMonthData = read.xlsx(xlsxFile="/Users/ransonliu/RB/主成分分析/单只债券月度数据.xlsx",sheet=1,colName = FALSE,rowName=FALSE)
bondBasicData = read.xlsx(xlsxFile="/Users/ransonliu/RB/BNDall.xlsx",sheet=1,colName = FALSE,rowName=FALSE)

LipOne_oneMonth = function(tradeData) {
  totalVolume = 0
  priceVer = c()
  maxPrice = 0
  minPrice = 10000

  # moneyTotal = 0

  for(i in 1:length(tradeData[,1])){
    volume = as.numeric(tradeData[i,8])
    price = as.numeric(tradeData[i,9]) 
    
    # moneyTotal = moneyTotal + as.numeric(tradeData[i,])

    if(is.na(price)){
      print('NA price')
      next()
    }
    totalVolume = totalVolume + volume
    priceVer[length(priceVer) + 1] = price
    if(price > maxPrice) {
      maxPrice = price
    }
    if(price < minPrice) {
      minPrice = price
    }
  }
  lipIII = 0
  lipIV = 0
  if (totalVolume != 0) {
    lipIII = 0 - (10^8 * sd(priceVer, na.rm=TRUE))/totalVolume
    lipIV = 0 - ((maxPrice - minPrice)/((maxPrice + minPrice) / 2)) * 10^8 /totalVolume 
  }
  c(lipIII, lipIV)
}

LipCountByMonthData = function(date, bondCode){
  dateFrame = bondMonthData[which(bondMonthData$X2 == date),]
  bondDateFrame = dateFrame[which(dateFrame$X1 == bondCode),]
  if(is.na(bondDateFrame) || length(bondDateFrame[,1]) == 0 || is.na(dateFrame)) {
    print('err, can not find dataFrame')
    return(c(0, 0))
  }

  basicData = bondBasicData[which(bondBasicData$X1 == bondCode),]

  lipI = as.numeric(bondDateFrame[1,8])/(as.numeric(basicData[1,14]) * 10^8)

  allTrade = c()
  for(i in 1:length(dateFrame[,1])){  
    allTrade[length(allTrade) + 1] = as.numeric(dateFrame[i,7])
  }

  x = as.numeric(bondDateFrame[1,7]) - mean(allTrade) 
  sd = sd(allTrade)
  lipII = (x - mean(allTrade))/sd
  
  c(lipI, lipII)
}

computerNum = 1
resultFrame = data.frame(code=c(),year=c(),Liq1=c(),Liq2=c(),Liq3=c(),Liq4=c())
for(corpIndex in 4:length(countReadyList[,1])){
  bondCode = countReadyList[corpIndex,1]
  tradeData = corpBondData[which(corpBondData$X1 == bondCode),]
  tradeData = tradeData[order(tradeData$X2),]

  totalVolume = 0
  priceVer = c()
  oneMonth = NA
  for(i in 1:length(tradeData[,1])){ 
    date = strftime(tradeData[i,2],format='%Y-%m')
    nextDate = strftime(tradeData[i + 1,2],format='%Y-%m')
    if((is.na(nextDate)) || date != nextDate){

      oneMonth = bindFrame(oneMonth, tradeData[i,])
      result = LipCountByMonthData(date,bondCode)
      resultTWO = LipOne_oneMonth(oneMonth)
      resultFrame = rbind(resultFrame, data.frame(code=c(bondCode),year=c(date),Liq1=c(result[1]),Liq2=c(result[2]),Liq3=c(resultTWO[1]),Liq4=c(resultTWO[2])))
      oneMonth = NA
      computerNum = computerNum + 1
    } else {
      oneMonth = bindFrame(oneMonth, tradeData[i,])
    }
  }
  print(corpIndex)
}

lnormResult = resultFrame
lnormResult$Liq2 = plnorm(resultFrame$Liq2,0.5)
oldResultFrame = resultFrame
resultFrame = lnormResult

count = 1
for(i in 1:length(resultFrame[,1])){
  bondCode = resultFrame$code[i]
  if(is.na(resultFrame[i,4])){
    print(bondCode)
    count = count + 1
  }
}
save(data,file="主成分分析/result.Rdata")

compareResultFile = "/Users/ransonliu/R/主成分分析/stepOne.xlsx"
resultWb <- createWorkbook()
addWorksheet(resultWb, "result")
writeDataTable(resultWb, "result", resultFrame, startCol = 1, startRow = 1, rowNames = FALSE,colNames = TRUE)
saveWorkbook(resultWb, compareResultFile, overwrite = TRUE)

