
groupFrame = lmBondList
HKbondList = c()
j = 1
for( i in 1:length(groupFrame[,1])){
    stockCode = groupFrame[i,17]
    stockCode = str_split(stockCode,'HK.')[[1]][2]
    fullStockCode = stockCode
    stockCode = as.numeric(stockCode)
    baseFrame = HKStockAllData[which(as.numeric(HKStockAllData$X2) == stockCode),]
    if(length(baseFrame[,1]) == 0){
      j = j +1
      print(stockCode)
    } else {
      HKbondList[length(HKbondList) + 1] = fullStockCode
      # HKbondList = bindFrame(HKbondList, data.frame(X1=fullStockCode,X2='name'))
    }
}
HKbondList =data.frame(X1=HKbondList,X2=c('name'))

HKStockAllData = HKStockAllData[-1,]
HKStockAllData = HKStockAllData[-1,]
HKStockAllData = HKStockAllData[-1,]
HKStockAllDataNew = HKStockAllData
HKStockAllDataNew$X1 = strftime(HKStockAllData$X1,format = '%Y-%m')
