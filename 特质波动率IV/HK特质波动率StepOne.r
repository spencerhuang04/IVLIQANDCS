#计算港股的 三因子 五因子

HKbondList = readXLSXFromPath('港股数据/公司基本信息表/')

HKStockAllData = readXLSXFromPath('港股数据/港股历史行情表0812/')
HKStockAllData = rbind(HKStockAllData, readXLSXFromPath('港股数据/港股历史行情表1216/'))
HKStockAllData = rbind(HKStockAllData, readXLSXFromPath('港股数据/港股历史行情表1617/'))

incomeData = readXLSXFromPath('港股数据/HK利润表（非金融）000137652/')
balanceData = read.xlsx(xlsxFile='港股数据/HK资产负债表/HK_STK_Balance.xlsx',sheet=1,colName = FALSE,rowName=FALSE)

hiborYear = read.xlsx(xlsxFile='港股数据/2008-2017hibor.xlsx',sheet=1,colName = FALSE,rowName=FALSE)
HSIday = read.xlsx(xlsxFile='港股数据/HSIday.xlsx',sheet=1,colName = FALSE,rowName=FALSE)


countBE = function(year, code) {
  
  balanceFrame = balanceData[which(balanceData$X1 == code),]
  balanceFrame = balanceFrame[which(balanceFrame$X3 == 'A'),]
  balanceFrame = balanceFrame[which(strftime( balanceFrame$X2, format = '%Y') == year),]
  balanceFrame = balanceFrame[which(balanceFrame$X4 == 12),]
  response = NA
  
  if(length(balanceFrame[,1]) != 0 && !is.na(balanceFrame[1,9])) {
    if(is.na(balanceFrame[1,7])){
      balanceFrame[1,7] = '0'
    }

    if(is.na(balanceFrame[1,5])){
      balanceFrame[1,5] = '0'
    }

    BE = as.numeric(balanceFrame[1,9]) + as.numeric(balanceFrame[1,7]) - as.numeric(balanceFrame[1,5])
    response = BE
  }
  
  response
}

getRNMParts = function(nowyear, part) {
  
  year = as.numeric(nowyear) -1
  
  OPValues = c()
  OPFrame = NA
  R = NA
  N = NA
  W = NA
  for (i in 1:length(HKbondList[,1])) {
    code = HKbondList[i,1]
    incomeFrame = incomeData[which(balanceData$X1 == code),]
    incomeFrame = incomeFrame[which(incomeFrame$X3 == 'A'),]
    incomeFrame = incomeFrame[which(strftime( incomeFrame$X2, format = '%Y') == year),]
    incomeFrame = incomeFrame[which(incomeFrame$X4 == 12),]
    if(length(incomeFrame[,1]) == 0) {
      next
    }
    
    for (j in 5:9) {
      if(is.na(incomeFrame[1,j])){
        incomeFrame[1,j] = '0'
      }
    }
    OP = as.numeric(incomeFrame[1,5]) 
    
    for (k in 6:9) {
      OP = OP - as.numeric(incomeFrame[1,k]) 
    }
    BE = countBE(year, code)
    if(is.na(BE)) {
      next
    }
    
    OP = OP/BE
    HKbondList[i,3] = OP
    OPFrame = bindFrame(OPFrame,HKbondList[i,])
    OPValues[length(OPValues) + 1] = OP
  }

  NumOne = as.numeric(quantile(OPValues,probs = seq(0.3,1))[1])
  NumTwo = as.numeric(quantile(OPValues,probs = seq(0.7,1))[1])
  
  if(!is.na(OPFrame)){
    for (i in 1:length(OPFrame[,1])) {
      OPV = as.numeric(OPFrame[i,3])
      if(OPV <= NumOne) {
        R = bindFrame(R, OPFrame[i,])
      } else if(OPV < NumTwo) {
        N = bindFrame(N, OPFrame[i,])
      } else {
        W = bindFrame(W, OPFrame[i,])
      }
    }
  }
  
  response = NA
  
  if(part == 'R'){
    response = R
  } else if(part == 'N') {
    response = N
  } else {
    response = W
  }
  response
}

getCNAParts = function(year, part){
  t1 = as.numeric(year) - 1
  t2 = as.numeric(year) - 2
  INVValues = c()
  INVFrame = NA
  C = NA
  N = NA
  A = NA
  for (i in 1:length(HKbondList[,1])) {
    code = HKbondList[i,1]
    balanceFrame = balanceData[which(balanceData$X1 == code),]
    balanceFrame = balanceFrame[which(balanceFrame$X3 == 'A'),]
    balanceFrame = balanceFrame[which(balanceFrame$X4 == 12),]
    t1balanceFrame = balanceFrame[which(strftime( balanceFrame$X2, format = '%Y') == t1),]
    t2balanceFrame = balanceFrame[which(strftime( balanceFrame$X2, format = '%Y') == t2),]
    
    if(length(t1balanceFrame[,1]) == 0 || length(t2balanceFrame[,1]) == 0) {
      next
    }
    
    if(is.na(t1balanceFrame[1,6]) || is.na(t2balanceFrame[1,6])){
      print('err! none')
      next
    }
    
    INV = as.numeric(t1balanceFrame[1,6]) - as.numeric(t2balanceFrame[1,6])/as.numeric(t2balanceFrame[1,6])
    
    if(is.nan(INV)){
      next
    }
    
    INVValues[length(INVValues) + 1] = INV
    
    HKbondList[i,3] = INV
    INVFrame = bindFrame(INVFrame,HKbondList[i,])
  }
  
  NumOne = as.numeric(quantile(INVValues,probs = seq(0.3,1),na.rm = TRUE)[1])
  NumTwo = as.numeric(quantile(INVValues,probs = seq(0.7,1),na.rm = TRUE)[1])
  if(!is.na(INVFrame)){
    for (i in 1:length(INVFrame[,1])) {
      INV = as.numeric(INVFrame[i,3])
      if(INV <= NumOne) {
        C = bindFrame(C, INVFrame[i,])
      } else if(INV < NumTwo) {
        N = bindFrame(N, INVFrame[i,])
      } else {
        A = bindFrame(A, INVFrame[i,])
      }
    }
  }
  
  response = NA
  
  if(part == 'C'){
    response = C
  } else if(part == 'N') {
    response = N
  } else {
    response = A
  }
  response
}

getHMLParts = function(year, part){
  HML = c()
  HMLFrame = NA
  for (i in 1:length(HKbondList[,1])) {
    code = HKbondList[i,1]
    balanceFrame = balanceData[which(balanceData$X1 == code),]
    balanceFrame = balanceFrame[which(balanceFrame$X3 == 'A'),]
    balanceFrame = balanceFrame[which(strftime( balanceFrame$X2, format = '%Y') == year),]
    balanceFrame = balanceFrame[which(balanceFrame$X4 == 12),]
    
    if(length(balanceFrame[,1]) == 0) {
      next
    }
    
    dayFrame = HKStockAllData[which(HKStockAllData$X1 == balanceFrame[1,2]),]
    dayFrame = dayFrame[which(dayFrame$X2 == code),]
    if(length(dayFrame[,1]) == 0) {
      next
    }
    
    capital = as.numeric(balanceFrame[1,8]) 
    dayPrice = as.numeric(dayFrame[1,4])
    
    if(is.na(balanceFrame[1,9])){
      next
    }
    
    if(is.na(balanceFrame[1,7])){
      balanceFrame[1,7] = '0'
    }
    
    if(is.na(balanceFrame[1,5])){
      balanceFrame[1,5] = '0'
    }
    
    
    BE = as.numeric(balanceFrame[1,9]) + as.numeric(balanceFrame[1,7]) - as.numeric(balanceFrame[1,5])
    ME = capital * dayPrice
    
    if(is.na(ME)) {
      next
    }
    
    value = BE / ME
    
    HML[length(HML) + 1] = value
    
    HKbondList[i,3] = value
    
    HMLFrame = bindFrame(HMLFrame,HKbondList[i,])
  }
  NumOne = as.numeric(quantile(HML,probs = seq(0.3,1))[1])
  NumTwo = as.numeric(quantile(HML,probs = seq(0.7,1))[1])
  H = NA
  M = NA
  L = NA
  if(!is.na(HMLFrame)){
    for (i in 1:length(HMLFrame[,1])) {
      value = as.numeric(HMLFrame[i,3])
      if(value <= NumOne) {
        H = bindFrame(H, HMLFrame[i,])
      } else if(value < NumTwo) {
        M = bindFrame(M, HMLFrame[i,])
      } else {
        L = bindFrame(L, HMLFrame[i,])
      }
    }
  }
  
  response = NA
  
  if(part == 'H'){
    response = H
  } else if(part == 'M') {
    response = M
  } else {
    response = L
  }
  response
  
}

getLastDayPrice = function(date, code) {
  
  dayFrame = HKStockAllData[which(HKStockAllData$X1 == date),]
  if(length(dayFrame[,1]) == 0) {
    dayFrame = HKStockAllData[which(HKStockAllData$X1 == as.character(as.Date(date)-1)),]
  }
  dayFrame = dayFrame[which(dayFrame$X2 == code),]
  dayFrame
}

getSBParts = function(year, part) {
  allValue = c()
  SBFrame = NA
  for (i in 1:length(HKbondList[,1])) {
    code = HKbondList[i,1]
    balanceFrame = balanceData[which(as.numeric(balanceData$X1) == as.numeric(code)),]
    balanceFrame = balanceFrame[which(balanceFrame$X3 == 'A'),]
    balanceFrame = balanceFrame[which(strftime( balanceFrame$X2, format = '%Y') == year),]
    balanceFrame = balanceFrame[which(balanceFrame$X4 == 6),]
    if(length(balanceFrame[,1]) == 0) {
      next
    }
    
    # dayFrame = HKStockAllData[which(HKStockAllData$X1 == balanceFrame[1,2]),]
    # dayFrame = dayFrame[which(dayFrame$X2 == code),]
    dayFrame = getLastDayPrice(balanceFrame[1,2],code)
    if(length(dayFrame[,1]) == 0) {
      next
    }
    
    capital = as.numeric(balanceFrame[1,8]) 
    dayPrice = as.numeric(dayFrame[1,4])
    
    value = capital * dayPrice
    
    if(is.na(value)){
      next
    }
    
    allValue[length(allValue) + 1] = value
    
    HKbondList[i,3] = value
    
    SBFrame = bindFrame(SBFrame,HKbondList[i,])
    
  }
  middleNum = as.numeric(quantile(allValue)[3])
  S = NA
  B = NA
  for (i in 1:length(SBFrame[,1])) {
    value = SBFrame[i,3]
    if(is.na(value)){
      next
    }
    value = as.numeric(value)
    
    if(value > middleNum) {
      B = bindFrame(B,SBFrame[i,])
    } else {
      S = bindFrame(S,SBFrame[i,])
    }
  }
  
  response = NA
  
  if(part == 'S'){
    response = S
  } else {
    response = B
  }
  response
  
}

getSBParts(2012,'S')

getSBParts('2008','B')
getHMLParts('2008','M')
getRNMParts('2010','R')
getCNAParts('2008','A')

getMatchFrame = function(oneFrame,twoFrame) {
  resultFrame = NA
  if(!is.na(oneFrame) && !is.na(twoFrame)){
    for (i in 1: length(oneFrame[,1])) {
      code = oneFrame[i,1]
      matchFrame = twoFrame[which(twoFrame$X1 == code),]
      if(length(matchFrame[,1]) != 0) {
        resultFrame = bindFrame(resultFrame,oneFrame[i,])
      }
    }
  }
  resultFrame
}

getMatchFrame(BFrame,HFrame)

getDateOfOneYear = function(year) {
  YearFrame = NA
  for (i in 1:length(HKbondList[,1])) {
    stocCode = HKbondList[i,1]
    YearFrame = HKStockAllData[which(HKStockAllData$X2 == stocCode),]
    YearFrame = YearFrame[which(strftime(YearFrame$X1,format = "%Y") == year),]
    if(length(YearFrame[,1]) < 247){
      next
    } else {
      break
    }
  }
  for (i in 1:length(YearFrame[,1])) {
    YearFrame[i,2] = NA
    YearFrame[i,3] = NA
    YearFrame[i,4] = NA
  }
  YearFrame
}

countRiskOneDay = function(year,date) {
  yearRate = hiborYear[which(hiborYear$X1 == year),]
  yearRate = as.numeric(yearRate[1,2])
  
  dateRate = HSIday[which(HSIday$X2 == date),]
  dateRate = as.numeric(dateRate[1,4])
  
  result = dateRate - yearRate
  result
}
countRiskOneDay(2017,'2017-01-05')


countSMBOneDay = function(SH, SM, SL, BH, BM, BL,S,B, date) {
  oneFrame = HKStockAllData[which(HKStockAllData$X1 == date),]
  sDivisor = 0
  sDividend = 0
  
  if(!is.na(SH)){
    sDivisor = sDivisor + 1
    sDividend = sDividend + countPartDay(SH,oneFrame)
  }
  if(!is.na(SM)){
    sDivisor = sDivisor + 1
    sDividend = sDividend + countPartDay(SM,oneFrame)
  }
  if(!is.na(SL)){
    sDivisor = sDivisor + 1
    sDividend = sDividend + countPartDay(SL,oneFrame)
  }
  
  if(sDivisor == 0){
    sDivisor = sDivisor + 1
    sDividend = sDividend + countPartDay(S,oneFrame)
  }
  
  bDivisor = 0
  bDividend = 0
  
  if(!is.na(BH)){
    bDivisor = bDivisor + 1
    bDividend = bDividend + countPartDay(BH,oneFrame)
  }
  if(!is.na(BM)){
    bDivisor = bDivisor + 1
    bDividend = bDividend + countPartDay(BM,oneFrame)
  }
  if(!is.na(BL)){
    bDivisor = bDivisor + 1
    bDividend = bDividend + countPartDay(BL,oneFrame)
  }
  
  if(bDivisor == 0){
    bDivisor = bDivisor + 1
    bDividend = bDividend + countPartDay(B,oneFrame)
  }
  SMB = sDividend/sDivisor - bDividend/bDivisor
  SMB
}

countHMLOneDay = function(SH,BH,SL,BL,H,L,date) {
  oneFrame = HKStockAllData[which(HKStockAllData$X1 == date),]
  HDrivisor = 0
  HDridend = 0
  HML = NA
  if(!is.na(H) && !is.na(L)){
    if(!is.na(SH)){
      HDrivisor = HDrivisor + 1
      HDridend = HDridend + countPartDay(SH,oneFrame)
    }
    if(!is.na(BH)){
      HDrivisor = HDrivisor + 1
      HDridend = HDridend + countPartDay(BH,oneFrame)
    }
    if(HDrivisor == 0){
      HDrivisor = HDrivisor + 1
      HDridend = HDridend + countPartDay(H,oneFrame)
    }
    
    LDrivisor = 0
    LDridend = 0
    
    if(!is.na(SL)){
      LDrivisor = LDrivisor + 1
      LDridend = LDridend + countPartDay(SL,oneFrame)
    }
    if(!is.na(BL)){
      LDrivisor = LDrivisor + 1
      LDridend = LDridend + countPartDay(BL,oneFrame)
    }
    if(LDrivisor == 0){
      LDrivisor = LDrivisor + 1
      LDridend = LDridend + countPartDay(L,oneFrame)
    }
    
    HML = HDridend/HDrivisor - LDridend/LDrivisor
  }
  HML
}

countPartDay = function(SH, oneFrame) {
  SHResult = 0
  returnValues = c()
  for (j in 1:length(SH[,1])) {
    code = SH[j,1]
    codeFrame = oneFrame[which(oneFrame$X2 == code),]
    if(length(codeFrame[,1]) == 0) {
      # print('can not find trade msg!')
    }
    p1 = as.numeric(codeFrame[1,3])  
    p2 = as.numeric(codeFrame[1,4])  
    returns = log(p2/p1,base=exp(1))
    returnValues[length(returnValues) + 1] = returns
  }
  SHResult = mean(returnValues)
  SHResult
}

countEveryDay = function(SH, SM, SL, BH, BM, BL, YearFrame) {
  
  for (i in 1:length(YearFrame[,1])) {
    date = YearFrame[i,1]
    oneFrame = HKStockAllData[which(HKStockAllData$X1 == date),]
    sDivisor = 0
    sDividend = 0
    
    if(!is.na(SH)){
      sDivisor = sDivisor + 1
      sDividend = sDividend + countPartDay(SH,oneFrame)
    }
    if(!is.na(SM)){
      sDivisor = sDivisor + 1
      sDividend = sDividend + countPartDay(SM,oneFrame)
    }
    if(!is.na(SL)){
      sDivisor = sDivisor + 1
      sDividend = sDividend + countPartDay(SL,oneFrame)
    }
    
    if(sDivisor == 0){
      sDivisor = sDivisor + 1
      sDividend = sDividend + countPartDay(S,oneFrame)
    }
    
    bDivisor = 0
    bDividend = 0
    
    if(!is.na(BH)){
      bDivisor = bDivisor + 1
      bDividend = bDividend + countPartDay(BH,oneFrame)
    }
    if(!is.na(BM)){
      bDivisor = bDivisor + 1
      bDividend = bDividend + countPartDay(BM,oneFrame)
    }
    if(!is.na(BL)){
      bDivisor = bDivisor + 1
      bDividend = bDividend + countPartDay(BL,oneFrame)
    }
    
    if(bDivisor == 0){
      bDivisor = bDivisor + 1
      bDividend = bDividend + countPartDay(B,oneFrame)
    }
    
    SMB = sDividend/sDivisor - bDividend/bDivisor
    YearFrame[i,2] = SMB
    
    HDrivisor = 0
    HDridend = 0
    
    if(!is.na(SH)){
      HDrivisor = HDrivisor + 1
      HDridend = HDridend + countPartDay(SH,oneFrame)
    }
    if(!is.na(BH)){
      HDrivisor = HDrivisor + 1
      HDridend = HDridend + countPartDay(BH,oneFrame)
    }
    if(HDrivisor == 0){
      HDrivisor = HDrivisor + 1
      HDridend = HDridend + countPartDay(H,oneFrame)
    }
    
    LDrivisor = 0
    LDridend = 0
    
    if(!is.na(SL)){
      LDrivisor = LDrivisor + 1
      LDridend = LDridend + countPartDay(SL,oneFrame)
    }
    if(!is.na(BL)){
      LDrivisor = LDrivisor + 1
      LDridend = LDridend + countPartDay(BL,oneFrame)
    }
    if(LDrivisor == 0){
      LDrivisor = LDrivisor + 1
      LDridend = LDridend + countPartDay(L,oneFrame)
    }
    
    HML = HDridend/HDrivisor - LDridend/LDrivisor
    YearFrame[i,3] = HML
  }
  YearFrame
}



resultFrame = NA
for (year in 2008:2017) {
  YearFrame =  getDateOfOneYear(year)
  #three factor
  SFrame = getSBParts(year,'S')
  BFrame = getSBParts(year,'B')
  HFrame = getHMLParts(year,'H')
  MFrame = getHMLParts(year,'M')
  LFrame = getHMLParts(year,'L')
  
  SH = getMatchFrame(SFrame,HFrame)
  SM = getMatchFrame(SFrame,MFrame)
  SL = getMatchFrame(SFrame,LFrame)
  
  BH = getMatchFrame(BFrame,HFrame)
  BM = getMatchFrame(BFrame,MFrame)
  BL = getMatchFrame(BFrame,LFrame)
  
  # X1 date, X2 SMB, X3 HML
  # YearFrame = countEveryDay(SH,SM,SL,BH,BM,BL,YearFrame)
  
  #five factor
  RFrame = getRNMParts(year,'R')
  NFrame = getRNMParts(year,'N')
  WFrame = getRNMParts(year,'W')
  
  CFrame = getCNAParts(year,'C')
  NNFrame = getCNAParts(year,'N')
  AFrame = getCNAParts(year,'A')
  
  SR = getMatchFrame(SFrame,RFrame)
  SN = getMatchFrame(SFrame,NFrame)
  SW = getMatchFrame(SFrame,WFrame)
  
  BR = getMatchFrame(BFrame,RFrame)
  BN = getMatchFrame(BFrame,NFrame)
  BW = getMatchFrame(BFrame,WFrame)
  
  SC = getMatchFrame(SFrame,CFrame)
  SNN = getMatchFrame(SFrame,NNFrame)
  SA = getMatchFrame(SFrame,AFrame)
  
  BC = getMatchFrame(BFrame,CFrame)
  BNN = getMatchFrame(BFrame,NNFrame)
  BA = getMatchFrame(BFrame,AFrame)
  
  for (i in 1:length(YearFrame[,1])) {
    date = YearFrame[i,1]
    SMBBM = countSMBOneDay(SH,SM,SL,BH,BM,BL,SFrame,BFrame,date)
    SMBOP = countSMBOneDay(SR,SN,SW,BR,BN,BW,SFrame,BFrame,date)
    SMBMV = countSMBOneDay(SC,SNN,SA,BC,BNN,BA,SFrame,BFrame,date)
    YearFrame[i,2] = SMBBM
    
    if(is.na(SMBBM)) {
      SMBBM = 0
    }
    if(is.na(SMBOP)) {
      SMBOP = 0
    }
    if(is.na(SMBMV)) {
      SMBMV = 0
    }
    
    YearFrame[i,3] =  (as.numeric(SMBBM) + as.numeric(SMBOP) + as.numeric(SMBMV))/3
    
    HML = countHMLOneDay(SH,BH,SL,BL,HFrame,LFrame,date)
    RMW = countHMLOneDay(SR,BR,SW,BW,RFrame,WFrame,date)
    CMA = countHMLOneDay(SC,BC,SA,BA,CFrame,AFrame,date)
    YearFrame[i,4] = HML
    YearFrame[i,5] = RMW
    YearFrame[i,6] = CMA
    YearFrame[i,7] = countRiskOneDay(year,date)
  }
  print(year)
  resultFrame = bindFrame(resultFrame,YearFrame)
}
HKFacResult = resultFrame
saveDataFrameToFile(resultFrame = resultFrame,path = '/Users/ransonliu/R/特质波动率IV/HKFacResult.xlsx')



test = HKStockAllData[which(HKStockAllData$X2 == '00390'),]  
test = test[which(strftime(test$X1,format = "%Y") == '2008' ),]

for (i in 1:length(HKbondList[,1])) {
  bondcode = HKbondList[i,1]
  financialReport = ''
}


