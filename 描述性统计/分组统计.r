#描述性统计

# 20181204 最新修改，长期短期分界线改为五年、分类里面多加一个有期权没期权

#按照长期、非长期，高评级、低评级，工业、非工业 分八组

#依赖 stringr、openxlsx 包, util.r文件

# get trade code
countReadyListPath = "countReadyListfinal.xlsx"
countReadyList = read.xlsx(xlsxFile=countReadyListPath,sheet=1,colName = FALSE,rowName=FALSE)
bondBasicData = read.xlsx(xlsxFile="BNDall.xlsx",sheet=1,colName = FALSE,rowName=FALSE)

# 债券对应的股票信息
stockInfo = read.xlsx(xlsxFile="股票所属行业.xlsx",sheet=1,colName = FALSE,rowName=FALSE)
supplementInfo = read.xlsx(xlsxFile="stind.xlsx",sheet=1,colName = FALSE,rowName=FALSE)

# 债券的日度数据
finalFrameFile = "finalFrame.xlsx"
finalFrame= read.xlsx(xlsxFile=finalFrameFile,sheet=1,colName = FALSE,rowName=FALSE)

stepTwoFrame = read.xlsx(xlsxFile='主成分分析/stepTwoFrame.xlsx',sheet=1,colName = TRUE,rowName=FALSE)

ivResultFrame = read.xlsx(xlsxFile='特质波动率IV/facResult.xlsx',sheet=1,colName=FALSE, rowName=FALSE)
HKivResultFrame = read.xlsx(xlsxFile='特质波动率IV/HKfacResult.xlsx',sheet=1,colName=TRUE, rowName=FALSE)

# 大路数据第一行没有数据，删除掉，将列名命名成香港数据一样的列名，方便后面进行rbind操作
ivResultFrame = ivResultFrame[-1,]
names(ivResultFrame) = c('code','month','thr','fiv')

#债券的日度利差数据
bondRateDay = read.xlsx(xlsxFile='债券利差/stockDayResult.xlsx',sheet=1,colName=TRUE, rowName=FALSE)

#债券对于股票的日度数据
stockDayData = readXLSXFromPath('沪深日度数据/')
HKstockDayData = readXLSXFromPath('港股日度数据/')
HKstockDayDataNew = read.xlsx(xlsxFile='HKstockDayNew.xlsx',sheet=1,colName=FALSE, rowName=FALSE)

# 按照是否香港上市分类
filterByMarket = function(bondList) {
    HKFrame = NA
    nationalFrame = NA
    for (i in 1:length(bondList[,1])) {
        stockCode = bondList[i,17]
        if(str_count(stockCode,'HK') != 0) {
            HKFrame = bindFrame(HKFrame,bondList[i,])
        } else {
            nationalFrame = bindFrame(nationalFrame,bondList[i,])
        }
    }
    list(HKFrame,nationalFrame)
}
test = filterByMarket(bondList)

# 按照是否可融资融券
filterByEquity = function(bondList) {
    canFrame = NA
    canNotFrame = NA
    for (i in 1:length(bondList[,1])) {
        can = bondList[i,19]
        if(can == 'Y') {
            canFrame = bindFrame(canFrame,bondList[i,])
        } else {
            canNotFrame = bindFrame(canNotFrame,bondList[i,])
        }
    }
    list(canFrame,canNotFrame)
}
test = filterByEquity(bondList)

# 按照期限分类，短期投资组合包含期限为5年或以下的债券，长期投资组合对应的债券期限大于八年
filterByPeriod = function(bondList) {
    shortFrame = NA
    longFrame = NA
    for (i in 1:length(bondList[,1])) {
       period = as.numeric(bondList[i,7])
       if (period > 5) {
          longFrame = bindFrame(longFrame,bondList[i,])
       } else {
           shortFrame = bindFrame(shortFrame,bondList[i,])
       }
    }
    list(longFrame,shortFrame)
}

# 按照有无期权分类，
filterByOption = function(bondList) {
    shareFrame = NA
    unshareFrame = NA
    for (i in 1:length(bondList[,1])) {
        bondCode = bondList[i,1]
        bondBasic = bondBasicData[which(bondBasicData$X1 == bondCode),]
        share = bondBasic[1,51]
        if (is.na(share)) {
          unshareFrame = bindFrame(unshareFrame,bondList[i,])
        } else {
          shareFrame = bindFrame(shareFrame,bondList[i,])
        }
    }
    list(shareFrame, unshareFrame)
}
test = filterByOption(bondList)

# 按照评级分类 低评级，AA、AA-和A评级；高评级，即AAA和AA+评级
filterByRate = function(bondList) {
    highFrame = NA
    lowFrame = NA
    for(i in 1:length(bondList[,1])){
        bondCode = bondList[i,1]
        bondBasic = bondBasicData[which(bondBasicData$X1 == bondCode),]
        if(!is.na(bondBasic[1,32]) && (str_count(bondBasic[1,32],'A') >2)){
            highFrame = bindFrame(highFrame,bondList[i,])
        } else if(!is.na(bondBasic[1,32]) && str_count(bondBasic[1,32],'A') == 2 && bondBasic[1,32] == "AA+"){
            highFrame = bindFrame(highFrame,bondList[i,])
        } else {
            lowFrame = bindFrame(lowFrame,bondList[i,])
        }
    }
    
    list(highFrame,lowFrame)
}
test = filterByRate(bondList)

#按照行业分类,工业非工业
filterByTrade = function(bondList) {
    industryFrame = NA
    otherFrame = NA
    for(i in 1:length(bondList[,1])){
        bondCode = bondList[i,1]

        stockCode = bondList[i,17]

        if(is.na(stockCode)){
            print('error stockMsg!')
            print(bondCode)
            next
        }

        trade = stockInfo[which(stockInfo$X1 == stockCode),][1,5]

        if(!is.na(trade)  && trade == '工业') {
            industryFrame = bindFrame(industryFrame,bondList[i,])
        } else {
            otherFrame = bindFrame(otherFrame, bondList[i,])
        }
    
    }
    list(industryFrame, otherFrame)
}

#计算一个 债券的 利差 平均
computeCS = function(bondCode) {
    codeDay = bondRateDay[which(bondRateDay$X1 == code),]
    rateMargin = mean(c(as.numeric(bondFinalFrame[,10])))
    rateMargin
}

#计算分组内债券利差时间序列
computeCSTimeLine = function(groupFrame) {
    #先把所有可能的日期怼在一起，然后在去除重复的
    timeLine = c()
    groupRateDay = NA
    for( i in 1:length(groupFrame[,1])){
        code = groupFrame[i,1]
        codeDay = bondRateDay[which(bondRateDay$X1 == code),]
        timeLine = c(timeLine,c(codeDay[,2]))
        groupRateDay = bindFrame(groupRateDay,codeDay)
    }
    #去除重复的日期
    timeLine = timeLine[!duplicated(timeLine)]
    timeLine = timeLine[order(timeLine)]
    timeFrame = NA
    monthLine = c()
    for(i in 1:length(timeLine)) {
        date = timeLine[i]
        dateFrame = groupRateDay[which(groupRateDay$X2 == date),]
        
        # 去除极端数据
        filterDateFrame = NA
        for (j in 1:length(dateFrame[,1])) {
          rateMargin = as.numeric(dateFrame[j,10])
          if(is.na(rateMargin) || abs(rateMargin) > 3){
            #dateFrame = dateFrame[-j,]
          } else {
            filterDateFrame = bindFrame(filterDateFrame, dateFrame[j,])
          }
        }
        if(is.na(filterDateFrame)){
          next()
        }
        
        rateMean = mean(filterDateFrame[,10],rm.na=TRUE)
        timeFrame = bindFrame(timeFrame,data.frame(date=c(date),rateMean=c(rateMean)))
        monthLine[length(monthLine) + 1] = strftime(date,format= '%Y-%m')
    }
    monthLine = monthLine[!duplicated(monthLine)]
    monthResult = NA
    for(i in 1:length(monthLine)){
      month = monthLine[i]
      monthFrame = timeFrame[which(strftime(timeFrame$date,format= '%Y-%m') == month),]
      monthMean = mean(monthFrame[,2], rm.na=TRUE)
      monthResult = bindFrame(monthResult,data.frame(month=c(month),rateMean=c(monthMean)))
    }
    monthResult
}
test = computeCSTimeLine(bondList)

#预处理香港数据
# HKstockDayDataNew = NA
# for(j in 1:length(bondList[,1])){
#   stockCode = bondList[j,17]
#   if(str_count(stockCode,'HK') != 0) {
#     stockCode = str_split(stockCode,'HK.')[[1]][2]
#     stockCode = as.numeric(stockCode)
#     dayFrame = HKstockDayData[which(as.numeric(HKstockDayData$X2) == stockCode),]
#     for(i in 1:length(dayFrame[,1])) {
#       rate = (as.numeric(dayFrame[i,4])/as.numeric(dayFrame[i,3])) - 1
#       HKstockDayDataNew = bindFrame(HKstockDayDataNew,data.frame(X1=dayFrame[i,2],X2=dayFrame[i,1],X3=rate))
#       print(i)
#     }
#   }
#   print(j)
# }

#Equity_ret 股票收益率序列
computeEqRetTimeLine = function(groupFrame) {
  # groupFrame = classifyList$industry
  #先把所有可能的日期怼在一起，然后在去除重复的
  timeLine = c()
  groupRateDay = NA
  for( i in 1:length(groupFrame[,1])){
    
    stockCode = groupFrame[i,17]
    if(str_count(stockCode,'HK') != 0) {
      stockCode = str_split(stockCode,'HK.')[[1]][2]
      stockCode = as.numeric(stockCode)
      dayFrame = HKstockDayDataNew[which(as.numeric(HKstockDayDataNew$X1) == stockCode),]
      dayTimeLine = c(dayFrame$X2)
    } else {
      dayFrame = stockDayData[which(stockDayData$X1 == stockCode),]
      dayTimeLine = c(dayFrame$X2)
    }
    timeLine = c(timeLine,dayTimeLine)
    groupRateDay = bindFrame(groupRateDay,dayFrame)
  }
  #去除重复的日期
  timeLine = timeLine[!duplicated(timeLine)]
  timeLine = timeLine[order(timeLine)]
  timeFrame = NA
  monthLine = c()
  for(i in 1:length(timeLine)) {
    date = timeLine[i]
    
    # 不合格的日期跳过
    if(length(strsplit(as.character(date),'-')[[1]]) == 2) {
      next()
    }
    
    dateFrame = groupRateDay[which(groupRateDay$X2 == date),]
    rateMean = mean(as.numeric(dateFrame[,3]),rm.na=TRUE)
    
    timeFrame = bindFrame(timeFrame,data.frame(date=c(date),rateMean=c(rateMean)))
    monthLine[length(monthLine) + 1] = strftime(date,format= '%Y-%m')
  }
  monthLine = monthLine[!duplicated(monthLine)]
  monthResult = NA
  for(i in 1:length(monthLine)){
    month = monthLine[i]
    monthFrame = timeFrame[which(strftime(timeFrame$date,format= '%Y-%m') == month),]
    monthMean = mean(as.numeric(monthFrame[,2]), rm.na=TRUE)
    monthResult = bindFrame(monthResult,data.frame(month=c(month),rateMean=c(monthMean)))
  }
  monthResult
}
test = computeEqRetTimeLine(lmBondList)

for (date in test$X2) {
  tryCatch({
    strftime(date,format= '%Y-%m')
  },error = function(e) {
    print(date)
  })
}


# 选取一个债券的特质波动率，选取的是三因子的IV
computeIV = function(bondCode) {
    bondFrame = countReadyList[which(countReadyList$X1 == bondCode),]
    stockCode = bondFrame[1,17]
   
    ivFrame = data.frame()
     # 港股上市的
    if(str_count(stockCode,'HK') != 0) {
        stockCode = str_split(stockCode,'HK.')[[1]][2]
        stockCode = as.numeric(stockCode)
        ivFrame = HKivResultFrame[which(as.numeric(HKivResultFrame$code) == stockCode),]
    } else {
        ivFrame = ivResultFrame[which(ivResultFrame$code == stockCode),]
    }

    ivFrame
}

#计算 IV的时间序列，3因子
computeIVTimeLine = function(groupFrame) {
    #这里是月度数据，直接取月度时间
    monthLine = c()
    tmpIvResultFrame = NA
    j = 1
    for( i in 1:length(groupFrame[,1])){
        code = groupFrame[i,1]
        codeMonth = computeIV(code)
        monthLine = c(monthLine,c(codeMonth[,2]))
        
        if(length(codeMonth[,1]) == 0 && str_count(groupFrame[i,17],'HK') != 0) {
          print(groupFrame[i,17])
          j = j + 1
          print(j)
        }
        
        if(length(codeMonth[,1])!=0) {
          tmpIvResultFrame = bindFrame(tmpIvResultFrame,codeMonth)
        }

        
        # 筛选这一个code对应的IV信息
        # codeIV = ivResultFrame[which(ivResultFrame$X1 == code),]
        # if(length(codeIV[,1]) != 0) {
        #     tmpIvResultFrame = bindFrame(tmpIvResultFrame,codeMonth)
        # }
    }
    #去除重复的日期
    monthLine = monthLine[!duplicated(monthLine)]
    monthLine = monthLine[order(monthLine)]
    monthResult = NA
    monthFacVer = c()
    
    for(i in 1:length(monthLine)){
        month = monthLine[i]

        #大陆股市数据
        monthFrame = tmpIvResultFrame[which(tmpIvResultFrame$month == month),]
        #香港数据
        # HKmonthFrame = HKivResultFrame[which(HKivResultFrame$month == month),]
        
        if(length(monthFrame[,1]) !=0 ){
          monthFacVer = c(monthFacVer,as.numeric(monthFrame[,3]))
        }

        # if(length(HKmonthFrame[,1]) !=0 ){
        #   monthFacVer = c(monthFacVer,as.numeric(HKmonthFrame[,3]))
        # }
        monthMean = mean(monthFacVer, na.rm =TRUE)
        monthResult = bindFrame(monthResult,data.frame(month=c(month),rateMean=c(monthMean)))
    }
    monthResult
}

test = computeIVTimeLine(lmBondList)

#计算 IV的时间序列，5因子
computeIVFiveTimeLine = function(groupFrame) {
  #这里是月度数据，直接取月度时间
  monthLine = c()
  tmpIvResultFrame = NA
  j = 1
  for( i in 1:length(groupFrame[,1])){
    code = groupFrame[i,1]
    codeMonth = computeIV(code)
    monthLine = c(monthLine,c(codeMonth[,2]))
    
    if(length(codeMonth[,1]) == 0 && str_count(groupFrame[i,17],'HK') != 0) {
      print(groupFrame[i,17])
      j = j + 1
      print(j)
    }
    
    if(length(codeMonth[,1])!=0) {
      tmpIvResultFrame = bindFrame(tmpIvResultFrame,codeMonth)
    }

  }
  #去除重复的日期
  monthLine = monthLine[!duplicated(monthLine)]
  monthLine = monthLine[order(monthLine)]
  monthResult = NA
  monthFacVer = c()
  
  for(i in 1:length(monthLine)){
    month = monthLine[i]
    
    #大陆股市数据
    monthFrame = tmpIvResultFrame[which(tmpIvResultFrame$month == month),]
    #香港数据
    # HKmonthFrame = HKivResultFrame[which(HKivResultFrame$month == month),]
    
    if(length(monthFrame[,1]) !=0 ){
      monthFacVer = c(monthFacVer,as.numeric(monthFrame[,4]))
    }
    monthMean = mean(monthFacVer, na.rm =TRUE)
    monthResult = bindFrame(monthResult,data.frame(month=c(month),rateMean=c(monthMean)))
  }
  monthResult
}

test = computeIVFiveTimeLine(lmBondList)

computeLiqTimeLine = function(groupFrame) {
    monthLine = c()
    tmpLiqFrame = NA
    for( i in 1:length(groupFrame[,1])){
        code = groupFrame[i,1]
        codeMonth = stepTwoFrame[which(stepTwoFrame$code == code),]
        monthLine = c(monthLine,c(codeMonth[,2]))
        
        if(length(codeMonth[,1])!=0) {
          tmpLiqFrame = bindFrame(tmpLiqFrame,codeMonth)
        }
              
    }
    #去除重复的日期
    monthLine = monthLine[!duplicated(monthLine)]
    monthLine = monthLine[order(monthLine)]
    monthResult = NA

    for(i in 1:length(monthLine)){
        month = monthLine[i]

        monthFrame = tmpLiqFrame[which(tmpLiqFrame$year == month),]
       
        monthLiqVer = c(monthFrame[,7])
        monthMean = mean(monthLiqVer, na.rm=TRUE)
        monthResult = bindFrame(monthResult,data.frame(month=c(month),rateMean=c(monthMean)))
    }
    monthResult
}

test = computeLiqTimeLine(industryBondList)

# 三重循环，分八个组
periodName = c('长期','短期')
rateName = c('高评级','低评级')
tradeName = c('工业','非工业')

bondList = countReadyList[-1,]
bondList = bondList[-1,]

# 最新分类，工业、非工业-....
industry = filterByTrade(bondList)[[1]]
nonIndustry = filterByTrade(bondList)[[2]]

# 期限分
indusPeriod = filterByPeriod(industry)
noneIndusPeriod = filterByPeriod(nonIndustry)
# 评级分
indusRate = filterByRate(industry)
nonIndusRate = filterByRate(nonIndustry)
# 期权分
indusOption = filterByOption(industry)
nonIndusOption = filterByOption(nonIndustry)

classifyList = list(
    industry = industry,
    nonIndustry = nonIndustry,
    industryLong = indusPeriod[[1]],
    industryShort = indusPeriod[[2]],
    nonIndustryLong = noneIndusPeriod[[1]],
    nonIndustryShort = noneIndusPeriod[[2]],
    industryHigh = indusRate[[1]],
    industryLow = indusRate[[2]],
    nonIndustryHigh = nonIndusRate[[1]],
    nonIndustryLow = nonIndusRate[[2]],
    industryOption = indusOption[[1]],
    industryNoOption = indusOption[[2]],
    nonIndustryOption = nonIndusOption[[1]],
    nonIndustryNoOption = nonIndusOption[[2]]
)

classifyMarket = list(
  HKMatket = filterByMarket(bondList)[[1]],
  nationalMatket = filterByMarket(bondList)[[2]],
  EquityYes = filterByEquity(bondList)[[1]],
  EquityNo = filterByEquity(bondList)[[2]]
)

# 所有工业类债券
industryBondList = filterByTrade(bondList)[[1]]
industryCS = computeCSTimeLine(industryBondList)
industryIV = computeIVTimeLine(industryBondList)
industryLiq = computeLiqTimeLine(industryBondList)

ggplot(data=industryIV, aes(x=as.POSIXct(paste(month,'-01', sep='')), y=rateMean))+geom_point()+scale_x_datetime(date_breaks="1 years") 
ggplot(data=industryLiq, aes(x=as.POSIXct(paste(month,'-01', sep='')), y=rateMean))+geom_point()+scale_x_datetime(date_breaks="1 years")
# 工业-高评,低评

# 工业-长期、短期
period = filterByPeriod(industryBondList)

industryLong = period[[1]]
industryLongCS = computeCSTimeLine(industryLong)
industryLongIV = computeIVTimeLine(industryLong)
industryLongLiq = computeLiqTimeLine(industryLong)

industryShort = period[[2]]
industryShortCS = computeCSTimeLine(industryShort)
industryShortIV = computeIVTimeLine(industryShort)
industryShortLiq = computeLiqTimeLine(industryShort)

# IV 工业、工业长期、短期
industryIV = computeIVTimeLine(industryBondList)
industryLongIV = computeIVTimeLine(industryLong)
industryShortIV = computeIVTimeLine(industryShort)

#Liq 工业、工业长期，短期
industryLiq = computeLiqTimeLine(industryBondList)
industryLongLiq = computeLiqTimeLine(industryLong)
industryShortLiq = computeLiqTimeLine(industryShort)

# !!!工业类的评级分类，高评级只有一支债券，没法做。此处只做期限分类
                                                                                                                                                    
# 以2015年01月（含）为分界线，做时间子样本统计

spliteEarlyFrame = function(countFrame,name) {
    earlyFrame = countFrame[which(as.numeric(str_split(as.character(countFrame$month),pattern = '-')[[1]][1]) < 2015),]
    splitFrame = NA
    for (i in 1:length(countFrame[,1])) {
        month = countFrame[i,1]
        year = as.numeric(str_split(as.character(month),pattern = '-')[[1]][1])
        if(year < 2015) {
          splitFrame = bindFrame(splitFrame,countFrame[i,])
        }
    }
    earlyCountVer = c(splitFrame[,2])
    frame = data_outlineNew(earlyCountVer,paste(name,'early',seq="-"))
    frame
}
spliteRecentFrame = function(countFrame,name) {
    recentFrame = countFrame[which(as.numeric(str_split(as.character(countFrame$month),pattern = '-')[[1]][1]) >= 2015),] 
    splitFrame = NA
    for (i in 1:length(countFrame[,1])) {
      month = countFrame[i,1]
      year = as.numeric(str_split(as.character(month),pattern = '-')[[1]][1])
      if(year >= 2015) {
        splitFrame = bindFrame(splitFrame,countFrame[i,])
      }
    }
    earlyCountVer = c(splitFrame[,2])
    frame = data_outlineNew(earlyCountVer,paste(name,'recent',seq="-"))
    frame
}

statisticsResult = NA

statisticsResult = rbind(statisticsResult,spliteEarlyFrame(industryCS,'industry-all-CS'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(industryCS,'industry-all-CS'))

statisticsResult = rbind(statisticsResult,spliteEarlyFrame(industryLongCS,'industry-long-CS'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(industryLongCS,'industry-long-CS'))

statisticsResult = rbind(statisticsResult,spliteEarlyFrame(industryShortCS,'industry-short-CS'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(industryShortCS,'industry-short-CS'))

statisticsResult = rbind(statisticsResult,spliteEarlyFrame(industryIV,'industry-all-IV'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(industryIV,'industry-all-IV'))

statisticsResult = rbind(statisticsResult,spliteEarlyFrame(industryLongIV,'industry-long-IV'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(industryLongIV,'industry-long-IV'))

statisticsResult = rbind(statisticsResult,spliteEarlyFrame(industryShortIV,'industry-short-IV'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(industryShortIV,'industry-short-IV'))

statisticsResult = rbind(statisticsResult,spliteEarlyFrame(industryLiq,'industry-all-Liq'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(industryLiq,'industry-all-Liq'))

statisticsResult = rbind(statisticsResult,spliteEarlyFrame(industryLongLiq,'industry-long-Liq'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(industryLongLiq,'industry-long-Liq'))

statisticsResult = rbind(statisticsResult,spliteEarlyFrame(industryShortLiq,'industry-short-Liq'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(industryShortLiq,'industry-short-Liq'))

# 非工业数据
nonInBondList = filterByTrade(bondList)[[2]]
period = filterByPeriod(nonInBondList)

nonInCS = computeCSTimeLine(nonInBondList)
nonInIV = computeIVTimeLine(nonInBondList)
nonInLiq = computeLiqTimeLine(nonInBondList)

nonInLong = period[[1]]
nonInLongCS = computeCSTimeLine(nonInLong)
nonInLongIV = computeIVTimeLine(nonInLong)
nonInLongLiq = computeLiqTimeLine(nonInLong)

nonInShort = period[[2]]
nonInShortCS = computeCSTimeLine(nonInShort)
nonInShortIV = computeIVTimeLine(nonInShort)
nonInShortLiq = computeLiqTimeLine(nonInShort)

nonInCSPeriodALL = combineFrame(nonInCS,nonInLongCS,nonInShortCS)
ggplot(data=nonInCSPeriodALL, aes(x=as.POSIXct(paste(month,'-01', sep='')),
                                     y=rateMean))+geom_line(col='red')+scale_x_datetime(date_breaks="1 years")+geom_line(col='blue',aes(x=as.POSIXct(paste(month,'-01', sep='')),
                                                                                                                                        y=V3))+geom_line(col='yellow',aes(x=as.POSIXct(paste(month,'-01', sep='')),
                                                                                                                                                                          y=V4))

rateFilter = filterByRate(nonInBondList)

nonInHigh = period[[1]]
nonInHighCS = computeCSTimeLine(nonInHigh)
nonInHighIV = computeIVTimeLine(nonInHigh)
nonInHighLiq = computeLiqTimeLine(nonInHigh)

nonInLow = period[[2]]
nonInLowCS = computeCSTimeLine(nonInLow)
nonInLowIV = computeIVTimeLine(nonInLow)
nonInLowLiq = computeLiqTimeLine(nonInLow)


# 非工业
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInCS,'noindus-all-CS'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInCS,'noindus-all-CS'))

# 非工业，高评级
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInHighCS,'noindus-high-CS'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInHighCS,'noindus-high-CS'))

# 非工业，低评级
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInLowCS,'noindus-low-CS'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInLowCS,'noindus-low-CS'))

# 非工业，长期债券
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInLongCS,'noindus-long-CS'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInLongCS,'noindus-long-CS'))

# 非工业，短期债券
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInShortCS,'noindus-short-CS'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInShortCS,'noindus-short-CS'))

# 非工业IV
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInIV,'noindus-all-IV'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInIV,'noindus-all-IV'))

# 非工业，高评级
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInHighIV,'noindus-high-IV'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInHighIV,'noindus-high-IV'))

# 非工业，低评级
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInLowIV,'noindus-low-IV'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInLowIV,'noindus-low-IV'))

# 非工业，长期债券
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInLongIV,'noindus-long-IV'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInLongIV,'noindus-long-IV'))

# 非工业，短期债券
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInShortIV,'noindus-short-IV'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInShortIV,'noindus-short-IV'))

# 非工业Liq
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInLiq,'noindus-all-Liq'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInLiq,'noindus-all-Liq'))

# 非工业，高评级
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInHighLiq,'noindus-high-Liq'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInHighLiq,'noindus-high-Liq'))

# 非工业，低评级
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInLowLiq,'noindus-low-Liq'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInLowLiq,'noindus-low-Liq'))

# 非工业，长期债券
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInLongLiq,'noindus-long-Liq'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInLongLiq,'noindus-long-Liq'))

# 非工业，短期债券
statisticsResult = rbind(statisticsResult,spliteEarlyFrame(nonInShortLiq,'noindus-short-Liq'))
statisticsResult = rbind(statisticsResult,spliteRecentFrame(nonInShortLiq,'noindus-short-Liq'))

compareResultFile = "描述性统计/statisticsResult.xlsx"
resultWb <- createWorkbook()
addWorksheet(resultWb, "result")
writeDataTable(resultWb, "result", statisticsResult, startCol = 1, startRow = 1, rowNames = FALSE,colNames = TRUE)
saveWorkbook(resultWb, compareResultFile, overwrite = TRUE)

compareResultFile = "港股日度数据/HKstockDayNew.xlsx"
test = read.xlsx(xlsxFile=compareResultFile,sheet=1,colName = TRUE,rowName=FALSE)
resultWb <- createWorkbook()
addWorksheet(resultWb, "result")
writeDataTable(resultWb, "result", HKstockDayDataNew, startCol = 1, startRow = 1, rowNames = FALSE,colNames = TRUE)
saveWorkbook(resultWb, compareResultFile, overwrite = TRUE)

