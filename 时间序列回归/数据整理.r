# 一、数据整理
# 1， CS，IV，Liq  的 差分序列（一阶差分，前面有个三角号）
# 2，CS、 IV、Liq 的差分序列滞后一阶 （尾标为t-1）
# 3，AggIV和AggLiq的差分序列（不分投资子样本的总序列，不分工业非工业，期限，评级等）
# 4，Rt无风险利率序列的差分和差分序列的平方，shibor表，1w的月平均
# 5，Equity_ret 股票收益率序列，无需差分，大陆+香港的股票日度数据
# 类似子样本CS的算法，利差换成了对应的股票的收益率
# 6、国债利率差slope，十年期与一年期到期收益率之差的月平均值，然后差分
# 找一个日期全的十年期国债，然后分别找一年的，算出日度，在找月度
# 如果找不到就拼凑
# 7、股市波动率vix，沪深300日度数据的月度标准差，然后差分 （缺数据
# 8、股市收益率，当月收盘价/上月收盘价 - 1 （缺数据


#AggIV,AggLiq 计算，依赖分组统计.r
AggIV = computeIVTimeLine(bondList)
AggLiq = computeLiqTimeLine(bondList)

#Rt无风险利率序列的差分和差分序列的平方，shibor表，1w的月平均
SHIBOR_LdAvgRate = read.xlsx(xlsxFile="时间序列回归/SHIBOR_LdAvgRate.xlsx",sheet=1,colName = FALSE,rowName=FALSE)
SHIBORONEW = SHIBOR_LdAvgRate[which(SHIBOR_LdAvgRate$X3 == '1W'),]
allMonth = c()
for (i in 1:length(SHIBORONEW[,1])) {
   month = strftime(SHIBORONEW[i,1],format='%Y-%m') 
   allMonth[length(allMonth)+ 1] = month
}
allMonth = allMonth[!duplicated(allMonth)]
allMonth = allMonth[order(allMonth)]
RT = NA
diffRT = NA
squareRT = NA
for(i in 1:length(allMonth)) {
    month = allMonth[i]
    monthFrame = SHIBORONEW[which(strftime(SHIBORONEW$X1,format="%Y-%m") == month),]
    RT = bindFrame(RT,data.frame(month = month,result = mean(c(as.numeric(monthFrame[,5])), na.rm=TRUE)))
}
diffRT = data.frame(month= RT$month[-1],result = diff(RT$result))

for(i in 1:length(diffRT[,1])) {
    rt = as.numeric(diffRT$result[i]) 
    squareRT = bindFrame(squareRT,data.frame(month=diffRT$month[i],result = rt*rt))
}


# 5，股票收益率序列，无需差分，大陆+香港的股票日度数据，要按照子样本分类

# 6、国债利率差slope，十年期与一年期到期收益率之差的月平均值，然后差分
# 十年期债券代码： 019803

nationalDebtBasic = read.xlsx(xlsxFile =  '国债基本情况.xlsx',sheet = 1,colName = FALSE, rowName = FALSE)
nationalDebtDay  = readXLSXFromPath('国债交易信息/')

TenYeayDebtDay = nationalDebtDay[which(nationalDebtDay$X1 == '010107'),]
OneYearDebts = nationalDebtBasic[which(nationalDebtBasic$X17 == '1'),]
OneYearDebts = OneYearDebts[order(OneYearDebts$X19),]

# 选取一年中的最早发行的债券
OneYearDebtsCode = c(OneYearDebts[1,1])
firstYear = OneYearDebts[1,]
OneYearDebtsCode = NA
for(i in 1:length(OneYearDebts[,1])) {
    beginYear = strsplit(as.character(OneYearDebts[i,19]),split="-")[[1]]
    code = OneYearDebts[i + 1,1]

    rate = OneYearDebts[i + 1, 28]

    nextDate = OneYearDebts[i+1,19]
    nextYear = strsplit(as.character(OneYearDebts[i + 1,19]),split="-")[[1]]
    if (is.na(nextDate)) {
       next()
    }
    if(beginYear == nextYear) {
        if(length(nationalDebtDay[which(as.character(nationalDebtDay$X1) == OneYearDebtsCode[length(OneYearDebtsCode)]),1]) == 0) {
            # OneYearDebtsCode[length(OneYearDebtsCode)] = code
            OneYearDebtsCode = bindFrame(OneYearDebtsCode,data.frame(code=code,year=nextYear,rate=rate))
        } else {
            next()
        }
        
    } else {
        print(nextDate)
        OneYearDebtsCode = bindFrame(OneYearDebtsCode,data.frame(code=code,year=nextYear,rate=rate))
        # OneYearDebtsCode[length(OneYearDebtsCode) + 1] = code
    }
}

# 
for(i in 1:length(TenYeayDebtDay[,1])){
    date = TenYeayDebtDay[i,2]
    year = strsplit(as.character(date),split="-")[[1]]
    year = year[1]
    yearFrame = OneYearDebtsCode[which(OneYearDebtsCode$year == year),]
    yearBegin = paste(year,'-01-01', sep = "")
    yearBegin = as.Date(yearBegin,format = "%Y-%m-%d")
    date = as.Date(date, format = "%Y-%m-%d")
    days = as.numeric(date-yearBegin)
    print(days)
    TenYeayDebtDay[i,4] = as.numeric(yearFrame[1,3])  + (days/365)
}

# 算月度数据
monthVer = c()
for(i in 1:length(TenYeayDebtDay[,1])){
    date = TenYeayDebtDay[i,2]
    month = strftime(date,format="%Y-%m")
    monthVer[length(monthVer) + 1] = month
    TenYeayDebtDay[i,2] = month
}
monthVer =  monthVer[!duplicated(monthVer)]
slopeFrame = NA
for(i in 1:length(monthVer)) {
    month = monthVer[i]
    monthFrame = TenYeayDebtDay[which(TenYeayDebtDay$X2 == month),]
    result = mean(as.numeric(c(monthFrame$X3)))
    slopeFrame = bindFrame(slopeFrame,data.frame(month = month,result=result))
}
slopeDiff = data.frame(month=slopeFrame$month[-1],result=diff(slopeFrame$result))
# 7、股市波动率vix，沪深300日度数据的月度标准差，然后差分 

stockDayData = readXLSXFromPath('沪深300日度收盘价/')
stockDayData = stockDayData[-4:-1,]
stockMonthData = readXLSXFromPath('沪深300月度收盘价/')
stockMonthData = stockMonthData[-4:-1,]

# 首先把日期弄成只有月份
for(i in 1:length(stockDayData[,1])){
  stockDayData[i,1] = tryCatch({
    strftime(stockDayData[i,1],format="%Y-%m")
  },error = function(e) {
    ""
  })
}
stockVixFrame = NA
for(i in 1:length(stockMonthData[,1])){
    month = stockMonthData[i,1]
    monthFrame = stockDayData[which(stockDayData$X1 == month),]
    if(length(monthFrame[,1]) !=0) {
        sd = sd(c(monthFrame[,2]), na.rm=FALSE)
        stockVixFrame = bindFrame(stockVixFrame,data.frame(month=month,result=sd))
    }
}
stockVixFrameDiff = data.frame(month=stockVixFrame$month[-1],result=diff(stockVixFrame$result))

# 8、股市收益率，当月收盘价/上月收盘价 - 1 

stockReturnFrame = NA
for(i in 2:length(stockMonthData[,1])){
    month = stockMonthData[i,1]
    monthData = stockMonthData[i,2]
    preMonthData = stockMonthData[i-1,2]
    stockReturn = (as.numeric(monthData)/as.numeric(preMonthData)) - 1
    stockReturnFrame = bindFrame(stockReturnFrame,data.frame(month=month,result=stockReturn)) 
}
stockReturnFrameDiff = data.frame(month=stockReturnFrame$month[-1],result=diff(stockReturnFrame$result))
stockReturnFrameDiff = stockReturnFrame






