
stockDayData = readXLSXFromPath('港股数据/港股历史行情表0812/')
stockDayData = rbind(stockDayData, readXLSXFromPath('港股数据/港股历史行情表1216/'))
stockDayData = rbind(stockDayData, readXLSXFromPath('港股数据/港股历史行情表1617/'))

# thrFacDay = read.xlsx(xlsxFile='特质波动率IV/HKFacResult.xlsx',sheet=1,colName = FALSE,rowName=FALSE)
thrFacDay = HKFacResult

#无风险利率
riskFreeYear = read.xlsx(xlsxFile='港股数据/2008-2017hibor.xlsx',sheet=1,colName = FALSE,rowName=FALSE)

HKbondList = readXLSXFromPath('港股数据/公司基本信息表/')
countReadyListPath = "countReadyListfinal.xlsx"
countReadyList = read.xlsx(xlsxFile=countReadyListPath,sheet=1,colName = FALSE,rowName=FALSE)

# 筛选出所需要的港股代码
HKstockList = c()
for(i in 3:length(countReadyList[,1])) {
    stockCode = countReadyList[i,17]
    if(str_count(stockCode,'HK') != 0){
        stockCode = str_split(stockCode,'HK.')[[1]][2]
        HKstockList[length(HKstockList) + 1] = stockCode
    }
}
HKstockList = HKbondList$X1
# 去除重复 duplicated
HKstockList = HKstockList[!duplicated(HKstockList)]

resultFrame = data.frame(code=c(),month=c(),thr=c(),fiv=c())
for (sIndex in 2:length(HKstockList)) {
    print(sIndex)

    # 代码可能位数不同，转换成数字进行对比
    stockCode = as.numeric(HKstockList[sIndex])
    stockFrame = stockDayData[which(as.numeric(stockDayData$X2) == stockCode),]
    
    if(is.na(stockFrame) || length(stockFrame[,1]) == 0) {
        print('na stockFrame')
        print(stockCode)
        next
    }
    
    Y = c()
    X1 = c()
    X2 = c()
    X3 = c()
    
    Fx1 = c()
    Fx2 = c()
    Fx3 = c()
    Fx4 = c()
    Fx5 = c()
  
    for (j in 1:length(stockFrame[,1])) {
        day = stockFrame[j,1]
        nowMonth = strftime(stockFrame[j,1],format='%m')
        nextMonth = strftime(stockFrame[j + 1,1],format='%m')
        month = strftime(stockFrame[j,1],format='%Y-%m')
        year = strftime(stockFrame[j,1],format='%Y')
        if(is.na(nextMonth)) {
            next
        } 
        thFacFrame = thrFacDay[which(thrFacDay$X1 == day),]
        
        if(is.na(thFacFrame[1,2]) || is.infinite(as.numeric(thFacFrame[1,2]))){
            thFacFrame[1,2] = '0'
        }
        if(is.na(thFacFrame[1,3]) || is.infinite(as.numeric(thFacFrame[1,3]))){
            thFacFrame[1,3] = '0'
        }
        if(is.na(thFacFrame[1,4]) || is.infinite(as.numeric(thFacFrame[1,4]))){
            thFacFrame[1,4] = '0'
        }
        if(is.na(thFacFrame[1,5]) || is.infinite(as.numeric(thFacFrame[1,5]))){
            thFacFrame[1,5] = '0'
        }
        if(is.na(thFacFrame[1,6]) || is.infinite(as.numeric(thFacFrame[1,6]))){
            thFacFrame[1,6] = '0'
        }
        if(is.na(thFacFrame[1,7]) || is.infinite(as.numeric(thFacFrame[1,7]))){
            thFacFrame[1,7] = '0'
        }

        Fx1[length(Fx1) + 1] =  thFacFrame[1,2]
        Fx2[length(Fx2) + 1] =  thFacFrame[1,4]
        Fx3[length(Fx3) + 1] =  thFacFrame[1,5]
        Fx4[length(Fx4) + 1] =  thFacFrame[1,6]
        Fx5[length(Fx5) + 1] =  thFacFrame[1,7]
        
        
        X1[length(X1) + 1] =  thFacFrame[1,2]
        X2[length(X2) + 1] = thFacFrame[1,4]
        X3[length(X3) + 1] = thFacFrame[1,7]
        
        SHIBFrame = riskFreeYear[which(riskFreeYear$X1 == year),]
        rft = as.numeric(str_split(SHIBFrame[1,2],'%')[[1]][1])/100
        

        rit = as.numeric(log(as.numeric(stockFrame[j,4])/as.numeric(stockFrame[j,3]),base=exp(1)))
        r = rit - rft
        if(is.na(r) || is.infinite(r)) {
          r = 0
        }
        Y[length(Y) + 1] = r

        if(nextMonth == nowMonth) {   
        
        } else {
        thff = lm(formula = Y ~ X1+X2+X3,
                    data = data.frame(X1=as.numeric(X1),
                                    X2=as.numeric(X2),
                                    X3=as.numeric(X3),
                                    Y=as.numeric(Y)))
        
        fivff = lm(
            formula = Y ~ Fx1+Fx2+Fx3+Fx4+Fx5,
            data = data.frame(Fx1=as.numeric(Fx1),
                            Fx2=as.numeric(Fx2),
                            Fx3=as.numeric(Fx3),
                            Fx4=as.numeric(Fx4),
                            Fx5=as.numeric(Fx5),
                            Y=as.numeric(Y)
            )
        )
        
        resultFrame = rbind(resultFrame, data.frame(code=c(stockCode),month=c(month),
                                                    thr=c(summary(thff)$sigma),fiv=c(summary(fivff)$sigma)))
        
        Y = c()
        X1 = c()
        X2 = c()
        X3 = c()

        Fx1 = c()
        Fx2 = c()
        Fx3 = c()
        Fx4 = c()
        Fx5 = c()
        }
        
    }
}
saveDataFrameToFile(resultFrame, '特质波动率IV/HKfacResult.xlsx')
