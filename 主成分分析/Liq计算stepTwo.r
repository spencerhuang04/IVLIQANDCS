# 第二步，开始进行分析
# 首先要adf检验是否平稳，不平稳的数据要进行 diff差分来变平稳

#数据项依赖于stepOne.r 中的结果

# 先定义一个一行的frame 用来保存结果

# adf检验
library(tseries)

stepTwoFrame = NA
computeNum = 1
for(corpIndex in 3:length(countReadyList[,1])){
    bondCode = countReadyList[corpIndex,1]
  
    # resultFrame是上面第一步算出来的总体liq
    bondLiqData = resultFrame[which(resultFrame$code == bondCode),]

    if(is.na(bondLiqData) || length(bondLiqData[,1]) == 0) {
        print('can not find bond data!')
        # bondLiqData = data.frame(code=c(bondCode),year=)
        print(bondCode)
        next()
    }

    #判断是否需要差分计算
    needDiff = TRUE
    if(length(bondLiqData[,1]) < 30) {
        needDiff = FALSE
    }

    # 循环检验liq1 ~ liq4
    for(i in 3:6) {
        x = c(bondLiqData[,i])
        x[is.na(x)] = 0

        if(needDiff) {
            testResult = adf.test(x , alternative = c("stationary", "explosive"),k = trunc((length(x)-1)^(1/3)))
            pValue = as.numeric(testResult$p.value)
            
            if(is.na(pValue)) {
                next()
            }
            #大于0.05 即为不平稳，进行diff差分
            if(pValue > 0.05) {
                x = diff(x)
            }
        }   

        # liq_i 的均值 和 标准差
        liqMean = mean(x, na.rm=TRUE)
        liqSd  = sd(x, na.rm=TRUE)
        #进行标准化
        for(j in 1:length(bondLiqData[,1])) {
            bondLiqData[j,i] = (x[j] - liqMean)/liqSd
        }
    }
   
    princompData = bondLiqData[,3:6]
    princompData[is.na(princompData)] = 0
    # if(mean(c(princompData[,1])) == 0 || mean(c(princompData[,2])) == 0 || mean(c(princompData[,3])) == 0 || mean(c(princompData[,4])) == 0){
    #     next()
    # }

    canPrincomp = FALSE
    if(length(princompData[,1]) > 4) {
        princompResult = summary(princomp(princompData), loadings = TRUE) 
        canPrincomp = TRUE
    }

    for(i in 1:length(bondLiqData[,1])) {

        if(canPrincomp) {
            mainElement = 1
            #循环乘以主成分系数
            for(j in 3:6) { 
                mainElement = mainElement * princompResult$loadings[1,j-2] * bondLiqData[i,j]
            }
        } else {
           mainElement = princompData[i,1]
        }
        
        bondLiqData[i,7] = mainElement            
    }

    stepTwoFrame = bindFrame(stepTwoFrame,bondLiqData)
    
    computeNum = computeNum + 1
    
}
compareResultFile = "主成分分析/stepTwoFrame.xlsx"
resultWb <- createWorkbook()
addWorksheet(resultWb, "result")
writeDataTable(resultWb, "result", stepTwoFrame, startCol = 1, startRow = 1, rowNames = FALSE,colNames = TRUE)
saveWorkbook(resultWb, compareResultFile, overwrite = TRUE)

