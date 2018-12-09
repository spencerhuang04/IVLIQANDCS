
alignMonth = function(firstFrame, secondFrame) {
    
    if (length(firstFrame[,1]) > length(secondFrame[,1])) {
       findFrame = firstFrame
       loopFrame = secondFrame
    } else {
        findFrame = secondFrame
        loopFrame = firstFrame
    }

    resultFrame = NA
    for(i in 1:length(loopFrame[,1])){
        month = loopFrame[i,1]
        findOneFrame  = findFrame[which(as.character(findFrame$month)  == as.character(month)),]
        if (length(findOneFrame[,1]) > 0) {
           loopFrame[i,3] = findOneFrame$rateMean[1]
           resultFrame = bindFrame(resultFrame,loopFrame[i,])
        } 
    }

    resultFrame
}

getOneFrameCorTest = function(oneFrame) {
    earlyFrame = NA
    recentFrame = NA
    for (i in 1:length(oneFrame[,1])) {
       month = oneFrame[i,1]
       year = as.numeric(str_split(as.character(month),pattern = '-')[[1]][1])
        if (year < 2015) {
            earlyFrame = bindFrame(earlyFrame, oneFrame[i,])
        } else {
          recentFrame = bindFrame(recentFrame, oneFrame[i,])
        }
    }
    
    earlyCorTest = cor.test(c(earlyFrame[,2]), c(earlyFrame[,3]), alternative = "two.sided", method = "pearson", conf.level = 0.95)
    recentCorTest = cor.test(c(recentFrame[,2]), c(recentFrame[,3]), alternative = "two.sided", method = "pearson", conf.level = 0.95)
    c(as.numeric(earlyCorTest$estimate),as.numeric(earlyCorTest$p.value),as.numeric(recentCorTest$estimate),as.numeric(recentCorTest$p.value))
}

computeCorTest = function(CSIVFrame, CSLiqFrame, LiqIVFrame, name) {
   CSIVResult = getOneFrameCorTest(CSIVFrame)
   CSLiqResult = getOneFrameCorTest(CSLiqFrame)
   LiqIVResult = getOneFrameCorTest(LiqIVFrame)
   data.frame( 
        name=name,
        earlyCSIV=CSIVResult[1], earlyCSIVPvalue=CSIVResult[2], 
        earlyCSLiq=CSLiqResult[1], earlyCSLiqPvalue=CSLiqResult[2],
        earlyLiqIV=LiqIVResult[1], earlyLiqIVPvalue=LiqIVResult[2],
        recentCSIV=CSIVResult[3], recentCSIVPvalue=CSIVResult[4], 
        recentCSLiq=CSLiqResult[3], recentCSLiqPvalue=CSLiqResult[4],
        recentLiqIV=LiqIVResult[3], recentLiqIVPvalue=LiqIVResult[4]
    )
}


corTestResult = NA
CSIVFrame = alignMonth(industryCS,industryIV)
CSLiqFrame = alignMonth(industryCS,industryLiq)
LiqIVFrame = alignMonth(industryLiq,industryIV)
corTestResult = bindFrame(corTestResult,computeCorTest(CSIVFrame,CSLiqFrame,LiqIVFrame,'industry-all'))


CSIVFrame = alignMonth(industryLongCS,industryLongIV)
CSLiqFrame = alignMonth(industryLongCS,industryLongLiq)
LiqIVFrame = alignMonth(industryLongLiq,industryLongIV)
corTestResult = bindFrame(corTestResult,computeCorTest(CSIVFrame,CSLiqFrame,LiqIVFrame,'industry-long'))

CSIVFrame = alignMonth(industryShortCS,industryShortIV)
CSLiqFrame = alignMonth(industryShortCS,industryShortLiq)
LiqIVFrame = alignMonth(industryShortLiq,industryShortIV)
corTestResult = bindFrame(corTestResult,computeCorTest(CSIVFrame,CSLiqFrame,LiqIVFrame,'industry-short'))


CSIVFrame = alignMonth(nonInCS,nonInIV)
CSLiqFrame = alignMonth(nonInCS,nonInLiq)
LiqIVFrame = alignMonth(nonInLiq,nonInIV)
corTestResult = bindFrame(corTestResult,computeCorTest(CSIVFrame,CSLiqFrame,LiqIVFrame,'nonIn-all'))

CSIVFrame = alignMonth(nonInLongCS,nonInLongIV)
CSLiqFrame = alignMonth(nonInLongCS,nonInLongLiq)
LiqIVFrame = alignMonth(nonInLongLiq,nonInLongIV)
corTestResult = bindFrame(corTestResult,computeCorTest(CSIVFrame,CSLiqFrame,LiqIVFrame,'nonIn-long'))

CSIVFrame = alignMonth(nonInShortCS,nonInShortIV)
CSLiqFrame = alignMonth(nonInShortCS,nonInShortLiq)
LiqIVFrame = alignMonth(nonInShortLiq,nonInShortIV)
corTestResult = bindFrame(corTestResult,computeCorTest(CSIVFrame,CSLiqFrame,LiqIVFrame,'nonIn-Short'))

CSIVFrame = alignMonth(nonInHighCS,nonInHighIV)
CSLiqFrame = alignMonth(nonInHighCS,nonInHighLiq)
LiqIVFrame = alignMonth(nonInHighLiq,nonInHighIV)
corTestResult = bindFrame(corTestResult,computeCorTest(CSIVFrame,CSLiqFrame,LiqIVFrame,'nonIn-High'))

CSIVFrame = alignMonth(nonInLowCS,nonInLowIV)
CSLiqFrame = alignMonth(nonInLowCS,nonInLowLiq)
LiqIVFrame = alignMonth(nonInLowLiq,nonInLowIV)
corTestResult = bindFrame(corTestResult,computeCorTest(CSIVFrame,CSLiqFrame,LiqIVFrame,'nonIn-Low'))

compareResultFile = "描述性统计/corTestResult.xlsx"
resultWb <- createWorkbook()
addWorksheet(resultWb, "result")
writeDataTable(resultWb, "result", corTestResult, startCol = 1, startRow = 1, rowNames = FALSE,colNames = TRUE)
saveWorkbook(resultWb, compareResultFile, overwrite = TRUE)

