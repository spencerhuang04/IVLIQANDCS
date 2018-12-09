
timeLineList = list(
    industryCS = industryCS,
    industryLongCS = industryLongCS,
    industryShortCS = industryShortCS,
    industryIV = industryIV,
    industryLongIV = industryLongIV,
    industryShortIV = industryShortIV,
    industryLiq = industryLiq,
    industryLongLiq = industryLongLiq,
    industryShortLiq = industryShortLiq,
    nonInCS = nonInCS,
    nonInHighCS = nonInHighCS,
    nonInLowCS = nonInLowCS,
    nonInIV = nonInIV,
    nonInHighIV = nonInHighIV,
    nonInLowIV = nonInLowIV,
    nonInLiq = nonInLiq,
    nonInHighLiq = nonInHighLiq,
    nonInLowLiq = nonInLowLiq
    )

diffTimeLineList = list()
LagTimeLineList = list()

diffStatisticsResult = NA
for (i in 1:18) {
  nowLine = timeLineList[[i]]
  diffFrame = data.frame(month = nowLine$month[-1],result = diff(nowLine$rateMean))
  diffTimeLineList[[length(diffTimeLineList) + 1]] = diffFrame
  
  lagFrame = data.frame(month = nowLine$month[-1],result = nowLine$rateMean[-length(nowLine$rateMean)])
  LagTimeLineList[[length(LagTimeLineList) + 1]] = lagFrame

  diffStatisticsResult = bindFrame(diffStatisticsResult, data_outlineNew(diffFrame$result,names(timeLineList)[i])) 
  
}
names(diffTimeLineList) = c(names(timeLineList))
names(LagTimeLineList) = c(names(timeLineList))

compareResultFile = "描述性统计/diffStatisticsResult.xlsx"
resultWb <- createWorkbook()
addWorksheet(resultWb, "result")
writeDataTable(resultWb, "result", diffStatisticsResult, startCol = 1, startRow = 1, rowNames = FALSE,colNames = TRUE)
saveWorkbook(resultWb, compareResultFile, overwrite = TRUE)
