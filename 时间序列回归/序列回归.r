
sink("result-market-5fac.txt") 
for (name in names(classifyMarket)) {
  
  splitLine = paste('----------------',name,'---------------------', sep = " ", " ")
  splitLine
  print(splitLine)

  lmBondList = classifyMarket[name][[1]]
  lmCS = computeCSTimeLine(lmBondList)
  # lmIV = computeIVTimeLine(lmBondList)
  lmIV = computeIVFiveTimeLine(lmBondList)
  lmLiq = computeLiqTimeLine(lmBondList)

  lmEquit = computeEqRetTimeLine(lmBondList)
  lmEquit$rateMean = as.numeric(lmEquit$rateMean) * 100
  lmEquitDiff = data.frame(month=lmEquit$month[-1],result=diff(lmEquit$rateMean))

  lmCSDiff = data.frame(month=lmCS$month[-1],result=diff(lmCS$rateMean))
  lmIVDiff = data.frame(month=lmIV$month[-1],result=diff(lmIV$rateMean))
  lmLiqDiff = data.frame(month=lmLiq$month[-1],result=diff(lmLiq$rateMean))

  lagResult = c(0,c(lmCSDiff$result[-length(lmCSDiff$result)]))
  lmCSDiffLag = data.frame(month=lmCSDiff$month,result=lagResult)

  lagResult = c(0,c(lmIVDiff$result[-length(lmIVDiff$result)]))
  lmIVDiffLag = data.frame(month=lmIVDiff$month,result=lagResult)

  lagResult = c(0,c(lmLiqDiff$result[-length(lmLiqDiff$result)]))
  lmLiqDiffLag = data.frame(month=lmLiqDiff$month,result=lagResult)

  AggIVDiff = data.frame(month=AggIV$month[-1],result=diff(AggIV$rateMean))
  AggLiqDiff = data.frame(month=AggLiq$month[-1],result=diff(AggLiq$rateMean))

  # slope 差分

  #先算Equity_ret

  for (i in 1:length(lmCSDiff[,1])) {
    month = as.character(lmCSDiff[i,1])

    diffEquitFrame = lmEquitDiff[which(lmEquitDiff$month == month),]
    lmCSDiff[i,3] = diffEquitFrame[1,2]

    diffRTFrame = diffRT[which(diffRT$month == month),]
    lmCSDiff[i,4] = diffRTFrame[1,2]

    squareRTFrame = squareRT[which(squareRT$month == month),]
    lmCSDiff[i,5] = squareRTFrame[1,2]

    slopeFrameMonth = slopeDiff[which(slopeDiff$month == month),]
    lmCSDiff[i,6] = slopeFrameMonth[1,2]

    stockVixFrameDiffMonth = stockVixFrameDiff[which(stockVixFrameDiff$month == month),]
    lmCSDiff[i,7] = stockVixFrameDiffMonth[1,2]

    stockReturnFrameDiffMonth = stockReturnFrameDiff[which(stockReturnFrameDiff$month == month),]
    lmCSDiff[i,8] = stockReturnFrameDiffMonth[1,2]

    lmIVDiffMonth = lmIVDiff[which(lmIVDiff$month == month),]
    lmCSDiff[i,9] = lmIVDiffMonth[1,2]

    lmLiqDifffMonth = lmLiqDiff[which(lmLiqDiff$month == month),]
    lmCSDiff[i,10] = lmLiqDifffMonth[1,2]

    lmCSDifffLagMonth = lmCSDiffLag[which(lmCSDiffLag$month == month),]
    lmCSDiff[i,11] = lmCSDifffLagMonth[1,2]

    lmIVDifffLagMonth = lmIVDiffLag[which(lmIVDiffLag$month == month),]
    lmCSDiff[i,12] = lmIVDifffLagMonth[1,2]

    # lag-diff-Liq
    lmLiqDifffLagMonth = lmLiqDiffLag[which(lmLiqDiffLag$month == month),]
    lmCSDiff[i,13] = lmLiqDifffLagMonth[1,2]

    # diff-AggLiq
    AggIVMonth = AggIVDiff[which(AggIVDiff$month == month),]
    lmCSDiff[i,14] = AggIVMonth[1,2]

    # diff-AggLiq
    AggLiqMonth = AggLiqDiff[which(AggLiqDiff$month == month),]
    lmCSDiff[i,15] = AggLiqMonth[1,2]
  }

  

  # 2.1
  twoOne = summary(lm(formula = CS ~ Equit+Rt+RTT+slope+VIX+MKRt,
            data = data.frame(Equit=as.numeric(lmCSDiff[,3]),
                              Rt=as.numeric(lmCSDiff[,4]),
                              RTT=as.numeric(lmCSDiff[,5]),
                              slope=as.numeric(lmCSDiff[,6]),
                              VIX=as.numeric(lmCSDiff[,7]),
                              MKRt=as.numeric(lmCSDiff[,8]),
                              CS=as.numeric(lmCSDiff[,2]))))
  print(twoOne)

  # 2.2
  
  twoTwoIV = summary(lm(formula = CS ~ Equit+Rt+RTT+slope+VIX+MKRt+IV,
                      data = data.frame(Equit=as.numeric(lmCSDiff[,3]),
                                        Rt=as.numeric(lmCSDiff[,4]),
                                        RTT=as.numeric(lmCSDiff[,5]),
                                        slope=as.numeric(lmCSDiff[,6]),
                                        VIX=as.numeric(lmCSDiff[,7]),
                                        MKRt=as.numeric(lmCSDiff[,8]),
                                        IV=as.numeric(lmCSDiff[,9]),
                                        CS=as.numeric(lmCSDiff[,2]))))
  print(twoTwoIV)
  
  twoTwoLiq = summary(lm(formula = CS ~ Equit+Rt+RTT+slope+VIX+MKRt+Liq,
                      data = data.frame(Equit=as.numeric(lmCSDiff[,3]),
                                        Rt=as.numeric(lmCSDiff[,4]),
                                        RTT=as.numeric(lmCSDiff[,5]),
                                        slope=as.numeric(lmCSDiff[,6]),
                                        VIX=as.numeric(lmCSDiff[,7]),
                                        MKRt=as.numeric(lmCSDiff[,8]),
                                        Liq=as.numeric(lmCSDiff[,10]),
                                        CS=as.numeric(lmCSDiff[,2]))))
  print(twoTwoLiq)
  
  twoTwo = summary(lm(formula = CS ~ Equit+Rt+RTT+slope+VIX+MKRt+IV+Liq,
            data = data.frame(Equit=as.numeric(lmCSDiff[,3]),
                              Rt=as.numeric(lmCSDiff[,4]),
                              RTT=as.numeric(lmCSDiff[,5]),
                              slope=as.numeric(lmCSDiff[,6]),
                              VIX=as.numeric(lmCSDiff[,7]),
                              MKRt=as.numeric(lmCSDiff[,8]),
                              IV=as.numeric(lmCSDiff[,9]),
                              Liq=as.numeric(lmCSDiff[,10]),
                              CS=as.numeric(lmCSDiff[,2]))))
  print(twoTwo)

  # 2.3
  
  twoThree = summary(lm(formula = CS ~ Equit+Rt+RTT+slope+VIX+MKRt+IV+Liq+CSLag,
            data = data.frame(Equit=as.numeric(lmCSDiff[,3]),
                              Rt=as.numeric(lmCSDiff[,4]),
                              RTT=as.numeric(lmCSDiff[,5]),
                              slope=as.numeric(lmCSDiff[,6]),
                              VIX=as.numeric(lmCSDiff[,7]),
                              MKRt=as.numeric(lmCSDiff[,8]),
                              IV=as.numeric(lmCSDiff[,9]),
                              Liq=as.numeric(lmCSDiff[,10]),
                              CSLag=as.numeric(lmCSDiff[,11]),
                              CS=as.numeric(lmCSDiff[,2]))))
  print(twoThree)
  

  # 2.4
  twoFour = summary(lm(formula = CS ~ Equit+Rt+RTT+slope+VIX+MKRt+IV+Liq+CSLag+IVLag+LiqLag,
            data = data.frame(Equit=as.numeric(lmCSDiff[,3]),
                              Rt=as.numeric(lmCSDiff[,4]),
                              RTT=as.numeric(lmCSDiff[,5]),
                              slope=as.numeric(lmCSDiff[,6]),
                              VIX=as.numeric(lmCSDiff[,7]),
                              MKRt=as.numeric(lmCSDiff[,8]),
                              IV=as.numeric(lmCSDiff[,9]),
                              Liq=as.numeric(lmCSDiff[,10]),
                              CSLag=as.numeric(lmCSDiff[,11]),
                              IVLag=as.numeric(lmCSDiff[,12]),
                              LiqLag=as.numeric(lmCSDiff[,13]),
                              CS=as.numeric(lmCSDiff[,2]))))
  print(twoFour)

  # 2.5
  
  twoFiveNew = summary(lm(formula = CS ~ Equit+Rt+RTT+slope+VIX+MKRt+CSLag+AggIV+AggLiq,
                       data = data.frame(Equit=as.numeric(lmCSDiff[,3]),
                                         Rt=as.numeric(lmCSDiff[,4]),
                                         RTT=as.numeric(lmCSDiff[,5]),
                                         slope=as.numeric(lmCSDiff[,6]),
                                         VIX=as.numeric(lmCSDiff[,7]),
                                         MKRt=as.numeric(lmCSDiff[,8]),
                                         CSLag=as.numeric(lmCSDiff[,11]),
                                         AggIV=as.numeric(lmCSDiff[,14]),
                                         AggLiq=as.numeric(lmCSDiff[,15]),
                                         CS=as.numeric(lmCSDiff[,2]))))
  print(twoFiveNew)
  
  twoFive = summary(lm(formula = CS ~ Equit+Rt+RTT+slope+VIX+MKRt+IV+Liq+CSLag+AggIV+AggLiq,
            data = data.frame(Equit=as.numeric(lmCSDiff[,3]),
                              Rt=as.numeric(lmCSDiff[,4]),
                              RTT=as.numeric(lmCSDiff[,5]),
                              slope=as.numeric(lmCSDiff[,6]),
                              VIX=as.numeric(lmCSDiff[,7]),
                              MKRt=as.numeric(lmCSDiff[,8]),
                              IV=as.numeric(lmCSDiff[,9]),
                              Liq=as.numeric(lmCSDiff[,10]),
                              CSLag=as.numeric(lmCSDiff[,11]),
                              AggIV=as.numeric(lmCSDiff[,14]),
                              AggLiq=as.numeric(lmCSDiff[,15]),
                              CS=as.numeric(lmCSDiff[,2]))))
  print(twoFive)

}
sink()



