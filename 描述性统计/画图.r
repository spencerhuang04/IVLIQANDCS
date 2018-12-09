# 图表一，CS 数据，工业总，工业高评级，工业低评级
combineFrame = function(frameOne,frameTwo,frameThree) {
    for(i in 1:length(frameOne[,1])){
        month = as.character(frameOne[i,1]) 
        frameTwoLine = frameTwo[which(as.character(frameTwo$month) == month),]
        frameThreeLine = frameThree[which(as.character(frameThree$month) == month),]
        if(length(frameTwoLine[,1]) != 0){
            frameOne[i,3] = frameTwoLine[1,2]
        } else {
            frameOne[i,3] = 0
        }
        if(length(frameThreeLine[,1]) != 0){
            frameOne[i,4] = frameThreeLine[1,2]
        } else {
            frameOne[i,4] = 0
        }
    }
    frameOne
}

industryCSPeriodALL = combineFrame(industryCS,industryLongCS,industryShortCS)

industryPeriodCSPlot = ggplot()+scale_x_datetime(date_breaks="1 years")
industryPeriodCSPlot = industryPeriodCSPlot + geom_line(data=industryCSPeriodALL,aes(color="red", x=as.POSIXct(paste(month,'-01', sep='')), y=rateMean))
industryPeriodCSPlot = industryPeriodCSPlot + geom_line(data=industryCSPeriodALL,aes(color="blue", x=as.POSIXct(paste(month,'-01', sep='')),y=V3))
industryPeriodCSPlot = industryPeriodCSPlot + geom_line(data=industryCSPeriodALL,aes(color="yellow", x=as.POSIXct(paste(month,'-01', sep='')), y=V4))
industryPeriodCSPlot = industryPeriodCSPlot + theme(text = element_text(family = "STXihei")) 
industryPeriodCSPlot = industryPeriodCSPlot + xlab('工业债券-利差图')
industryPeriodCSPlot = industryPeriodCSPlot + ylab('利差(%)')
industryPeriodCSPlot = industryPeriodCSPlot + scale_colour_manual(name = '图例', values =c('blue'='blue','red'='red','yellow'='yellow'), labels = c('工业长期','工业','工业短期'))
industryPeriodCSPlot

industryIVPeriodALL = combineFrame(industryIV,industryLongIV,industryShortIV)
industryPeriodIVPlot = ggplot()+scale_x_datetime(date_breaks="1 years")
industryPeriodIVPlot = industryPeriodIVPlot + geom_line(data=industryIVPeriodALL,aes(color="red", x=as.POSIXct(paste(month,'-01', sep='')), y=rateMean))
industryPeriodIVPlot = industryPeriodIVPlot + geom_line(data=industryIVPeriodALL,aes(color="blue", x=as.POSIXct(paste(month,'-01', sep='')),y=V3))
industryPeriodIVPlot = industryPeriodIVPlot + geom_line(data=industryIVPeriodALL,aes(color="yellow", x=as.POSIXct(paste(month,'-01', sep='')), y=V4))
industryPeriodIVPlot = industryPeriodIVPlot + theme(text = element_text(family = "STXihei")) 
industryPeriodIVPlot = industryPeriodIVPlot + xlab('工业债券-特质波动率图')
industryPeriodIVPlot = industryPeriodIVPlot + ylab('特质波动率(%)')
industryPeriodIVPlot = industryPeriodIVPlot + scale_colour_manual(name = '图例', values =c('blue'='blue','red'='red','yellow'='yellow'), labels = c('工业长期','工业','工业短期'))
industryPeriodIVPlot

industryLiqPeriodALL = combineFrame(industryLiq,industryLongLiq,industryShortLiq)
industryPeriodLiqPlot = ggplot()+scale_x_datetime(date_breaks="1 years")
industryPeriodLiqPlot = industryPeriodLiqPlot + geom_line(data=industryLiqPeriodALL,aes(color="red", x=as.POSIXct(paste(month,'-01', sep='')), y=rateMean))
industryPeriodLiqPlot = industryPeriodLiqPlot + geom_line(data=industryLiqPeriodALL,aes(color="blue", x=as.POSIXct(paste(month,'-01', sep='')),y=V3))
industryPeriodLiqPlot = industryPeriodLiqPlot + geom_line(data=industryLiqPeriodALL,aes(color="yellow", x=as.POSIXct(paste(month,'-01', sep='')), y=V4))
industryPeriodLiqPlot = industryPeriodLiqPlot + theme(text = element_text(family = "STXihei")) 
industryPeriodLiqPlot = industryPeriodLiqPlot + xlab('工业债券-主成分分析图')
industryPeriodLiqPlot = industryPeriodLiqPlot + ylab('主成分(%)')
industryPeriodLiqPlot = industryPeriodLiqPlot + scale_colour_manual(name = '图例', values =c('blue'='blue','red'='red','yellow'='yellow'), labels = c('工业长期','工业','工业短期'))
industryPeriodLiqPlot


# 非工业 
nonIndustryCSPeriodALL = combineFrame(nonInCS,nonInLongCS,nonInShortCS)
nonIndustryPeriodCSPlot = ggplot()+scale_x_datetime(date_breaks="1 years")
nonIndustryPeriodCSPlot = nonIndustryPeriodCSPlot + geom_line(data=nonIndustryCSPeriodALL,aes(color="red", x=as.POSIXct(paste(month,'-01', sep='')), y=rateMean))
nonIndustryPeriodCSPlot = nonIndustryPeriodCSPlot + geom_line(data=nonIndustryCSPeriodALL,aes(color="blue", x=as.POSIXct(paste(month,'-01', sep='')),y=V3))
nonIndustryPeriodCSPlot = nonIndustryPeriodCSPlot + geom_line(data=nonIndustryCSPeriodALL,aes(color="yellow", x=as.POSIXct(paste(month,'-01', sep='')), y=V4))
nonIndustryPeriodCSPlot = nonIndustryPeriodCSPlot + theme(text = element_text(family = "STXihei")) 
nonIndustryPeriodCSPlot = nonIndustryPeriodCSPlot + xlab('非工业债券-利差图')
nonIndustryPeriodCSPlot = nonIndustryPeriodCSPlot + ylab('利差(%)')
nonIndustryPeriodCSPlot = nonIndustryPeriodCSPlot + scale_colour_manual(name = '图例', values =c('blue'='blue','red'='red','yellow'='yellow'), labels = c('非工业长期','非工业','非工业短期'))
nonIndustryPeriodCSPlot

nonIndustryIVPeriodALL = combineFrame(nonInIV,nonInLongIV,nonInShortIV)
nonIndustryPeriodIVPlot = ggplot()+scale_x_datetime(date_breaks="1 years")
nonIndustryPeriodIVPlot = nonIndustryPeriodIVPlot + geom_line(data=nonIndustryIVPeriodALL,aes(color="red", x=as.POSIXct(paste(month,'-01', sep='')), y=rateMean))
nonIndustryPeriodIVPlot = nonIndustryPeriodIVPlot + geom_line(data=nonIndustryIVPeriodALL,aes(color="blue", x=as.POSIXct(paste(month,'-01', sep='')),y=V3))
nonIndustryPeriodIVPlot = nonIndustryPeriodIVPlot + geom_line(data=nonIndustryIVPeriodALL,aes(color="yellow", x=as.POSIXct(paste(month,'-01', sep='')), y=V4))
nonIndustryPeriodIVPlot = nonIndustryPeriodIVPlot + theme(text = element_text(family = "STXihei")) 
nonIndustryPeriodIVPlot = nonIndustryPeriodIVPlot + xlab('非工业债券-特质波动率图')
nonIndustryPeriodIVPlot = nonIndustryPeriodIVPlot + ylab('波动率(%)')
nonIndustryPeriodIVPlot = nonIndustryPeriodIVPlot + scale_colour_manual(name = '图例', values =c('blue'='blue','red'='red','yellow'='yellow'), labels = c('非工业长期','非工业','非工业短期'))
nonIndustryPeriodIVPlot

nonIndustryLiqPeriodALL = combineFrame(nonInLiq,nonInLongLiq,nonInShortLiq)
nonIndustryPeriodLiqPlot = ggplot()+scale_x_datetime(date_breaks="1 years")
nonIndustryPeriodLiqPlot = nonIndustryPeriodLiqPlot + geom_line(data=nonIndustryLiqPeriodALL,aes(color="red", x=as.POSIXct(paste(month,'-01', sep='')), y=rateMean))
nonIndustryPeriodLiqPlot = nonIndustryPeriodLiqPlot + geom_line(data=nonIndustryLiqPeriodALL,aes(color="blue", x=as.POSIXct(paste(month,'-01', sep='')),y=V3))
nonIndustryPeriodLiqPlot = nonIndustryPeriodLiqPlot + geom_line(data=nonIndustryLiqPeriodALL,aes(color="yellow", x=as.POSIXct(paste(month,'-01', sep='')), y=V4))
nonIndustryPeriodLiqPlot = nonIndustryPeriodLiqPlot + theme(text = element_text(family = "STXihei")) 
nonIndustryPeriodLiqPlot = nonIndustryPeriodLiqPlot + xlab('非工业债券-主成分分析图')
nonIndustryPeriodLiqPlot = nonIndustryPeriodLiqPlot + ylab('主成分(%)')
nonIndustryPeriodLiqPlot = nonIndustryPeriodLiqPlot + scale_colour_manual(name = '图例', values =c('blue'='blue','red'='red','yellow'='yellow'), labels = c('非工业长期','非工业','非工业短期'))
nonIndustryPeriodLiqPlot

# 工业-评级分类
nonIndustryCSRateALL = combineFrame(nonInCS,nonInHighCS,nonInLowCS)
nonIndustryRateCSPlot = ggplot()+scale_x_datetime(date_breaks="1 years")
nonIndustryRateCSPlot = nonIndustryRateCSPlot + geom_line(data=nonIndustryCSRateALL,aes(color="red", x=as.POSIXct(paste(month,'-01', sep='')), y=rateMean))
nonIndustryRateCSPlot = nonIndustryRateCSPlot + geom_line(data=nonIndustryCSRateALL,aes(color="blue", x=as.POSIXct(paste(month,'-01', sep='')),y=V3))
nonIndustryRateCSPlot = nonIndustryRateCSPlot + geom_line(data=nonIndustryCSRateALL,aes(color="yellow", x=as.POSIXct(paste(month,'-01', sep='')), y=V4))
nonIndustryRateCSPlot = nonIndustryRateCSPlot + theme(text = element_text(family = "STXihei")) 
nonIndustryRateCSPlot = nonIndustryRateCSPlot + xlab('非工业债券-利差图')
nonIndustryRateCSPlot = nonIndustryRateCSPlot + ylab('利差(%)')
nonIndustryRateCSPlot = nonIndustryRateCSPlot + scale_colour_manual(name = '图例', values =c('blue'='blue','red'='red','yellow'='yellow'), labels = c('非工业高评级','非工业','非工业低评级'))
nonIndustryRateCSPlot

nonIndustryIVRateALL = combineFrame(nonInIV,nonInHighIV,nonInLowIV)
nonIndustryRateIVPlot = ggplot()+scale_x_datetime(date_breaks="1 years")
nonIndustryRateIVPlot = nonIndustryRateIVPlot + geom_line(data=nonIndustryIVRateALL,aes(color="red", x=as.POSIXct(paste(month,'-01', sep='')), y=rateMean))
nonIndustryRateIVPlot = nonIndustryRateIVPlot + geom_line(data=nonIndustryIVRateALL,aes(color="blue", x=as.POSIXct(paste(month,'-01', sep='')),y=V3))
nonIndustryRateIVPlot = nonIndustryRateIVPlot + geom_line(data=nonIndustryIVRateALL,aes(color="yellow", x=as.POSIXct(paste(month,'-01', sep='')), y=V4))
nonIndustryRateIVPlot = nonIndustryRateIVPlot + theme(text = element_text(family = "STXihei")) 
nonIndustryRateIVPlot = nonIndustryRateIVPlot + xlab('非工业债券-特质波动率图')
nonIndustryRateIVPlot = nonIndustryRateIVPlot + ylab('波动率(%)')
nonIndustryRateIVPlot = nonIndustryRateIVPlot + scale_colour_manual(name = '图例', values =c('blue'='blue','red'='red','yellow'='yellow'), labels = c('非工业高评级','非工业','非工业低评级'))
nonIndustryRateIVPlot

nonIndustryLiqRateALL = combineFrame(nonInLiq,nonInHighLiq,nonInLowLiq)
nonIndustryRateLiqPlot = ggplot()+scale_x_datetime(date_breaks="1 years")
nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + geom_line(data=nonIndustryLiqRateALL,aes(color="red", x=as.POSIXct(paste(month,'-01', sep='')), y=rateMean))
nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + geom_line(data=nonIndustryLiqRateALL,aes(color="blue", x=as.POSIXct(paste(month,'-01', sep='')),y=V3))
nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + geom_line(data=nonIndustryLiqRateALL,aes(color="yellow", x=as.POSIXct(paste(month,'-01', sep='')), y=V4))
nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + theme(text = element_text(family = "STXihei")) 
nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + xlab('非工业债券-主成分分析图')
nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + ylab('主成分(%)')
nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + scale_colour_manual(name = '图例', values =c('blue'='blue','red'='red','yellow'='yellow'), labels = c('非工业高评级','非工业','非工业低评级'))
nonIndustryRateLiqPlot

# 回归数据画图

nonIndustryRateLiqPlot = ggplot()+scale_x_datetime(date_breaks="1 years")
# nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + geom_line(data=lmCSDiff,aes(color="red", x=as.POSIXct(paste(month,'-01', sep='')), y=rateMean))
nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + geom_line(data=lmCSDiff,aes(color="blue", x=as.POSIXct(paste(month,'-01', sep='')),y=V3))
# nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + geom_line(data=lmCSDiff,aes(color="yellow", x=as.POSIXct(paste(month,'-01', sep='')), y=V4))
# nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + geom_line(data=lmCSDiff,aes(color="green", x=as.POSIXct(paste(month,'-01', sep='')), y=V5))
# nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + geom_line(data=lmCSDiff,aes(color="black", x=as.POSIXct(paste(month,'-01', sep='')), y=V6))
# nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + geom_line(data=lmCSDiff,aes(color="black", x=as.POSIXct(paste(month,'-01', sep='')), y=V7))
# nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + geom_line(data=lmCSDiff,aes(color="purple", x=as.POSIXct(paste(month,'-01', sep='')), y=V8))
nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + theme(text = element_text(family = "STXihei")) 
nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + xlab('图')
nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + ylab('利差')
# nonIndustryRateLiqPlot = nonIndustryRateLiqPlot + scale_colour_manual(name = '图例', values =c('blue'='blue','red'='red','yellow'='yellow'), labels = c('非工业高评级','非工业','非工业低评级'))
nonIndustryRateLiqPlot
 
