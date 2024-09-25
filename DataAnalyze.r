setwd("/home/zhra/projects/R")
cpuData <- read.csv("CPUBenchMark.csv")
cpuData[!is.na(cpuData$core), ] -> cpuDataClean
cpuDataClean[!is.na(cpuDataClean$price), ] -> cpuDataClean
cpuDataClean[!is.na(cpuDataClean$mark), ] -> cpuDataClean
cpuDataClean$mark <- gsub(",", "", as.character(cpuDataClean$mark))
cpuDataClean$mark <- as.numeric(cpuDataClean$mark)
####quantitative variables####
##AVERAGE:
#mark:
    resultMeanMark <- mean(cpuDataClean$mark)
#TODO: make double data in csvfile 
#price:
    resultMeanPrice<- mean(cpuDataClean$price)
##MEDIAN:
#mark:
    resultMedianMark <- median(cpuDataClean$mark)
#price:
    resultMedianprice <- median(cpuDataClean$price)
##MODE:
    getmode <- function(v) 
    {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
    }
#mark:
    resultModeMark <- getmode(cpuDataClean$mark)
#price:
    resultModePrice <- getmode(cpuDataClean$price)
##MIN
#mark:
    resultMinMark <- min(cpuDataClean$mark)
#price:
    resultMinPrice <- min(cpuDataClean$price)
##MAX
#mark:
    resultMaxMark <- max(cpuDataClean$mark)
#price:
    resultMaxPrice <- max(cpuDataClean$price)
##VARIANCE
#mark:
    resultVarMark <- var(cpuDataClean$mark)
#price:
    resultVarPrice <- var(cpuDataClean$price)
##STANDARD DEVIATION
#mark:
    resultSDMark <- sd(cpuDataClean$mark)
#price:
    resultSDPrice <- sd(cpuDataClean$price)
##RANGE
#mark:
    resultRMark <- resultMaxMark-resultMinMark
#price:
    resultRPrice <- resultMaxPrice-resultMinPrice
##COVARIANCE
    resultCov <- cov(cpuDataClean$mark , cpuDataClean$price)
##CORRELATION
    resultCor <- cor(cpuDataClean$mark , cpuDataClean$price)
##BOX PLOT
    png(file = "boxplot.png")
    boxplot(mark ~ price, data = cpuDataClean, xlab = "set price",ylab = "benchmark in website", main = "BENCHMARK BOX PLOT")
    dev.off()
##HISTOGRAM
#mark:
    png(file = "histogramMark.png")
    hist(cpuDataClean$mark,xlab = "Weight",col = "yellow",border = "blue",breaks = 10)
    dev.off()
#price:
    png(file = "histogramPrice.png")
    hist(cpuDataClean$price,xlab = "Weight",col = "yellow",border = "blue",breaks = 10)
    dev.off()
##Calculating the confidence interval in 95% level of certainty (for price variable)
    #mean:
    error <- qt(0.975,df=length(cpuDataClean$price)-1)*resultSDPrice/sqrt(length(cpuDataClean$price))
    left <- resultMeanPrice-error
    right <- resultMeanPrice+error
    #variance:
    n <- length(cpuDataClean$price)
    (n-1)*resultVarMark/qchisq(c(.975,.025), n-1)
##Conducting One-sample
#price:
    #H0: mu = 10+average 
    #H1: mu > 10+average
    alpha=0.05 
    mu0 <- 10+resultMeanPrice
    n <- length(cpuDataClean$price)
    resultSEPrice <- resultSDPrice/sqrt(n)
    t.stat <- (resultMeanPrice - mu0)/resultSEPrice
    freedomDegree <- n-1
    t.score = qt(p=1-(alpha), df=freedomDegree,lower.tail=T)
    if( t.stat>t.score )
    {
        print("H0 rejected")
    }
    print("H0 accepted") 

###Qualitative variables###
##core:
#1:
    freqTable <- table(cpuDataClean$core)
    freqTable <- as.data.frame(freqTable)
    colnames(freqTable)[2] <- "freq"
    colnames(freqTable)[1] <- "core"
    freqTable$freq <- as.numeric(freqTable$freq)
    freqTable$core <- as.numeric(freqTable$core)
    freqTable$freqpercent <- freqTable$freq*100/sum(freqTable$freq)
    freqTable$cumfreq <- cumsum(freqTable)$freq
#2:
    png(file= "barchart.png")
    barplot( freqTable$freqpercent , names.arg=freqTable$core )
    dev.off()
#3:
    png(file= "piechart.png")
    pie(freqTable$freq,freqTable$core)
    dev.off()


