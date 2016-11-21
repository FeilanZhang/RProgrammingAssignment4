rankall <- function(outcome, num = "best"){
    #建一个空的数据框,并设定变量的名字和类型,stringAsFactor一定要设为F
    finaldf <- data.frame(hospital=character(0), state=character(0), stringsAsFactors = F) 
    #读取csv的文件,这里把所有的值都设为字符类型
    readout <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    #把State这一列的值类型设为factor后面用
    readout[,"State"] <- as.factor(readout[,"State"])
    #check一下outcome的值,并对应取出有医院名\州名\该outcome名的列,同时把数字转变为数值类型,去掉缺失值,返回到新的数据框
    if(outcome == "heart attack"){
        df <- readout[,c(2,7,11)]
        df[,3] <- as.numeric(df[,3])
        df <- na.omit(df)
    }
    else if(outcome == "heart failure"){
        df <- readout[,c(2,7,17)]
        df[,3] <- as.numeric(df[,3])
        df <- na.omit(df)
    }
    else if(outcome == "pneumonia"){
        df <- readout[,c(2,7,23)]
        df[,3] <- as.numeric(df[,3])
        df <- na.omit(df)
    }
    else{
        stop("invalid outcome")
    }
    #split上面取出的数据框, 用split函数按州名分组, 注意split返回的是一个list, list元素才是数据框
    alldf <- split(df, df[,"State"])
    #设置一个与list同样长的数值向量,做一个循环
    for(i in seq_along(alldf)){
        #取出每一个子数据框
        subdf <- alldf[[i]]
        #对每个子数据框排序,先排数值,再按州名排列,返回到子数据框中
        subdf <- subdf[order(subdf[,3], subdf[,1]),]
        #设定一个子数据框的行数
        nrows <- nrow(subdf)
        #check参数num,并且返回数据到空数据框中,注意subdf中的值是factor类型的,一定要转化成character
        if(num == "best"){
            finaldf[i, 1] <- as.character(subdf[1, 1])
            finaldf[i, 2] <- as.character(subdf[1, 2])
        }
        else if(num == "worst"){
            finaldf[i, 1] <- as.character(subdf[nrows, 1])
            finaldf[i, 2] <- as.character(subdf[nrows, 2])
        }
        else if(num > nrows){
            finaldf[i, 1] <- NA
            finaldf[i, 2] <- as.character(subdf[1, 2])
        }
        else{
            finaldf[i, 1] <- as.character(subdf[num, 1])
            finaldf[i, 2] <- as.character(subdf[num, 2])
        }
    }
    finaldf
}
