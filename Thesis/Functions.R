library(tm)
library(Rwordseg)
library(stringr)
library(caret)

## 针对从八爪鱼取的去哪儿评论数据进行预处理
preprocessing <- function(data, hoteltype) {
  
  ## 评分不是数字
  data$Score  <- as.numeric(gsub("[^0-9]", "", data$Score))  / 20
  data$Score1 <- as.numeric(gsub("[^0-9]", "", data$Score1)) / 20
  data$Score2 <- as.numeric(gsub("[^0-9]", "", data$Score2)) / 20
  data$Score3 <- as.numeric(gsub("[^0-9]", "", data$Score3)) / 20
  data$Score4 <- as.numeric(gsub("[^0-9]", "", data$Score4)) / 20
  cat("评分修改完毕...\n")
  
  ## TravelType是NA时，CheckinTime出现在了TravelType的位置上
  traveltype <- c("独自旅行", "夫妻/情侣", "家庭出行", "朋友结伴", "其他", 
                  "商务出行" )
  data$CheckinTime <- ifelse(is.na(data$CheckinTime), data$TravelType, data$CheckinTime)
  data$TravelType <- ifelse(data$TravelType %in% traveltype, data$TravelType, NA)
  cat("入住时间和出行类型修改完毕...\n")
  
  ## 酒店有两种，取数据时候为了方便就分成了HotelName1和HotelName2
  data$HotelName <- ifelse(is.na(data$HotelName1), data$HotelName2, data$HotelName1)
  data <- subset(data, select = -c(HotelName1, HotelName2))
  cat("酒店名字修改完毕...\n")
  
  ## 删除Score和Review有缺失值的样本
  data <- data[complete.cases(data$Score, data$Review), ]
  cat("评分和评论缺失数据删除完毕...\n")
  
  ## 删除重复的样本
  data <- unique(data)
  cat("重复样本删除完毕...\n")
  
  ## 标记酒店类型
  data$HotelType <- hoteltype
  cat("酒店类型标记完毕...\n")
  
  ## 删除全是非汉字的评论的样本
  data$Review_nn <- removeEverythingnotChinese(data$Review)
  data$Rev_Len <- nchar(data$Review_nn)
  data <- subset(data, Rev_Len != 0)
  cat("无汉字的评论删除完毕...\n")
  
  cat("预处理完毕!\n")
  return(data)
}



## 删除所有非汉语的文本
removeEverythingnotChinese <- function(x) {
  gsub("[^\u4e00-\u9fa5]", "", x)
}
allChinese <- function(x) {
  return(gsub("[^\u4e00-\u9fa5]", "", x) == x)
}



## 做成dtm，不要停止词
getDTM <- function(rvdata, ...) {
  cat("分词进行中...\n")
  tmdata <- segmentCN(rvdata, returnType = "tm")
  cat("分词已完成,DTM作成中...\n")
  tmdata <- as.vector(tmdata)
  cpdata <- VCorpus(VectorSource(tmdata), readerControl = list(language = "zh"))
  cat("Corpus已作成!\n")
  ctrl <- list(...)
  dtm <- DocumentTermMatrix(cpdata, control = ctrl)
  cat("DTM已作成!\n")
  return(dtm)
}



## 将已经分词的segtext，求term的tf
getTF <- function(segtext) {
  result <- table(unlist(segtext))
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  names(result) <- c("Term", "Freq")
  return(result)
}
getTF_usedtm <- function(dtm) {
  freq <- aggregate(dtm$v, list(dtm$j), sum)
  result <- data.frame(Terms = dtm$dimnames$Terms[freq$Group.1],
                       Freq = freq$x, stringsAsFactors = FALSE)
  result <- result[allChinese(result$Terms), ]
  return(result)
}


## 将已经分词的segtext，形成一个表侧是terms，表头是lable的cross表
getTFbyLabel <- function(segtext, term, label) {
  cat("计算所有单词频率...\n")
  result <- tapply(segtext, list(label), getTF)
  cat("删除不需要的单词...\n")
  result <- lapply(result, function(data) subset(data, Term %in% term))
  cat("不存在单词频率设为0...\n")
  result <- lapply(result, function(df, term) {
    df <- df[match(term, df$Term), 2]
    df[is.na(df)] <- 0
    df
  }, term = term)
  cat("得到Cross表...\n")
  result <- as.data.frame(result, row.names = term)
  return(result)
}
getPercentCross <- function(cross) {
  cross$Sum <- rowSums(cross)
  for(i in names(cross)) {
    cross[, i] <- cross[, i] / cross$Sum * 100
    cat(paste(i, "完成", "\n"))
  }
  cross <- subset(cross, select =  -c(Sum))
  return(cross)
}



## 寻找包含某个词的review
getReview <- function(word, dtm, data) {
  colNum <- which(dtm$dimname$Terms == word)
  rowNum <- dtm$i[which(dtm$j == colNum)]
  review <- data$Review[sample(rowNum, 1)]
  return(review)
}



## 由dtm，label,term，形成一个表侧是terms，表头是label的表
getDF <- function(dtm) {
  result <- table(dtm$j)
  names(result) <- dtm$dimnames$Terms[as.integer(names(result))]
  return(result)
}
getDFbyLabel <- function(dtm, term, label) {
  category <- names(table(label))
  rowNum <- lapply(category, function(t) which(label == t))
  names(rowNum) <- category
  freq <- lapply(rowNum, function(location) dtm[location, term])
  freq <- lapply(freq, getDF)
  result <- data.frame(row.names = term)
  for (i in names(freq)) {
    result[i] <- freq[i]
  }
  return(result)
}



## 选取统计指标计算各个词的指标值
getKeyword <- function(word, data_tf, corpus_tf, 
                       method = c("MI", "dice", "cos", "CSM", "McNemar", "LLR", "Chi2", "Yates")) {
  a <- data_tf$Freq[which(data_tf$Terms == word)]
  b <- corpus_tf$Freq[which(corpus_tf$Terms == word)]
  b <- ifelse(length(b) == 0, 0, b)
  c <- sum(data_tf$Freq) - a
  d <- sum(corpus_tf$Freq) - b
  n <- a + b + c + d
  
  method <- match.arg(method)
  if (method == 'MI') {
    result <- log(a*n / (a+b) / (a+c))
  } else if (method == 'dice') {
    result <- 2*a / (a+b+a+c)
  } else if (method == 'cos') {
    result <- a / sqrt(a+b+a+c)
  } else if (method == 'CSM') {
    result <- (a*d-b*c) / sqrt((a+c)*(b+d))
  } else if (method == "McNemar") {
    result <- (b-c)^2 / (b+c)
  } else if (method == "LLR") {
    result <- (a * log(a*n / (a+b+a+c)) + b * log(b*n / (a+b+b+d)) +
                 c * log(c*n / (c+d+a+c)) + d * log(d*n / (c+d+b+d)))
  } else if (method == "Chi2") {
    result <- (n*(a*d-c*b)^2 / (a+c) / (b+d) / (a+b) / (c+d))
  } else if (method == "Yates") {
    result <- (n * (abs(a*d-b*c) - n/2)^2 / (a+b) / (c+d) / (a+c) / (b+d))
  }
  return(result)
}



## 特征选择
result_featureSelect <- function(dtm, stopword, corpus, method) {
  terms <- removeSparseTerms(dtm, 0.98)$dimnames$Terms
  terms <- terms[allChinese(terms)]
  terms <- terms[!terms %in% stopword]
  tf <- getTF_usedtm(dtm)
  result <- sapply(terms, getKeyword, data_tf = tf, corpus_tf = corpus, method = method)
  result <- result[order(result, decreasing = TRUE)]
  return(result)
}



## 从dtm取分词结果
result_segregation <- function(dtm) {
  tf <- getTF_usedtm(dtm)
  cat("レビュー数:", dtm$nrow, '\n')
  cat("異語数:", nrow(tf), '\n')
  cat("延べ語数:", sum(tf$Freq), '\n')
  dtm <- removeSparseTerms(dtm, 0.98)
  tf <- getTF_usedtm(dtm)
  cat("DF>2%異語数:", nrow(tf), '\n')
}



## 
getlmData <- function(data, dtm, term, type) {
  if (type %in% c("apartment", "best", "business", "chain", "youth")) {
    df <- dtm[which(data$HotelType == type), term]
  } else {
    df <- dtm[which(data$TravelType == type), term]
  }
  df <- as.matrix(df)
  df <- apply(df, 2, function(x) ifelse(x == 0, "no", "yes"))
  df <- as.data.frame(df)
  if (type %in% c("apartment", "best", "business", "chain", "youth")) {
    df$Score <- data$Score[which(data$HotelType == type)]
  } else {
    df$Score <- data$Score[which(data$TravelType == type)]
  }
  df[1:(ncol(df)-1)] <- lapply(df[1:(ncol(df)-1)], as.factor)
  return(df)
}



##
getCAData <- function(data, term, n, type) {
  set.seed(0203)
  sample <- sample(which(data$HotelType == type), n)
  result <- data.frame(Review = data$Review[sample],
                       Score = data$Score[sample], stringsAsFactors = FALSE)
  for (i in term) {
    result[[i]] <- NA
  }
  return(result)
}
getCAData2 <- function(type, term, data, dtm, n) {
  if (type %in% c("apartment","best","business","chain","youth")) {
    term <- term[[type]]
    result <- NULL
    for (word in term) {
      sample1 <- dtm$i[which(dtm$j == which(dtm$dimnames$Terms == word))]
      sample2 <- which(data$HotelType == type)
      set.seed(0203)
      sample <- sample(intersect(sample1, sample2), n)
      sample <- data.frame(Review = data$Review[sample],
                           Score = data$Score[sample],
                           Term = word, Type = type, stringsAsFactors = FALSE)
      result <- rbind(result, sample)
      cat(type, word, "OK!\n")
    }
    return(result)
  } else {
    chinese <- c("独自旅行","夫妻/情侣","家庭出行","朋友结伴","商务出行")
    english <- c("alone", "couple", "family", "friend", "tbusiness")
    term <- term[[type]]
    result <- NULL
    for (word in term) {
      sample1 <- dtm$i[which(dtm$j == which(dtm$dimnames$Terms == word))]
      sample2 <- which(data$TravelType == chinese[which(english == type)])
      set.seed(0203)
      sample <- sample(intersect(sample1, sample2), n)
      sample <- data.frame(Review = data$Review[sample],
                           Score = data$Score[sample],
                           Term = word, Type = type, stringsAsFactors = FALSE)
      result <- rbind(result, sample)
      cat(type, word, "OK!\n")
    }
    return(result)
  }
}



## 
tTest <- function(cadata, type, term) {
  data_pos <- subset(cadata, select = "Score", 
                     Type == type & Term == term & Attitude == "pos", drop = TRUE)
  data_neg <- subset(cadata, select = "Score",
                     Type == type & Term == term & Attitude == "neg", drop = TRUE)
  if(length(data_pos) < 2 | length(data_neg) < 2) {
    result <- list(p.value = "样本数太少无法检验")
    return(result)
  } else {
    return(t.test(data_pos, data_neg))
  }
}

##
getStar <- function(p) {
  p <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
  return(p)
}
showResult <- function(reg, num, bin, type, pchangetoStar = TRUE) {
  reg <- reg[[type]]$coefficients
  num <- rbind(matrix(NA, 1, 3), num[[type]])
  bin <- rbind(matrix(NA, 1, 1), as.matrix(bin[[type]]))
  result <- cbind(reg, num, bin)
  result <- as.data.frame(result, row.names = rownames(result))
  if (pchangetoStar) {
    result[,4] <- getStar(result[,4])
    result[,8] <- getStar(result[,8])
  }
  return(result)
}
