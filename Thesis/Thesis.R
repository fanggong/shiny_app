rm(list = ls())
source("Functions.R", encoding = "utf-8")

####    各类型酒店开业时间及客房数调查    ####
load("data4Ana.RData")
hotel_inf <- read.csv("HotelInf.csv", header = TRUE, as.is = TRUE, na.strings = "")

## 统一酒店名和评论数
hotel_inf$HotelName <- ifelse(is.na(hotel_inf$HotelName1), 
                              hotel_inf$HotelName2, hotel_inf$HotelName1)
hotel_inf$ReviewNum <- ifelse(is.na(hotel_inf$ReviewNum1), 
                              hotel_inf$ReviewNum2, hotel_inf$ReviewNum1)

## 取开业时间，客房数，点评数，酒店所在区域
hotel_inf$Information <- paste(hotel_inf$Contact, hotel_inf$Information, sep = " ")
hotel_inf$OpenYear <- as.integer(str_match(hotel_inf$Information, "([0-9]{4})年开业")[, 2])
hotel_inf$RoomNum <- as.integer(str_match(hotel_inf$Information, "([0-9]+)间客房")[, 2])
hotel_inf$ReviewNum <- as.integer(str_match(hotel_inf$ReviewNum, "([0-9]+)条点评")[, 2])
hotel_inf$Area <- str_match(hotel_inf$Address, "[宝山崇明奉贤虹口黄浦嘉定金静安
                            卢湾闵行普陀青松江徐汇闸北长宁周浦东新]{2}区")[, 1]
hotel_inf$Area <- ifelse(hotel_inf$Area == "东新区", "浦东新区", hotel_inf$Area)

## 选择需要的列
hotel_inf <- subset(hotel_inf, select = c("HotelName", "Address", "HotelType", 
                                          "ReviewNum", "OpenYear", "RoomNum", "Area"))
hotel_inf <- hotel_inf[!is.na(hotel_inf$ReviewNum), ]
hotel_inf <- list(chain = hotel_inf[which(hotel_inf$HotelType == "chain"), ],
                  business = hotel_inf[which(hotel_inf$HotelType == "business"), ],
                  best = hotel_inf[which(hotel_inf$HotelType == "best"), ],
                  youth = hotel_inf[which(hotel_inf$HotelType == "youth"), ],
                  apartment = hotel_inf[which(hotel_inf$HotelType == "apartment"), ])
name_list <- lapply(hotel_inf, function(x) x$HotelName)

## 检查重复情况
length(intersect(name_list$chain, name_list$business))
length(intersect(name_list$chain, name_list$best))
length(intersect(name_list$chain, name_list$youth))
length(intersect(name_list$chain, name_list$apartment))
length(intersect(name_list$business, name_list$best))
length(intersect(name_list$business, name_list$youth))
length(intersect(name_list$business, name_list$apartment))
length(intersect(name_list$best, name_list$youth))
length(intersect(name_list$best, name_list$apartment))
length(intersect(name_list$youth, name_list$apartment))

## 優先順位 youth = apartment = best > business > chain
name_list$chain <- Reduce(setdiff, name_list)
name_list$business <- Reduce(setdiff, name_list[-1])
name_list$apartment <- setdiff(name_list$apartment, name_list$best)
hotel_inf <- Map(function(x, y) subset(x, HotelName %in% y), x = hotel_inf, y = name_list)
hotel_inf <- Reduce(rbind, hotel_inf)


## 基本分析
aggregate(hotel_inf$RoomNum, list(hotel_inf$HotelType), mean, na.rm = TRUE)
addmargins(table(hotel_inf$Area, hotel_inf$HotelType))
aggregate(hotel_inf$OpenYear, list(hotel_inf$HotelType), 
          function(x) length(which(x > 2015)) / length(x))
k <- subset(data4Ana, !is.na(TravelType) & TravelType != "其他")
m <- table(k$HotelType, k$TravelType)
m / rowSums(m)
k <- table(hotel_inf$OpenYear[which(hotel_inf$HotelType == 'chain')])
sum(k[c("2017","2018")]) / sum(k)
####    对所有数据进行预处理    ####

apartment <- read.csv("Apartment.csv", header = TRUE, as.is = TRUE, na.strings = "")
youth <- read.csv("Youth.csv", header = TRUE, as.is = TRUE, na.strings = "")
best <- read.csv("Best.csv", header = TRUE, as.is = TRUE, na.strings = "")
chain <- read.csv("Chain.csv", header = TRUE, as.is = TRUE, na.strings = "")
business <- read.csv("Business.csv", header = TRUE, as.is = TRUE, na.strings = "")

apartment <- preprocessing(apartment, "apartment")
youth <- preprocessing(youth, "youth")
best <- preprocessing(best, "best")
chain <- preprocessing(chain, "chain")
business <- preprocessing(business, "business")

## 检验重复情况
length(intersect(chain$HotelName, business$HotelName))
length(intersect(chain$HotelName, best$HotelName))
length(intersect(chain$HotelName, youth$HotelName))
length(intersect(chain$HotelName, apartment$HotelName))
length(intersect(business$HotelName, best$HotelName))
length(intersect(business$HotelName, youth$HotelName))
length(intersect(business$HotelName, apartment$HotelName))
length(intersect(best$HotelName, youth$HotelName))
length(intersect(best$HotelName, apartment$HotelName))
length(intersect(youth$HotelName, apartment$HotelName))

##　優先順位 youth = apartment = best > business > chain

name <- setdiff(chain$HotelName, 
                rbind(apartment, best, business, youth)$HotelName)
chain <- subset(chain, HotelName %in% name)
name <- setdiff(business$HotelName,
                rbind(apartment, best, youth)$HotelName)
business <- subset(business, HotelName %in% name)

data4Ana <- rbind(apartment, youth, chain, business, best)

save(data4Ana, file = "data4Ana.RData")



####    做成所有数据的dtm    ####

load("data4Ana.RData")
funs <- list(table = table, table_percent = function(x) table(x) / length(x) * 100, 
             length = length, mean = mean, sd = sd)
hoteltype <- c("apartment", "best", "business", "chain", "youth")
lapply(funs, function(f) 
  lapply(list(apartment = data4Ana[which(data4Ana$HotelType == hoteltype[1]), "Score"],
              best = data4Ana[which(data4Ana$HotelType == hoteltype[2]), "Score"], 
              business = data4Ana[which(data4Ana$HotelType == hoteltype[3]), "Score"],
              chain = data4Ana[which(data4Ana$HotelType == hoteltype[4]), "Score"],
              youth = data4Ana[which(data4Ana$HotelType == hoteltype[5]), "Score"],
              sum = data4Ana[which(data4Ana$HotelType %in% hoteltype), "Score"]), f))

data4Ana_dtm <- getDTM(data4Ana[which(data4Ana$HotelType %in% hoteltype), "Review"],
                       weighting = weightTf, wordLengths = c(1, Inf))

save(data4Ana_dtm, file = "data4Ana_dtm.RData")


####    做成所有数据的非tm格式的分词结果    ####

load("data4Ana.RData")
data4Ana_seg <- segmentCN(data4Ana[, "Review"])
save(data4Ana_seg, file = "data4Ana_seg.RData")



####    各酒店类型和出游类型的分词结果展示    ####
load("data4Ana.RData")
load("data4Ana_dtm.RData")
load("corpus_dtm.RData")
apartment_dtm <- data4Ana_dtm[which(data4Ana$HotelType == "apartment"), ]
best_dtm <- data4Ana_dtm[which(data4Ana$HotelType == "best"), ]
business_dtm <- data4Ana_dtm[which(data4Ana$HotelType == "business"), ]
chain_dtm <- data4Ana_dtm[which(data4Ana$HotelType == "chain"), ]
youth_dtm <- data4Ana_dtm[which(data4Ana$HotelType == "youth"), ]
alone_dtm <- data4Ana_dtm[which(data4Ana$TravelType == "独自旅行"), ]
couple_dtm <- data4Ana_dtm[which(data4Ana$TravelType == "夫妻/情侣"), ]
family_dtm <- data4Ana_dtm[which(data4Ana$TravelType == "家庭出行"), ]
friend_dtm <- data4Ana_dtm[which(data4Ana$TravelType == "朋友结伴"), ]
tbusiness_dtm <- data4Ana_dtm[which(data4Ana$TravelType == "商务出行"), ]
byTraveltype_dtm <- data4Ana_dtm[!is.na(data4Ana$TravelType) & data4Ana$TravelType != '其他', ]

result_segregation(tbusiness_dtm)

####    得到corpus的dtm    ####

corpus_tf <- read.table("corpus.txt", quote = "", stringsAsFactors = FALSE)
corpus_tf <- getDTM(corpus_tf[,1], weighting = weightTf, wordLengths = c(1, Inf))
save(corpus_tf, file = "corpus_dtm.RData")


####    提取关键词, MI,Chi2,DF    ####
load("data4Ana_dtm.RData")
load("data4Ana.RData")
load("corpus_dtm.RData")

corpus_tf <- getTF_usedtm(corpus_tf)
stopword_cn <- c("酒店","入住","青旅","旅舍","旅社","公寓","住","上海")

#stopword_cn <- read.table("stopword_CN.txt", quote = "")
#stopword_cn <- as.vector(stopword_cn[, 1])
#stopword_cn <- stopword_cn[allChinese(stopword_cn)]
#stopword_cn <- stopword_cn[!duplicated(stopword_cn)]
#stopword_cn <- stopword_cn[!stopword_cn %in% c("安全", "方便", "附近", "周围", "合理",
#                                               "丰富", "好")]

apartment_dtm <- data4Ana_dtm[which(data4Ana$HotelType == "apartment"), ]
best_dtm <- data4Ana_dtm[which(data4Ana$HotelType == "best"), ]
business_dtm <- data4Ana_dtm[which(data4Ana$HotelType == "business"), ]
chain_dtm <- data4Ana_dtm[which(data4Ana$HotelType == "chain"), ]
youth_dtm <- data4Ana_dtm[which(data4Ana$HotelType == "youth"), ]

alone_dtm <- data4Ana_dtm[which(data4Ana$TravelType == "独自旅行"), ]
couple_dtm <- data4Ana_dtm[which(data4Ana$TravelType == "夫妻/情侣"), ]
family_dtm <- data4Ana_dtm[which(data4Ana$TravelType == "家庭出行"), ]
friend_dtm <- data4Ana_dtm[which(data4Ana$TravelType == "朋友结伴"), ]
tbusiness_dtm <- data4Ana_dtm[which(data4Ana$TravelType == "商务出行"), ]

## 酒店类型 / 出游类型
dtm_list <- list(apartment = apartment_dtm, best = best_dtm, business = business_dtm,
                 chain = chain_dtm, youth = youth_dtm, alone = alone_dtm, 
                 couple = couple_dtm, family = family_dtm, friend = friend_dtm, 
                 tbusiness = tbusiness_dtm)

terms_MI <- lapply(dtm_list, result_featureSelect, stopword = stopword_cn,
                   corpus = corpus_tf, method = "MI")
terms_Chi2 <- lapply(dtm_list, result_featureSelect, stopword = stopword_cn,
                     corpus = corpus_tf, method = "Chi2")
terms_DF <- lapply(dtm_list, function(dtm) {
  result <- getDF(dtm)
  result <- result[!names(result) %in% stopword_cn]
  result <- result[order(result, decreasing = TRUE)]
  return(result)
})


terms <- read.csv("terms_MI.csv", as.is = TRUE)
View(Map(function(term, MI) MI[term], term = terms, MI = terms_MI))
unique(unlist(terms))



####    前二十位做自变量，评价做因变量做回归    ####
load("data4Ana.RData")
load("data4Ana_dtm.RData")

terms <- read.csv("terms_MI.csv", as.is = TRUE)

reg_result <- list(
  alone = summary(lm(Score ~ .,getlmData(data4Ana, data4Ana_dtm, terms[["alone"]], "独自旅行"))),
  apartment = summary(lm(Score ~ .,getlmData(data4Ana, data4Ana_dtm, terms[["apartment"]], "apartment"))),
  best = summary(lm(Score ~ .,getlmData(data4Ana, data4Ana_dtm, terms[["best"]], "best"))),
  business = summary(lm(Score ~ .,getlmData(data4Ana, data4Ana_dtm, terms[["business"]], "business"))),
  chain = summary(lm(Score ~ .,getlmData(data4Ana, data4Ana_dtm, terms[["chain"]], "chain"))),
  couple = summary(lm(Score ~ .,getlmData(data4Ana, data4Ana_dtm, terms[["couple"]], "夫妻/情侣"))),
  family = summary(lm(Score ~ .,getlmData(data4Ana, data4Ana_dtm, terms[["family"]], "家庭出行"))),
  friend = summary(lm(Score ~ .,getlmData(data4Ana, data4Ana_dtm, terms[["friend"]], "朋友结伴"))),
  tbusiness = summary(lm(Score ~ .,getlmData(data4Ana, data4Ana_dtm, terms[["tbusiness"]], "商务出行"))),
  youth = summary(lm(Score ~ .,getlmData(data4Ana, data4Ana_dtm, terms[["youth"]], "youth"))))


####    对含有关键词的评论进行抽样    ####

load("data4Ana.RData")
load("data4Ana_dtm.RData")
terms <- read.csv("terms_MI.csv", as.is = TRUE)
type_list <- names(terms)
sample <- lapply(type_list, getCAData2, term = terms, data = data4Ana, 
                 dtm = data4Ana_dtm, n = 30)
sample <- Reduce(rbind, sample)
write.csv(sample, "sample.csv", row.names = FALSE)


####    内容分析结果    ####

## 内容分析各type关键词积极消极数
CAData <- read.csv("sample_complete.csv", as.is = TRUE)
CAData$Attitude <- ifelse(is.na(CAData$Attitude), "nasi", 
                          ifelse(CAData$Attitude == 1, "pos", "neg"))
result <- by(CAData, list(CAData$Type), function(x) table(x$Term, x$Attitude))

terms <- read.csv("terms_MI.csv", as.is = TRUE)
terms <- terms[names(result)]

num_result <- Map(function(x, y) x[y, ], x = result, y = terms)

bin_result <- lapply(num_result, function(table) {
  apply(table, 1, function(word) {
    binom.test(x = word[3], n = sum(word[2:3]), p = 0.7270567)$p.value
  })
})
bin_result <- lapply(bin_result, as.data.frame)

showResult(reg_result, num_result, bin_result, "apartment")


## 有各个特征词的30条评论中积极消极比例(词语别)
words <- unique(unlist(terms[1:10]))
result <- lapply(words, function(word) {
  as.data.frame(lapply(result, function(x) {
    if (word %in% rownames(x)) {
      return(x[word, ])
    } else {
      return(c(NA,NA,NA))
    }
  }))
})
names(result) <- words


####    内容分析信赖性    ####

CAData <- read.csv("sample_complete.csv", as.is = TRUE)
set.seed(123)
reliability <- by(seq(nrow(CAData)), list(CAData$Term), sample, size = 10)
reliability <- CAData[unlist(reliability), ]
write.csv(reliability, "sample.csv", row.names = FALSE)

reliability <- read.csv("sample_reliability.csv", as.is = FALSE)
table(reliability$Attitude, useNA = "ifany")
table(reliability$Coder, useNA = "ifany")
reliability[5:6] <- lapply(reliability[5:6], 
                           function(x) ifelse(is.na(x), 0.5, x))
reliability$identical <- ifelse(reliability$Attitude == reliability$Coder, 
                                1, 0)
result <- aggregate(reliability$identical, list(reliability$Term),
                    function(x) sum(x) / 10)
result
#######################################################################
####    做成所有数据的非tm格式的分词结果(带词性)?    ####

load("data4Ana.RData")
data4Ana_segNt <- segmentCN(data4Ana[, "Review"], nature = TRUE)
save(data4Ana_segNt, file = "data4Ana_segNt.RData")

####    抽样500个，用关键词做评定项目，内容分析?    ####

apartment_sample <- getCAData(data4Ana, terms[["apartment"]], 500, "apartment")
write.csv(apartment_sample, file = "apartment_sample.csv", row.names = FALSE, na = "")

best_sample <- getCAData(data4Ana, terms[["best"]], 500, "best")
write.csv(best_sample, file = "best_sample.csv", row.names = FALSE, na = "")
####    方差分析?    ####
k <- read.csv("apartment_sample.csv", as.is = TRUE)
k <- read.csv("best_sample.csv", as.is = TRUE)
k <- k[, 2:8]
k <- k[, c(2,23:28)]
k[2:7] <- lapply(k[2:7], function(x) ifelse(is.na(x), "nasi", ifelse(x == 1, "pos", "neg")))
m <- k[[7]]
fit <- aov(Score ~ m, data = k)
aggregate(k$Score, list(m), mean)
aggregate(k$Score, list(m), length)
summary(fit)
TukeyHSD(fit)
library(car)
bartlett.test(Score ~ 設備, data = k)
####    因子分析?    ####

load("data4Ana.RData")
seg <- data4Ana_segNt[which(data4Ana$HotelType == "apartment")]
seg <- unlist(seg)
seg <- data.frame(Part = names(seg), Word = seg, stringsAsFactors = FALSE)
part_list <- list("ag","a","ad","an","b","c","dg","d","e","f","g","h","i","j","k","l","m","ng",
                  "n","nr","ns","nt","nz","o","p","q","r","s","tg","t","u","vg","v","vd","vn","w",
                  "x","y","z","userDefine")
seg <- lapply(part_list, function(x) table(seg$Word[which(seg$Part == x)]))
seg <- lapply(seg, function(x) x[order(x, decreasing = TRUE)])
names(seg) <- unlist(part_list)
apartment_dtm <- data4Ana_dtm[which(data4Ana$HotelType == "apartment"), ]
terms <- removeSparseTerms(apartment_dtm, 0.98)$dimnames$Terms
terms <- terms[terms %in% unlist(lapply(seg[c("an","n","vn","ns","userDefine")], names))]

k <- apartment_dtm[, terms]
k <- as.matrix(k)
library(psych)
k <- k[rowSums(k) != 0, ]
fa.parallel(k)
fa <- fa(k, nfactors = 6)
fa.diagram(fa)
##################################################
load("data4Ana1_dtm.RData")
load("data4Ana1_seg.RData")
load("data4Ana.RData")
load("mykeyword.RData")

mykeyword <- k
TF_Cross <- getTFbyLabel(data4Ana1_seg, mykeyword, data4Ana$HotelType)
TF_PercentCross <- getPercentCross(TF_Cross)

DF_Cross <- getDFbyLabel(data4Ana_dtm, mykeyword, data4Ana$HotelType)
DF_PercentCross <- getPercentCross(DF_Cross)
num_each_type <- table(data4Ana$HotelType)

## 各类型中关键词按文书频率排列
result <- as.data.frame(Map(function(x, y) as.vector(x)/y*100, DF_Cross, num_each_type),
                        row.names = row.names(DF_Cross))
result[] <- lapply(result, function(v, name) {
  index_top_10 <- order(v, decreasing = TRUE)
  num_top_10 <- sapply(round(v[index_top_10], 2), as.String)
  term_top_10 <- name[index_top_10]
  return(paste(term_top_10, " (", num_top_10, "%)", sep = ""))
}, name = row.names(result))
row.names(result) <- seq(nrow(result))
write.csv(result, file = "1.csv")

## 各类型中关键词的相对频率
result <- DF_PercentCross
result[] <- lapply(result, function(v, name) {
  index_top_10 <- order(v, decreasing = TRUE)
  num_top_10 <- sapply(round(v[index_top_10], 2), as.String)
  term_top_10 <- name[index_top_10]
  return(paste(term_top_10, " (", num_top_10, "%)", sep = ""))
}, name = row.names(result))
row.names(result) <- seq(nrow(result))
write.csv(result, file = "2.csv")
