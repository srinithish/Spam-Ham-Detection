library(wordnet)
library(tm)
?stopwords()
?VectorSource
library(data.table)
library(dplyr)
library(dtplyr)
library(caret)
library(xgboost)

SmsRaw = fread(file.choose())
glimpse(SmsRaw)
SmsRaw = SmsRaw %>% mutate(type = as.factor(type))
smscorpus = VCorpus(VectorSource(SmsRaw$text))
smsTestCorpus = VCorpus(VectorSource(SmsRaw$text[nrow(SmsRaw)]))
textMatrix = DocumentTermMatrix(smscorpus,list(removePunctuation = T,stopwords = TRUE,
                                               stemming = TRUE,
                                               removeNumbers = TRUE
                                               
                                               )
                                )

testTextMatrix =  DocumentTermMatrix(smsTestCorpus,list(removePunctuation = T,stopwords = TRUE,
                                                                stemming = TRUE,
                                                                removeNumbers = TRUE
                                                                
                                                    )
                                )

trainData = as.matrix(textMatrix)
testData = as.matrix(testTextMatrix)

discardlater = bind_rows(as.data.frame(trainData),as.data.frame(testData))
discardlater[nrow(discardlater),1:10]
tail.matrix(discardlater[,1:6])
head.matrix(discardlater[,1:6])
mutate_all(discardlater,funs(coalesce(.,0L)))[nrow(discardlater),]
discardlater = sapply(as.data.frame(tail(discardlater)),FUN= function(x) x[is.na(x)] = 0)
discardlater[is.na(discardlater)] = 0
discardlater = as.data.frame(discardlater)
?sapply
class(discardlater)
?coalesce()
fix(trainData)
# trainData = bind_cols(as.data.frame(trainData),as.data.frame(SmsRaw$type))
str(SmsRaw$type)
class(trainData)
class(trainForXG)
typeof(trainForXG)
trainData  = as.numeric(as.data.frame(trainData))

# numeric matrix conversion
trainForXG = apply(trainData,2,FUN = as.numeric)
testForXG = apply(discardlater[(nrow(discardlater)-1):nrow(discardlater),],2,FUN = as.numeric)
class(testForXG)


nrow(trainData)
class(m)
rownames(trainData)
dim(m)
dim(trainData)
colnames(m) = colnames(trainData)
write.csv(trainData,"TrainDataCSV")
fix(m)

# waste code ignore
trainData = cbind(trainData,SmsRaw$type)
colnames(trainData)[ncol(trainData)] = "Spam_NoSpam"
trainData = as.data.frame(trainData)
trainData = trainData %>% mutate(Spam_NoSpam = as.factor(Spam_NoSpam))
trainData = trainData %>% select(-Spam_NoSpam)
trainData = as.matrix(as.integer(trainData))



###working code
myLabel = if_else(SmsRaw$type == "spam",1,0)
trainXGBMatrix = xgb.DMatrix(trainData,label = myLabel)
str(trainData)
head(trainData)
partTrain = createDataPartition(SmsRaw$type,p=0.7)
?createDataPartition



testModel = train(Spam_NoSpam ~ .,data = trainData[1:100,],method = 'bayesglm')

partTrain$Resample1
smsXGBoost <- xgboost(data = trainXGBMatrix, label = as.factor(myLabel), max_depth = 4,
                       eta = 0.1, nthread = 2, nrounds = 80,objective = "binary:logistic")

prediction = predict(smsXGBoost,newdata = testForXG)
prediction

bind_cols()
ncol(trainForXG)
