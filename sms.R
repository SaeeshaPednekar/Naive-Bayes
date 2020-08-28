sms<-read.csv("F:/Excelr/Assignments/dataset/naive bayes/sms_raw_NB.csv",stringsAsFactors = F)
str(sms)

sms$type<-factor(sms$type)
str(sms)
 table(sms$type)

 install.packages("tm") 
 library(tm)
 #prepare corpuse for data
 sms_corpuse<-Corpus(VectorSource(sms$text))
sms_corpuse$content[1:10] 
 
?tm_map
#cleaning of the data
corpus_clean<-tm_map(sms_corpuse,tolower)
corpus_clean<-tm_map(corpus_clean,removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords,stopwords())
corpus_clean<-tm_map(corpus_clean,removePunctuation)


remove_num_punc<-function(x) gsub("[^[:alpha:][:space:]]*","",x)
corpus_clean<-tm_map(corpus_clean,content_transformer(remove_num_punc))
corpus_clean<-tm_map(corpus_clean,stripWhitespace)

class(corpus_clean)

corpus_clean$content[1:10]


sms_dtm<-DocumentTermMatrix(corpus_clean)
class(sms_dtm)
  
#creating testing and training dataset
sms_train<-sms[1:4169,]
sms_test<-sms[4169:5559,]

sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4169:5559,]

sms_corpus_train<-corpus_clean[1:4169]
sms_corpus_test<-corpus_clean[4169:5559]





prop.table(table(sms$type))

prop.table(table(sms_test$type))


#frequent words
sms_dict<-findFreqTerms(sms_dtm_train,5)
list(sms_dict[1:100])



sms_train_1<-DocumentTermMatrix(sms_corpus_train,list(sms_dict))
sms_test_1<-DocumentTermMatrix(sms_corpus_test,list(sms_dict))


convert_counts<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels=c(0,1),labels=c("no","yes"))
}
?apply
sms_train_1<-apply(sms_train_1,MARGIN = 2,convert_counts)
sms_test_1<-apply(sms_test_1,MARGIN = 2,convert_counts)

View(sms_test_1)



#training on the model data
install.packages("e1071")
library(e1071)
sms_classifier<-naiveBayes(sms_train_1,sms_train$type)
sms_classifier$levels


sms_test_pred<-predict(sms_classifier,sms_test_1)
sms_test_pred[1:25]


table1<-table(sms_test_pred,sms_test$type)
table(sms_test$type,sms_test_pred)


install.packages("gmodels")
library(gmodels)
CrossTable(sms_test_pred,sms_test$type,prop.chisq=FALSE,prop.t=FALSE,prop.R=FALSE,
           dnn=c('predicted','actual'))

sms_classifier2<-naiveBayes(sms_train_1,sms_train$type,laplace=9)
sms_test_pred2<-predict(sms_classifier2,sms_test_1)

table2<-table(sms_test_pred2,sms_test$type)

CrossTable(sms_test_pred2,sms_test$type,prop.chisq=FALSE,prop.t=FALSE,prop.R=FALSE,
           dnn=c('predicted','actual'))

accuracy1<-(sum(diag(table1))/sum(table1))

accuracy2<-(sum(diag(table2))/sum(table2))

mean(sms_test_pred==sms_test$type)

mean(sms_test_pred2==sms_test$type)


#the first gives better accuracy