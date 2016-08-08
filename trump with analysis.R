##########Load the necessary packages##########

library(devtools)
library(ROAuth)
library(twitteR)
library(tm)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(e1071)
library(RTextTools)

#also check this for more text analyis: http://www.r-bloggers.com/intro-to-text-analysis-with-r/

##########Step 2:  Connect to twitter##########

reqURL <- "https://apps.twitter.com/app/9392321"
accessURL <- "https://apps.twitter.com/app/9392321/access_token"
authURL <- "https://apps.twitter.com/app/9392321/authorize"

#necessary file for Windows
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'yT9rztl1L1u5dsnJ1erbrYctC'
consumer_secret <- 'PYTrEEMq6UfJ4aUVCVRPIZlI4NcEyMzdKCJywGpeHB5lGWv6fu'
access_token <- '4805728995-pjgiZkeOKrbU4j1mVUv8dqh1G632M0dF1z9hwGI'
access_secret <- 'ArvVBDmZ6vxLDv5RqgL35g9NdvTMdAzkAECC8DzgPsNvq'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

#the cainfo parameter is necessary only on Windows
register_mysql_backend("mv", user='root', password='Belly890', host='127.0.0.1')

##########Step 3: Acquire DATA##########

search_twitter_and_store("#Trump", table_name = "tweets1", since='2016-1-12', until= '2016-1-13')
search_twitter_and_store("#Trump", table_name = "tweets2", since='2016-1-11', until= '2016-1-12')
search_twitter_and_store("#Trump", table_name = "tweets3", since='2016-1-10', until= '2016-1-11')
search_twitter_and_store("#Trump", table_name = "tweets4", since='2016-1-9', until= '2016-1-10')
search_twitter_and_store("#Trump", table_name = "tweets5", since='2016-1-8', until= '2016-1-9')
search_twitter_and_store("#Trump", table_name = "tweets6", since='2016-1-7', until= '2016-1-8')
search_twitter_and_store("#Trump", table_name = "tweets7", since='2016-1-6', until= '2016-1-7')

#get sample data
trump1 <- searchTwitter("#Trump", n=500, lang="en", resultType="recent")

#need to filter out news updates  RT@

##########clean and prepare the data##########
trump1.1 <- do.call("rbind", lapply(trump1, as.data.frame))  #save text: deals with the emoji issue
trump1.2 <- trump1.1$text
#create column with response valence

# remove retweet entities
trump3 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", trump1.2)
# remove at people
trump3 = gsub("@\\w+", " ", trump3)
# remove punctuation
trump3 = gsub("[[:punct:]]", "", trump3)
# remove numbers
trump3 = gsub("[[:digit:]]", " ", trump3)
# remove html links
trump3 = gsub("http\\+", " ", trump3)
# remove unnecessary spaces
trump3 = gsub("[ \t]{2,}", " ", trump3)
trump3 = gsub("^\\s+|\\s+$", " ", trump3)
trump3 = gsub("amp", "", trump3)             #because $ got turned into amp

#need to filter out news updates  RT@

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
trump4 = sapply(trump3, try.error)

# remove NAs in some_txt
trump5 = trump4[!is.na(trump4)]
names(trump5) = NULL

write.table(trump5, file = "tr.csv", sep = ",", col.names = NA)

trump10 <- read.csv("tr2.csv", header=FALSE, sep = ",")

##########Step 4: Sentiment Analysis##########

trump_all <- trump10[,1]
trump_sentiment <- trump10[,2]

# naive bayes
mat= create_matrix(trump_all, language="english", 
                   removeStopwords=FALSE, removeNumbers=TRUE, 
                   stemWords=FALSE, tm::weightTfIdf)

mat = as.data.frame(lapply(data.frame(mat), as.factor))

classifier = naiveBayes(mat[1:115,], as.factor(trump_sentiment[1:115]))
predicted = predict(classifier, mat[116:145,]); predicted

table(trump_sentiment[116:145], predicted)
recall_accuracy(trump_sentiment[116:145], predicted)
#.67

##########graph##########
names(trump10)[names(trump10)=="V2"] <- "valence"
names(trump10)[names(trump10)=="V1"] <- "tweets"

##########Other methods##########

mat= create_matrix(trump_all, language="english", 
                   removeStopwords=FALSE, removeNumbers=TRUE, 
                   stemWords=FALSE, tm::weightTfIdf)

container = create_container(mat, as.numeric(trump_all),
                             trainSize=1:115, testSize=116:145,virgin=FALSE) #removeSparseTerms

models = train_models(container, algorithms=c("MAXENT",
                                              "SVM",
                                              #"GLMNET", "BOOSTING", 
                                              "SLDA","BAGGING", 
                                              "RF", # "NNET", 
                                              "TREE" 
))

# test the model
results = classify_models(container, models)

table(as.numeric(as.numeric(sentiment_all[161:180])), results[,"FORESTS_LABEL"])

recall_accuracy(as.numeric(as.numeric(sentiment_all[161:180])), results[,"FORESTS_LABEL"])

##########test SVM##########
library(e1071)
data(cats, package="MASS")
inputData <- data.frame(cats[, c (2,3)], response = as.factor(cats$Sex)) # response as factor

##########test pulling saved mySQL tables##########
db = dbConnect(MySQL(), user='root', password='Belly890', dbname='mv', host='127.0.0.1')

db1 <- dbGetQuery(db, "
                  SELECT *
                  FROM tweets 
                  ")

#got ya!
