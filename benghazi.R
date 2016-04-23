library(devtools)
library("ROAuth")
library(twitteR)
library("wordcloud")
library("tm")


reqURL <- "https://apps.twitter.com/app/9392321"
accessURL <- "https://apps.twitter.com/app/9392321/access_token"
authURL <- "https://apps.twitter.com/app/9392321/authorize"

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'yT9rztl1L1u5dsnJ1erbrYctC'
consumer_secret <- 'PYTrEEMq6UfJ4aUVCVRPIZlI4NcEyMzdKCJywGpeHB5lGWv6fu'
access_token <- '4805728995-pjgiZkeOKrbU4j1mVUv8dqh1G632M0dF1z9hwGI'
access_secret <- 'ArvVBDmZ6vxLDv5RqgL35g9NdvTMdAzkAECC8DzgPsNvq'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

ben <- searchTwitter("#benghazi", n=2000, lang="en")
ben_text <- do.call("rbind", lapply(ben, as.data.frame))  #save text: deals with the emoji issue

ben_text$text <- sapply(ben_text$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
ben_corpus <- Corpus(VectorSource(ben_text$text))     #create corpus

myStopwords <- c(stopwords('english'), "available", "via")
idx <- which(myStopwords == "r")
myStopwords <- myStopwords[-idx]
myCorpus <- tm_map(ben_corpus, removeWords, myStopwords)

btdm1 = TermDocumentMatrix(myCorpus,control = list(removePunctuation = TRUE,
          stopwords = c("benghazi", "Benghazi", stopwords("english")), removeNumbers = TRUE, tolower = TRUE))
m1 = as.matrix(btdm1)
# get word counts in decreasing order
word_freqs1 = sort(rowSums(m1), decreasing = TRUE) 
# create a data frame with words and their frequencies
dm1 = data.frame(word = names(word_freqs1), freq = word_freqs1)
wordcloud(dm1$word, dm1$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

dm2 <- dm1[-c(5, 13, 18, 19), ]  
dm3 <- dm2[1:150,]
wordcloud(dm2$word, dm2$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
wordcloud(dm3$word, dm3$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

