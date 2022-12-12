

install.packages("XML")


install.packages("tm")
install.packages("tm.corpus.Reuters21578", repos = "http://datacube.wu.ac.at")
library(tm)
library(tm.corpus.Reuters21578)

install.packages("stopwords")
install.packages("wordcloud")
library(stopwords)

install.packages("textstem")

data(Reuters21578)

Reut_lists <-lapply(Reuters21578, FUN=unlist)
names(Reut_lists[[1]])

Reut_content <- lapply(Reut_lists, function(x) x[names(x) == "content"])

#기사 내용 추출
Reut_topics <- lapply(Reut_lists, function(x) x[names(x) == "meta.topics_cat"])

#주제 추출
sort(table(unlist(Reut_topics)), decreasing = T)
Reut_content <- Reut_content[Reut_topics=="money-fx" | Reut_topics=="interest"]
Reut_topics <- Reut_topics[Reut_topics=="money-fx" | Reut_topics=="interest"]

#텍스트 전처리.
Reut_content <- gsub(Reut_content, pattern = "\n", replacement = " ")
Reut_content <- gsub(Reut_content, pattern = "'s", replacement = "")
Reut_content <- gsub(Reut_content, pattern = "([^[:alnum:][:blank:]'-])", replacement = "")
Reut_content <- tolower(Reut_content)
Reut_content <- strsplit(Reut_content," ")

which(Reut_content=="character0")
Reut_topics <- Reut_topics[Reut_content!="character0"]
Reut_content <- Reut_content[Reut_content!="character0"]
library(stopwords)

Reut_content <- lapply(Reut_content, function(x) x[! x %in% c(stopwords(), "")])
Reut_content <- lapply(Reut_content, function(x) gsub(x, pattern = "'", replacement = ""))
library(textstem)
Reut_content <- lapply(Reut_content, lemmatize_strings)

Reut_lev <- sort(unique(unlist(Reut_content)))
Reut_DTM <- lapply(Reut_content, FUN = function(x, lev){table(factor(x, lev, ordered = T))},
                   lev = Reut_lev )

Reut_DTM <- matrix(unlist(Reut_DTM), nrow = length(Reut_DTM), byrow = TRUE)

dim(Reut_DTM)
sum(Reut_DTM>0) # 0 아닌 셀
sum(Reut_DTM==0) # 0인 셀
1-sum(Reut_DTM>0)/sum(Reut_DTM>=0) # 희소도
colnames(Reut_DTM) <- Reut_lev #Reut_DTM 각 열을 단어로

Reut_table <- sort(table(unlist(Reut_content)), decreasing = T)
Reut_table

barplot(Reut_table[1:32])
library(wordcloud)
wordcloud(words = names(Reut_table), freq = Reut_table, max.words = 200,
          random.order = F)
