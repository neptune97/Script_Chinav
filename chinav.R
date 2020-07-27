#Análise da #ChinaVirus
library (tm)
library (wordcloud)
library (ggplot2)

#Abrindo bd
chinav <- read_csv("~/scrap/proj_chivanirus30/tweets.csv")

#Limpeza do banco de dados
##eliminando acentos
chinav$tweet <- stringi::stri_trans_general(chinav$tweet, "Latin-ASCII")

#coletando tweets
#construindo corpus
mycorpus <- Corpus(VectorSource(chinav$tweet), readerControl = list(language = "pt"))
mycorpus <- tm_map(mycorpus, content_transformer(tolower))

#Removendo URLS'S, emojis e html content
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
mycorpus <- tm_map(mycorpus, content_transformer(removeURL))

removeURL2 <- function(x) gsub("\\s*[^[:space:]/]+/[^[:space:]/]+", "", x)
mycorpus<- tm_map(mycorpus, content_transformer(removeURL2))

removeEMOJI <- function(x) gsub("[^\x01-\x7F]","", x)
mycorpus <- tm_map (mycorpus, content_transformer(removeEMOJI))

removeC <- function(x) gsub("[\r\n]", "", x)
mycorpus<- tm_map(mycorpus, content_transformer(removeC))

#Removendo pontuação e números
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, removeNumbers)

#STOPWORDS
mycorpus <- tm_map(mycorpus, removeWords, c(stopwords(kind = "pt")))

#STEMMING
mycorpus2 <- mycorpus
mycorpus <- tm_map (mycorpus, stemDocument)

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

mycorpus <- lapply(mycorpus, stemCompletion2, dictionary=mycorpus2)
myCorpus <- Corpus(VectorSource(mycorpus))

#TDM

tdm <- TermDocumentMatrix(myCorpus, control=list(removePunctuation = TRUE,
                                                 stopwords = c((kind = "pt"), "character", "datetimestamp", "description",
                                                               "desde", "id", "hour", "heading", "listsec", "mday",
                                                               "meta", "min", "mon","ja","origin", "wday", "yday","year",
                                                               "language", "isdst", "r","listauthor","listcontent", "m",
                                                               "pr","f", "of", "the", "i", "b", "nao", "ja", "pra", "so",
                                                               "bi", "mi", "am", "h", "g","s", "c", "work", "q", "om","la",
                                                               "kkkkkk", "aaaaa","prestahahahahahahahahahaa", "ai", "vc"
                                                 ),
                                                 removeNumbers = TRUE, wordLengths=c(1,Inf)))
findFreqTerms(tdm, lowfreq = 10)
findAssocs(tdm, "cloroquina", 0.25)


#Nuvem de palavras
#criando matrix palavras/freq
m <- as.matrix(tdm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)

#criando paleta de cores
mc <- colors()[c(270,50,258)]

# nuvem de palavras
wordcloud(words=names(wordFreq), freq=wordFreq,random.order=F, max.words = 150,
          colors=mc)

