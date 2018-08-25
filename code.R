library('tidyverse')
library("tm")
library("SnowballC")
library("wordcloud")

lyrics <- read_csv('thankyoufortoday.csv')

each.line <- lyrics %>% group_by(line) %>% 
  summarise(freq = n()) %>% arrange(-freq) %>% filter(line != 'Oh oh oh oh oh')

set.seed(1234)

png("wordcloud.png", width=10, height=10, units="in", res=300)

wordcloud(words = each.line$line, freq = each.line$freq, 
          min.freq = 2, scale=c(3,.1),
          max.words = 30000000000, random.order=FALSE, rot.per = 0)

dev.off()

# for each word ... not super compelling
docs <- Corpus(VectorSource(lyrics[lyrics$chorus_or_verse != 'chorus', ]$line))
docs <- Corpus(VectorSource(lyrics$line))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)

d <- d %>% filter( word != 'the' & word != 'and'& word != 'you'& word != 'your' & word != 'for' & word != 'that' & word!= 'like')

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words = 100, random.order=FALSE) 
