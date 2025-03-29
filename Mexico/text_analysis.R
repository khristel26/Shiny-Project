# Text analysis of Variables That Predict Verdicts in Domestic Violence Cases

setwd("D:\\DAP 2\\final-project-diego_khristel")

library(tidyverse)
library(rvest)
library(tidytext)
library(udpipe)
library(pdftools)
library(tm)
library(textdata)
# install.packages("wordcloud")
library(wordcloud2)
# install.packages("webshot")
library(webshot)
library(htmlwidgets)



# load the policy documents from both Mexico and Peru

mimp_peru <- pdf_text("Data\\Peru\\MIMP-violencia-basada_en_genero.pdf")

mpg_mexico <- pdf_text("Data\\Mexico\\Manual_Violencia_de_G_nero_en_Diversos_Contextos2.pdf")



mexico <- tibble(text=mpg_mexico)
word_tokens_mexico <- unnest_tokens(mexico, word_tokens,  text, token = "words")
sentence_tokens_mexico <- unnest_tokens(mexico, sent_tokens,  text, token = "sentences")
ngram_tokens_mexico <-    unnest_tokens(mexico, ngram_tokens, text, token = "ngrams", n = 2)

peru <- tibble(text=mimp_peru)
word_tokens_peru <- unnest_tokens(peru, word_tokens,  text, token = "words")
sentence_tokens_peru <- unnest_tokens(peru, sent_tokens,  text, token = "sentences")
ngram_tokens_peru<-    unnest_tokens(peru, ngram_tokens, text, token = "ngrams", n = 2)

# Adding spanish words to stop words using the tm library

stop_words_spanish  <-  bind_rows(stop_words, 
                                  data_frame(word = stopwords("spanish"),
                                          lexicon = "custom"))

peru_no_sw <- anti_join(word_tokens_peru, stop_words_spanish, by = c("word_tokens" = "word")) 
mexico_no_sw <- anti_join(word_tokens_mexico, stop_words_spanish, by = c("word_tokens" = "word"))
  



sentiment_nrc <-   
  get_sentiments("nrc") %>%
  rename(nrc = sentiment)
sentiment_afinn <- 
  get_sentiments("afinn") %>%
  rename(affin = value)
sentiment_bing <-  
  get_sentiments("bing") %>%
  rename(bing = sentiment)

# Plotting the Peruvian policy document

peru_no_sw <- peru_no_sw %>%
  left_join(sentiment_nrc, by = c("word_tokens" = "word")) %>%
  left_join(sentiment_afinn, by = c("word_tokens" = "word")) %>%
  left_join(sentiment_bing, by = c("word_tokens" = "word"))

ggplot(data = filter(peru_no_sw, !is.na(bing))) +
  geom_histogram(aes(bing, fill=bing), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  labs(title = "Peruvian Gender Based Policy Conceptual Framework Sentiment (Bing)") + 
  theme(legend.position = "none")

ggsave("images\\peru1.png")

ggplot(data = filter(peru_no_sw, !is.na(affin))) +
  geom_histogram(aes(affin, fill=affin<0), stat = "count") +
  scale_x_continuous(n.breaks = 7) +
  labs(title = "Peruvian Gender Based Policy Conceptual Framework Statement (AFFIN)") + 
  theme(legend.position="none")

ggsave("images\\peru2.png")

# Plotting the Mexican policy document



mexico_no_sw <- mexico_no_sw %>%
  left_join(sentiment_nrc, by = c("word_tokens" = "word")) %>%
  left_join(sentiment_afinn, by = c("word_tokens" = "word")) %>%
  left_join(sentiment_bing, by = c("word_tokens" = "word"))

ggplot(data = filter(mexico_no_sw, !is.na(bing))) +
  geom_histogram(aes(bing, fill=bing), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  labs(title = "Mexican Gender Based Policy Conceptual Framework Sentiment (Bing)") + 
  theme(legend.position = "none")

ggsave("images\\Mexico1.png")

ggplot(data = filter(mexico_no_sw, !is.na(affin))) +
  geom_histogram(aes(affin, fill=affin<0), stat = "count") +
  scale_x_continuous(n.breaks = 7) +
  labs(title = "Mexican Gender Based Policy Conceptual Framework Statement (AFFIN)") + 
  theme(legend.position="none")

ggsave("images\\mexico2.png")

# Create wordclouds

mexico_wordnumber <- table(mexico_no_sw$word_tokens)
mexico_wordcloud <-wordcloud2(data=tail(mexico_wordnumber, 500), size=5)


peru_wordnumber <- table(peru_no_sw$word_tokens)
peru_wordcloud <- wordcloud2(data=tail(peru_wordnumber, 500), size=5)


# webshot::install_phantomjs()
saveWidget(mexico_wordcloud, "images\\mexico_wordcloud.html", selfcontained = FALSE)
webshot("images\\mexico_wordcloud.html", "images\\mexico_wordcloud.png",vwidth = 1992, vheight = 1744)

saveWidget(peru_wordcloud, "images\\peru_wordcloud.html", selfcontained=FALSE)
webshot("images\\peru_wordcloud.html", "images\\peru_wordcloud.png", vwidth=1992, vheight=1774)
