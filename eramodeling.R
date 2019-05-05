era_analysis = function(text, language, modeling) {
  if(language == "English"){
    englishyears = c("1066", "1500","1660","1785","1832","1901", "1914")
    English_1066$year = rep("1066", nrow(English_1066))
    English_1500$year = rep("1500", nrow(English_1500))
    English_1660$year = rep("1660", nrow(English_1660))
    English_1785$year = rep("1785", nrow(English_1785))
    English_1832$year = rep("1832", nrow(English_1832))
    English_1901$year = rep("1901", nrow(English_1901))
    English_1914$year = rep("1901", nrow(English_1914))
    work = rbind(English_1066, English_1500, English_1660, English_1785, English_1832, English_1901, English_1914)
    stopped = as.data.frame(stopwords(kind = "en"))
    colnames(stopped) <- c("word")
  }

  if(language == "Italian"){
    italianyears = c("1200","1400","1550","1700","1815","1915")
    Italian_1200$year = rep("1200", nrow(Italian_1200))
    Italian_1400$year = rep("1400", nrow(Italian_1400))
    Italian_1550$year = rep("1550", nrow(Italian_1550))
    Italian_1700$year = rep("1700", nrow(Italian_1700))
    Italian_1815$year = rep("1815", nrow(Italian_1815))
    Italian_1915$year = rep("1915", nrow(Italian_1915))
    work = rbind(Italian_1200,Italian_1400,Italian_1550,Italian_1700,Italian_1815,Italian_1915)
    stopped = as.data.frame(stopwords(kind = "it"))
    colnames(stopped) <- c("word")
  }
  
  if(language == "Spanish"){
    spanishyears = c("1400","1600","1700","1800","1850", "1900")
    Spanish_1400$year = rep("1400", nrow(Spanish_1400))
    Spanish_1600$year = rep("1600", nrow(Spanish_1600))
    Spanish_1700$year = rep("1700", nrow(Spanish_1700))
    Spanish_1800$year = rep("1800", nrow(Spanish_1800))
    Spanish_1850$year = rep("1850", nrow(Spanish_1850))
    Spanish_1900$year = rep("1900", nrow(Spanish_1900))
    work = rbind(Spanish_1400,Spanish_1600,Spanish_1700,Spanish_1800,Spanish_1850,Spanish_1900)
    stopped = as.data.frame(stopwords(kind = "es"))
    colnames(stopped) <- c("word")
  }
  library(tidytext)
  library(stringr)
  library(tidyr)
  
  by_word <- work %>%
    unnest_tokens(word, text)
  
  word_counts <- by_word %>%
    anti_join(stopped) %>%
    count(year, word, sort = TRUE)
  
  topic_dtm <- word_counts %>%
    cast_dtm(year, word, n)
  
  
  library(topicmodels)
  topic_lda <- LDA(topic_dtm, k = 6, control = list(seed = 1234))
  
  topic_lda_td <- tidy(topic_lda)
  
  topic1 = filter(topic_lda_td, topic == 1)
  topic2 = filter(topic_lda_td, topic == 2)
  topic3 = filter(topic_lda_td, topic == 3)
  topic4 = filter(topic_lda_td, topic == 4)
  topic5 = filter(topic_lda_td, topic == 5)
  topic6 = filter(topic_lda_td, topic == 6)
  
  text_df = data.frame(text = text)
  wordcount = text_df %>% unnest_tokens(word, text) %>%
    count(word)
  topic1scores = inner_join(topic1, wordcount, by = term)
  topic2scores = inner_join(topic2, wordcount, by = term)
  topic3scores = inner_join(topic2, wordcount, by = term)
  topic4scores = inner_join(topic2, wordcount, by = term)
  topic5scores = inner_join(topic2, wordcount, by = term)
  topic6scores = inner_join(topic2, wordcount, by = term)
  
  
  topic1score = 0
  for (i in 1:nrow(topic1scores)) {
    topic1score = topic1score + topic1scores$count * topic1scores$beta
  }
  
  topic2score = 0
  for (i in 1:nrow(topic2scores)) {
    topic2score = topic2score + topic2scores$count * topic2scores$beta
  }
  topic3score = 0
  for (i in 1:nrow(topic3scores)) {
    topic3score = topic3score + topic3scores$count * topic3scores$beta
  }
  topic4score = 0
  for (i in 1:nrow(topic4scores)) {
    topic4score = topic4score + topic4scores$count * topic4scores$beta
  }
  topic5score = 0
  for (i in 1:nrow(topic5scores)) {
    topic5score = topic5score + topic5scores$count * topic5scores$beta
  }
  topic6score = 0
  for (i in 1:nrow(topic6scores)) {
    topic6score = topic6score + topic6scores$count * topic6scores$beta
  }
  
  
  topic_lda_gamma <- tidy(topic_lda, matrix = "gamma")
  book_topics <- topic_lda_gamma %>%
    group_by(document) %>%
    top_n(1, gamma) %>%
    ungroup() %>%
    arrange(gamma)
  scores = rbind(topic1score,topic2score,topic3score,topic4score,topic5score,topic6score)
  if(language == "English"){
    scores = rbind(scores, topic7score)
  }
  finalscores = inner_join(scores, book_topics, by = topic)
  return(finalscores)
}