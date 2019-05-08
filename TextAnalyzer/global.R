library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(readtext)
library(syuzhet)
library(purrrlyr)
library(topicmodels)
library(ggwordcloud)


##Model Variables
en_bigram = readRDS("../data/en_bigram.rds")
it_bigram = readRDS("../data/it_bigram.rds")
es_bigram = readRDS("../data/es_bigram.rds")
en_trigram = readRDS("../data/en_trigram.rds")
it_trigram = readRDS("../data/it_trigram.rds")
es_trigram = readRDS("../data/es_trigram.rds")
en_qgram = readRDS("../data/en_qgram.rds")
it_qgram = readRDS("../data/it_qgram.rds")
es_qgram = readRDS("../data/es_qgram.rds")

English = readRDS("../data/english.rds")
Spanish = readRDS("../data/spanish.rds")
Italian = readRDS("../data/italian.rds")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Language Analysis Section ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# find a better way to tokenize, exclude number?
# Code derived from www.tidytextmining.com/ngrams.html
create_ngram_model = function(books, number = 2) {
  colnames = paste0("word", 1:number)
  ngram = books %>% 
    unnest_tokens(output = ngram, input = text, token = "ngrams", n = number) %>% 
    separate(ngram, colnames, sep = " ") %>% 
    plyr::count(colnames)
  return (ngram)
}

row_wise_laplace = function(target, model, V, number = 2) {
  targetdf = model[model$word1 == target$word1,]
  
  i = 2
  while (length(targetdf) & (i <= (number - 1))) { 
    targetdf = targetdf[targetdf[[i]] == target[[i]],]
    i = i + 1
  }
  
  numer = 1
  num_value = 0
  denom = V
  
  if (nrow(targetdf) > 0) {
    targetdf_n = targetdf[targetdf[[number]] == target[[number]],]
    
    if (nrow(targetdf_n)) {
      num_value = targetdf_n$freq
    }
    
    numer = 1 + num_value
    denom = V + sum(targetdf$freq)
  }
  
  return (log(numer/denom) * target$freq)
}

# this is laplace smoothing
# returns log(probability), e^return_value to get the probability
ngram_evaluator_laplace = function(test_ngram, model, number = 2) {
  V = length(unique(model[["word1"]]))
  prob = 0
  comb = test_ngram %>% by_row(row_wise_laplace, model = model, V = V, number = number, .collate = "rows")
  return (sum(comb$.out))
}

# if x is large enough just consider itself
# Katz gave me 5
get_const = function(freq, freq_array) {
  if (freq < 5) {
    # because array starts from 1
    return ((freq + 1) * freq_array[freq] / freq_array[freq - 1])
  } else {
    return (freq)
  }
}

row_wise_gt = function(target, model, V, freq_arr, number = 2) {
  targetdf = model[model$word1 == target$word1,]
  seen_pairs = nrow(model)
  
  i = 2
  while (nrow(targetdf) & (i <= (number - 1))) { 
    targetdf = targetdf[targetdf[[i]] == target[[i]],]
    i = i + 1
  }
  
  denom = seen_pairs
  num_value = 0
  numer = 0
  
  if (nrow(targetdf) > 0) {
    targetdf_n = targetdf[targetdf[[number]] == target[[number]],]
    denom = sum(targetdf$freq)
    
    if (nrow(targetdf_n) > 0) {
      num_value = targetdf_n$freq
    }
  }
  
  if (num_value < 5) {
    numer = (num_value + 1) * freq_arr[num_value + 2] / freq_arr[num_value + 1]
  } else {
    numer = num_value
  }
  
  return (log(numer/denom) * target$freq)
}

# this is good turing smoothing
# returns log(probability), e^return_value to get the probability
ngram_evaluator_gt = function(test_ngram, model, number = 2) {
  V = length(unique(model[["word1"]]))
  possible_pairs = V^number
  unseen_pairs = possible_pairs - nrow(model)
  
  freq_array = c(unseen_pairs,
                 nrow(subset(model, freq == 1)),
                 nrow(subset(model, freq == 2)),
                 nrow(subset(model, freq == 3)),
                 nrow(subset(model, freq == 4)),
                 nrow(subset(model, freq == 5)))
  
  comb = test_ngram %>% by_row(row_wise_gt, model = model, V = V, freq_arr = freq_array, number = number, .collate = "rows")
  
  return (sum(comb$.out))
}

ngram_prob = function(filename, model, func, number = 2) {
  testfile = readtext(filename)
  test_ngram = create_ngram_model(testfile, number)
  func(test_ngram, model, number)
}

#Language Vizualization
language_df = function(filename, ngram, methodNumber) {

  print("loaded in the file...")

  #probable a better way to do this, but this'll do for now
  if (str_detect(ngram, pattern = "Bi")){
    lap_english_prob = ngram_prob(filename, en_bigram, ngram_evaluator_laplace)
    lap_italian_prob = ngram_prob(filename, it_bigram, ngram_evaluator_laplace)
    lap_spanish_prob = ngram_prob(filename, es_bigram, ngram_evaluator_laplace)
    gt_english_prob = ngram_prob(filename, en_bigram, ngram_evaluator_gt)
    gt_italian_prob = ngram_prob(filename, it_bigram, ngram_evaluator_gt)
    gt_spanish_prob = ngram_prob(filename, es_bigram, ngram_evaluator_gt)
  }
  
  if (str_detect(ngram, pattern = "Tri")){
    lap_english_prob = ngram_prob(filename, en_trigram, ngram_evaluator_laplace, number = 3)
    lap_italian_prob = ngram_prob(filename, it_trigram, ngram_evaluator_laplace, number = 3)
    lap_spanish_prob = ngram_prob(filename, es_trigram, ngram_evaluator_laplace, number = 3)
    gt_english_prob = ngram_prob(filename, en_trigram, ngram_evaluator_gt, number = 3)
    gt_italian_prob = ngram_prob(filename, it_trigram, ngram_evaluator_gt, number = 3)
    gt_spanish_prob = ngram_prob(filename, es_trigram, ngram_evaluator_gt, number = 3)
  }
  
  if (str_detect(ngram, pattern = "Q")) {
    lap_english_prob = ngram_prob(filename, en_qgram, ngram_evaluator_laplace, number = 4)
    lap_italian_prob = ngram_prob(filename, it_qgram, ngram_evaluator_laplace, number = 4)
    lap_spanish_prob = ngram_prob(filename, es_qgram, ngram_evaluator_laplace, number = 4)
    gt_english_prob = ngram_prob(filename, en_qgram, ngram_evaluator_gt, number = 4)
    gt_italian_prob = ngram_prob(filename, it_qgram, ngram_evaluator_gt, number = 4)
    gt_spanish_prob = ngram_prob(filename, es_qgram, ngram_evaluator_gt, number = 4)
  }
  print("made all the probs!")
  if (methodNumber == 1) {
    lap_df = data_frame("Prob" = c(lap_english_prob, lap_italian_prob, lap_spanish_prob), 
                        "Language" = c("English", "Italian", "Spanish"),
                        "Minimum" = ifelse(min(-Prob) == -Prob, T, F))
    return(lap_df)
  }
  if (methodNumber == 2) {
    gt_df = data_frame("Prob" = c(gt_english_prob, gt_italian_prob, gt_spanish_prob), 
                       "Language" = c("English", "Italian", "Spanish"),
                       "Minimum" = ifelse(min(-Prob) == -Prob, T, F))
    return(gt_df)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Sentiment Section ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Sentiment Modeling
#type = positivity or emotion
sent_modeling = function(filename, language_name, type = "positivity") {
  testfile = readtext(filename)
  test_sent = testfile %>% 
    unnest_tokens(output = word, input = text, token = "words")
  if (type == "emotion") {
    table = get_sentiment_dictionary("nrc", language_name) %>% 
      filter(sentiment != "positive" & sentiment != "negative")
  } else {
    table = get_sentiment_dictionary("nrc", language_name) %>% 
      filter(sentiment == "positive" | sentiment == "negative")
  }
  
  sent_analysis(test_sent, table)
}

# bag of words approach
sent_analysis = function(test_sent, model) {
  assigned_v = test_sent %>%
    count(word, sort = TRUE) %>%
    inner_join(model)
  
  return (assigned_v)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Era Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


era_analysis = function(text, language) {
  print("Era Analysis")
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
    stopped = as.data.frame(get_stopwords(language = "en"))
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
    stopped = as.data.frame(get_stopwords(language = "it"))
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
    stopped = as.data.frame(get_stopwords(language = "es"))
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
  print("Excluding Stopwords")
  topic_dtm <- word_counts %>%
    cast_dtm(year, word, n)
  
  
  topic_lda <- LDA(topic_dtm, k = 6, control = list(seed = 1234))
  
  topic_lda_td <- tidy(topic_lda)
  print("topic modleing")
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