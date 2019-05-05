library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(readtext)

library(purrrlyr)

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

langauge_viz = function(filename, ngram, methodNumber) {

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
  lap_df = data_frame("Prob" = c(lap_english_prob, lap_italian_prob, lap_spanish_prob), "Language" = c("English", "Italian", "Spanish"))
  gt_df = data_frame("Prob" = c(gt_english_prob, gt_italian_prob, gt_spanish_prob), "Language" = c("English", "Italian", "Spanish"))
  
  if (methodNumber == 1) {
  print("making laplace graph!")
  lap_graph = lap_df %>%
    mutate(Minimum = ifelse(min(-Prob) == -Prob, T, F)) %>%
      ggplot() +
      aes(x = Language, y =-Prob, fill = Minimum) +
      labs(title = "Language projection using laplace smoothing", subtitle = "Where the smallest ln(probability) is the least likely", y = "ln(Probability)") +
      geom_col() + 
      scale_fill_manual(values = c('grey', 'Tomato'), guide = FALSE)
  return(lap_graph)
  }
  if (methodNumber == 2) {
  print("making gt graph!")
  gt_graph = gt_df %>%
    mutate(Minimum = ifelse(min(-Prob) == -Prob, T, F)) %>%
      ggplot() +
      aes(x = Language, y = -Prob, fill = Minimum) +
      labs(title = "Language projection using Good Turings smoothing", subtitle = "Where the smallest ln(probability) is the least likely", y = "ln(Probability)") +
      geom_col() + 
      scale_fill_manual(values = c('grey', 'Tomato'), guide = FALSE)
  return(gt_graph)
  }
}