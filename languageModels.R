library(plyr)
library(dplyr)
library(tidyr)
library(tidytext)
library(readtext)

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

ngram_prob(filename, model, eval_function, number = 2) {
  testfile = readtext(filename)
  test_ngram = create_ngram_model(testfile, number)
  return (exp(1)^eval_function(test_ngram, model, number))
}

# this is laplace smoothing
# returns log(probability), e^return_value to get the probability
ngram_evaluator_laplace = function(test_ngram, model, number = 2) {
  V = length(unique(model[["word1"]]))
  prob = 0
  
  for (row in 1:nrow(test_ngram)) {
    target = test_ngram[row,]
    targetdf = model[target[[1]] == model[[1]],]
    
    i = 1
    # while length(targetdf) > 0
    while (length(targetdf) & (i <= (number - 1))) { 
      targetdf = targetdf[target[[i]] == targetdf[[i]],]
      i = i + 1
    }
    
    num_value = targetdf[target[[number]] == targetdf[[number]],]$freq
    numer = 1
    
    if (length(num_value) > 0) {
      numer = numer + num_value
    }
    
    denom = V + sum(targetdf$freq)
    prob = prob + log(numer/denom) * target$freq
  }
  
  return (prob)
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

# this is good turing smoothing
# returns log(probability), e^return_value to get the probability
ngram_evaluator_gt = function(test_ngram, model, number = 2) {
  V = length(unique(model[["word1"]]))
  possible_pairs = V^number
  seen_pairs = nrow(model)
  unseen_pairs = possible_pairs - seen_pairs
  prob = 0
  
  freq_array = c(unseen_pairs,
                 nrow(subset(model, freq == 1)),
                 nrow(subset(model, freq == 2)),
                 nrow(subset(model, freq == 3)),
                 nrow(subset(model, freq == 4)),
                 nrow(subset(model, freq == 5)))
  
  for (row in 1:nrow(test_ngram)) {
    target = test_ngram[row,]
    targetdf = model[target[[1]] == model[[1]],]
    
    i = 1
    # while length(targetdf) > 0
    while (length(targetdf) & (i <= (number - 1))) { 
      targetdf = targetdf[target[[i]] == targetdf[[i]],]
      i = i + 1
    }
    
    num_value = targetdf[target[[number]] == targetdf[[number]],]$freq
    
    if (length(num_value) == 0) {
      num_value = 0
    }
    
    numer = get_const(num_value, freq_array)
    denom = seen_pairs
    prob = prob + log(numer/denom) * target$freq
  }
  
  return (prob)
}

# if n > 2, give option for backoff
# get lambda

# English = read.csv("data/English.csv")[-1]
# Spanish = read.csv("data/Spanish.csv")[-1]
# Italian = read.csv("data/Italian.csv")[-1]

# n-gram models are saved in data folder, but it exceeds 100 MB
# Thus, cannot commit to github
# run this program to get the files

# en_unigrams = create_ngram_model(English, 1)
# es_unigrams = create_ngram_model(Spanish, 1)
# it_unigrams = create_ngram_model(Italian, 1)
# write.csv(en_unigrams, "data/en_unigram.csv")
# write.csv(es_unigrams, "data/es_unigram.csv")
# write.csv(it_unigrams, "data/it_unigram.csv")

# en_bigrams = create_ngram_model(English, 2)
# es_bigrams = create_ngram_model(Spanish, 2)
# it_bigrams = create_ngram_model(Italian, 2)
# write.csv(en_bigrams, "data/en_bigram.csv")
# write.csv(es_bigrams, "data/es_bigram.csv")
# write.csv(it_bigrams, "data/it_bigram.csv")

# en_trigrams = create_ngram_model(English, 3)
# es_trigrams = create_ngram_model(Spanish, 3)
# it_trigrams = create_ngram_model(Italian, 3)
# write.csv(en_trigrams, "data/en_trigram.csv")
# write.csv(es_trigrams, "data/es_trigram.csv")
# write.csv(it_trigrams, "data/it_trigram.csv")

# en_qgrams = create_ngram_model(English, 4)
# es_qgrams = create_ngram_model(Spanish, 4)
# it_qgrams = create_ngram_model(Italian, 4)
# write.csv(en_qgrams, "data/en_qgram.csv")
# write.csv(es_qgrams, "data/es_qgram.csv")
# write.csv(it_qgrams, "data/it_qgram.csv")