library(tm)
library(dplyr)
library(tidyr)
library(tidytext)
library(topicmodels)

# saveRDS(en_bigram, file = "../data/en_bigram.rds")
# saveRDS(it_bigram, file = "../data/it_bigram.rds")
# saveRDS(es_bigram, file = "../data/es_bigram.rds")
# saveRDS(en_trigram, file = "../data/en_trigram.rds")
# saveRDS(it_trigram, file = "../data/it_trigram.rds")
# saveRDS(es_trigram, file = "../data/es_trigram.rds")
# saveRDS(en_qgram, file = "../data/en_qgram.rds")
# saveRDS(it_qgram, file = "../data/it_qgram.rds")
# saveRDS(es_qgram, file = "../data/es_qgram.rds")

# Moved files from data to data-raw that were csvs
# csv_files = list.files("data", pattern = ".csv")
# 
# file.rename(file.path("data", csv_files),
#             file.path("data-raw", csv_files))

English = readRDS("../data/english.rds")
Spanish = readRDS("../data/spanish.rds")
Italian = readRDS("../data/italian.rds")

en_stopped = data.frame(word = stopwords(kind = "en"))
it_stopped = data.frame(word = stopwords(kind = "it"))
es_stopped = data.frame(word = stopwords(kind = "es"))

set.seed(4142)
en_topic_lda_td = English %>% 
  unnest_tokens(word, text) %>%
  anti_join(en_stopped, by = "word") %>%
  count(era, word, sort = TRUE) %>% 
  cast_dtm(era, word, n) %>% 
  LDA(k = 6, control = list(seed = 1234)) %>%
  tidy()

en_tidy_work = en_topic_lda_td %>% 
  spread(key = "term", value = "beta") %>%
  t() %>% 
  tail(-1)

en_vocab = rownames(en_tidy_work)
colnames(en_tidy_work) = c("Middle", "Renaissance", "Victorian", "Neoclassical", "Modern", "Romantic")
rownames(en_tidy_work) = NULL
en_tidy_work = data.frame(cbind(en_tidy_work, word = en_vocab))

set.seed(4142)
it_topic_lda_td = Italian %>% 
  unnest_tokens(word, text) %>%
  anti_join(it_stopped, by = "word") %>%
  count(era, word, sort = TRUE) %>% 
  cast_dtm(era, word, n) %>% 
  LDA(k = 6, control = list(seed = 1234)) %>%
  tidy()

it_tidy_work = it_topic_lda_td %>% 
  spread(key = "term", value = "beta") %>%
  t() %>% 
  tail(-1)

it_vocab = rownames(it_tidy_work)
colnames(it_tidy_work) = c("Baroque", "Contemporary", "Romanticism", "Medieval", "Classicism", "Renaissance")
rownames(it_tidy_work) = NULL
it_tidy_work = data.frame(cbind(it_tidy_work, word = it_vocab))

set.seed(4142)
es_topic_lda_td = Spanish %>% 
  unnest_tokens(word, text) %>%
  anti_join(es_stopped, by = "word") %>%
  count(era, word, sort = TRUE) %>% 
  cast_dtm(era, word, n) %>% 
  LDA(k = 6, control = list(seed = 1234)) %>%
  tidy()

es_tidy_work = es_topic_lda_td %>% 
  spread(key = "term", value = "beta") %>%
  t() %>% 
  tail(-1)

es_vocab = rownames(es_tidy_work)
colnames(es_tidy_work) = c("Modernism", "Realism", "Renaissance", "Baroque", "Enlightenment", "Romanticism")
rownames(es_tidy_work) = NULL
es_tidy_work = data.frame(cbind(es_tidy_work, word = es_vocab))

saveRDS(en_topic_lda_td, file = "../data/en_era.rds")
saveRDS(it_topic_lda_td, file = "../data/it_era.rds")
saveRDS(es_topic_lda_td, file = "../data/es_era.rds")

saveRDS(en_tidy_work, file = "../data/en_era2.rds")
saveRDS(it_tidy_work, file = "../data/it_era2.rds")
saveRDS(es_tidy_work, file = "../data/es_era2.rds")

en_era = readRDS("../data/en_era2.rds")
es_era = readRDS("../data/es_era2.rds")
it_era = readRDS("../data/it_era2.rds")

for (i in 1:6) {
  en_era[,i] = as.numeric(levels(en_era[,i]))[en_era[,i]]
  es_era[,i] = as.numeric(levels(es_era[,i]))[es_era[,i]]
  it_era[,i] = as.numeric(levels(it_era[,i]))[it_era[,i]]
}

saveRDS(en_era, file = "../data/en_era3.rds")
saveRDS(es_era, file = "../data/it_era3.rds")
saveRDS(it_era, file = "../data/es_era3.rds")
