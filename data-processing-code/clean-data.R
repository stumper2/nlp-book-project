# en_bigram = read.csv("data-raw/en_bigram.csv")[-1]
# it_bigram = read.csv("data-raw/it_bigram.csv")[-1]
# es_bigram = read.csv("data-raw/es_bigram.csv")[-1]
# en_trigram = read.csv("data-raw/en_trigram.csv")[-1]
# it_trigram = read.csv("data-raw/it_trigram.csv")[-1]
# es_trigram = read.csv("data-raw/es_trigram.csv")[-1]
# en_qgram = read.csv("data-raw/en_qgram.csv")[-1]
# it_qgram = read.csv("data-raw/it_qgram.csv")[-1]
# es_qgram = read.csv("data-raw/es_qgram.csv")[-1]

saveRDS(en_bigram, file = "../data/en_bigram.rds")
saveRDS(it_bigram, file = "../data/it_bigram.rds")
saveRDS(es_bigram, file = "../data/es_bigram.rds")
saveRDS(en_trigram, file = "../data/en_trigram.rds")
saveRDS(it_trigram, file = "../data/it_trigram.rds")
saveRDS(es_trigram, file = "../data/es_trigram.rds")
saveRDS(en_qgram, file = "../data/en_qgram.rds")
saveRDS(it_qgram, file = "../data/it_qgram.rds")
saveRDS(es_qgram, file = "../data/es_qgram.rds")

# Moved files from data to data-raw that were csvs
# csv_files = list.files("data", pattern = ".csv")
# 
# file.rename(file.path("data", csv_files),
#             file.path("data-raw", csv_files))
