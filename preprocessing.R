#install.packages(c("dplyr", "gutenbergr"))
library(gutenbergr)
library(dplyr)
library(tidyr)

#Italian Books

Italian_1200_id = c(1012, 997, 1009, 999, 1011, 1010, 998, 1000, 29977, 26961)
Italian_1200_id = c(1000, 29977, 26961)
Italian_1400_id = c(57780, 3747, 50306, 57787, 46599, 56498, 45126)
Italian_1550_id = c(19688, 48530, 17440, 54167, 30839, 28372, 28368, 34639, 22502, 47051)
Italian_1700_id = c(31079, 38012, 31080, 47664, 21425, 19808, 20094, 33782,42881, 43575, 56482)
Italian_1815_id = c(55236, 46031, 46463, 45334, 57565, 46887, 46888, 19808, 47480, 48445, 42436)
Italian_1915_id = c(38637, 38338, 48779, 51587, 26169, 51624, 42198, 44763, 41555, 43316)

Italian_1200 <- gutenberg_download(Italian_1200_id, meta_fields = "title")
Italian_1400 <- gutenberg_download(Italian_1400_id, meta_fields = "title")
Italian_1550 <- gutenberg_download(Italian_1550_id, meta_fields = "title")
Italian_1700 <- gutenberg_download(Italian_1700_id, meta_fields = "title")
Italian_1815 <- gutenberg_download(Italian_1815_id, meta_fields = "title")
Italian_1915 <- gutenberg_download(Italian_1915_id, meta_fields = "title")

#Spanish Books
Spanish_1400_id = c(57505, 50430, 25705, 46201, 51465, 50526, 12457, 56454, 53207)
Spanish_1600_id = c(2000, 15115, 32315, 18580, 28408, 16110, 57035, 15027)
Spanish_1700_id = c(50027, 52682, 50492, 55796, 12840, 5985, 7109, 29497)
Spanish_1800_id = c(29105, 26284, 52262, 55215, 36990)
Spanish_1850_id = c(37590, 14765, 15066, 27214, 10909, 25671, 54228)
Spanish_1900_id = c(16109,29799, 43861, 28592, 39947, 55038, 58484, 28002, 43400, 30275)

Spanish_1400 = gutenberg_download(Spanish_1400_id, meta_fields = "title")
Spanish_1600 = gutenberg_download(Spanish_1600_id, meta_fields = "title")
Spanish_1700 = gutenberg_download(Spanish_1700_id, meta_fields = "title")
Spanish_1800 = gutenberg_download(Spanish_1800_id, meta_fields = "title")
Spanish_1850 = gutenberg_download(Spanish_1850_id, meta_fields = "title")
Spanish_1900 = gutenberg_download(Spanish_1900_id, meta_fields = "title")


#English Books
English_1066_id = c(14305,17400,22120,16845,17179,14568)
English_1500_id = c(55765,44083,14496,6953,1112,2264,2265,23042,38549,22421)
English_1660_id = c(17221,131,829,4085,2160,521,1292,6130,2667,1079,370)
English_1785_id = c(9622,10219,16357,2323,10851,34470,158,42671,21839,8448)
English_1832_id = c(16950,19188,4212,44919,5159,766,786,730,19337,1400)
English_1901_id = c(974,2021,1142,6424,524,4264,177,432,32772,35555)
English_1914_id = c(5670,144,42589,49204)

English_1066 <- gutenberg_download(English_1066_id, meta_fields = "title")
English_1500 <- gutenberg_download(English_1500_id, meta_fields = "title")
English_1660 <- gutenberg_download(English_1660_id, meta_fields = "title")
English_1785 <- gutenberg_download(English_1785_id, meta_fields = "title")
English_1832 <- gutenberg_download(English_1832_id, meta_fields = "title")
English_1901 <- gutenberg_download(English_1901_id, meta_fields = "title")
English_1914 <- gutenberg_download(English_1914_id, meta_fields = "title")

# checking
Italian_1200 %>% count(title)
Italian_1400 %>% count(title)
Italian_1550 %>% count(title)
Italian_1700 %>% count(title)
Italian_1815 %>% count(title)
Italian_1915 %>% count(title)

Spanish_1400 %>% count(title)
Spanish_1600 %>% count(title)
Spanish_1700 %>% count(title)
Spanish_1800 %>% count(title)
Spanish_1850 %>% count(title)
Spanish_1900 %>% count(title)

English_1066 %>% count(title)
English_1500 %>% count(title)
English_1660 %>% count(title)
English_1785 %>% count(title)
English_1832 %>% count(title)
English_1901 %>% count(title)
English_1914 %>% count(title)

# Combine
Italian = rbind(Italian_1200, Italian_1400, Italian_1550, Italian_1700, Italian_1815, Italian_1915)
Spanish = rbind(Spanish_1400, Spanish_1600, Spanish_1700, Spanish_1800, Spanish_1850, Spanish_1900)
English = rbind(English_1066, English_1500, English_1660, English_1785, English_1832, English_1901, English_1914)

# find a better way to tokenize, exclude number?
# Code derived from www.tidytextmining.com/ngrams.html
create_ngram = function(books, number) {
  bigrams = books %>% 
    unnest_tokens(output = bigram, input = text, token = "ngrams", n = number) %>% 
    # general function
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    count(word1, word2, sort = TRUE)
  return (bigrams)
}

en_bigrams = create_ngram(English, 2)
es_bigrams = create_ngram(Spanish, 2)
it_bigrams = create_ngram(Italian, 2)
