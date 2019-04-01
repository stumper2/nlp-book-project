library(gutenbergr)
library(dplyr)

Italian_1200_id = c(1012, 997, 1009, 999, 1011, 1010, 998, 1000, 29977, 26961)
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

# checking
Italian_1200 %>% count(title)
Italian_1400 %>% count(title)
Italian_1550 %>% count(title)
Italian_1700 %>% count(title)
Italian_1815 %>% count(title)
Italian_1915 %>% count(title)