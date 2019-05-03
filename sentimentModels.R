library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(readtext)

sent_modeling = function(filename, language_name, type) {
  testfile = readtext(filename)
  test_sent = testfile %>% 
    unnest_tokens(output = word, input = text, token = "words")
  if (type == "emotion") {
    table = get_sentiment_dictionary("nrc", language_name) %>% 
      filter(sentiment != "positive" & sentiment != "negative")
  } else {
    table2 = get_sentiment_dictionary("nrc", language_name) %>% 
      filter(sentiment == "positive" & sentiment == "negative")
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

# example:
it = sent_modeling("italian.txt", "italian", "emotion")
ggplot(it) +
  geom_bar(aes(x =sentiment)) # + title and color and stuff
  