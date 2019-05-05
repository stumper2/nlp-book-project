langauge_viz = function(filename, ngram) {
    # filename = file$datapath
    #probable a better way to do this, but this'll do for now
  if (str_detect(ngram, pattern = "Bi")){
    lap_english_prob = ngram_prob(filename, read.csv("./STAT 385/data/en_bigram.csv")[-1], ngram_evaluator_laplace)
    lap_italian_prob = ngram_prob(filename, read.csv("./STAT 385/data/it_bigram.csv")[-1], ngram_evaluator_laplace)
    lap_spanish_prob = ngram_prob(filename, read.csv("./STAT 385/data/es_bigram.csv")[-1], ngram_evaluator_laplace)
    gt_english_prob = ngram_prob(filename, read.csv("./STAT 385/data/en_bigram.csv")[-1], ngram_evaluator_gt)
    gt_italian_prob = ngram_prob(filename, read.csv("./STAT 385/data/it_bigram.csv")[-1], ngram_evaluator_gt)
    gt_spanish_prob = ngram_prob(filename, read.csv("./STAT 385/data/es_bigram.csv")[-1], ngram_evaluator_gt)
  }
  if (str_detect(ngram, pattern = "Tri")){
    lap_english_prob = ngram_prob(filename, read.csv("./STAT 385/data/en_trigram.csv")[-1], ngram_evaluator_laplace, number = 3)
    lap_italian_prob = ngram_prob(filename, read.csv("./STAT 385/data/it_trigram.csv")[-1], ngram_evaluator_laplace, number = 3)
    lap_spanish_prob = ngram_prob(filename, read.csv("./STAT 385/data/es_trigram.csv")[-1], ngram_evaluator_laplace, number = 3)
    gt_english_prob = ngram_prob(filename, read.csv("./STAT 385/data/en_trigram.csv")[-1], ngram_evaluator_gt, number = 3)
    gt_italian_prob = ngram_prob(filename, read.csv("./STAT 385/data/it_trigram.csv")[-1], ngram_evaluator_gt, number = 3)
    gt_spanish_prob = ngram_prob(filename, read.csv("./STAT 385/data/es_trigram.csv")[-1], ngram_evaluator_gt, number = 3)
  }
  if (str_detect(ngram, pattern = "Quad")) {
    lap_english_prob = ngram_prob(filename, read.csv("./STAT 385/data/en_quadgram.csv")[-1], ngram_evaluator_laplace, number = 4)
    lap_italian_prob = ngram_prob(filename, read.csv("./STAT 385/data/it_quadgram.csv")[-1], ngram_evaluator_laplace, number = 4)
    lap_spanish_prob = ngram_prob(filename, read.csv("./STAT 385/data/es_quadgram.csv")[-1], ngram_evaluator_laplace, number = 4)
    gt_english_prob = ngram_prob(filename, read.csv("./STAT 385/data/en_quadgram.csv")[-1], ngram_evaluator_gt, number = 4)
    gt_italian_prob = ngram_prob(filename, read.csv("./STAT 385/data/it_quadgram.csv")[-1], ngram_evaluator_gt, number = 4)
    gt_spanish_prob = ngram_prob(filename, read.csv("./STAT 385/data/es_quadgram.csv")[-1], ngram_evaluator_gt, number = 4)
  }
  
  lap_df = data_frame("Percentage" = c(lap_english_prob, lap_italian_prob, lap_spanish_prob), "Language" = c("English", "Italian", "Spanish"))
  gt_df = data_frame("Percentage" = c(gt_english_prob, gt_italian_prob, gt_spanish_prob), "Language" = c("English", "Italian", "Spanish"))
  
  ggplot(lap_df) +
    aes(x = Language, y = (Percentage), fill = Language) +
    labs(title = "Language projection using laplace smoothing") +
    geom_col()

  ggplot(gt_df) +
    aes(x = Language, y = (Percentage), fill = Language) +
    labs(title = "Language projection using Good Turings smoothing") +
    geom_col()
}
