library(tidyverse)


test <- read_csv("./data/coder1texts_testfile.csv") %>% 
  rename(relevance = classification1,
         richness = classification2,
         analytic = classification3,
         specificity = classification4,
         infoqual = classification5)
