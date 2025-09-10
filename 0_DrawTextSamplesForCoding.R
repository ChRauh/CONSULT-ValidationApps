############################################################
# Project:  COMM consult
# Task:     Draw text samples for human coding / validation
# Author:   @ChRauh (10.09.2025)
############################################################



# Context / setup ####

# We need to validate our text-based measures for 'information quality' of consultation responses
# against the assessments of human coders.

# Coding full texts is not possible - text length varies drastically and can span several pages of text
# We thus agreed to let them code shorter units (3-4 sentences)

# As we have no consistent formatting markers in the text data (such as paragraphs or line breaks),
# we decided to take the first few sentences of each contribution (typically containing the key messages)
# of consultation responses.
# In this context, we want to exclude the first sentence, as  this often just informs about who the responder is.

# We are able to hire three human coders who can code around ~ 600 units (based on testing coding for units of 3-4 sentences length)

# We want to assess intercoder reliability as well, requiring us to generat an overlap sample seen by all coders,
# meaning the targeted samples are the following:

# One 'overlap' sample of 100 text snippets (contained in all sets)
# Three seperate/distinct samples of 500 text snippets (one for each coder)

# To get a valid measure while maximising variation across our measures, we go for stratified random samples.
# The relevant strata are:

# Quintiles of the text based measures (Flesch reading ease, named entities, legal language - calculated for all text snippets before sampling)
# Type of legislative procedure the Comm consults on (as this is the key analytical dimension in the analysis, var: policy_stage_short with three values)



# Work plan ####

# 1. Load full text data and extract text snippets
# 2. (Re-) Calculate text measures for the snippets
# 3. Draw the four stratified random samples (avoid duplicates across samples to maximise number of obs)
# 4. Build and store three samples to be fed into the coding app (unique id, keep measure values,, mark ooverlap obs)


# Packages used #####
library(tidyverse)
library(purrr)
library(tokenizers)


# Load and prepare consultation data ####

# Using absolute path for now (Dropbox, WZB office machine)
fbs <- read_rds("C:/Users/rauh/Dropbox/CR data/CommConsultText/Data/FeedbackTextData-Complete_new.rds")

# Select vars and calculated measures on full text level
# N.B.: 'feedback_ref' is a unique id 
fbs <- fbs %>%
  filter(!text_full == " ") %>% # Drop observations without full text
  select(c(feedback_ref, policy_stage_short, text_full, n_sentence, starts_with("ne_"), flesch, legalese)) %>% 
  rowwise() %>%
  mutate(entities = sum(c_across(starts_with("ne_")), na.rm = TRUE)) %>% # Sum all named entities in text
  ungroup()

fbs <- fbs %>% 
  select(-c(starts_with("ne_"))) %>% # Drop individual entity counts
  mutate(entities =  entities/n_sentence) # Normalize NE sum to text length



# Extract text snippets for coding ####

# Helper function
# The first sentence almost always introduces the sender of the feedback - we do not want this in the data the humans will see
# But we also have a number of shorter feedbacks whihc we do not want to artifically reduce too much
# Thus we remove the first setnce only when the ovall comment is more than 2 sentences long
remove_first_if_long <- function(text) {
  sents <- tokenize_sentences(text, simplify = TRUE)
  if (length(sents) > 2) {
    sents <- sents[-1]  # drop first
  }
  paste(sents, collapse = " ") |> str_squish()
}


# Helper function
# We want the human coders to see meaningful text (= full setences), while avoid overly long comments 
# and having too much variation in the length of the snippets coders get to see.
# Thus the function returns the shortest number of sentences around 200 words
# = completing the text with the sentence that contains the 200th word)
snippet_by_words <- function(text, target = 200) {
  sents <- tokenize_sentences(text, simplify = TRUE)
  if (length(sents) == 0L) return(NA_character_)
  wc <- count_words(sents)
  idx <- which(cumsum(wc) >= target)[1]           # first sentence that passes target
  if (is.na(idx)) idx <- length(sents)            # text shorter than target → whole text
  paste(sents[seq_len(idx)], collapse = " ") |> str_squish()
}


# Apply both of the above logics to the consultation text (takes some time ...)
fbs <- fbs %>%
  mutate(
    text_mod = map_chr(text_full, remove_first_if_long),
    snippet  = map_chr(text_mod, snippet_by_words, target = 200)
  ) %>% 
  select(-text_mod)

# Word count of resulting snippets
fbs$snippet_wc <- count_words(fbs$snippet)
hist(fbs$snippet_wc)
summary(fbs$snippet_wc)

# Inspect unusually long snippets
test <- fbs %>% filter(snippet_wc > 300)

i <- sample(nrow(test), 1)
test$snippet[i]

sents <- tokenize_sentences(test$snippet[i], simplify = TRUE)
sents

# Many correct cases (with awkward sentences ;)), 
# but also a few that are off because list items or annex tables are not properly detected as separate sentences
rm(test)


# For the sake of human coder sanity, I drop the 224 observations with snippets longer than 300 words
fbs <- fbs %>% filter(snippet_wc <= 300)


# Finally, we do not want to have duplicated snippet texts in the final coder samples
# N.B. concerted campaigns!
sum(duplicated(fbs$snippet))
duplictes <- fbs %>% filter(duplicated(snippet))
fbs <- fbs %>% mutate(dupl = duplicated(snippet)) %>% filter(!dupl) %>% select(-dupl)


# Random example inspection
# Kable an example table here in the qmd

i <- sample(nrow(fbs), 1)
fbs$snippet[i]
fbs$snippet_wc[i]

# Intermediary saving
write_rds(fbs, "./data/snippets.rds")




# Calculate text based measures for the snippets ####











# Draw a test sample ####

set.seed(123)  # for reproducibility
N <- 300
vars <- c("flesch", "legalese", "entities")

# 1) Count strata and compute proportional targets
targets <- fbs %>%
  count(across(all_of(vars)), name = "n") %>%
  mutate(prop   = n / sum(n),
         target = floor(prop * N),
         frac   = prop * N - target)

# 2) Cap targets by available n, then redistribute leftover to biggest fractions with slack
targets <- targets %>%
  mutate(target = pmin(target, n)) 

leftover <- N - sum(targets$target)
if (leftover > 0) {
  targets <- targets %>%
    mutate(slack = n - target) %>%
    arrange(desc(frac)) %>%
    mutate(add = as.integer(row_number() <= leftover & slack > 0),
           target = target + add) %>%
    select(-prop, -frac, -slack, -add)
} else {
  targets <- targets %>%
    select(-prop, -frac)
}

# 3) Sample per stratum (n varies by group → use nest + map2)
sampled_300 <- fbs %>%
  inner_join(targets, by = vars) %>%
  group_by(across(all_of(vars))) %>%
  group_modify(~ {
    n_take <- dplyr::first(.x$target)          # scalar per stratum
    if (is.na(n_take) || n_take <= 0) return(.x[0, , drop = FALSE])
    dplyr::slice_sample(.x, n = n_take)
  }) %>%
  ungroup() %>%
  select(-n, -target)



# Prepare input for the app ####

s <- sampled_300 %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(id, legalese, entities, flesch, testtext) %>% 
  rename(text = testtext)

# Individual files

s %>% 
  slice_sample(prop = 1) %>% # Shuffle rows
  write.csv("./CONSULT-Dim-Model/coder0texts.csv",
            row.names = FALSE,    # don’t add row numbers
            quote = TRUE)

s %>% 
  slice_sample(prop = 1) %>% # Shuffle rows
  write.csv("./CONSULT-Dim-Model/coder1texts.csv",
            row.names = FALSE,    # don’t add row numbers
            quote = TRUE)


s %>% 
  slice_sample(prop = 1) %>% # Shuffle rows
  write.csv("./CONSULT-Dim-Model/coder2texts.csv",
            row.names = FALSE,    # don’t add row numbers
            quote = TRUE)

s %>% 
  slice_sample(prop = 1) %>% # Shuffle rows
  write.csv("./CONSULT-Dim-Model/coder3texts.csv",
            row.names = FALSE,    # don’t add row numbers
            quote = TRUE)
