library(tidyverse)


# Pull from Dropbox, WZB machine
fbs <- read_rds("C:/Users/rauh/Dropbox/CR data/CommConsultText/Data/FeedbackTextData-Complete_new.rds")

  
# Prepare the data #### 
fbs <- fbs %>%
  select(c(feedback_ref, policy_stage_short, text_full, n_sentence, starts_with("ne_"), flesch, legalese)) %>% 
  rowwise() %>%
  mutate(entities = sum(c_across(starts_with("ne_")), na.rm = TRUE)) %>% # Sum all named entities in text
  ungroup()

fbs <- fbs %>% 
  select(-c(starts_with("ne_"))) %>% # Drop individual entity counts
  mutate(entities =  entities/n_sentence) # Normalize NE sum to text length


# Shorter texts for coding ####

# Extract the first four sentences from full text
fbs <- fbs %>% 
  mutate(firstfour = str_extract(text_full, "^([^.!?]*[.!?]\\s*){1,4}")) # Sentence punctuation, tokenizer might be better

# Random example inspection
fbs %>% 
  slice_sample(n = 1) %>% 
  pull(firstfour)

# First sentence is virtually always an intro - remove
fbs$testtext <- str_remove(fbs$firstfour, "^[^.!?]*[.!?]\\s*")

fbs %>% 
  slice_sample(n = 1) %>% 
  pull(testtext)



# Filter data to get more meaningful sample ####

fbs <- fbs %>% filter(!is.na(testtext))
fbs <- fbs %>% filter(!is.na(flesch) & !is.na(legalese) & !is.na(entities))
fbs <- fbs %>% mutate(dupl = duplicated(testtext)) %>% filter(!dupl) %>% select(-dupl)


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
