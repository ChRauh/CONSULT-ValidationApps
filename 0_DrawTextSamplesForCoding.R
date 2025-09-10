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
# 4. Build and store three samples to be fed into the coding app (unique id, keep measure values, mark overlap obs)


# Packages used #####
library(tidyverse)
library(purrr)
library(tokenizers)
library(quanteda)
library(spacyr)
library(sophistication) # https://github.com/kbenoit/sophistication
library(patchwork)


# Load and prepare consultation data ####

# Using absolute path for now (Dropbox, WZB office machine)
fbs <- read_rds("C:/Users/rauh/Dropbox/CR data/CommConsultText/Data/FeedbackTextData-Complete_new.rds")

# Select vars and calculated measures on full text level
# N.B.: 'feedback_ref' is a unique id 
fbs <- fbs %>%
  filter(!text_full == " ") %>% # Drop observations without full text
  select(c(feedback_ref, policy_stage_short, text_full, n_sentence, starts_with("ne_"), flesch, legalese, entropy)) %>% 
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


# Quanteda corpus object of the snippet texts
qcorp <- corpus(fbs$snippet, docvars = data.frame(fbs[, c("feedback_ref")]))
docids <- docvars(qcorp) %>% 
  mutate(doc_id = as.character(docid(qcorp)))



# Flesch Reading Ease Score, based on sophistication package

re <- covars_make(qcorp, readability_measure = "Flesch") 
re$doc_id <- paste0("text",rownames(re))

readability <- merge(docids[ ,c("doc_id", "feedback_ref")],
                    re[, c("doc_id", "meanSentenceLength", "Flesch")],
                    by = "doc_id", all.x = T) %>% 
  rename(snippet_flesch = Flesch,
         snippet_meanSentenceLength = meanSentenceLength) %>% 
  select(-doc_id)

fbs <- fbs %>% 
  left_join(readability, by = "feedback_ref")



# Named entity counts

ent <- spacy_extract_entity(qcorp, output = "data.frame", type = "all") # Extraction (takes some time)

ent <- ent %>% # Join in doc_ids
  left_join(docids,
            by = "doc_id") %>% 
  select(c(feedback_ref, ent_type, text))

# Summarize: Count number of entity type by each feedback text
# N.B.: if the respective text didn't contain any NE it is not in the data
# respectively missing values in the assembled data set need to be filled with zeros 
ent2 <- 
  ent %>% 
  select(-text) %>% # The entity text
  group_by(feedback_ref) %>% 
  count(across(everything())) %>% 
  pivot_wider(id_cols = "feedback_ref", names_from = "ent_type", values_from = "n", values_fill = 0)

names(ent2)[2:ncol(ent2)] <- paste0("NE_", names(ent2)[2:ncol(ent2)]) %>% tolower() # Clarify names

entities <- ent2 %>% 
  rowwise() %>% 
  mutate(snippet_ne_sum = sum(c_across(starts_with("ne_")), na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(c(feedback_ref, snippet_ne_sum))

# Add to target data - and ensure that missing values are set to zero (cf. above)
fbs <- fbs %>% 
  left_join(entities, by = "feedback_ref") %>% 
  mutate(across(.cols = starts_with("snippet_ne_"), function(x){ifelse(is.na(x), 0, x)}))


# Part-of-speech distributions (sophistication/spacyr) - to get the number of sentences as in main analyses
# Puts out doc_id itself, order not necessarily correct
pos <- covars_make_pos(qcorp)

pos2 <- pos %>% # Join in doc_ids
  left_join(docids,
            by = "doc_id") %>% 
  select(-doc_id)

fbs <- fbs %>% 
  left_join(pos2 %>% 
              select(c(feedback_ref, n_sentence)) %>% 
              rename(snippet_n_sentence = n_sentence), 
            by = "feedback_ref")

fbs$snippet_entities <- fbs$snippet_ne_sum/fbs$snippet_n_sentence # Normalize entity count to text length


# Legalese (dictionary)

legalese <- read_rds("C:/Users/rauh/Dropbox/CR data/CommConsultText/Data/EU_Legalese_Dict.rds")
legalese.r <- paste(legalese$token, collapse = "|")
fbs$snippet_legalese <- str_count(tolower(fbs$snippet), legalese.r)/fbs$snippet_wc # relative frequency of words from legalese dictionary in text
hist(fbs$snippet_legalese)

test <- fbs %>% filter(snippet_legalese >= .5)
# remove ultra short snippets! XXX




# Entropy 

# Quanteda stopword list as RegEx
stops <- paste(stopwords("english"), collapse = " | ")
stops <- paste(" ", stops, " ", sep = "")

# Prepare the text bits
fbs$snippet_entropytext <- tolower(fbs$snippet) # Lower case
fbs$snippet_entropytext <- gsub(stops, " ", fbs$snippet_entropytext, fixed = F) # remove stopwords 

# Calculate entropy per feedback snippet
fbs$snippet_entropy <- NA
progress_bar = txtProgressBar(min=0, max=nrow(fbs), style = 3, char="=")
for (i in 1:nrow(fbs)) {
  text <- tokens(fbs$snippet_entropytext[i], remove_numbers = T, remove_punct = T, remove_symbols = T, remove_separators = T) # tokenize EU references with some preprocessing
  text <- as.character(text) # character vector of each individual token
  if(length(text) == 0){next}
  freqs <- table(text)/length(text) # relative token frequency
  fbs$snippet_entropy[i] <- -sum(freqs * log2(freqs)) # entropy formula following Katz/Bommarito II (2014)
  setTxtProgressBar(progress_bar, value = i) # Progress indicator
}
close(progress_bar)




# Compare distribution of text measures between full text and snippets ####


# Helper function for distribution comparision
plot_density_compare <- function(data, var1, var2) {
  # capture the variable names
  var1 <- rlang::ensym(var1)
  var2 <- rlang::ensym(var2)
  
  data_long <- data %>%
    select(!!var1, !!var2) %>%
    pivot_longer(cols = everything(),
                 names_to = "variable",
                 values_to = "value")
  
  ggplot(data_long, aes(x = value, fill = variable, color = variable)) +
    geom_density(alpha = 0.3) +
    theme_minimal() +
    labs(x = "Value", y = "Density",
         title = paste(rlang::as_name(var1), "vs.", rlang::as_name(var2)))+
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
}

# Compare vars
p1 <- plot_density_compare(fbs, entropy, snippet_entropy)
p2 <- plot_density_compare(fbs, flesch, snippet_flesch)
p3 <- plot_density_compare(fbs, entities, snippet_entities)
p4 <- plot_density_compare(fbs, legalese, snippet_legalese)

# Plot comparison
(p1+p2) / (p3+p4) +
  plot_annotation(
    title = "Comparing the distribution of 'information quality' measures\nacross full texts and shorter snippets for coding"
  ) &
  theme(plot.title = element_text(hjust = 0.5))  # center the title


ggsave(
  filename = "./plots/TextMeasuresFullVsSnippets.png",
  width = 30, height = 25, units = "cm", dpi = 300
)


# Entropy is an issue (shorter text, less potental for variable vocabulary - probably also an issue for the analyses?)
# Entities is also somewhat smaller for shorter text, but ample variation ...




# Save the snippet information to disc ####

snips <- fbs %>% select(c(feedback_ref, policy_stage_short, starts_with("snippet")))
write_rds(snips, "./data/snippets_indicators.rds")
snips <- read_rds("./data/snippets_indicators.rds")



# Draw coding samples ####

# Prepare population data
pop <- snips %>% 
  select(feedback_ref, snippet, policy_stage_short, snippet_entropy, snippet_flesch, snippet_entities, snippet_legalese) %>% 
  rename(id = feedback_ref,
         text = snippet,
         stage = policy_stage_short)
names(pop) <- names(pop) %>% str_remove("snippet")


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
