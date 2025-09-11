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
library(cld2)
library(textclean)
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

set.seed(20250911)  # for reproducibility

# Prepare population data
pop <- snips %>% 
  select(feedback_ref, snippet, policy_stage_short, snippet_wc, snippet_entropy, snippet_flesch, snippet_entities, snippet_legalese) %>% 
  rename(id = feedback_ref,
         text = snippet,
         stage = policy_stage_short)
names(pop) <- names(pop) %>% str_remove("snippet_")


# Filter population data

pop <- pop %>% 
  mutate(lang = detect_language(text)) %>% 
  filter(lang == "en") %>% 
  select(-lang) %>% 
  filter(wc > 10) %>% # A.o. removes extreme legalese cases
  mutate(flesch = ifelse(flesch < 0, 0, flesch), # Cap Flesch measures as in analysis (and to avoid oversampling of extreme cases)
         flesch = ifelse(flesch > 100, 100, flesch)) %>% 
  mutate(text = iconv(text, from = "latin1", to = "UTF-8"),
         text = textclean::replace_non_ascii(text)) # Tackle (some) encoding issues - SHOULD BE DONE IN MAIN ANALYSES AS WELL!


# Sampling
# Reminder: Each coder should see 500 units that cover variation across all measures and balances the policy stages
# Overlap sample of 100 obs with the same features

# - > stratified sampling

# Mark quintile bins for the the text measures + stratum ids (including stage)

pop_binned <- pop %>%
  mutate(
    q_entropy  = ntile(`entropy`,  5),
    q_flesch   = ntile(`flesch`,   5),
    q_entities = ntile(`entities`, 5),
    q_legalese = ntile(`legalese`, 5),
    strata = interaction(stage, q_entropy, q_flesch, q_entities, q_legalese, drop = TRUE)
  )


# Stratum fill function
# proportional integer allocation within a stage 
# Given available counts per stratum (s), and a target size m,
# allocate integers n_i with:
#   - proportional to size,
#   - capped by availability,
#   - remainder distributed by largest fractional parts,
#   - if caps reduce total, re-distribute to strata with remaining capacity

alloc_counts <- function(s, m) {
  if (sum(s) == 0 || m <= 0) return(rep(0L, length(s)))
  # initial proportional allocation
  p <- s / sum(s)
  raw <- m * p
  n <- pmin(s, floor(raw))
  # distribute remainder by largest fractional part (Hamilton method)
  rem <- m - sum(n)
  if (rem > 0) {
    frac <- raw - floor(raw)
    # prefer strata with capacity left
    cap_left <- s - n
    frac[cap_left <= 0] <- -Inf
    if (any(is.finite(frac))) {
      add_ix <- order(frac, decreasing = TRUE)[seq_len(min(rem, sum(is.finite(frac))))]
      n[add_ix] <- n[add_ix] + 1L
    }
  }
  # if caps caused under-allocation, top up randomly among strata with capacity
  deficit <- m - sum(n)
  if (deficit > 0) {
    cap_left <- s - n
    pool <- which(cap_left > 0)
    if (length(pool) > 0) {
      # sample proportional to remaining capacity
      probs <- cap_left[pool] / sum(cap_left[pool])
      extra <- sample(pool, size = deficit, replace = FALSE, prob = probs)
      tab <- table(extra)
      n[as.integer(names(tab))] <- n[as.integer(names(tab))] + as.integer(tab)
    }
  }
  n
}


# Sampling function  within one stage, spreading across multi-way strata of the text measure values
sample_one_stage <- function(df_stage, m) {
  if (nrow(df_stage) == 0L || m <= 0L) return(df_stage[0, ])
  
  # counts per composite stratum (within this stage)
  strata_counts <- df_stage %>%
    dplyr::count(strata, name = "s")
  
  # integer allocation per stratum (proportional, capped by availability)
  strata_counts <- strata_counts %>%
    dplyr::mutate(n_take = alloc_counts(s, m)) %>%
    dplyr::filter(n_take > 0L)
  
  # join plan, randomize within stratum, then keep first n_take rows per stratum
  df_stage %>%
    dplyr::inner_join(strata_counts, by = "strata") %>%
    dplyr::group_by(strata) %>%
    dplyr::mutate(.rand = runif(dplyr::n())) %>%
    dplyr::arrange(.rand, .by_group = TRUE) %>%
    dplyr::mutate(.rn = dplyr::row_number()) %>%
    dplyr::filter(.rn <= dplyr::first(n_take)) %>%  # <- varies by group: OK in filter()
    dplyr::ungroup() %>%
    dplyr::select(-.rand, -.rn, -s, -n_take)
}


# Function: build one full sample with ~equal stage balance 
take_one_sample <- function(df_remaining, total_n) {
  # target per stage ~ equal split
  stages <- df_remaining %>% count(stage, name = "avail")
  # split total_n as evenly as possible across the 3 stages, favoring stages with availability
  base <- floor(total_n / 3)
  rem  <- total_n - base * 3
  # distribute the remainder to the stages with most available rows
  add <- rep(0L, nrow(stages))
  if (rem > 0) {
    add_ix <- order(stages$avail, decreasing = TRUE)[seq_len(rem)]
    add[add_ix] <- 1L
  }
  targets <- pmin(stages$avail, base + add)
  plan <- tibble(stage = stages$stage, target = targets)
  
  # sample within each stage using multi-way strata
  sampled <- map2_dfr(
    plan$stage, plan$target,
    ~ df_remaining %>%
      filter(stage == .x) %>%
      sample_one_stage(m = .y)
  )
  
  # If this falls short of the target (e.g., not enough availability in some strata/stages),
  # top up from the remaining rows while preserving stage balance as far as possible.
  short <- total_n - nrow(sampled)
  if (short > 0) {
    pool <- anti_join(df_remaining, sampled, by = "id")
    # gentle bias toward under-represented stages in the current sample
    have <- sampled %>% count(stage, name = "have")
    want <- plan %>% left_join(have, by = "stage") %>% mutate(have = replace_na(have, 0L),
                                                              gap = pmax(target - have, 0L))
    pool <- pool %>% left_join(want %>% select(stage, gap), by = "stage")
    # weight by (1 + gap) to favor stages needing top-up
    probs <- (pool$gap + 1) / sum(pool$gap + 1)
    extra_idx <- sample(seq_len(nrow(pool)), size = short, replace = FALSE, prob = probs)
    sampled <- bind_rows(sampled, pool[extra_idx, ])
  }
  sampled
}


# Build four non-overlapping samples (100, 500, 500, 500)

sizes <- c(100L, 500L, 500L, 500L)

samples <- vector("list", length(sizes))
remaining <- pop_binned

for (i in seq_along(sizes)) {
  samp_i <- take_one_sample(remaining, sizes[i])
  samples[[i]] <- samp_i
  remaining <- anti_join(remaining, samp_i, by = "id")  # ensure no overlap
}

samp1 <- samples[[1]]  # 100 rows
samp2 <- samples[[2]]  # 500 rows
samp3 <- samples[[3]]  # 500 rows
samp4 <- samples[[4]]  # 500 rows




# Quick checks:
lapply(list(samp1, samp2, samp3, samp4), \(x) count(x, stage) %>% mutate(p = n/sum(n)))
lapply(list(samp1, samp2, samp3, samp4), \(x)
       x %>% count(q_entropy, q_flesch, q_entities, q_legalese) %>% summarise(nonempty = sum(n > 0)))

  


# Visual comparison of the samples and the population ####

# Function to reshape the different data sets for ggplot
to_long <- function(df, src) {
  df %>%
    transmute(
      source  = src,
      entropy = `entropy`,
      flesch  = `flesch`,
      entities= `entities`,
      legalese= `legalese`
    ) %>%
    pivot_longer(cols = entropy:legalese, names_to = "metric", values_to = "value")
}

# Stacking the samples and population
all_long <- bind_rows(
  to_long(pop,   "Population"),
  to_long(samp1, "Sample 1 (overlap, n = 100)"),
  to_long(samp2, "Sample 2 (n= 500)"),
  to_long(samp3, "Sample 3 (n= 500)"),
  to_long(samp4, "Sample 4 (n= 500)")
) %>%
  mutate(
    metric = factor(metric, levels = c("entropy","flesch","entities","legalese")),
    source = factor(source, levels = c("Population","Sample 1 (overlap, n = 100)","Sample 2 (n= 500)","Sample 3 (n= 500)","Sample 4 (n= 500)"))
  )

# Plot comparison
ggplot() +
  # population baseline (black line, no fill)
  geom_density(
    data = filter(all_long, source == "Population"),
    aes(x = value),
    linewidth = 2, color = "black"
  ) +
  # samples (transparent fills)
  geom_density(
    data = filter(all_long, source != "Population"),
    aes(x = value, fill = source),
    color = NA,
    alpha = 0.35
  ) +
  facet_wrap(~ metric, scales = "free", ncol = 2) +
  labs(
    x = NULL, y = "Density",
    fill = NULL,
    title = "Distribution of the text measures in snippet population and coder samples",
    subtitle = "Population shown as black outline; samples mark by fill color"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold.italic"))

ggsave(
  filename = "./plots/MeasureDistributionInCoderSamples.png",
  width = 30, height = 25, units = "cm", dpi = 300
)


# Visualize distribution across policy stages

tag_stage <- function(df, src) df %>% transmute(source = src, stage)

pop_vs_samples <- bind_rows(
  tag_stage(pop,   "Population"),
  tag_stage(samp1, "Sample 1"),
  tag_stage(samp2, "Sample 2"),
  tag_stage(samp3, "Sample 3"),
  tag_stage(samp4, "Sample 4")
) %>%
  mutate(source = factor(source, levels = c("Population","Sample 1","Sample 2","Sample 3","Sample 4")))


counts <- 
  pop_vs_samples %>%
  count(source, stage, name = "n") %>%
  group_by(source) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()


ggplot(counts, aes(x = stage, y = prop, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = NA) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Population" = "grey30",          # dark grey
      "Sample 1"  = "#1b9e77",          # teal-green
      "Sample 2"  = "#d95f02",          # orange
      "Sample 3"  = "#7570b3",          # purple
      "Sample 4"  = "#e7298a"           # magenta
    )
  ) +
  labs(x = NULL, y = "Share of observations\n", fill = NULL,
       title = "Distribution of policy stages in population and coder samples") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  filename = "./plots/PolicyStagesInCoderSamples.png",
  width = 30, height = 12, units = "cm", dpi = 300
)



# Finalize and export coder samples ####
# Combine coder samples (2-4) with (marked) overlap sample, shuffle rows, and write to csv as required by shiny app

coder1 <- 
  rbind(samp1 %>% select(id, text, entropy, legalese, entities, flesch) %>% mutate(overlap = T),
                samp2 %>% select(id, text, entropy, legalese, entities, flesch) %>% mutate(overlap = F)) %>% 
  slice_sample(prop = 1) # Shuffle rows

coder2 <- 
  rbind(samp1 %>% select(id, text, entropy, legalese, entities, flesch) %>% mutate(overlap = T),
        samp3 %>% select(id, text, entropy, legalese, entities, flesch) %>% mutate(overlap = F)) %>% 
  slice_sample(prop = 1) # Shuffle rows

coder3 <- 
  rbind(samp1 %>% select(id, text, entropy, legalese, entities, flesch) %>% mutate(overlap = T),
        samp4 %>% select(id, text, entropy, legalese, entities, flesch) %>% mutate(overlap = F)) %>% 
  slice_sample(prop = 1) # Shuffle rows



coder1 %>% 
  write.csv("./coder_samples/coder1texts.csv",
          row.names = FALSE,    # don’t add row numbers
          quote = TRUE)

coder2 %>% 
  write.csv("./coder_samples/coder2texts.csv",
            row.names = FALSE,    # don’t add row numbers
            quote = TRUE)

coder3 %>% 
  write.csv("./coder_samples/coder3texts.csv",
            row.names = FALSE,    # don’t add row numbers
            quote = TRUE)



