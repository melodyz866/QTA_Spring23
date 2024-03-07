library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(stm)
library(lubridate)
library(dplyr)


# Load the extracted text data
text_data <- lapply(list("/Users/zhangshengbinmelody/Downloads/extracted_text_0.txt", 
                         "/Users/zhangshengbinmelody/Downloads/extracted_text_1.txt", 
                         "/Users/zhangshengbinmelody/Downloads/extracted_text_2.txt",
                         "/Users/zhangshengbinmelody/Downloads/esg 4.txt",
                         "/Users/zhangshengbinmelody/Downloads/esg 5.txt"), readLines)
                          
# Combine into a data frame
data <- data.frame(content = unlist(text_data), stringsAsFactors = FALSE)
corpus <- corpus(data, text_field = "content")
corpus <- corpus(data)

tokens <- tokens(corpus, what = "word", 
                 remove_punct = TRUE, remove_numbers = TRUE, 
                 remove_symbols = TRUE, remove_url = TRUE, 
                 remove_separators = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en"))

library(wordcloud)
wordcloud(words = tokens, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
# Re-tokenize the corpus by sentences to get accurate sentence lengths
tokens_sentences <- tokens(corpus, what = "sentence")

# Calculate the length of each sentence by counting tokens within each sentence
sentence_lengths <- sapply(tokens_sentences, length)
# Create a data frame for plotting
df_sentence_lengths <- data.frame(SentenceLength = sentence_lengths)
# Plotting the distribution of sentence lengths
ggplot(df_sentence_lengths, aes(x = SentenceLength)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Sentence Lengths",
       x = "Sentence Length (Number of Words)",
       y = "Frequency")

# Generate a document-feature matrix
dfm <- dfm(tokens)

text_matrix <- convert(dfm, to = "stm")

# Fit STM model for the most frequent words. Adjust the number of topics as needed.
stm_model <- stm(text_matrix$documents, text_matrix$vocab, K = 20, max.em.its = 75, data = text_matrix$meta, init.type = "Spectral")

# Check the model's output
print(summary(stm_model))

#print top keywords of each topic
labelTopics(stm_model)
# plot prevalence and top terms
plot(stm_model, type="summary", xlim=c(0, 0.5))


library(stm)
library(ggplot2)


# Extract the topic probability for the first topic across all documents
topic_probabilities <- stm_model$theta[1, ]  # Adjust the index for any topics

# Create a data frame for plotting
# Assume `doc_id` is a sequence number of your documents or any other identifier you have
data_frame_for_plot <- data.frame(
  doc_id = 1:length(topic_probabilities),
  topic_probability = topic_probabilities
)

# Plot using ggplot2
ggplot(data_frame_for_plot, aes(x = doc_id, y = topic_probability)) +
  geom_line() +  # Use geom_point() if you prefer points
  labs(title = "Topic Probability Across Documents",
       x = "Document ID",
       y = "Topic Probability") +
  theme_minimal()

# Extract topic correlations
topic_correlations <- topicCorr(stm_model)

# Plot correlations
plot.topicCorr(topic_correlations,
               vlabels = seq(1:ncol(stm_model$theta)),
               vertex.color = "white",
               main = "Topic Correlations")

#Calculate Semantic Exclusivity
excl <- exclusivity(stm_model, M = 10)
print(excl)

# Plotting the estimated topic proportions
plot(stm_model, type = "summary", main = "Estimated Topic Proportions")


set.seed(123) # For reproducibility
coherence_scores <- runif(length(excl), min = 0, max = 1)

# Create a data frame for plotting
topic_quality <- data.frame(Topic = 1:length(excl),
                            Exclusivity = excl,
                            Coherence = coherence_scores)

# Plotting Semantic Exclusivity vs. Coherence (hypothetical coherence values)
ggplot(topic_quality, aes(x = Exclusivity, y = Coherence, label = Topic)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 1.5) +
  theme_minimal() +
  labs(title = "Semantic Exclusivity vs. Coherence",
       x = "Exclusivity",
       y = "Coherence",
       caption = "Note: Coherence values are hypothetical in this example")
