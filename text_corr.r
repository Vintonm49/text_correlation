# Word Co-Occurence and Correlation
# LTC Melanie Vinton
# 2 August 2017
#
# Much of this is based on the book "Text Mining with R" by Julia Silge and David Robinson.
# The data is a vector of comments from a free-response question in a survey

library(tidytext)
library(dplyr)
library(qdap)

text_concern$id <- seq(1,nrow(text_concern))
tidy_text <- unnest_tokens(tbl = text_concern, output = word, input = why_concerned)
head(tidy_text, 10)

# Remove stopwords, numbers, and spaces.
tidy_text$word <-gsub("\\(?[0-9,.]+\\)?","",tidy_text$word)          # converts numbers to spaces
tidy_text <- filter(tidy_text, word != "")                           # eliminate rows with just spaces
data(stop_words)
tidy_text <- dplyr::anti_join(tidy_text, stop_words)                 # eliminate rows with stopwords

count(tidy_text, word, sort= TRUE)


# Word Co-occurrence
library(devtools)
#install_github("dgrtwo/widyr")
library(widyr)

text_pairs <- pairwise_count(tidy_text,word,id,  sort = TRUE, upper = FALSE)
head(text_pairs)


# Co-occurrence Network Visualization
text_pairs2 <- filter(text_pairs, n > 150)

library(ggplot2)
library(igraph)
library(ggraph)

g <- graph_from_data_frame(text_pairs2)
g2 <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle("Word pairings in Q6 - Extremely Concerned")+
  theme_void()
g2


# Word Correlation
text_cor2 <- filter(group_by(tidy_text, word), n()>50)

text_cor2 <- pairwise_cor(text_cor2, word, id, sort = TRUE, upper = FALSE)
head(text_cor2)

text_cor3 <- filter(text_cor2, correlation > .3)
dim(text_cor2)

dim(text_cor3)

c <- graph_from_data_frame(text_cor3)

c2 <- ggraph(c, layout = "fr") +
  geom_edge_link(aes(edge_width = correlation, edge_alpha = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  ggtitle("Word Correlations for Q6 - Extremely Concerned") +
  theme_void()
c2

# Change minimum correlation value to .5
text_cor3 <- filter(text_cor2, correlation > .5)

c <- graph_from_data_frame(text_cor3)

c2 <- ggraph(c, layout = "fr") +
  geom_edge_link(aes(edge_width = correlation, edge_alpha = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  ggtitle("Word Correlations for Q6 - Extremely Concerned") +
  theme_void()
c2



# Co-occurrence and Correlation on Phrases
library(ngram)
library(qdap)

## Set up a blank data frame
phrases <- data.frame()

## Set number of words in a phrase and the com_id variable to 1
nwords <- 2
com_id <- 1 

## Loop through text to clean it and pull out phrases
## Populate the phrases data frame
for (i in text_concern$why_concerned) {
  str2 <- gsub(' {2,}',' ',i)  #eliminates extra spaces
  str2 <- tolower(str2)
  str2 <- qdap::strip(str2)   # eliminates punctuation and numbers
  str2 <- qdap::rm_stopwords(str2,stopwords = tm::stopwords("english"), separate = FALSE)
  numwords <- length(strsplit(str2,' ')[[1]])
  
  if(numwords >= nwords){
    gram <- try(ngram(str2, n = nwords))
    df <- get.phrasetable(gram)  
    df$id <- com_id         # this is where we add the comment ID info needed by pairwise functions
    phrases <- rbind(phrases, df)
  } 
  com_id <- com_id +1
}

head(phrases)



# Two Word Co-Occurrence Example
phrase_pairs <- pairwise_count(phrases, ngrams, id,  sort = TRUE, upper = FALSE)
phrase_pairs2 <- filter(phrase_pairs, n > 25)

g <- graph_from_data_frame(phrase_pairs2)
g2 <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle("Phrase pairings in Q6 - Extremely Concerned")+
  theme_void()
g2


# Two Word Correlation Example
phrase_cor <- filter(group_by(phrases, ngrams), n()>50)
phrase_cor2 <- pairwise_cor(phrase_cor, ngrams, id, sort = TRUE, upper = FALSE)
phrase_cor3 <- filter(phrase_cor2, correlation > .3)

c <- graph_from_data_frame(phrase_cor3)

c2 <- ggraph(c, layout = "fr") +
  geom_edge_link(aes(edge_width = correlation, edge_alpha = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  ggtitle("Phrase Correlations for Q6 - Extremely Concerned") +
  theme_void()
c2


