library(tidyverse)

# Date: Oct 31 2025
# Author: Sami Salem

library(tidyverse)
library(dplyr)
library(tidytext)
library(ggplot2)
library(textstem)

# The method of plotting the top words based on tf-idf was adapted from the following source

#Silge J and Robinson D. Welcome to Tidy Text Mining with R.
#O’Reilly, 2017


########TF-IDF###############

# function to add the tf-idf per Part / per relationship / based on type of attribution (Correct / ML / Baseline)

relations_tfidf <- function(book_name, Speaker, Addressee, type){
  
  book <- read_csv(paste0('Quotes/',type,'/',book_name,'quotes.csv'))
  
  # filtering the book on the speaker and addressee
  book <- book %>%
    filter(speaker == Speaker) %>%
    filter(addressee == Addressee) 
  
  # splitting the book by part
  divisor <- ceiling(length(book$speaker)/3)
  
  if (length(book$speaker) %% 3 == 0){
    
    book <- book %>%
      mutate(Part = paste0('Part ',floor(row_number()/divisor)+1)) %>%
      filter(row_number() != length(book$speaker))
  }
  
  if (length(book$speaker) %% 3 != 0){
    
    book <- book %>%
      mutate(Part = paste0('Part ',floor(row_number()/divisor)+1)) 
      
  }
  
  # concatenating all dialogue per part
  book_dialogue <- book %>%
    select(dialogue, Part) %>%
    group_by(Part) %>%
    mutate(text = paste0(dialogue, collapse = " ")) %>%
    mutate(text = str_remove_all(text, '[:punct:]')) %>% # no need for punctuation text for tokenizing
    slice(1) %>%
    select(Part, text) 
  

  # tokenizing and lemmatizing 
  relations_tokens <- book_dialogue %>%
    unnest_tokens(word, text) %>%
    mutate(word = lemmatize_words(word)) %>%
    count(Part,word) %>%
    anti_join(stop_words) %>%
    filter(!(word %in% c('sir','newquote'))) #sir is a stop-word, newquote just means new paragraph
  
  # completing tf-idf
  relations_tfidf <- relations_tokens %>%
    bind_tf_idf(word, Part, n)
  
  return(relations_tfidf)
  
}


#PRIDE-AND-PREJUDICE

# plotting the top tf-idf words per type of attribution 

pride_tfidf_correct <- relations_tfidf('PrideandPrejudice','Elizabeth','Mr. Darcy','Correct')

pride_tfidf_correct %>%
  group_by(Part) %>%
  filter(n > 1) %>%
  filter(idf > 0) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Part)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Part, ncol =3, scales = "free") + theme_bw() +
  labs(x = "tf-idf (Correct)", y = "token", title = "Elizabeth to Mr. Darcy") + scale_fill_brewer(palette = "Dark2")


pride_tfidf_BL <- relations_tfidf('PrideandPrejudice','Elizabeth','Mr. Darcy','Baseline')

pride_tfidf_BL %>%
  group_by(Part) %>%
  filter(n > 1) %>%
  filter(idf > 0) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Part)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Part, ncol =3, scales = "free") + theme_bw() + 
  labs(x = "tf-idf (BL)", y = "token", title = "Elizabeth to Mr. Darcy") + scale_fill_brewer(palette = "Dark2")

pride_tfidf_ML <- relations_tfidf('PrideandPrejudice','Elizabeth','Mr. Darcy','ML') 

pride_tfidf_ML %>%
  group_by(Part) %>%
  filter(n > 1) %>%
  filter(idf > 0) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Part)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Part, ncol =3, scales = "free") + theme_bw() + 
  labs(x = "tf-idf (ML)", y = "token", title = "Elizabeth to Mr. Darcy") + scale_fill_brewer(palette = "Dark2")



#ANNE-OF-GREEN-GABLES

# plotting the top tf-idf words per type of attribution 

pride_tfidf_correct <- relations_tfidf('AnneOfGreenGables','Anne Shirley','Marilla Cuthbert','Correct')

pride_tfidf_correct %>%
  group_by(Part) %>%
  filter(n > 1) %>%
  filter(idf > 0) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Part)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Part, ncol =3, scales = "free") + theme_bw() + 
  labs(x = "tf-idf (Correct)", y = "token", title = "Anne Shirley to Marilla Cuthbert") + scale_fill_brewer(palette = "Dark2")

pride_tfidf_BL <- relations_tfidf('AnneOfGreenGables','Anne Shirley','Marilla Cuthbert','Baseline')

pride_tfidf_BL %>%
  group_by(Part) %>%
  filter(n > 1) %>%
  filter(idf > 0) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Part)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Part, ncol =3, scales = "free") + theme_bw() + 
  labs(x = "tf-idf (BL)", y = "token", title = "Anne Shirley to Marilla Cuthbert") + scale_fill_brewer(palette = "Dark2")

pride_tfidf_ML <- relations_tfidf('AnneOfGreenGables','Anne Shirley','Marilla Cuthbert','ML')

pride_tfidf_ML %>%
  group_by(Part) %>%
  filter(n > 1) %>%
  filter(idf > 0) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Part)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Part, ncol =3, scales = "free") + theme_bw() + 
  labs(x = "tf-idf (ML)", y = "token", title = "Anne Shirley to Marilla Cuthbert") + scale_fill_brewer(palette = "Dark2")

