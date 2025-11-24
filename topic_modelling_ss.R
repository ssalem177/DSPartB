####################### STATIC TOPIC MODELLING ################################


# Date: Oct 31 2025
# Author: Sami Salem



library(tidyverse)
library(dplyr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(textstem)
library(gridExtra)


# The function that extracts all 3 topics per relationship / per type of attribution

topic_relations <- function(book_name, prediction, Speaker, Addressee){
  
  book <- read_csv(paste0('Quotes/',prediction,'/',book_name,'quotes.csv'))
  
  # filtering relationships
  book <- book %>%
    filter(speaker == Speaker) %>%
    filter(addressee == Addressee) 
  
  # splitting into parts
  
  divisor <- ceiling(length(book$speaker)/3)
  
  if (length(book$speaker) %% 3 == 0){

    book <- book %>%
      mutate(Part = paste0('Part ',floor(row_number()/divisor)+1)) %>%
      mutate(Part = as.factor(Part)) %>%
      filter(row_number() != length(book$speaker))
  }
  
  if (length(book$speaker) %% 3 != 0){
    
    book <- book %>%
      mutate(Part = paste0('Part ',floor(row_number()/divisor)+1)) %>%
      mutate(Part = as.factor(Part)) 
      
  }
  
  
  book_dialogue <- book %>%
    select(dialogue, speaker, addressee, Part) %>%
    group_by(Part) %>%
    mutate(text = paste0(dialogue, collapse = " ")) %>%
    mutate(text = str_remove_all(text, '[:punct:]')) %>% # no need for punctuation text for tokenizing
    slice(1) %>%
    select(Part, text)

  # tokenize and lemmatize
  
  conversation_tokens <- book_dialogue %>%
      unnest_tokens(word, text) %>%
      count(Part, word) %>%
      mutate(word = lemmatize_words(word)) %>%
      anti_join(stop_words) %>%
      filter(!(word %in% c('sir','newquote'))) #sir is a stop-word, newquote just means new paragraph
  
  # Turn into document-term matrix
  
  book_dtm <- conversation_tokens %>%
    cast_dtm(Part, word, n)
  
  # topic model (LDA) 
  
  book_lda <- LDA(book_dtm, k = 3, control = list(seed = 1774231), method = "Gibbs")
  
  # per topic per word probabilities called beta
  
  topics_beta <- tidy(book_lda, matrix = "beta")
  
  # topic distributions (gamma)
  chapters_gamma <- tidy(book_lda, matrix = "gamma")
  
  return(list(topics_beta, chapters_gamma))
}

# It looks like choosing larger amounts of topics allow you to isolate specific ones that are more 
# prevalent. Here we see that picking 5 topics allow the third and fourth ones to be more prevalent. 

## PRIDE-AND-PREJUDICE

relations_ML <- topic_relations("PrideandPrejudice","ML","Elizabeth","Mr. Darcy")

gamma_probabilites_ML <- relations_ML[2][[1]]
beta_probailities_ML <- relations_ML[1][[1]]

relations_BL <- topic_relations("PrideandPrejudice","Baseline","Elizabeth","Mr. Darcy")

gamma_probabilites_BL <- relations_BL[2][[1]]
beta_probailities_BL <- relations_BL[1][[1]]

relations_Correct <- topic_relations("PrideandPrejudice","Correct","Elizabeth","Mr. Darcy")

gamma_probabilites_Correct <- relations_Correct[2][[1]]
beta_probailities_Correct <- relations_Correct[1][[1]]


plot_1 <- gamma_probabilites_Correct %>%
  mutate(topic = factor(topic)) %>%
  mutate(document = factor(document)) %>%
  ggplot(aes(document, gamma, fill = topic)) + 
  labs(title = "Correct Topic Modelling (ED)", x = 'Parts', y = 'Gamma') + 
  geom_col(position = position_dodge(width = 0.9)) + theme_bw() + scale_fill_brewer(palette = "Dark2")


plot_3 <- gamma_probabilites_BL %>%
  mutate(topic = factor(topic)) %>%
  mutate(document = factor(document)) %>%
  ggplot(aes(document, gamma, fill = topic)) + 
  labs(title = "Baseline Topic Modelling (ED)", x = 'Parts', y = 'Gamma') + 
  geom_col(position = position_dodge(width = 0.9)) + theme_bw() + scale_fill_brewer(palette = "Dark2")

plot_2 <- gamma_probabilites_ML %>%
  mutate(topic = factor(topic)) %>%
  mutate(document = factor(document)) %>%
  ggplot(aes(document, gamma, fill = topic)) + 
  labs(title = "ML Topic Modelling (ED)", x = 'Parts', y = 'Gamma') + 
  geom_col(position = position_dodge(width = 0.9)) + theme_bw() + scale_fill_brewer(palette = "Dark2")


grid.arrange(plot_1,plot_2,plot_3,ncol = 2)



## Top-Words plots

relation_topics <- beta_probailities_Correct

diff_topics <- relation_topics %>% spread(key = "topic", value = "beta")

diff_topics$diff_1 <- log(diff_topics$'1'/(diff_topics$'2'+diff_topics$'3'))
diff_topics$diff_2 <- log(diff_topics$'2'/(diff_topics$'1'+diff_topics$'3'))
diff_topics$diff_3 <- log(diff_topics$'3'/(diff_topics$'1'+diff_topics$'2'))

top_first_topic <- diff_topics %>% arrange(-diff_1)
top_second_topic <- diff_topics %>% arrange(-diff_2) 
top_third_topic <- diff_topics %>% arrange(-diff_3) 

# saving top 33 terms in each of the three topics

library(xtable)

m_top_1 <- top_first_topic %>% arrange(desc(diff_1)) %>% head(n = 33)
m_top_2 <- top_second_topic %>% arrange(desc(diff_2)) %>% head(n = 33)
m_top_3 <- top_third_topic %>% arrange(desc(diff_3)) %>% head(n = 33)


m_top_1 <- m_top_1 %>% add_column(rank = 1:33, .before = "term")
m_top_2 <- m_top_2 %>% add_column(rank = 1:33, .before = "term")
m_top_3 <- m_top_3 %>% add_column(rank = 1:33, .before = "term")


m_top_1['diff_1'] <- m_top_1$diff_1
m_top_1['rank2'] <- m_top_2$rank
m_top_1['term2'] <- m_top_2$term
m_top_1['diff_2'] <- m_top_2$diff_2
m_top_1['rank3'] <- m_top_3$rank
m_top_1['term3'] <- m_top_3$term
m_top_1['diff_3'] <- m_top_3$diff_3

m_top_1 <- m_top_1 %>% select(rank,term,diff_1,rank2,term2,diff_2,rank3,term3,diff_3)

cptn <- paste("Top 33 terms for each Correct Topic with their probabilities.")
lbl <- paste("top terms for Correct topics (ED)")
fn <- paste('Tables/',lbl, ".tex", sep = "")

print(xtable(m_top_1, type = "latex", caption = cptn, label = lbl, digits=c(0,0,0,4,0,0,4,0,0,4)), file = fn, include.rownames = FALSE)


# ML Topics

relation_topics <- beta_probailities_ML

diff_topics <- relation_topics %>% spread(key = "topic", value = "beta")

diff_topics$diff_1 <- log(diff_topics$'1'/(diff_topics$'2'+diff_topics$'3'))
diff_topics$diff_2 <- log(diff_topics$'2'/(diff_topics$'1'+diff_topics$'3'))
diff_topics$diff_3 <- log(diff_topics$'3'/(diff_topics$'1'+diff_topics$'2'))

top_first_topic <- diff_topics %>% arrange(-diff_1)
top_second_topic <- diff_topics %>% arrange(-diff_2) 
top_third_topic <- diff_topics %>% arrange(-diff_3) 


# saving top 33 terms in each of the three topics

library(xtable)

m_top_1 <- top_first_topic %>% arrange(desc(diff_1)) %>% head(n = 33)
m_top_2 <- top_second_topic %>% arrange(desc(diff_2)) %>% head(n = 33)
m_top_3 <- top_third_topic %>% arrange(desc(diff_3)) %>% head(n = 33)


m_top_1 <- m_top_1 %>% add_column(rank = 1:33, .before = "term")
m_top_2 <- m_top_2 %>% add_column(rank = 1:33, .before = "term")
m_top_3 <- m_top_3 %>% add_column(rank = 1:33, .before = "term")


m_top_1['diff_1'] <- m_top_1$diff_1
m_top_1['rank2'] <- m_top_2$rank
m_top_1['term2'] <- m_top_2$term
m_top_1['diff_2'] <- m_top_2$diff_2
m_top_1['rank3'] <- m_top_3$rank
m_top_1['term3'] <- m_top_3$term
m_top_1['diff_3'] <- m_top_3$diff_3

m_top_1 <- m_top_1 %>% select(rank,term,diff_1,rank2,term2,diff_2,rank3,term3,diff_3)

cptn <- paste("Top 33 terms for each ML Topic with their probabilities.")
lbl <- paste("top terms for ML topics (ED)")
fn <- paste('Tables/',lbl, ".tex", sep = "")

print(xtable(m_top_1, type = "latex", caption = cptn, label = lbl, digits=c(0,0,0,4,0,0,4,0,0,4)), file = fn, include.rownames = FALSE)

# BL Topics

relation_topics <- beta_probailities_BL

diff_topics <- relation_topics %>% spread(key = "topic", value = "beta")

diff_topics$diff_1 <- log(diff_topics$'1'/(diff_topics$'2'+diff_topics$'3'))
diff_topics$diff_2 <- log(diff_topics$'2'/(diff_topics$'1'+diff_topics$'3'))
diff_topics$diff_3 <- log(diff_topics$'3'/(diff_topics$'1'+diff_topics$'2'))

top_first_topic <- diff_topics %>% arrange(-diff_1)
top_second_topic <- diff_topics %>% arrange(-diff_2) 
top_third_topic <- diff_topics %>% arrange(-diff_3) 

top_first_topic %>% print(n=25)
top_second_topic %>% print(n=25)
top_third_topic %>% print(n=25)


# saving top 33 terms in each of the three topics

library(xtable)

m_top_1 <- top_first_topic %>% arrange(desc(diff_1)) %>% head(n = 33)
m_top_2 <- top_second_topic %>% arrange(desc(diff_2)) %>% head(n = 33)
m_top_3 <- top_third_topic %>% arrange(desc(diff_3)) %>% head(n = 33)


m_top_1 <- m_top_1 %>% add_column(rank = 1:33, .before = "term")
m_top_2 <- m_top_2 %>% add_column(rank = 1:33, .before = "term")
m_top_3 <- m_top_3 %>% add_column(rank = 1:33, .before = "term")


m_top_1['diff_1'] <- m_top_1$diff_1
m_top_1['rank2'] <- m_top_2$rank
m_top_1['term2'] <- m_top_2$term
m_top_1['diff_2'] <- m_top_2$diff_2
m_top_1['rank3'] <- m_top_3$rank
m_top_1['term3'] <- m_top_3$term
m_top_1['diff_3'] <- m_top_3$diff_3

m_top_1 <- m_top_1 %>% select(rank,term,diff_1,rank2,term2,diff_2,rank3,term3,diff_3)

cptn <- paste("Top 33 terms for each BL Topic with their probabilities.")
lbl <- paste("top terms for BL topics (ED)")
fn <- paste('Tables/',lbl, ".tex", sep = "")

print(xtable(m_top_1, type = "latex", caption = cptn, label = lbl, digits=c(0,0,0,4,0,0,4,0,0,4)), file = fn, include.rownames = FALSE)






## ANNE-OF-GREENGABLES

relations_ML <- topic_relations("AnneOfGreenGables","ML","Anne Shirley","Marilla Cuthbert")

gamma_probabilites_ML <- relations_ML[2][[1]]
beta_probailities_ML <- relations_ML[1][[1]]

diff_topics <- beta_probailities_ML %>% spread(key = "topic", value = "beta")


relations_BL <- topic_relations("AnneOfGreenGables","Baseline","Anne Shirley","Marilla Cuthbert")

gamma_probabilites_BL <- relations_BL[2][[1]]
beta_probailities_BL <- relations_BL[1][[1]]

relations_Correct <- topic_relations("AnneOfGreenGables","Correct","Anne Shirley","Marilla Cuthbert")

gamma_probabilites_Correct <- relations_Correct[2][[1]]
beta_probailities_Correct <- relations_Correct[1][[1]]


plot_1 <- gamma_probabilites_Correct %>%
  mutate(topic = factor(topic)) %>%
  mutate(document = factor(document)) %>%
  ggplot(aes(document, gamma, fill = topic)) + 
  labs(title = "Correct Topic Modelling (AG)", x = 'Parts', y = 'Gamma') + 
  geom_col(position = position_dodge(width = 0.9)) + theme_bw() + scale_fill_brewer(palette = "Dark2")


plot_3 <- gamma_probabilites_BL %>%
  mutate(topic = factor(topic)) %>%
  mutate(document = factor(document)) %>%
  ggplot(aes(document, gamma, fill = topic)) + 
  labs(title = "Baseline Topic Modelling (AG)", x = 'Parts', y = 'Gamma') + 
  geom_col(position = position_dodge(width = 0.9)) + theme_bw() + scale_fill_brewer(palette = "Dark2")

plot_2 <- gamma_probabilites_ML %>%
  mutate(topic = factor(topic)) %>%
  mutate(document = factor(document)) %>%
  ggplot(aes(document, gamma, fill = topic)) + 
  labs(title = "ML Topic Modelling (AG)", x = 'Parts', y = 'Gamma') + 
  geom_col(position = position_dodge(width = 0.9)) + theme_bw() + scale_fill_brewer(palette = "Dark2")


grid.arrange(plot_1,plot_2,plot_3,ncol = 2)


# most popular words in topics

# Correct Topics

relation_topics <- beta_probailities_Correct

diff_topics <- relation_topics %>% spread(key = "topic", value = "beta")

diff_topics$diff_1 <- log(diff_topics$'1'/(diff_topics$'2'+diff_topics$'3'))
diff_topics$diff_2 <- log(diff_topics$'2'/(diff_topics$'1'+diff_topics$'3'))
diff_topics$diff_3 <- log(diff_topics$'3'/(diff_topics$'1'+diff_topics$'2'))

top_first_topic <- diff_topics %>% arrange(-diff_1)
top_second_topic <- diff_topics %>% arrange(-diff_2) 
top_third_topic <- diff_topics %>% arrange(-diff_3) 


# saving top 33 terms in each of the three topics

library(xtable)

m_top_1 <- top_first_topic %>% arrange(desc(diff_1)) %>% head(n = 33)
m_top_2 <- top_second_topic %>% arrange(desc(diff_2)) %>% head(n = 33)
m_top_3 <- top_third_topic %>% arrange(desc(diff_3)) %>% head(n = 33)


m_top_1 <- m_top_1 %>% add_column(rank = 1:33, .before = "term")
m_top_2 <- m_top_2 %>% add_column(rank = 1:33, .before = "term")
m_top_3 <- m_top_3 %>% add_column(rank = 1:33, .before = "term")


m_top_1['diff_1'] <- m_top_1$diff_1
m_top_1['rank2'] <- m_top_2$rank
m_top_1['term2'] <- m_top_2$term
m_top_1['diff_2'] <- m_top_2$diff_2
m_top_1['rank3'] <- m_top_3$rank
m_top_1['term3'] <- m_top_3$term
m_top_1['diff_3'] <- m_top_3$diff_3

m_top_1 <- m_top_1 %>% select(rank,term,diff_1,rank2,term2,diff_2,rank3,term3,diff_3)

cptn <- paste("Top 33 terms for each Correct Topic with their probabilities.")
lbl <- paste("top terms for Correct topics (AG)")
fn <- paste('Tables/',lbl, ".tex", sep = "")

print(xtable(m_top_1, type = "latex", caption = cptn, label = lbl, digits=c(0,0,0,4,0,0,4,0,0,4)), file = fn, include.rownames = FALSE)


# ML Topics

relation_topics <- beta_probailities_ML

diff_topics <- relation_topics %>% spread(key = "topic", value = "beta")

diff_topics$diff_1 <- log(diff_topics$'1'/(diff_topics$'2'+diff_topics$'3'))
diff_topics$diff_2 <- log(diff_topics$'2'/(diff_topics$'1'+diff_topics$'3'))
diff_topics$diff_3 <- log(diff_topics$'3'/(diff_topics$'1'+diff_topics$'2'))

top_first_topic <- diff_topics %>% arrange(-diff_1)
top_second_topic <- diff_topics %>% arrange(-diff_2) 
top_third_topic <- diff_topics %>% arrange(-diff_3) 


# saving top 33 terms in each of the three topics

library(xtable)

m_top_1 <- top_first_topic %>% arrange(desc(diff_1)) %>% head(n = 33)
m_top_2 <- top_second_topic %>% arrange(desc(diff_2)) %>% head(n = 33)
m_top_3 <- top_third_topic %>% arrange(desc(diff_3)) %>% head(n = 33)


m_top_1 <- m_top_1 %>% add_column(rank = 1:33, .before = "term")
m_top_2 <- m_top_2 %>% add_column(rank = 1:33, .before = "term")
m_top_3 <- m_top_3 %>% add_column(rank = 1:33, .before = "term")


m_top_1['diff_1'] <- m_top_1$diff_1
m_top_1['rank2'] <- m_top_2$rank
m_top_1['term2'] <- m_top_2$term
m_top_1['diff_2'] <- m_top_2$diff_2
m_top_1['rank3'] <- m_top_3$rank
m_top_1['term3'] <- m_top_3$term
m_top_1['diff_3'] <- m_top_3$diff_3

m_top_1 <- m_top_1 %>% select(rank,term,diff_1,rank2,term2,diff_2,rank3,term3,diff_3)

cptn <- paste("Top 33 terms for each ML Topic with their probabilities.")
lbl <- paste("top terms for ML topics (AG)")
fn <- paste('Tables/',lbl, ".tex", sep = "")

print(xtable(m_top_1, type = "latex", caption = cptn, label = lbl, digits=c(0,0,0,4,0,0,4,0,0,4)), file = fn, include.rownames = FALSE)


# BL Topics

relation_topics <- beta_probailities_BL

diff_topics <- relation_topics %>% spread(key = "topic", value = "beta")

diff_topics$diff_1 <- log(diff_topics$'1'/(diff_topics$'2'+diff_topics$'3'))
diff_topics$diff_2 <- log(diff_topics$'2'/(diff_topics$'1'+diff_topics$'3'))
diff_topics$diff_3 <- log(diff_topics$'3'/(diff_topics$'1'+diff_topics$'2'))

top_first_topic <- diff_topics %>% arrange(-diff_1)
top_second_topic <- diff_topics %>% arrange(-diff_2) 
top_third_topic <- diff_topics %>% arrange(-diff_3) 


# saving top 33 terms in each of the three topics

library(xtable)

m_top_1 <- top_first_topic %>% arrange(desc(diff_1)) %>% head(n = 33)
m_top_2 <- top_second_topic %>% arrange(desc(diff_2)) %>% head(n = 33)
m_top_3 <- top_third_topic %>% arrange(desc(diff_3)) %>% head(n = 33)


m_top_1 <- m_top_1 %>% add_column(rank = 1:33, .before = "term")
m_top_2 <- m_top_2 %>% add_column(rank = 1:33, .before = "term")
m_top_3 <- m_top_3 %>% add_column(rank = 1:33, .before = "term")


m_top_1['diff_1'] <- m_top_1$diff_1
m_top_1['rank2'] <- m_top_2$rank
m_top_1['term2'] <- m_top_2$term
m_top_1['diff_2'] <- m_top_2$diff_2
m_top_1['rank3'] <- m_top_3$rank
m_top_1['term3'] <- m_top_3$term
m_top_1['diff_3'] <- m_top_3$diff_3

m_top_1 <- m_top_1 %>% select(rank,term,diff_1,rank2,term2,diff_2,rank3,term3,diff_3)

cptn <- paste("Top 33 terms for each BL Topic with their probabilities.")
lbl <- paste("top terms for BL topics (AG)")
fn <- paste('Tables/',lbl, ".tex", sep = "")

print(xtable(m_top_1, type = "latex", caption = cptn, label = lbl, digits=c(0,0,0,4,0,0,4,0,0,4)), file = fn, include.rownames = FALSE)























