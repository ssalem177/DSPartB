# Author - Sami Salem
# Date - Sep 27 2025

# Loading required packages

library(tidyverse)
library(stringr)
library(dplyr)
library(tidytext)
library(textstem)
library(lexicon)

# getting dictionaries

data("hash_sentiment_senticnet") # SenticNet
data("hash_sentiment_sentiword") # SentiWordNet

# Making the dictionaries usable for inner-join and unnest_tokens
senticnet <- tibble(hash_sentiment_senticnet) %>%
  mutate(word = x) %>%
  mutate(value = y) %>% 
  select(word, value)

sentiwordnet <- tibble(hash_sentiment_sentiword) %>%
  mutate(word = x) %>%
  mutate(value = y) %>% 
  select(word, value)


AFINN <- get_sentiments("afinn") # AFINN


# Trying to see performance for the two key relationships per dictionary

dictionary_performance <- function(book_name, prediction, Speaker, Addressee){
  
  quotes <- read_csv(paste0('Quotes/',prediction,'/',book_name,'quotes.csv'))
  
  quotes <- quotes %>%
    filter(speaker == Speaker) %>%
    filter(addressee == Addressee)
  
  
  for (j in 1:length(quotes$dialogue)){
    quotes$dialogue[j] = str_remove_all(quotes$dialogue[j], '[:punct:]')
  }
  
  match_afinn <- 0 # how many words the book covers
  match_senticnet <- 0 # how many words the book covers
  match_sentiwordnet <- 0 # how many words the book covers
  
  total <- 0 # total of how many words the book covers
  
  for (j in 1:length(quotes$dialogue)){
    
    words <- tibble(text = quotes$dialogue[j]) %>% 
      unnest_tokens(word, text) %>%
      anti_join(stop_words)  #removal of stop words
    
    words$word <- 
      lemmatize_words(words$word) #lemmatization
    
    words <- words %>%
      filter(word != 'newquote')
    
    total <- total + length(words$word)
    
    afinn_matches <- length(inner_join(words, AFINN)$word)
    
    senticnet_matches <- length(inner_join(words, senticnet)$word)
    
    sentiwordnet_matches <- length(inner_join(words, sentiwordnet)$word)
    
    match_afinn <- match_afinn + afinn_matches
    
    match_senticnet <- match_senticnet + senticnet_matches
    
    match_sentiwordnet <- match_sentiwordnet + sentiwordnet_matches
    
  }
  
  
  
  return(c(match_afinn/total, match_senticnet/total, match_sentiwordnet/total))
  
}

AG_dictionary_performance_cor <- dictionary_performance('AnneOfGreenGables', 'Correct', 'Anne Shirley', 'Marilla Cuthbert')
AG_dictionary_performance_bl <- dictionary_performance('AnneOfGreenGables', 'Baseline', 'Anne Shirley', 'Marilla Cuthbert')
AG_dictionary_performance_ml <- dictionary_performance('AnneOfGreenGables', 'ML', 'Anne Shirley', 'Marilla Cuthbert')

ED_dictionary_performance_cor <- dictionary_performance('PrideAndPrejudice', 'Correct', 'Elizabeth', 'Mr. Darcy')
ED_dictionary_performance_bl <- dictionary_performance('PrideAndPrejudice', 'Baseline', 'Elizabeth', 'Mr. Darcy')
ED_dictionary_performance_ml <- dictionary_performance('PrideAndPrejudice', 'ML', 'Elizabeth', 'Mr. Darcy')

return <- tibble(' ' = c('AFINN','SENTICNET','SENTIWORD'),
                 'AG_Cor' = AG_dictionary_performance_cor,
                 'AG_BL' = AG_dictionary_performance_bl, 
                 'AG_ML' = AG_dictionary_performance_ml,
                 'ED_Cor' = ED_dictionary_performance_cor,
                 'ED_BL' = ED_dictionary_performance_bl,
                 'ED_ML' = ED_dictionary_performance_ml)

write_csv(return,'dictionary_performance.csv')

# Comparing sentiment for two key relationships in the novels

sentiment_extractor_senticnet <- function(book_name,prediction, Speaker, Addressee){
  
  quotes <- read_csv(paste0('Quotes/',prediction,'/',book_name,'quotes.csv'))
  
  quotes <- quotes %>%
    filter(speaker == Speaker) %>%
    filter(addressee == Addressee)
  
  for (i in 1:length(quotes$dialogue)) {
    
    words <- tibble(text = quotes$dialogue[i]) %>% 
      unnest_tokens(word, text) %>%
      anti_join(stop_words)  #removal of stop words
    
    words$word <- 
      lemmatize_words(words$word) #lemmatization
    
    words <- words %>%
      filter(word != 'newquote')
    
    words_sent <- words %>% inner_join(senticnet)
    
    quotes$sentiment[i] <- sum(words_sent$value, na.rm = TRUE)/(length(words$word))
    
    if (is.na(quotes$sentiment[i])){ # if there is no sentiment in the quote, it will be NaN, so just make it 0 to not lose data
      
      quotes$sentiment[i] <- 0
      
    }
  }
  
  return(quotes)
  
}

## PRIDE-AND-PREJUDICE

baseline_quotes <- read_csv(paste0('Quotes/Baseline/PrideandPrejudicequotes.csv'))

ML_quotes <- read_csv(paste0('Quotes/ML/PrideandPrejudicequotes.csv'))

Correct_quotes <- read_csv(paste0('Quotes/Correct/PrideandPrejudicequotes.csv'))

library(gridExtra)

sub_baseline_quotes <- sentiment_extractor_senticnet('PrideandPrejudice','Baseline','Elizabeth','Mr. Darcy')

sub_ML_quotes <- sentiment_extractor_senticnet('PrideandPrejudice','ML','Elizabeth','Mr. Darcy')

sub_Correct_quotes <- sentiment_extractor_senticnet('PrideandPrejudice','Correct','Elizabeth','Mr. Darcy')


# grouping sentiment by chapter

sent_baseline_quotes <- sub_baseline_quotes %>%
  group_by(chapter) %>%
  summarise(mean = mean(sentiment, na.rm = TRUE))

sent_ML_quotes <- sub_ML_quotes %>%
  group_by(chapter) %>%
  summarise(mean = mean(sentiment, na.rm = TRUE))

sent_Correct_quotes <- sub_Correct_quotes %>%
  group_by(chapter) %>%
  summarise(mean = mean(sentiment, na.rm = TRUE))

# plots

plot_1 <- sent_ML_quotes %>% ggplot(aes(chapter, mean)) + geom_line(col = "gray") +  geom_point() + theme_bw() + labs(x = "Chapter", y = "Sentiment", title = "ML Attribution") + xlim(0,61) + ylim(-0.6,0.6)
plot_2 <- sent_baseline_quotes %>% ggplot(aes(chapter, mean)) + geom_line(col = "gray") +  geom_point() + theme_bw() + labs(x = "Chapter", y = "Sentiment", title = "Baseline Attribution") + xlim(0,61) + ylim(-0.6,0.6)
plot_3 <- sent_Correct_quotes %>% ggplot(aes(chapter, mean)) + geom_line(col = "gray") + geom_point() + theme_bw() + labs(x = "Chapter", y = "Sentiment", title = "Correct Attribution") + xlim(0,61) + ylim(-0.6,0.6)

grid.arrange(plot_3,plot_1,plot_2,ncol = 2)

# nice interaction for elizabeth and mr. darcy

## ANNE-OF-GREEN-GABLES

baseline_quotes <- read_csv(paste0('Quotes/Baseline/AnneOfGreenGablesquotes.csv'))

ML_quotes <- read_csv(paste0('Quotes/ML/AnneOfGreenGablesquotes.csv'))

Correct_quotes <- read_csv(paste0('Quotes/Correct/AnneOfGreenGablesquotes.csv'))


sub_baseline_quotes <- sentiment_extractor_senticnet('AnneOfGreenGables','Baseline','Anne Shirley','Marilla Cuthbert')

sub_ML_quotes <- sentiment_extractor_senticnet('AnneOfGreenGables','ML','Anne Shirley','Marilla Cuthbert')

sub_Correct_quotes <- sentiment_extractor_senticnet('AnneOfGreenGables','Correct','Anne Shirley','Marilla Cuthbert')



# grouping sentiment by chapter

sent_baseline_quotes <- sub_baseline_quotes %>%
  group_by(chapter) %>%
  summarise(mean = mean(sentiment, na.rm = TRUE))

sent_ML_quotes <- sub_ML_quotes %>%
  group_by(chapter) %>%
  summarise(mean = mean(sentiment, na.rm = TRUE))

sent_Correct_quotes <- sub_Correct_quotes %>%
  group_by(chapter) %>%
  summarise(mean = mean(sentiment, na.rm = TRUE))



plot_1 <- sent_ML_quotes %>% ggplot(aes(chapter, mean)) + geom_line(col = "gray") + geom_point() + theme_bw() + labs(x = "Chapter", y = "Sentiment", title = "ML Attribution") + xlim(0,40) + ylim(-0.6,0.6)
plot_2 <- sent_baseline_quotes %>% ggplot(aes(chapter, mean)) + geom_line(col = "gray") +  geom_point() + theme_bw() + labs(x = "Chapter", y = "Sentiment", title = "Baseline Attribution") + xlim(0,40) + ylim(-0.6,0.6)
plot_3 <- sent_Correct_quotes %>% ggplot(aes(chapter, mean)) + geom_line(col = "gray") +  geom_point() + theme_bw() + labs(x = "Chapter", y = "Sentiment", title = "Correct Attribution") + xlim(0,40) + ylim(-0.6,0.6)

grid.arrange(plot_3,plot_1,plot_2,ncol = 2)



