# Author - Sami Salem
# Date - Sep 19 2025

# The goal of this code is to aggregate the data-cleaning process, it is a function
# that reads in a clean file, which is to be used in all stages

######################### DATA CLEANING OF TXT FILE ###########################

cleaning <- function(book_name){
  
  # loading required packages
  
  # loading required package
  
  library(tidyverse)
  
  # reading in jeyre book
  
  book <- read.delim(paste0('pdnc_dataset/data/',book_name,'/novel_text.txt'), 
                      header = FALSE, # There is no header as it is simply a text file
                      sep = "\t", # normal delimiter for text files
                      quote = "",
                      blank.lines.skip = FALSE # blank lines are necessary because they refer to line breaks
  )
  
  book <- tibble(book)
  
  #fixing column title
  
  book$text = book$V1
  
  
  book <- book %>% select(text)
  
  
  # adding new rows for chapter number - useful for end of project in dynamic changes in relationships
  
  book <- book %>%
    mutate(chapter = cumsum(str_detect(text,
                                       regex("^chapter [\\divxlc]",
                                             ignore_case = TRUE))), # TidyText - reference add in
           line_number = row_number()) # needed for data cleaning
  
  new_chapter <- book # a newer copy to get reduced rows
  
  
  # Removing Chapter Number
  
  for (i in 1:length(book$text)){
    if (str_sub(book$text[i],1,7) == 'CHAPTER'){
      new_chapter <- new_chapter %>% filter(line_number != book$line_number[i])
    }
  }
  
  book_new <- new_chapter # a second newer copy to get reduced rows
  
  
  # Removing double line breaks
  
  
  for (i in 1:(length(new_chapter$text)-1)){
    if ((new_chapter$text[i+1] == "") & (new_chapter$text[i] == "")){
      book_new <- book_new %>% filter(line_number != new_chapter$line_number[i])
    }
  }
  
  
  
  # We remove the first now, not useful for anything
  
  book_new <- book_new %>% tail(nrow(book_new) - 1)
  
  
  # Calculating paragraph numbers
  
  book_paragraph <- book_new %>%
    mutate(paragraph = cumsum(text == ""))
  
  # Now we can remove empty lines as they do not mean anything
  
  book_paragraph <- book_paragraph %>% filter(text != "")
  
  # Now that paragraphs are identified, the paragraphs need to be grouped into one row. This is important for seperation into quotation delimiters
  
  book_paragraph <- book_paragraph %>%
    select(paragraph, text, chapter) %>%
    group_by(paragraph) %>%
    mutate(text = paste0(text, collapse = " ")) %>%
    slice(1) %>%
    ungroup()
  
  
  # Now seperation can now be made by quotation mark delimiters, used to identify quotes
  
  # We create two vectors now, one that contains all dialogue in each paragraph, 
  # (seperated by full stop if there is more than one dialogue in paragraph)
  
  # Another vector containing other text in the paragraph that isn't dialogue
  
  book_paragraph <- book_paragraph %>%
    separate_longer_delim(text, "\"") %>%
    separate_longer_delim(text, "\"")
  book_paragraph
  
  # Now we assign a new column marking each row as either 'quote' or 'append'. Note that due to the delimiters
  # we use a method described in Methods section
  
  # add a new column for class list
  
  book_paragraph <- book_paragraph %>%
    mutate(class = character(length(book_paragraph$text)),
           row_number = row_number())
  
  for (i in book_paragraph$paragraph){
    section <- book_paragraph %>% filter(paragraph == i)
    row = section$row_number[1] # tracking the row number needed to change the class column
    if (section$text[1] == ""){
      k = 1
      while (k <= length(section$class)){
        book_paragraph$class[row] = "append"
        row = row + 1
        k = k + 1
        if (k <= length(section$class)){
          book_paragraph$class[row] = "dialogue"
        }
        row = row + 1
        k = k + 1
      }
    }
    if (section$text[1] != ""){
      k = 1
      while (k <= length(section$class)){
        book_paragraph$class[row] = "append"
        row = row + 1
        k = k + 1
        if (k <= length(section$class)){
          book_paragraph$class[row] = "dialogue"
        }
        row = row + 1
        k = k + 1
      }
    }
  }
  
  # Now blank rows as they won't be necessary
  
  book_paragraph <- book_paragraph %>%
    filter(!(text == ""))
  
  # Now there is a natural grouping of quotes and appends for each paragraph, there is now also no need for row number
  
  book_paragraph <- book_paragraph %>%
    select(-row_number)
  
  # There needs to now be a natural grouping of all quotes / all appends for each paragraph. Meaning, that only one paragraph per row
  # Reasoning being that for each paragraph, by assumption, there is only one speaker, and one addressee
  
  # Utilise group_by functions in R to complete this task, noting however, we must group by both paragraph and class, as we do
  # not want the classes, (quote and appends), to be mixed with each other
  
  # Before doing this we need to track the chapter numbers for each paragraph
  
  book_paragraph_check <- book_paragraph %>% select(paragraph, chapter)
  
  book_paragraph_check <- unique(book_paragraph_check)
  
  # Ideally also to differentiate between quotes, we use a full stop to distinguish between quotes which are
  # appearing across different lines, however, this will likely be unnecessary for further analysis. 
  
  
  book_grouped <- book_paragraph %>%
    select(paragraph, text, class) %>%
    group_by(paragraph, class) %>%
    mutate(text = paste0(text, collapse = " /newquote ")) %>% # sepeartor sign new quote 
    slice(1) %>%
    ungroup()
  
  # Now there are at most 2 rows for each paragraph, one which concatenates all the
  # quotes, the other which concatenates all the appends
  
  
  # There is an issue here if there is no append for certain paragraphs, because they should be exactly 2 rows 
  # for each paragraph, because this will allow for the two quotes and appends vectors to be placed side by side
  
  for (i in book_grouped$paragraph){
    section <- book_grouped %>% filter(paragraph == i)
    
    if (length(section$class) == 1){
      if (section$class[1] == "append"){
        book_grouped <- book_grouped %>% bind_rows(
          tibble(
            paragraph = i,
            text = "",
            class = "dialogue"
          )
        )
      }
      if (section$class[1] == "dialogue"){
        book_grouped <- book_grouped %>% bind_rows(
          tibble(
            paragraph = i,
            text = "",
            class = "append"
          )
        )
      }
    }
  }
  
  # Now sort by paragraph number to get them back lined up with each other
  
  book_grouped <- book_grouped %>%
    arrange(paragraph)
  
  # Checking if the above has worked correctly
  count = 0
  for (i in book_grouped$paragraph){
    section <- book_grouped %>% filter(paragraph == i) 
    if (length(section$paragraph) == 3){
      count = count + 1
    }
  }
  
  # Now to ensure each row has a unique paragraph, which is done by adding a column which has the quote/s for the paragraph, then
  # another column that has the append for the paragraph
  
  # This is possible note, based on the class row which has done the chunk of this work already
  
  
  # Essentially, in data taming words, the data is in long form, and needs to be converted into wide form
  
  book_grouped_wide <- book_grouped %>%
    spread(key = "class", value = "text")
  
  # Now we can put the chapter numbers back in, there is a unique paragraph number for each row now
  
  book_grouped_wide <- book_grouped_wide %>%
    mutate(chapter = numeric(length(book_grouped_wide$dialogue)))
  
  
  for (i in 1:length(book_grouped_wide$chapter)){
    book_grouped_wide$chapter[i] = book_paragraph_check$chapter[i]
  }
  
  book_grouped_wide <- book_grouped_wide %>%
    mutate(speaker = character(length(book_grouped_wide$append)),
           addressee = character(length(book_grouped_wide$append)),
           subject = character(length(book_grouped_wide$append)),
           sentiment = numeric(length(book_grouped_wide$append)))
  
  # Strictly denoting empty append values
  
  for (i in 1:length(book_grouped_wide$append)){
    if (book_grouped_wide$append[i] == ""){
      book_grouped_wide$append[i] = "NO APPEND"
    }
  }
  for (i in 1:length(book_grouped_wide$dialogue)){
    if (book_grouped_wide$dialogue[i] == ""){
      book_grouped_wide$dialogue[i] = "NO QUOTE"
    }
    if (book_grouped_wide$dialogue[i] != "NO QUOTE"){
      book_grouped_wide$subject[i] = "NO MONOLOGUE"
    }
  }
  
  # Also need to remove punctuation, can cause some issues for Named Entity Recognition further on, but can't remove
  # capital letters as this is needed for Named Entity Recognition
  
  # dealing with blanks
  
  for (i in 1:(length(book_grouped_wide$append))){
    if (book_grouped_wide$append[i] == ""){
      book_grouped_wide$append[i] = 'NO APPEND'
    } 
  }
  for (i in 1:(length(book_grouped_wide$dialogue))){
    if (book_grouped_wide$dialogue[i] == ""){
      book_grouped_wide$dialogue[i] = 'NO QUOTE'
    } 
  }
  
  # fix up the ordering a bit of the columns, makes it more readable
  
  book_grouped_wide <- book_grouped_wide %>%
    select(paragraph, append, dialogue, speaker, addressee, subject, sentiment, chapter) 
  
  # writing final tibble as a csv file (novel database), sending to the cleaned_books folder
  
  write_csv(book_grouped_wide, paste0('cleaned_books/',book_name,'wide.csv'))

}
