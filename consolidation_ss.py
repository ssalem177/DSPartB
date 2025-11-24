import pandas as pd
import numpy as np 

def consolidation(book_name):

    book_quotes = pd.read_csv('pdnc_dataset/data/'+book_name+'/quotation_info.csv')

    # Lets merge this dataset with the filtered wide dataset from R
    
    book_wide = pd.read_csv('cleaned_books/'+book_name+'wide.csv')
    
    book_wide['syntactic'] = np.ones(len(book_wide['sentiment']))
    book_wide['referringExpression'] = np.ones(len(book_wide['sentiment']))
    
    # for each quote row in book_wide, looking for a match in PDNC
    
    for i in book_wide.index:
        
        if book_wide.at[i,'dialogue'] != 'NO QUOTE': # search for corresponding row in PDNC
    
            max_match = 0
            max_index = 0
    
            for j in book_quotes.index:
                
                common = 0
                
                word = book_wide.at[i,'dialogue'].split()
                word_2 = book_quotes.at[j,'quoteText'].split()
    
                for entry in word:
    
                    if (entry in word_2):
    
                        common += 1
    
                # update max_match and max_index
    
                if common > max_match:
    
                    max_match = common
    
                    max_index = j
    
    
            # adding terms
    
            book_wide.at[i,'speaker'] = book_quotes.at[max_index,'speaker']
            book_wide.at[i,'addressee'] = book_quotes.at[max_index,'addressees']
            book_wide.at[i,'syntactic'] = book_quotes.at[max_index,'quoteType']
            book_wide.at[i,'referringExpression'] = book_quotes.at[max_index,'referringExpression']     
            
    book_new = pd.DataFrame(book_wide)
            
    # storing the file in the annoted_files directory

    book_new.to_csv('annotated_books/'+book_name+'annotated.csv')
