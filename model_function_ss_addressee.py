import pandas as pd
import numpy as np

def model_fitting_ad(book, book_name):    

########                                                                                                         ########
            ##################################### PRE-PROCESSING #####################################
########                                                                                                         ########


    # making addressees lists in book_read. This way you can restrict on training data with single addressee values

    def book_read(book_name):
    
        book = pd.read_csv('annotated_books/'+book_name+'annotated.csv')
    
        # make addressees lists
    
        for i in book.index:
    
            if book.at[i,'dialogue'] != 'NO QUOTE':
    
                list_ad = []
    
                addressee = book.at[i,'addressee']
    
                for j in range(len(addressee)):
    
                    if addressee[j] == '\'':
    
                        word = ''
    
                        for k in range(j+1,len(addressee)):
    
                            if (addressee[k] != '\''):
    
                                word = word + addressee[k]
    
                            else:
                    
                                break
                    
            
                        list_ad.append(word)
    
    
                while ', ' in list_ad:
                        
                    list_ad.remove(', ')
    
                while ']' in list_ad:
                        
                    list_ad.remove(']')
                    


                # As the focus of ML based addressee attribution is to only predict single addressees, multi addressee entries should be np.nan
                
                if len(list_ad) == 1: # single addressee
                
                    book.at[i,'addressee'] = list_ad[0]

                else: # multi addresseees

                    book.at[i,'addressee'] = np.nan

        return book

    
    book_real = book_read(book_name) # the correct value of the book, to be used for testing later on
    
        
    # extracting character info
    
    character_info = pd.read_csv('pdnc_dataset/data/'+book_name+'/character_info.csv')
    character_info
    
    Characters = character_info['Main Name']
    Characters = list(Characters)
    
    Aliases = []
    
    for i in character_info.index:
        new_list = []
        for j in range(len(character_info.at[i,'Aliases'])):
            string = character_info.at[i,'Aliases']
            if string[j] == '\'':
                new = ''
                index = j+1
                while (string[index] != '\'' and index < (len(string)-1)):
                    new = new + string[index]
                    index += 1
                new_list.append(new)
    
        # removing ', ' and '' terms
    
        for j in new_list:
            if (j == ', ' or j == ''):
                new_list.remove(j)
        
        Aliases.append(new_list)

    
    # there is a slight issue with Aliases where (Mr and Mrs appears instead of Mr. and Mrs.)

    new_Aliases = []

    for i in Aliases:
        new_list = []
        for alias in i:
            new_alias = ''
            for character in alias:
                if character != '.':
                    new_alias = new_alias + character
            new_list.append(new_alias)

        new_Aliases.append(new_list)


########                                                                                                         ########
            ##################################### FEATURE EXTRACTION #####################################
########                                                                                                         ########

    
    
    character_length = len(character_info['Main Name'])
    quote_length = len(book['dialogue'])
    paragraph_numbers = list(book.index)
    feature_set_ad = [] # addressee feature set
    
    for row in range(len(paragraph_numbers)):
    
        if book.at[paragraph_numbers[row],'dialogue'] != 'NO QUOTE':
                
            data_index_ad = np.zeros((character_length,7))  # addressee dataset

            # Feature extraction

            for i in range(len(Characters)):
            
 
                new_row_ad = np.zeros(7) # row for addressees    


                # populating the addressee row
                
                # Looking for closest mention of candidate

                lengths = []


                for alias in Aliases[i]:

                    count = 0

                    text = '' # text to put words in

                    for row_num in range(row, row-10, -1):
                            
                        list_appends = book.at[paragraph_numbers[row_num], 'append'] # can't do words because struggles with prefixes
                                                
                        list_quote = book.at[paragraph_numbers[row_num], 'dialogue'] # can't do words because struggles with prefixes
                                    
                        text = text + ' ' + list_quote + ' ' + list_appends                        
            
                    text = text.split(', ') # turning into a vector

                    text.reverse() # making closer words first
                    
                    for word in text:
                             
                        if ((alias in word) or (Characters[i] in word))== False:
            
                            count += 1
                                        
                        else:
                                        
                            lengths.append(count)

                    try :
                        new_row_ad[0] = min(lengths) # looking at closest mention of character
                        
                    except:
                        
                        new_row_ad[0] = 100

                # looking at current and previous vocatives
                    
                    if (', ' + alias) in book.at[paragraph_numbers[row], 'dialogue']: # likely addressee
                        new_row_ad[1] = 1
                    elif (alias + ', ') in book.at[paragraph_numbers[row], 'dialogue']: # likely addressee
                        new_row_ad[1] = 1
                    
                    if (', ' + alias) in book.at[paragraph_numbers[row-1], 'dialogue']: # not likely the addressee
                        new_row_ad[2] = 1 
                    elif (alias + ', ') in book.at[paragraph_numbers[row-1], 'dialogue']: # not likely the addressee 
                        new_row_ad[2] = 1   

                
                # current speaker / previous speaker / next speaker
                                    
                search_index = row - 1

                if search_index >= 0:
                    
                    if book.at[paragraph_numbers[search_index],'dialogue'] != 'NO QUOTE':
                            
                        if book.at[paragraph_numbers[search_index],'speaker'] == Characters[i]:
    
                            new_row_ad[3] = 1
                    

                # check if candidate is current speaker

                search_index = row
                        
                if book.at[paragraph_numbers[search_index],'speaker'] == Characters[i]:

                    new_row_ad[4] = 1

                # check if candidate is speaker in next dialogue
                    
                search_index = row + 1


                if search_index < len(paragraph_numbers):
    
                    if book.at[paragraph_numbers[search_index],'dialogue'] != 'NO QUOTE':
                            
                        if book.at[paragraph_numbers[search_index],'speaker'] == Characters[i]:
    
                            new_row_ad[5] = 1

                    # popularity of character
                
                if character_info.at[i,'Category'] == 'minor':
                    new_row_ad[6] = 0
                elif character_info.at[i,'Category'] == 'intermediate':
                    new_row_ad[6] = 1
                elif character_info.at[i,'Category'] == 'major':
                    new_row_ad[6] = 2                    

                
                data_index_ad[i,:] = new_row_ad

            # appending data_index to feature_set

            feature_set_ad.append((paragraph_numbers[row],data_index_ad,book_real.at[paragraph_numbers[row],'addressee'])) # need to track book indices for testing set predictions, in order to collate final dataset

    # consolidated model fitting function

    # bulding folds, along side testing and training
    
    data_folds_ad = []
    
    # k-fold validation
    
    fold_no = 10

    #test_size = int(len(feature_set)/fold_no)
    
    initial_folds_ad = []
    
    # Cut the feature_set into a set of 'fold_no'

    test_size = len(feature_set_ad) // fold_no
    

    for i in range(fold_no-1):

        initial_folds_ad.append(feature_set_ad[(i*test_size):((i+1)*test_size)])

    # final append

    initial_folds_ad.append(feature_set_ad[((fold_no-1)*test_size):len(feature_set_ad)])
    
    # doing the cross validation

    for i in range(fold_no):

        testing_set_ad = initial_folds_ad[i]

        training_set_ad = []

        # iterate through data_folds to get elements that are not in testing_set
        
        for entry in range(len(initial_folds_ad)):
            if entry != i:
                for val in initial_folds_ad[entry]:
                    training_set_ad.append(val)


        book_quotes_training_ad = [j[2] for j in training_set_ad] 
        book_quotes_testing_ad = [j[2] for j in testing_set_ad]

        training_set_ad = [j[1] for j in training_set_ad]

        # testing sets will have row numbers, which is required for final updates to the dataset
        
        testing_set_ad = [(j[0],j[1]) for j in testing_set_ad]

        data_folds_ad.append((training_set_ad,testing_set_ad,book_quotes_training_ad,book_quotes_testing_ad))

    
    predictions_ad = [] # list where all predictions will be for addressees
    
    # doing the cross validation
    
    for fold in range(fold_no):
        
        training_set_ad = data_folds_ad[fold][0]
        testing_set_ad = data_folds_ad[fold][1]
        book_quotes_training_ad = data_folds_ad[fold][2]
        book_quotes_testing_ad = data_folds_ad[fold][3]

        # Getting the dataset for each character per row, for training

        data_training_ad = []

        for j in range(len(Characters)):

            database = []
            
            for entry in range(len(training_set_ad)):

                data = training_set_ad[entry]

                row = list(data[j]) # row corresponding to Character i

                Value = (book_quotes_training_ad[entry] == Characters[j])

                row.append(Value)

                database.append(row)

            database = pd.DataFrame(database)
    
            data_training_ad.append(database)

        # Getting the dataset for each character per row, for testing

        data_testing_ad = []

        for j in range(len(Characters)):

            database = []
    
            for entry in range(len(testing_set_ad)):

                row_num = [testing_set_ad[entry][0]]

                data = testing_set_ad[entry][1]

                row = list(data[j]) # row corresponding to Character i

                row_num.extend(row)

                Value = (book_quotes_testing_ad[entry] == Characters[j])

                row_num.append(Value)

                database.append(row_num)

            database = pd.DataFrame(database)
    
            data_testing_ad.append(database)

########                                                                                                         ########
            ##################################### MODEL BUILDING #####################################
########                                                                                                         ########

            
        from sklearn.linear_model import LogisticRegression
        from sklearn.pipeline import make_pipeline
        from sklearn.preprocessing import StandardScaler
        
        # Process is as follows
        
        # Create a dataset for each candidate speaker, let each row be its feature vector as above. Fit A Logistic, with the output being a probabilitly
        # Let the classification for the quote be the candidate with the highest probability

        Models_sp = [] # List where each entry will correspond to a model
        Models_ad = [] # List where each entry will correspond to a model

        for j in range(len(Characters)):
            
            try:

                model_ad = make_pipeline(
                StandardScaler(),
                LogisticRegression(random_state = 1774231, max_iter = 1000)) # setting random-state for reproducibility
                    
                model_ad.fit(data_training_ad[j].iloc[:,0:7],data_training_ad[j].iloc[:,7])

                Models_ad.append(model_ad)

            except:
                
                Models_ad.append(False)

########                                                                                                         ########
            ##################################### GETTING PREDICTIONS #####################################
########                                                                                                         ########

        # Now use the models to make a prediction for the speaker for each row

        testing_set_predictions_ad = []
        
        for k in range(len(testing_set_ad)):


            probabilities_ad = [] # by insering the (prob,index) for each character_model, you can extract the highest probability, and thus the index
    
            index_new = 0

            for model in range(len(Models_ad)): # same number of models for speakers and addressees
                    
                index_new = data_testing_ad[model].iloc[k,0] # this is the index of the row of the dataset getting predicted on
                    
                if Models_ad[model] != False:
                        
                    probs_ad = Models_ad[model].predict_proba(data_testing_ad[model].iloc[:,1:8])
                    probabilities_ad.append((probs_ad[k][1],model,index_new))
                
            try:
    
                max_prob_ad = max(probabilities_ad)
    
                if book.at[index_new,'speaker'] != Characters[max_prob_ad[1]]: # speaker and addressee prediction is different
                    testing_set_predictions_ad.append((Characters[max_prob_ad[1]],max_prob_ad[2],max_prob_ad[0]))
    
                else:
                    # take next likely option from addressees probabilities
    
                    probabilities_ad.remove(max_prob_ad)
                    max_prob_ad = max(probabilities_ad)
                    testing_set_predictions_ad.append((Characters[max_prob_ad[1]],max_prob_ad[2],max_prob_ad[0]))                     
            except:
    
                testing_set_predictions_ad.append((np.nan,index_new))


        # appending predictions_ad with testing_set_predictions_ad

        predictions_ad.extend(testing_set_predictions_ad)


########                                                                                                         ########
            ##################################### FINAL ACCURACY TESTS #####################################
########
    ########
    
    # Creating up-dated databases and saving it into a new folder, however, not assigning values to real addressees that are np.nan

    for i in predictions_ad:

        if (book_real.at[i[1],'addressee'] is np.nan) == False:

            book.at[i[1],'addressee'] = i[0]
            book.at[i[1],'probability_ad'] = i[2] # probability of assignment

        else: # multi-entry addressee

            book.at[i[1],'addressee'] = np.nan


    book.to_csv('predicted_books/ML_Full/'+book_name+'MLFullpredicted.csv')
        
    return book
