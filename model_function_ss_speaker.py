import pandas as pd
import numpy as np

def model_fitting_sp(book_name, fold_no = 10):    

########                                                                                                         ########
            ##################################### PRE-PROCESSING #####################################
########                                                                                                         ########

    book = pd.read_csv('annotated_books/'+book_name+'annotated.csv') 

    # make addressees lists, in order to remove single addressee entries. 

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
                

            book.at[i,'addressee'] = list_ad

    
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

    def character_match(row, book, Characters, new_Aliases):

        found = False # function to state if a character is found
        
        for i in range(len(Characters)):
            for alias in new_Aliases[i]:
                if alias.lower() in (book.at[row,'referringExpression']).lower():
                    if found == True: # Character has already been found, see which characters' alias is contained in the other
                        change = True # a placeholder variable if we should change the already matched character 
                        for alias in Aliases[i]:
                            for alias_2 in new_Aliases[candidate]:
                                if alias in alias_2:
                                    # Character i alias is contained inside alias of candidate, meaning it is not as strong a match
                                    change = False

                        if change == True:
                            candidate = i
                    else:
                        found = True # update counter
                        candidate = i

        try:
            return Characters[candidate]

        except:
            return None


    # Lets do a test to see if any of the rows in the dataset actually contain either the speaker or addressees
    
    character_length = len(character_info['Main Name'])
    quote_length = len(book['dialogue'])
    paragraph_numbers = list(book.index)
    feature_set_sp = [] # speaker feature set
    
    for row in range(len(paragraph_numbers)):
    
        if book.at[paragraph_numbers[row],'dialogue'] != 'NO QUOTE':
                
            data_index_sp = np.zeros((character_length,10)) # speaker dataset

            data_index_ad = np.zeros((character_length,7))  # addressee dataset

            # Feature extraction
            
            for i in range(len(Aliases)): # list of lists of Aliases

                new_row_sp = np.zeros(10) # row for speakers

                # populating the speaker row
                
                if book.at[paragraph_numbers[row],'syntactic'] == 'Explicit':
                            
                    new_row_sp[6] = 1
                    
                    try:
                        if Characters[i] == character_match(paragraph_numbers[row], book, Characters, new_Aliases):
                            new_row_sp[3] = 1
                            
                    except:
                        
                        new_row_sp[3] = 0
    
                
                elif book.at[paragraph_numbers[row], 'syntactic'] == 'Anaphoric':
                    
                    new_row_sp[7] = 1
                
                elif book.at[paragraph_numbers[row], 'syntactic'] == 'Implicit':
                    
                    new_row_sp[8] = 1
    
    
                if character_info.at[i,'Category'] == 'minor':
                    
                    new_row_sp[1] = 0
                
                elif character_info.at[i,'Category'] == 'intermediate':
                    
                    new_row_sp[1] = 1
                
                elif character_info.at[i,'Category'] == 'major':
                    
                    new_row_sp[1] = 2                    
                                
                
                lengths = []
    
                for alias in Aliases[i]:

                    # • vocative
                        
                    if alias in book.at[paragraph_numbers[row], 'dialogue']: 
                    
                        new_row_sp[2] = 1
    
                        
                    count = 0
                        
                    
                    # distance in words between candidate and quote, just looking at appends here, but will likely change this
        
                    
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

                try:
                    new_row_sp[0] = min(lengths)
                except:
                    new_row_sp[0] = 100 # generic meaning very far away
                    
                try:
                    if book.at[paragraph_numbers[row], 'syntactic'] == 'Anaphoric': # detecting anaphoric speakers
                            
                        if character_info.at[i,'Gender'] == 'F':
                                    
                            if 'her' in book.at[paragraph_numbers[row], 'referringExpression'].lower():
                                    new_row_sp[4] = 1
                            elif 'she' in book.at[paragraph_numbers[row], 'referringExpression'].lower():
                                    new_row_sp[4] = 1
            
                        elif character_info.at[i,'Gender'] == 'M':
                                    
                            if 'he' in book.at[paragraph_numbers[row], 'referringExpression'].lower():
                                    new_row_sp[5] = 1
                            elif 'his' in book.at[paragraph_numbers[row], 'referringExpression'].lower():
                                        ew_row_sp[5] = 1
    
                except:
                    new_row_sp[4] = 0
                    new_row_sp[5] = 0
    
                    # length of quote
    
                new_row_sp[9] = len(book.at[paragraph_numbers[row], 'dialogue'])
    
                
                data_index_sp[i, :] = new_row_sp # update row

            # appending data_index to feature_set

            feature_set_sp.append((paragraph_numbers[row],data_index_sp,book.at[paragraph_numbers[row],'speaker']))

    # consolidated model fitting function

    # bulding folds, along side testing and training
    
    data_folds_sp = []
    
    # k-fold validation
    
    fold_no = 10

    #test_size = int(len(feature_set)/fold_no)
    
    initial_folds_sp = []
    
    # Cut the feature_set into a set of 'fold_no'

    test_size = len(feature_set_sp) // fold_no
    

    for i in range(fold_no-1):

        initial_folds_sp.append(feature_set_sp[(i*test_size):((i+1)*test_size)])

    # final append

    initial_folds_sp.append(feature_set_sp[((fold_no-1)*test_size):len(feature_set_sp)])
    
    # doing the cross validation

    for i in range(fold_no):

        testing_set_sp = initial_folds_sp[i]

        # iterate through data_folds to get elements that are not in testing_set

        training_set_sp = []
        
        for entry in range(len(initial_folds_sp)):
            if entry != i:
                for val in initial_folds_sp[entry]:
                    training_set_sp.append(val)
        

        book_quotes_training_sp = [j[2] for j in training_set_sp] 
        book_quotes_testing_sp = [j[2] for j in testing_set_sp]

        training_set_sp = [j[1] for j in training_set_sp]

        # testing sets will have row numbers, which is required for final updates to the dataset
        
        testing_set_sp = [(j[0],j[1]) for j in testing_set_sp]

        data_folds_sp.append((training_set_sp,testing_set_sp,book_quotes_training_sp,book_quotes_testing_sp))

    performance_sp = [book_name,'speaker']
    performance_full = [book_name,'full']

    predictions_sp = [book_name,'speaker']
    
    # doing the cross validation
    
    for fold in range(fold_no):
        
        training_set_sp = data_folds_sp[fold][0]
        testing_set_sp = data_folds_sp[fold][1]
        book_quotes_training_sp = data_folds_sp[fold][2]
        book_quotes_testing_sp = data_folds_sp[fold][3]

        # Getting the dataset for each character per row, for training

        data_training_sp = []

        for j in range(len(Characters)):

            database = []
    
            for entry in range(len(training_set_sp)):

                data = training_set_sp[entry]

                row = list(data[j]) # row corresponding to Character i

                Value = (book_quotes_training_sp[entry] == Characters[j])

                row.append(Value)

                database.append(row)

            database = pd.DataFrame(database)

            data_training_sp.append(database)


        # Getting the dataset for each character per row, for testing

        data_testing_sp = []

        for j in range(len(Characters)):

            database = []
    
            for entry in range(len(testing_set_sp)):

                row_num = [testing_set_sp[entry][0]]

                data = testing_set_sp[entry][1]

                row = list(data[j]) # row corresponding to Character i

                row_num.extend(row) # now you have a row of features for character i fitted to row = row_num

                Value = (book_quotes_testing_sp[entry] == Characters[j])

                row_num.append(Value)

                database.append(row_num)

            database = pd.DataFrame(database)
    
            data_testing_sp.append(database)

        

########                                                                                                         ########
            ##################################### MODEL BUILDING #####################################
########                                                                                                         ########

    
        # Try and create a feature vector following a consolidation framework as described in https://ojs.aaai.org/index.php/AAAI/article/view/7720
        
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

                model_sp = make_pipeline(
                StandardScaler(),
                LogisticRegression(random_state = 1774231, max_iter = 1000)) # setting random-state for reproducibility
                
                model_sp.fit(data_training_sp[j].iloc[:,0:10],data_training_sp[j].iloc[:,10])
    
                Models_sp.append(model_sp)
    
            except:
                    
                Models_sp.append(False)

########                                                                                                         ########
            ##################################### GETTING PREDICTIONS #####################################
########                                                                                                         ########

        # Now use the models to make a prediction for the speaker for each row

        testing_set_predictions_sp = []


        for k in range(len(testing_set_sp)):

            probabilities_sp = [] # by insering the (prob,index) for each character_model, you can extract the highest probability, and thus the index

            for model in range(len(Models_sp)): # same number of models for speakers and addressees

                index_new = data_testing_sp[model].iloc[k,0] # this is the index of the row of the dataset getting predicted on
                
                if Models_sp[model] != False:

                    probs_sp = Models_sp[model].predict_proba(data_testing_sp[model].iloc[:,1:11])
                    probabilities_sp.append((probs_sp[k][1],model,index_new)) # data_testing_sp[model].iloc[k,0] is the dialogue paragraph it is predicting

            try:
                    
                max_prob_sp = max(probabilities_sp)
    
                testing_set_predictions_sp.append((Characters[max_prob_sp[1]],max_prob_sp[2],max_prob_sp[0]))

            except:
                
                testing_set_predictions_sp.append((np.nan,index_new))


        # updating predictions

        predictions_sp.extend(testing_set_predictions_sp)
                            

            # giving higher weightings to candidates who have previously spoken 

            #probabilities.sort(reverse = False)

            # seeing proportion of previously predicted results to each character
            #new_probs = []
    
            #for prob in probabilities[len(probabilities):(len(probabilities)-5):-1]:
        
            #count = 0
        
            #for j in training_set_predictions[40:35:-1]:

                #if Characters[prob[1]] == j:
    
                    #count += 1

                # modifying proability
    
            #prob_new = prob[0]*(1+(count/5))
    
            #new_probs.append((prob_new,prob[1]))


            # modifying probabilties

            #for i in range(5):
        
            #probabilities.pop()

            #probabilities.append(new_probs[1])
        

            #speaker alteration pattern

            #if k > 1:
                
            #    if Characters[max_prob[1]] == training_set_predictions[k-1]:

                    # speaker alteration pattern 
        
            #        probabilities.remove(max_prob)
            #        max_prob = max(probabilities)

            #        training_set_predictions.append(Characters[max_prob[1]])
            #    else:
            #        training_set_predictions.append(Characters[max_prob[1]])

            #else:
    
    # replacing results with the predictions

    # looking at complete final prediction accuracies 

    prediction_sp_new = predictions_sp[2:len(predictions_sp)] # removing metadata on beginning of both quotes

    # Creating up-dated databases and saving it into a new folder

    for i in range(len(prediction_sp_new)): # same length of speakers and addressees yo

        book.at[prediction_sp_new[i][1],'speaker'] = prediction_sp_new[i][0] # first entry is predictions, second is the index
        book.at[prediction_sp_new[i][1],'probability_sp'] = prediction_sp_new[i][2] # second entry is probability, second is the index
        
    return book
