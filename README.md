# DSPartB

There are several code files in the repository that will reproduce all results shown in the report. 

They can be explained by going through three stages; cleaning, quotation attribution and relationship analysis. In general, they have to be followed in chronological order. However, due to excess computational time, all the files have been run and supplied to the folders, so any file can be run at any time. 

**Cleaning**

  - The 'pdnc_dataset' folder contains all data on the thirteen novels used throughout the project. Each novel has data on the annotated quotations, character info as well as the raw text file of the novel. 
  - The 'cleaning_ss_run.R' file converts each novel in the 'pdnc_dataset' folder into a novel database. It stores these in the 'cleaned_books' folder. The main function it uses to do this comes from the 'cleaning_ss.R' file. 
  - The 'consolidation_ss_run.py' file merges each novel database in the 'cleaned_books' folder with annotated quotations (from the 'pdnc_dataset' folder. It also uses the annotated quotations files from the 'pdnc_dataset' folder. These annotated novel databases are stored in the 'annotated_books' folder. 


**Quotation Attribution**

  - The baseline algorithm is completed by running the 'baseline_attribution.ipynb' file. It takes a novel databse from the 'cleaned_books' folder (does not require annotations) and puts annotations for each quotation. It stores this in the 'predicted_books/baseline' folder. 
  - The ML algorithm is completed by running the 'model_function_ss_run.ipynb' file. It uses a speaker attribution function ('model_function_ss_speaker.py' file) and an addressee attribution function file ('model_function_ss_addressee.py' file). It needs annotated novel databses (from the 'annotated_books' folder) to run, because it uses training data. It stores its predicted books on the 'predicted_books/ML_Full' folder.
  - In order to prepare for analysing quotations, the baseline and correct attributions needs to remove any multi-addressee or nan values. This is done using the 'quote_generator.ipynb' file, as well as using updated novel databases in the 'Annotated_New' and 'Baseline_new' folder. All quotes are then stored in the 'Quotes' folder.  

**Relationship Analysis**
 
 - The tf-idf analysis is done by running the 'tf_idf_ss.R' file. Its outputs are in the 'Figures/TF-IDF Plots' folder.
 - The topic modelling analysis is done by running the 'topic_modelling_ss.R' file. Its outputs are in the 'Figures/Topic Modelling Plots' folder.
 - The sentiment analysis is done by running the 'sentiment_analysis_ss.R' file. The first part analyses sentiment dictionary performances, storing the results in the 'dictionary_performance.csv' file. The second part extracts sentiment graphs, storing the results in the 'Figures/Sentiment_Plots' folder. 
