import pandas as pd

#from Alex_nba import DatasetHandler
# Now we have downloaded the R dataframe into a pandas dataframe.
loaded_obj=pd.read_pickle('dataframe.pkl')
print(type(loaded_obj))
