# Imports
import pickle
import pandas as pd
import numpy as np



# Functions
def show_dataset_info(dataset_path):
    """
    Function which prints out general information about the dataset
    Args:
        dataset_path: The path to the file which we are interested in.

    Returns:
        None

    """
    with open(dataset_path, 'rb') as file:
        our_dataset = pickle.load(file)
    print(f" The type of the dataset container dataset is:\n {type(our_dataset)}")
    print(f" The length of the dictionary containing our dataset is: \n {len(our_dataset)}")
    print(f" The keys to our dictionary are: \n {our_dataset.keys()}")
    print(f" The type of the length elements are: {type(our_dataset['lengths'][0])}")
    print(f" The average length is {our_dataset['lengths'].mean()}")
    print(f" The shortest sequence in our dataset is {our_dataset['lengths'].min()}")
    print(f" The longest sequence in our dataset is {our_dataset['lengths'].max()}")
    print(f" The type of the values associated with timestamp key is:\n {type(our_dataset['timestamps'])}")
    print(f" The type of the values associated with timestamp key first element is:\n {type(our_dataset['timestamps'][0])}")
    print(f" The type of the values associated with timestamp key array 0 elem 0 is:\n {type(our_dataset['timeintervals'][0][0])}")
    print(f" The type of each entry in the types array is type :\n {type(our_dataset['types'][0])}")
    print(f" the type of the entries in the value array array is: \n {type(our_dataset['types'][0][0])}")
    print(" So our structure for each key is array[array1[float11,float12...],array2[float21,float22...]"
                  "....arrayn[arrayn1,arrayn2...]]")
    print(f"the lengths of the varying keys, They should share the same length:\ntimestamps:{len(our_dataset['timestamps'])} \n types: {len(our_dataset['types'])} \nlengths: {len(our_dataset['lengths'])}\ntimentervals:{len(our_dataset['timeintervals'])}")
    return


def csv_pd_converter(csv):
    """
    Function to convert the csv file into a pandas dataframe.
    Args:
        csv: An R dataframe to convert to pandas

    Returns:
        nba_player_box_df:A pandas dataframe containing our data

    """
    nba_player_box_df = pd.read_csv(csv, low_memory = False)
    return nba_player_box_df


def dataformatter(pd_df):
    """
        This is the funtion where we will apply the final edit and return the dictionary containing the correct format
        Args:
            pd_df: The pandas dataframe to put into dictionary

        Returns:
            new_df:The dictionary containing the keys ['timestamps', 'types', 'lengths', 'timeintervals']

    """

    columns_to_keep = ['time_s', 'mark_val', 'seq_no', 'time_int']
    new_df = pd_df[columns_to_keep]
    return new_df


def df_to_dict_step1(df):
    """

    Function which converts the dataframe into a dictionary. This dictionary has keys representing the sequence number.
    Each sequence number has the dataframe representing the game associated with it.
    Args:
        df: The dataframe to edit

    Returns:
        df_dict: A dictionary where each key represents a sequence in our dataset. (in the NBA setting a sequence is a game)

    """
    df_dict = {}

    # Group by 'seq_no' and create a DataFrame for each group
    for seq_no, group in df.groupby('seq_no'):
        df_dict[seq_no] = group.drop(columns='seq_no')
    return df_dict


def df_to_dictstep2(dict_of_dfs):
    """
    Function to convert dictionary into final format.
    Args:
        Dict: Dictionary where each key is a number corresponding to the game being considered

    Returns:
        new_dict: A dictionary of the correct format to apply our model to.
        """
    new_dict = {
    'timestamps': [],
    'types': [],
    'lengths': [],
    'timeintervals': []
    }

    for i in range(len(dict_of_dfs)):
        # Access the ith dataframe (assuming keys are integers starting from 1)
        ith_game_df = dict_of_dfs[i + 1]

        # Append arrays to the lists in the dictionary
        new_dict['timestamps'].append(ith_game_df['time_s'].to_numpy(dtype=np.float32))
        new_dict['types'].append(ith_game_df['mark_val'].to_numpy(dtype=np.int32))
        new_dict['lengths'].append(np.int32(len(ith_game_df)))  # Length as an int32
        new_dict['timeintervals'].append(ith_game_df['time_int'].to_numpy(dtype=np.float64))

    for j in range(len(new_dict['timestamps'])):
        new_dict['timestamps'][j] = new_dict['timestamps'][j]-new_dict['timestamps'][j][0]
        new_dict['timeintervals'][j][0] = np.float64(0)

    new_dict = {k: np.array(v, dtype=object) for k, v in new_dict.items()}

    return new_dict


def transition_matrix(df, column_name, num_states=28):
    """
    Function which given a dataframe computes the transition matrix of a specific column. In our example the column we
    consider is the mark_val column.
    Args:
        df: The dataframe with the column we are interested in

    Returns:
        transition_matrix: A 28x28 transition matrix

    """
    count_matrix = np.zeros((num_states, num_states), dtype=int)

    # Get the values from the specified column
    column_values = df[column_name].values

    # Count transitions
    for i in range(len(column_values) - 1):
        current_state = column_values[i]-1
        next_state = column_values[i + 1]-1
        count_matrix[current_state, next_state] += 1

    # Calculate the transition probabilities
    transition_matrix = np.zeros((num_states, num_states), dtype=float)

    for i in range(num_states):
        row_sum = np.sum(count_matrix[i, :])
        if row_sum > 0:
            transition_matrix[i, :] = count_matrix[i, :] / row_sum

    return transition_matrix



def check(final_df):
    """
    Function which checks that all sequences do not have illegal values.
    For example we can not have simultaneous events.
    Hence we are not allowed timeinterval values of 0.
    Args:
        None
    Returns:
        None
    """
    count = 0
    count_mark = 0
    count_times = 0
    count_neg = 0
    count_len = 0
    count_isnan = 0
    for i in range(len(final_df['timeintervals'])):
        for j in range(len(final_df['timeintervals'][i])):
            if final_df['timeintervals'][i][j] == 0. and j != 0:
                final_df['timestamps'][i][j] = np.float32(final_df['timestamps'][i][j] + 0.1)
                final_df['timeintervals'][i][j] += np.float64(0.1)
            if final_df['timestamps'][i][j] <= 0. and j != 0:
                final_df['timestamps'][i][j] = np.float32(final_df['timestamps'][i][j-1]+0.1)
            if final_df['types'][i][j] == 0:
                final_df['types'][i][j] = np.int32(25)
    for i in range(len(final_df['timeintervals'])):
        for j in range(len(final_df['timeintervals'][i])):
            if final_df['timeintervals'][i][j] <= 0. or final_df['timeintervals'][i][j]>1000:
                count += 1
            if final_df['timestamps'][i][j] <= 0. or final_df['timestamps'][i][j]>4000:
                count_times += 1
            if final_df['types'][i][j] <= 0. or final_df['types'][i][j]>30:
                count_mark += 1
            if final_df['timeintervals'][i][j] <= 0.:
                count_neg += 1
            if final_df['lengths'][i] <= 0 or final_df['lengths'][i]>1000:
                count_len += 1
            # Check types
            if not isinstance(final_df['timeintervals'][i][j], np.float64):
                count_isnan += 1
            if not isinstance(final_df['timestamps'][i][j], np.float32):
                count_isnan += 1
            if not isinstance(final_df['types'][i][j], np.int32):
                count_isnan += 1
            if not isinstance(final_df['lengths'][i], np.int32):
                count_isnan += 1
    print("--------------------------------")
    print("If any of the following are non-zero there are type errors in the dataframe")
    print("--------------------------------")
    print(f"The number of timeintervals outside range [0,1000]:{count}")
    print(f"The number of timestamps outside range [0,4000]:{count_times}")
    print(f"The number of non-defined marks :{count_mark}")
    print(f"The number of non-valid lengths:{count_len}")
    print(f"The number of NaN values: {count_isnan}")
    return


def compare_dict_structures(dict1, dict2):
    """
    Compare the data structures of two dictionaries with 4 keys each.
    Each key has a different datatype.
    Args:
        dict1: First dictionary to compare
        dict2: Second dictionary to compare
    Returns:
        Bool: Boolean value determining if the two dictionarys have the same structure
    """
    # Check if both dictionaries have the same keys
    if dict1.keys() != dict2.keys():
        print("The dictionaries do not have the same keys.")
        return False
    print(dict1.keys())
    print(dict2.keys())
    # Iterate over the keys and compare the datatypes of the values
    for key in dict1.keys():
        type1 = type(dict1[key])
        type2 = type(dict2[key])
        # Layer 1
        if type1 != type2:
            print(f"Different data types for key '{key}': {type1} vs {type2}")
            return False
        else:
            print(f"Same data type for key '{key}': {type1}")
        if type(dict1[key][0]) != type(dict2[key][0]):
            print("different")
    if type(dict1['lengths'][0]) != type(dict2['lengths'][0]):
        print("different")
    if type(dict1['types'][0][0]) != type(dict2['types'][0][0]):
        print("different")
    if type(dict1['timestamps'][0][0]) != type(dict2['timestamps'][0][0]):
        print("different")
    if type(dict1['timeintervals'][0][0]) != type(dict2['timeintervals'][0][0]):
        print("different")
    print("Both dictionaries have the same structure.")
    return True


def pickler(obj_to_pickle):
    """
    Function to pickle our dataframe.
    NOTE: The pickle object is stored with the name nba_df.pkl in the data folder.
    Args:
        obj_to_pickle
    Returns:
        None
    """
    with open('../data/nba_df.pkl', 'wb') as file:
        pickle.dump(obj_to_pickle, file)
    return

# --------------------------------------------------------------
# Script
# NOTE: HERE YOU MUST BRING THE CSV FILE FROM R TO THEN CONVERT TO A PANDAS DATAFRAME
# converting csv file to pandas dataframe
pd_df_finalinit = csv_pd_converter('nba_finalv3.csv')
# dropping all columns apart from 'time_s', 'mark_val', 'seq_no', 'time_int'
pd_df_4col = dataformatter(pd_df_finalinit)
# perform first step of separating data into the separate games
dict_v1 = df_to_dict_step1(pd_df_4col)

# combine all games into a single dictionary.
dict_v2 = df_to_dictstep2(dict_v1)
# open the mimic data structure to check the datatypes align.
with open('/Users/alexanderdiermuidnilsahl/Downloads/URSS/Interpretable-Point-Processes-main/data/mimic.pkl', 'rb') as file:
    mimic_data = pickle.load(file)

pickler(dict_v2)
show_dataset_info('/Users/alexanderdiermuidnilsahl/Downloads/URSS/Interpretable-Point-Processes-main/data/nba_df.pkl')
# Compare the dictionary structures to ensure we have correct types and format.
#compare_dict_structures(dict_v2,mimic_data)



