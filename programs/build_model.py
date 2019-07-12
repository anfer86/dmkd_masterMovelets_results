'''
Created on May 16, 2018

@author: andres
'''

# ---------------------------------------------------------------------------
# Para garantir reprodutibilidade
from numpy.random import seed
seed(1)
from tensorflow import set_random_seed
set_random_seed(2)
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Importing the libraries
import os
import sys
import numpy as np
import pandas as pd
# ---------------------------------------------------------------------------

# Paramter Verification
if (len(sys.argv) != 2):
    print("You need to give the directory path of train and test data.")
    exit()
    
dir_path = sys.argv[1]
# Importing the dataset
print("Loading train and test data from... " + dir_path)
dataset_train = pd.read_csv(dir_path + "/train.csv")
dataset_test  = pd.read_csv(dir_path + "/test.csv")
print("Done.")

nattr = len(dataset_train.iloc[1,:])
print("Number of attributes: " + str(nattr))

# Separating attribute data (X) than class attribute (y)
X_train = dataset_train.iloc[:, 0:(nattr-1)].values
y_train = dataset_train.iloc[:, (nattr-1)].values

X_test = dataset_test.iloc[:, 0:(nattr-1)].values
y_test = dataset_test.iloc[:, (nattr-1)].values

n_estimators = [300]
save_results = True
print(n_estimators)

from helper import ApproachRF

ApproachRF(X_train, y_train, X_test, y_test, n_estimators, save_results, dir_path)

# ---------------------------------------------------------------------------------


# replace distance 0 for presence 1
# and distance 2 to non presence 0
X_train[X_train == 0] = 1
X_train[X_train == 2] = 0
X_test[X_test == 0] = 1
X_test[X_test == 2] = 0

from helper import Approach2
par_droupout = 0.5
par_batch_size = 200
save_results = True

print("Building neural network")

lst_par_epochs = [80,50,50,30,20]
lst_par_lr = [0.00095,0.00075,0.00055,0.00025,0.00015]
Approach2(X_train, y_train, X_test, y_test, par_batch_size, lst_par_epochs, lst_par_lr, par_droupout, save_results, dir_path)

print("Done.")



# ---------------------------------------------------------------------------------
print("Done.")
print("Finished.")
