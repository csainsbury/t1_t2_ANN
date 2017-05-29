import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# import dataset
dataset = pd.read_csv('diagSet_7p.csv')
X = dataset.iloc[:, 0:7].values
y = dataset.iloc[:, 7].values

# enocde categorical data
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
labelencoder_X_1 = LabelEncoder()
X[:, 1] = labelencoder_X_1.fit_transform(X[:, 1])
labelencoder_X_2 = LabelEncoder()
X[:, 2] = labelencoder_X_2.fit_transform(X[:, 2])
onehotencoder = OneHotEncoder(categorical_features = [1])
X = onehotencoder.fit_transform(X).toarray()
X = X[:, 1:]

# split
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 0)

# feature scaling
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X_train = sc.fit_transform(X_train)
X_test = sc.transform(X_test)

# ANN
# import keras libraries
import keras
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import Dropout

# initialise
classifier = Sequential()

# add input layer with Dropout
classifier.add(Dense(activation="relu", units=16, input_dim=30, kernel_initializer="uniform"))
# classifier.add(Dropout(0.1))

# add hidden layer
classifier.add(Dense(activation="relu", units=16, kernel_initializer="uniform"))
# classifier.add(Dropout(0.1))

# add output layer
classifier.add(Dense(activation="sigmoid", units=1, kernel_initializer="uniform"))

# compile the ANN
classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])

# fit the ann to the training set
classifier.fit(X_train, y_train, batch_size = 10, nb_epoch = 10)

# predict test set results
y_pred = classifier.predict(X_test)
y_pred = (y_pred > 0.5)

from sklearn.metrics import confusion_matrix
cm = confusion_matrix(y_test, y_pred)

cm

from sklearn.metrics import roc_auc_score
roc_auc_score(y_test, y_pred)

## evaluate
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import cross_val_score
def build_classifier():
    classifier = Sequential()
    classifier.add(Dense(activation="relu", units=16, input_dim=30, kernel_initializer="uniform"))
    classifier.add(Dense(activation="relu", units=16, kernel_initializer="uniform"))
    classifier.add(Dense(activation="sigmoid", units=1, kernel_initializer="uniform"))
    classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])
    return(classifier)

classifier = KerasClassifier(build_fn = build_classifier, batch_size = 10, nb_epoch = 10)
accuracies = cross_val_score(estimator = classifier, X = X_train, y = y_train, cv = 10)

## tuning ANN
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import GridSearchCV
from keras.models import Sequential
from keras.layers import Dense
def build_classifier():
    classifier = Sequential()
    classifier.add(Dense(activation="relu", units=16, input_dim=30, kernel_initializer="uniform"))
    classifier.add(Dense(activation="relu", units=16, kernel_initializer="uniform"))
    classifier.add(Dense(activation="sigmoid", units=1, kernel_initializer="uniform"))
    classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])
    return(classifier)

classifier = KerasClassifier(build_fn = build_classifier)
parameters = {'batch_size': [5, 10, 20, 40, 100],
                'epochs': [5, 10, 20, 30, 40]}
grid_search = GridSearchCV(estimator = classifier, param_grid = parameters, scoring = 'accuracy', cv = 10)
grid_search = grid_search.fit(X_train, y_train)
best_parameters = grid_search.best_params_
best_accuracy = grid_search.best_score_
