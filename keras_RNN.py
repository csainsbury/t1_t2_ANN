# LSTM for sequence classification in the IMDB dataset
import numpy
from keras.datasets import imdb
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers.embeddings import Embedding
from keras.preprocessing import sequence

# fix random seed for reproducibility
numpy.random.seed(7)

# load the dataset but only keep the top n words, zero the rest
top_words = 5000
#(X_train, y_train), (X_test, y_test) = imdb.load_data(nb_words=top_words)
# import drug number array and split into test/train

import pandas as pd
# import numpy as np
dataset = pd.read_csv('./numericalDrugsFrame_20y_125_chained_y.csv')
drug_dataset = dataset.values

random_y_set = pd.read_csv('./3y_mortality_y_for_numericalDrugsFrame_20y_125_chained_y.csv')
random_y = random_y_set.values

X_train = drug_dataset[0:40000]
X_test = drug_dataset[40001:55565]
y_train = random_y[0:40000]
y_test = random_y[40001:55565]
# truncate and pad input sequences
max_review_length = 125
X_train = sequence.pad_sequences(X_train, maxlen=max_review_length)
X_test = sequence.pad_sequences(X_test, maxlen=max_review_length)
# create the model
embedding_vecor_length = 32
model = Sequential()
model.add(Embedding(top_words, embedding_vecor_length, input_length=max_review_length))
model.add(LSTM(100))
model.add(Dense(1, activation='sigmoid'))
model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
print(model.summary())
model.fit(X_train, y_train, nb_epoch=3, batch_size=128)
# Final evaluation of the model
scores = model.evaluate(X_test, y_test, verbose=1)
print("Accuracy: %.2f%%" % (scores[1]*100))

## code with Dropout
# LSTM with Dropout for sequence classification in the IMDB dataset
import numpy
from keras.datasets import imdb
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers import Dropout
from keras.layers.embeddings import Embedding
from keras.preprocessing import sequence
# fix random seed for reproducibility
numpy.random.seed(7)
# load the dataset but only keep the top n words, zero the rest
top_words =50
# (X_train, y_train), (X_test, y_test) = imdb.load_data(num_words=top_words)
X_train = drug_dataset[0:40000]
X_test = drug_dataset[40001:62112]
y_train = random_y[0:40000]
y_test = random_y[40001:62112]

# truncate and pad input sequences
max_review_length = 125
X_train = sequence.pad_sequences(X_train, maxlen=max_review_length)
X_test = sequence.pad_sequences(X_test, maxlen=max_review_length)
# create the model
embedding_vecor_length = 32
model = Sequential()
model.add(Embedding(top_words, embedding_vecor_length, input_length=max_review_length))
model.add(Dropout(0.2))
model.add(LSTM(100))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
print(model.summary())
model.fit(X_train, y_train, nb_epoch=4, batch_size=256)
# Final evaluation of the model
scores = model.evaluate(X_test, y_test, verbose=0)
print("Accuracy: %.2f%%" % (scores[1]*100))

# predict test set results
y_pred = model.predict(X_test)
y_pred = (y_pred > 0.5)
from sklearn.metrics import confusion_matrix
cm = confusion_matrix(y_test, y_pred)

from sklearn.metrics import roc_auc_score
roc_auc_score(y_test, y_pred)

from sklearn import metrics
from sklearn import svm, datasets
from sklearn.metrics import roc_curve, auc
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import label_binarize
from sklearn.multiclass import OneVsRestClassifier
from scipy import interp

fpr, tpr, _ = metrics.roc_curve(y_test, y_pred)

import matplotlib.pyplot as plt
import numpy as np

fpr = fpr # false_positive_rate
tpr = tpr # true_positive_rate

# This is the ROC curve
plt.plot(fpr,tpr)
# plt.show()
plt.savefig('roc_mortality_at_3y_40000ktrain.png')
auc = np.trapz(tpr, fpr)

numpy.savetxt('./y_pred.csv', y_pred, fmt='%.18e', delimiter=',')
numpy.savetxt('./y_test.csv', y_test, fmt='%.18e', delimiter=',')
