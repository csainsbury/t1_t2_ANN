import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# import dataset
dataset = pd.read_csv('diagSet_7p.csv')
# only pick those where col 8 (diagnosis ==1 or 0)
dataset = dataset[dataset.iloc[:, 7] == 0]
#dataset = dataset[dataset[:, -1] == 1]
X = dataset.iloc[:, :-1].values
y = dataset.iloc[:, -1].values

from sklearn.preprocessing import MinMaxScaler
sc = MinMaxScaler(feature_range = (0, 1))
X = sc.fit_transform(X)

from minisom import MiniSom
som = MiniSom(x = 10, y = 10, input_len = 7, sigma = 1, learning_rate = 0.5)
som.random_weights_init(X)
som.train_random(data = X, num_iteration = 400)

from pylab import bone, pcolor, colorbar, plot, show
f = plt.figure()
bone()
pcolor(som.distance_map().T)
colorbar()
# markers = ['o', 's']
# colors = ['r', 'g']
f# or i, x in enumerate(X):
#    w = som.winner(x)
#    plot(w[0] + 0.5,
#    w[1] + 0.5,
#    markers[y[i]],
#    markeredgecolor = colors[y[i]],
#    markerfacecolor = 'None',
#    markersize = 5,
#    markeredgewidth = 1)
show()

f.savefig('diagSet_10x10_400.pdf')

mappings = som.win_map(X)

# need to look at map and find coordinates
output = mappings[(1, 3)]

output = np.concatenate((mappings[(8,6)], mappings[(3,4)]), axis = 0)

output = sc.inverse_transform(output)


# Creating the matrix of features
customers = dataset.iloc[:, 1:].values

# Creating the dependent variable
is_fraud = np.zeros(len(dataset))
for i in range(len(dataset)):
    if dataset.iloc[i,0] in output:
        is_fraud[i] = 1


# Feature Scaling
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
customers = sc.fit_transform(customers)

# Part 2 - Now let's make the ANN!

# Importing the Keras libraries and packages
from keras.models import Sequential
from keras.layers import Dense

# Initialising the ANN
classifier = Sequential()

# Adding the input layer and the first hidden layer
classifier.add(Dense(units = 20, kernel_initializer = 'uniform', activation = 'relu', input_dim = 7))

# Adding the output layer
classifier.add(Dense(units = 1, kernel_initializer = 'uniform', activation = 'sigmoid'))

# Compiling the ANN
classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])

# Fitting the ANN to the Training set
classifier.fit(customers, is_fraud, batch_size = 50, epochs = 40)

# Predicting the probabilities of frauds
y_pred = classifier.predict(customers)
y_pred = np.concatenate((dataset, y_pred), axis = 1)

# y_pred = np.concatenate((dataset.iloc[:, 0:1].values, y_pred), axis = 1)
# y_pred = y_pred[y_pred[:, 1].argsort()]


np.savetxt('./som_ann_output.csv', y_pred, fmt='%.18e', delimiter=',')
