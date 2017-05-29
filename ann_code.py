import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

dataset = pd.read_csv('diagSet.csv')
X = dataset.iloc[:, 0:5].values
