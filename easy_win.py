#march madness random forest modeling? 
import numpy as np 
import pandas as pd
import sklearn 
from sklearn import preprocessing 
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
import matplotlib.pyplot as plt 
import seaborn as sns 
from sklearn.metrics import confusion_matrix
from sklearn.metrics import classification_report
from sklearn import metrics
from sklearn.model_selection import GridSearchCV
from sklearn.preprocessing import MinMaxScaler 

#march madness data on high and low seeds 
features=pd.read_csv("new_madnessXX.csv")
features=features[features['year'] != 2018]
features.columns 
features['Location'].unique() 
features['Location'].value_counts()

#percentage of home wins
count_homeW=len(features[features['win_home']==1])
count_homeL=len(features[features['win_home']==0])
pct_homeW=count_homeW/len(features) #54.8

#Location metrics (home does much better with <500 58.6%), 500-1K (51.9%)
loc_metrics=features.groupby('distance_f').mean()[["win_home","seed1","seed2"]]

#win home plot 
pd.crosstab(features.distance_f,features.win_home).plot(kind='bar')
plt.title('The Higher Seed Wins More By Traveling Less')
plt.xlabel('Distance Traveled High Seed')
plt.ylabel('Win%')
plt.savefig('purchase_fre_job')
plt.savefig('yum_yum.png')

#histogram conference w/l fav plot 
features['w-l%_pct_conf_f'].hist()
plt.title('Histogram of Conference Win Percentage (High Seed)')
plt.xlabel('Win%')
plt.ylabel('Frequency')
plt.savefig('yum_yum1.png')

#subset features for the model  
features1=features.iloc[:,10:march.shape[1]]
dist=features.iloc[:,1:3]
features1=pd.concat([features1,dist],axis=1)

X=features1.iloc[:,1:34]
y=features1.iloc[:,0]
X_train, X_test, y_train,y_test=train_test_split(X,y,test_size=0.25)

#logistic regression models-----

#model I.----
lr=LogisticRegression(random_state=143,class_weight='balanced')
lr.fit(X_train,y_train)
y_pred=lr.predict(X_test)
conf_matrix=confusion_matrix(y_test,y_pred)
classification_report(y_test,y_pred)
metrics.accuracy_score(y_test, y_pred) #68.7% 

#model II. ---- 
scaler=MinMaxScaler(feature_range=(0,1))
scaler.fit(X_train)
X_train=scaler.transform(X_train)
X_test=scaler.transform(X_test)

lr.fit(X_train,y_train)
y_pred=lr.predict(X_test)
metrics.accuracy_score(y_test,y_pred) #67.3% 

#model III. ------- 

#create regularization penalty space
penalty=['l1','l2']
c=np.logspace(0,4,10)
hyperparams=dict(C=c,penalty=penalty)

#grid search using 5-fold cross 
clf=GridSearchCV(lr,hyperparams,cv=5,verbose=0)

#conduct grid search
best_model=clf.fit(X,y)
best_model.best_estimator_.get_params()['penalty'] 
best_model.best_estimator_.get_params()['C']

#predict with grid search parameters
y_pred=best_model.predict(X_test)
metrics.accuracy_score(y_test,y_pred) #55.4% 
















