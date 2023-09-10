import numpy as np
import pandas as pd
from sklearn import tree
from sklearn.tree import DecisionTreeClassifier
import matplotlib.pyplot as plt 
from sklearn.model_selection import KFold
from sklearn.model_selection import train_test_split


#excel导入数据，之后划分三个集合，先把测试集分开，之后再kfold函数区分训练集和验证集，k=8
ds=pd.read_excel('data_used.xlsx',sheet_name="Sheet1")
num_col=ds.columns.size
X_frame=ds.iloc[1:208,0:6]
Y=ds.iloc[1:208,6:7]
X_train,X_test,y_train,y_test = train_test_split(X_frame, Y, test_size=0.2)
kf=KFold(n_splits=8,shuffle=True,random_state=1)    #shuffle=False保证每一次分类是一致的

#开始调参，每一层都跑kfold确立平均值，比较平均值大小，最小者为最优参数跑测试集
criestimator=[]   #建立空列表存放数值
for i in range(40):
   clf=DecisionTreeClassifier(criterion="gini",splitter='random',max_depth=(i+1),min_samples_split=2,min_samples_leaf=1)
   error_each=[]
   for train,test in kf.split(X_train,y_train):
     x_actrain,x_valid=X_train.iloc[train,:],X_train.iloc[test,:]
     y_actrain,y_valid=y_train.iloc[train,:],y_train.iloc[test,:]
     clf=clf.fit(x_actrain,y_actrain)
     y_true=y_valid.iloc[:,0]
     y_predict=clf.predict(x_valid)
     error=(y_predict-y_true)
     error=abs(error)
     error_rate=np.mean(error)
     error_each.append(error_rate)      #error_each存放的是每一折的错误率
   criestimator.append(np.mean(error_each))   #criestimator存放的是每一个depths的错误率
print(min(criestimator),criestimator.index(min(criestimator)))   # 输出正确率最大和其对应的轮数
print(criestimator)

#用测试集测试
testcri=[]
best_clf=DecisionTreeClassifier(criterion="gini",splitter='random',max_depth=((criestimator.index(min(criestimator)))+1),min_samples_split=2,min_samples_leaf=1)
for train,test in kf.split(X_train,y_train):
     x_actrain=X_train.iloc[train,:]
     y_actrain=y_train.iloc[train,:]
     best_clf=best_clf.fit(x_actrain,y_actrain)
     y_truetest=y_test.iloc[:,0]
     y_predicttest=best_clf.predict(X_test)
     error=(y_predicttest-y_truetest)
     error=abs(error)
     error_rate=np.mean(error)
     error_each.append(error_rate)     #error_each存放的是每一折的残差值
error_real=np.mean(error_each)         #error_real存放的是测试集的平均残差值
plt.figure(1)
tree.plot_tree(best_clf,filled=True)
print(error_real)
for i in range(40):
   clf=DecisionTreeClassifier(criterion="gini",splitter='random',max_depth=(i+1),min_samples_split=2,min_samples_leaf=1)
   error_each=[]
   for train,test in kf.split(X_train,y_train):
     x_actrain=X_train.iloc[train,:]
     y_actrain=y_train.iloc[train,:]
     clf=clf.fit(x_actrain,y_actrain)
     y_true=y_test.iloc[:,0]
     y_predict=clf.predict(X_test)
     error=(y_predict-y_true)
     error=abs(error)
     error_rate=np.mean(error)
     error_each.append(error_rate)      #error_each存放的是每一折的残差值
   testcri.append(np.mean(error_each))

#可视化
plt.figure(2)
plt.title("error rate of DecisionTreeClassifier")
plt.ylabel("error_rate")
plt.xlabel("max_depth")
plt.plot(range(1,41),criestimator,color='deepskyblue',label='training set')
plt.plot(range(1,41),testcri,color='cornflowerblue',label='testing set')
plt.legend()
plt.show()
