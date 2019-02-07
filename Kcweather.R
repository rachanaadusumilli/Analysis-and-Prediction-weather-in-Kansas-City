library('e1071')
kcweather<-kcweather[,2:9]
n=366
nt=290
neval=n-nt
rep=100
accuracy=dim(rep)
precision_Rain=dim(rep)
Recall_Rain=dim(rep)
precision_Rain_Thunderstorm=dim(rep)
Recall_Rain_Thunderstorm=dim(rep)
precision_Snow=dim(rep)
Recall_Snow=dim(rep)

for(k in 1:rep)
{
  Tkcweather = sample(1:n,nt)
  kcweather.Train = kcweather[Tkcweather,]
  kcweather.Test = kcweather[-Tkcweather,]
  svmfit = svm(Events~.,data=kcweather.Train,kernel='linear',cost=1)
  p=predict(svmfit,kcweather.Test)
  cmatrix = table(p,kcweather.Test$Events)
  accuracy[k] = sum(diag(cmatrix))/sum(cmatrix)
  precision_Rain[k] = cmatrix[1,1]/(cmatrix[1,1]+cmatrix[2,1]+cmatrix[3,1])
  precision_Rain_Thunderstorm[k] = cmatrix[2,2]/(cmatrix[1,2]+cmatrix[2,2]+cmatrix[3,2])
  precision_Snow[k] = cmatrix[3,3]/(cmatrix[1,3]+cmatrix[2,3]+cmatrix[3,3])
  Recall_Rain[k] = cmatrix[1,1]/(cmatrix[1,1]+cmatrix[1,2]+cmatrix[1,3])
  Recall_Rain_Thunderstorm[k] = cmatrix[2,2]/(cmatrix[2,1]+cmatrix[2,2]+cmatrix[2,3])
  Recall_Snow[k] = cmatrix[3,3]/(cmatrix[3,1]+cmatrix[3,2]+cmatrix[3,3])
}  
summary(svmfit)
# calculate mean
mean(accuracy)
mean(precision_Rain)
mean(Recall_Rain)
mean(precision_Rain_Thunderstorm)
mean(Recall_Rain_Thunderstorm)
mean(precision_Snow)
mean(Recall_Snow)

# Compute 95% confidence interval on Accuracy using t-distribution
AccySR_svm_err = qt(0.975, df = rep-1) * sd(accuracy) / sqrt(rep)
# show the confidence interval [for Accuracy]
mean(accuracy) - AccySR_svm_err; mean(accuracy) + AccySR_svm_err

# summaries
summary(accuracy)
summary(precision_Rain)
summary(Recall_Rain)
summary(precision_Rain_Thunderstorm)
summary(Recall_Rain_Thunderstorm)
summary(precision_Snow)
summary(Recall_Snow)
svm_tune <- tune(svm, 
                 Events~.,data=kcweather.Train,kernel='linear', ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
summary(svm_tune)

