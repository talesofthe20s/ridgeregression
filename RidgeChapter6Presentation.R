Ridge
What the method does
R code you used to fit
Answers you got
#RIDGE REGRESSION DOESNT PERFORM VARIABLE SELECTION
#performs well in terms of prediction accuracy it does poorly in terms of 
#offering a clear interpretation
#The glmnet function has an alpha argument that determines what type of model is fit
#If alpha=0 then a ridge regression model is fit and if alpha=1 then alsso

library(glmnet)
x=model.matrix(Apps~.,College)[,-1]
y=College$Apps
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
#By default the glmnet function performs a ridge regression for an 
#automatically selected ranges of lambda values.

dim(coef(ridge.mod))
[1]  18 100
#18x100 matrix
#One for each predictor, plus an intercept 
#100 columns one for each value of lambda
#associated with each value of lambda is a vector of ridge regression coefficients


ridge.mod$lambda[50]
[1] 11497.57
#These are the coefficients when lambda=11497.57
coef(ridge.mod)[,50]
(Intercept)    PrivateYes        Accept        Enroll     Top10perc 
-1.458263e+03 -4.648071e+02  2.489066e-01  5.134601e-01  9.530257e+00 
Top25perc   F.Undergrad   P.Undergrad      Outstate    Room.Board 
7.841425e+00  9.162011e-02  1.103401e-01  7.305875e-03  9.438862e-02 
Books      Personal           PhD      Terminal     S.F.Ratio 
3.409711e-01  9.181531e-02  8.323631e+00  8.360791e+00  1.112056e+01 
perc.alumni        Expend     Grad.Rate 
-5.211799e+00  2.860713e-02  5.769915e+00 
#This is the l2 norm
sqrt(sum(coef(ridge.mod)[-1,50]^2))
[1] 465.319

ridge.mod$lambda[60]
[1] 705.4802
#Here are the coefficients when lambda=705 along with their l2 norm 
coef(ridge.mod)[,60]
(Intercept)    PrivateYes        Accept        Enroll     Top10perc 
-1.805760e+03 -5.407085e+02  8.176940e-01  6.449477e-01  2.048255e+01 
Top25perc   F.Undergrad   P.Undergrad      Outstate    Room.Board 
3.869387e+00  1.012262e-01  3.212733e-02 -6.268056e-03  2.022578e-01 
Books      Personal           PhD      Terminal     S.F.Ratio 
1.827551e-01 -8.631141e-03 -1.964431e+00 -3.538636e+00  1.172025e+01 
perc.alumni        Expend     Grad.Rate 
-1.017522e+01  7.017382e-02  1.180568e+01 
#l2 norm
sqrt(sum(coef(ridge.mod)[-1,60]^2))
[1] 541.4776
#We expect the coefficients estimates to be smaller (in terms of l2 norm)
#when a large value of lambda is used compared to when a small value of 
#lambda is used

predict(ridge.mod,s=50,type="coefficient")[1:18,]
(Intercept)    PrivateYes        Accept        Enroll     Top10perc 
-721.57879002 -507.70723989    1.43465389   -0.40158149   42.70625114 
Top25perc   F.Undergrad   P.Undergrad      Outstate    Room.Board 
-9.66483834    0.03691197    0.03905792   -0.07054490    0.16763712 
Books      Personal           PhD      Terminal     S.F.Ratio 
0.04364945    0.02016147   -7.59457081   -4.07761441   15.32077664 
perc.alumni        Expend     Grad.Rate 
-2.41361127    0.07900968    9.30824207 

 set.seed(100)
 train=sample(1:777,277)
 test=sample(1:777,500)
 y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
[1] 1191098

> ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
> mean((mean(y[train])-y.test)^2)
[1] 13503050
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
[1] 13503016
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
[1] 1195047

lm(y~x,subset=train)
Call:
  lm(formula = y ~ x, subset = train)

Coefficients:
  (Intercept)   xPrivateYes       xAccept       xEnroll    xTop10perc  
-288.12433    -537.81338       1.40805      -0.37838      61.20622  
xTop25perc  xF.Undergrad  xP.Undergrad     xOutstate   xRoom.Board  
-23.40729       0.02723       0.10956      -0.09865       0.34713  
xBooks     xPersonal          xPhD     xTerminal    xS.F.Ratio  
0.02181       0.03177      -8.06322      -0.16724     -27.86727  
xperc.alumni       xExpend    xGrad.Rate  
-13.80780       0.06080      11.21530  

predict(ridge.mod,s=0,exact=T,type="coefficients")[1:18,]
(Intercept)    PrivateYes        Accept        Enroll     Top10perc 
-288.14493579 -537.82361712    1.40803340   -0.37833164   61.20545999 
Top25perc   F.Undergrad   P.Undergrad      Outstate    Room.Board 
-23.40676427    0.02722680    0.10956014   -0.09864663    0.34713044 
Books      Personal           PhD      Terminal     S.F.Ratio 
0.02181796    0.03177005   -8.06324775   -0.16718301  -27.86755522 
perc.alumni        Expend     Grad.Rate 
-13.80808013    0.06079677   11.21536909 

set.seed(100)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
[1] 331.2333
#The best lambda from cross validation is 331
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
[1] 1278350
#Which gives us this MSE
out=glmnet(x,y,alpha=0)
> predict(out,type="coefficients",s=bestlam)[1:18,]
(Intercept)    PrivateYes        Accept        Enroll     Top10perc 
-1.468326e+03 -5.278781e+02  1.004588e+00  4.313442e-01  2.580619e+01 
Top25perc   F.Undergrad   P.Undergrad      Outstate    Room.Board 
5.501092e-01  7.258520e-02  2.420595e-02 -2.407454e-02  1.987732e-01 
Books      Personal           PhD      Terminal     S.F.Ratio 
1.285477e-01 -8.146130e-03 -4.028284e+00 -4.811071e+00  1.302180e+01 
perc.alumni        Expend     Grad.Rate 
-8.544783e+00  7.589013e-02  1.126699e+01 
baseMSE=mean((mean(y[train])-y.test)^2)
baseMSE
[1] 13503050
1278350/13503050
[1] 0.0946712

#6. How well does the model do at predicting the training data. 
#Since the MSE is very large in this case it is hard to tell how
#good the model is working
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
MSE=mean((ridge.pred-y.test)^2)
>MSE
[1] 1191098
baseMSE=mean((mean(y[train])-y.test)^2)
baseMSE
[1] 13503050
MSE/baseMSE
[1] 0.08820957
redict(ridge.mod,type="coefficients",s=4)[1:18,]
(Intercept)    PrivateYes        Accept        Enroll     Top10perc 
-317.65630508 -550.22184627    1.38898887   -0.32927492   60.11807760 
Top25perc   F.Undergrad   P.Undergrad      Outstate    Room.Board 
-22.58327437    0.02549065    0.10849687   -0.09628980    0.34944721 
Books      Personal           PhD      Terminal     S.F.Ratio 
0.02789858    0.02985554   -8.02337113   -0.18989400  -28.01495826 
perc.alumni        Expend     Grad.Rate 
-14.17076691    0.06102775   11.22673085 
#When we use lambda = 4 we get the smallest ratio, so even though cv gave us 331
#lambda = 4 works better and shows us that ridge fits the data 