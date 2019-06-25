
setwd('C:/Users/mites/Desktop/college/quarter_2/STAT/project')
## Step 0  - Read in Data
data=read.csv("casestudydata.csv")
names(data)
data=data[,-1]  ## remove ID

## Step 1 - Explore and relabel Data

out_sample=which(is.na(data$CKD)==1)
data_out=data[out_sample,]   ## the ones without a disease status
data_in=data[-out_sample,]   ## the ones with a disease status
summary(data_in)

data_in=na.omit(data_in)

y=data_in$CKD
#class(data)
summary(data_in)



## Step 2  - Run the Logistic Regression with one variable

names(data)
model=glm(CKD~Age,family="binomial",data=data_in)
summary(model)
# the coeffienct of age is positive indicating that an increase in age is associated
#  with an increase in the probability of someone having CKD 


## Step 3  - Run the Logistic Regression on all data, explore backward elimination
dim(data)
model=glm(CKD~.,family="binomial",data=data_in)
summary(model)
# notice the logistic regression automatically separated the categorical variables
#  if a category is not listed, it is included in the intercept

model2=step(model,direction="forward")
summary(model2)
model3=step(model,direction="backward")
# this will run the ENTIRE model with all variables, and then remove one at a time according to a 
#  p-value of the coefficient. it will result in only those variables "which matter"
#   ** note - some consider this unethical because you see the results before you select variables

## Step 4 - Explore your new model
formula(model3)
summary(model3)



model4=glm(CKD ~ Age + Female  + Weight + Obese + 
             HDL  + Activity + Hypertension + 
             Diabetes + CVD + Anemia,family="binomial",data=data_in)

# confidence intervals of the model coefficients (should not include 0 if p-value<.05)
#confint.default(model4)
#confint(model3)

#model after removal of variables from conf interval






#newdata1=data_in  ## these are 4 "new patients"
#newdata1  ## uh oh, 1 has missing data! damn!
#phatnew=predict(model4, newdata = newdata1, type = "response")
#output=as.data.frame(unlist(phatnew))
#write.csv(output,file='output_train.csv')


## Step 6 - Predict probabilities and Odds Ratios of New Data
## predictions of new data (data with empty CKD values)
newdata1=data_out  ## these are 4 "new patients"
newdata1  ## uh oh, 1 has missing data! damn!
phatnew=predict(model4, newdata = newdata1, type = "response")
output=as.data.frame(unlist(phatnew))
write.csv(output,file='output.csv')



# notice it only gives you 3 probabilities because 1 has missing data 
#    (you will have to fix that)

## odds ratios
phatnew/(1-phatnew)
# see if you can interpret these. it's the prob. of it happening/prob. of it not happening
#  related to gambling and betting on something

## Step 7 - Predict and Plot in-sample data
phat3=predict(model4,data=data_in,type="response")  # predicts for ALL in sample data
summary(phat3)  # probabilities range from .01% to 83.4%   - cool!




## Step 8 - Classification
#summary(phat3)
classify=ifelse(phat3>0.2,1,0)  # this is a threshold, we say if probability >50% , then say "yes"
#summary(classify)  # notice that not many are "yes"  - is this desirable?

c_accuracy(data_in$CKD,classify)  # to run this you must run my code below first.

# notice these are the accuracy results when you use 50% as the threshold. they are kind of extreme
#    the false positive rate is almost 0% BECAUSE you almost NEVER say "yes"
#         true positive rate is 13.9% which isn't that bad because you have almost no false positives

## Step 9 - Caclculate Costs
acc=c_accuracy(data_in$CKD,classify)
c1=100   # penalize me  $100 for a false positive
c2=200  #  penalize me $200 for a false negatives
cost=acc[9]*c1+acc[10]*c2

cost   ## my costs are $48,800  because I got hit with a ton of false negative costs 
        # i said no too much!  many FN.  hmmm, 

## YOu must change the threshold of 50% above , to lower your costs.
##    How to do this?  One way is to search over 101 thresholds (each % from 0 to 100)

##  you may realize at some point, that plotting an ROC curve with roc()  gives you all possibilities
##   that's a high level understanding
#install.packages('pROC')
library(pROC)
roc(data_in$CKD,classify)
model=roc(data_in$CKD~phat3,percent=TRUE,plot=TRUE)

## AUC is actually the probability of ranking a randomly chosen diseased person
##    higher than a randomly chosen non-disease person   (using the predictions from your model)

## wow, that's a good model!







## Function Below, RUN THIS FIRST
## make sure actuals and classifications are 0 (no) or 1 (yes) only 
##  Built by Matthew J. Schneider

c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);
  
  
  tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
  fp=nrow(df[df$classifications==1 & df$actuals==0,]);
  fn=nrow(df[df$classifications==0 & df$actuals==1,]);
  tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
  
  
  recall=tp/(tp+fn)
  precision=tp/(tp+fp)
  accuracy=(tp+tn)/(tp+fn+fp+tn)
  tpr=recall
  fpr=fp/(fp+tn)
  fmeasure=2*precision*recall/(precision+recall)
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn")
  
  #print(scores)
  return(scores);
}


cost_list<-c()
accuracy<-c()
falsepositive<-c()
falsenegative<-c()
fscore<-c()
precision<-c()
auc<c() 

x <- seq(0.1, 1, by = 0.1)
y <- seq(1, 10, by = 1)
i=1
for (val in x) {
    
    classify=ifelse(phat3>val,1,0)  # this is a threshold, we say if probability >50% , then say "yes"
    
    summary(classify)  # notice that not many are "yes"  - is this desirable?
      
    c_accuracy(data_in$CKD,classify)
    
    
    temp=y[i]
       
    print (auc(roc(data_in$CKD,classify)))
    print (temp)
    
       
    
   
    acc=c_accuracy(data_in$CKD,classify)
    c1=100   # penalize me  $100 for a false positive

    c3=1300 #  bonus 1300 for True positive
    cost_list[temp]=acc[7]*c3-acc[9]*c1
    accuracy[temp]=acc[3]
    falsepositive[temp]=acc[9]
    falsenegative[temp]=acc[10]
    fscore[temp]=acc[6]
    precision[temp]=acc[2]
    
    i=i+1
   
   #print (acc[7]*c3-acc[9]*c1-acc[10]*c2)
}
