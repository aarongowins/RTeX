# Write-Up
aaron gowins  
October 11, 2015  

# Writeup
### Load data, take a look...


```r
URL<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(URL,destfile="TRAIN",method="curl")
trainer<-read.csv(file="TRAIN",header=TRUE,sep=",")
URL2<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(URL2,destfile="TEST",method="curl")
tester<-read.csv(file="TEST",header=TRUE,sep=",")
#head(trainer)
```


```r
library(caret)
inTrain<-createDataPartition(y=trainer$classe,p=.75,list=FALSE)
training<-trainer[inTrain,]
testing<-trainer[-inTrain,]


library(MASS)
#head(tester)
training$classe<-as.numeric(training$classe)
training$classe<-as.factor(training$classe)
#training[training==""]<-0
training[is.na(training)]<-0
#is.data.frame(training)
dim(training)
```

```
## [1] 14718   160
```

```r
x<-nearZeroVar(training)
training<-training[,-x]
testing<-testing[,-x]
dim(x)
```

```
## NULL
```

```r
#m<-x[x[,"zeroVar"] + x[,"nzv"] > 0, ] 
#typeof(m)
#ls(training)
#rm(training[m])
#head(training)
modelFit<-lda(classe ~ ., data=training)
```

```
## Warning in lda.default(x, grouping, ...): variables are collinear
```

```r
table(predict(modelFit)$class)
```

```
## 
##    1    2    3    4    5 
## 4185 2848 2567 2412 2706
```

```r
table(predict(modelFit, type="class")$class, training$classe)
```

```
##    
##        1    2    3    4    5
##   1 4185    0    0    0    0
##   2    0 2848    0    0    0
##   3    0    0 2567    0    0
##   4    0    0    0 2412    0
##   5    0    0    0    0 2706
```

```r
ldaPred<-predict(object=modelFit,newdata=testing)
typeof(ldaPred)
```

```
## [1] "list"
```

```r
typeof(modelFit)
```

```
## [1] "list"
```

```r
table(ldaPred$class, testing$classe)
```

```
##    
##        A    B    C    D    E
##   1 1395    0    0    0    0
##   2    0  949    0    0    0
##   3    0    0  855    0    0
##   4    0    0    0  804    0
##   5    0    0    0    0  901
```

```r
#ct <- table(training$classe, modelFit$class)
#diag(prop.table(ct, 1))
# total percent correct
#sum(diag(prop.table(ct)))
tester<-tester[,-x]
dim(tester)
```

```
## [1] 20 59
```

```r
dim(testing)
```

```
## [1] 4904   59
```

```r
trainer[is.na(trainer)]<-0
testPred<-predict(object=modelFit,newdata=tester)
tester$classe<-0
#head(tester)
typeof(testPred)
```

```
## [1] "list"
```

```r
#typeof(modelFit)
table(testPred$class, tester$classe)
```

```
##    
##      0
##   1 20
##   2  0
##   3  0
##   4  0
##   5  0
```

