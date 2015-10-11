# Write-Up
aaron gowins  
October 11, 2015  

# Writeup
### Load data, take a look...


```r
URL<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(URL,destfile="TRAIN",method="curl")
training<-read.csv(file="TRAIN",header=TRUE,sep=",")
dim(training)
```

```
## [1] 19622   160
```


