library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(ggplot2)

### submission function
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}


#### Assignement
setwd("~/R/R directory/machine learning assignement//")
training<-read.csv("pml-training.csv",header = T)
T<-training[,1:7]
train<-training[,-(1:7)]

##Removing variables with too many NA
Matrix_Na<-is.na(train)
Vect_Na<-(apply(Matrix_Na,2,sum))/length(training$X)
rm(Matrix_Na)
bad_var<-(Vect_Na>0.9)
bad_var<-grep("TRUE",bad_var)
train<-train[,-bad_var]

## Analysing 2 factors variables to see if they are relevant
facttwo<-c(7,10,13,75,72,69,53,50,47)
test<-train[,c(facttwo,86)]
for (i in 1:length(facttwo)){
        test[,i]<-as.numeric(test[,i])
}
cor(test[,-(length(facttwo)+1)])
train<-train[,-c(7,10,72,69,50,47)]

for (i in 1:79){
        train[,i]<-as.numeric(train[,i])
}


## tree fit
set.seed(12345)
tree1<-rpart(classe~.,data=train,control = rpart.control(cp = 0.0000001))
#jpeg(file = "~/R/R directory/tree.jpeg",width=900,height=900,quality=100)
#fancyRpartPlot(tree1,cex=0.5)
##dev.off()
#plot(tree1,mar=0.1)
#text(tree1,use.n=T,cex=0.9)

plotcp(tree1)
tree1$cptable


cptable<-as.data.frame(tree1$cptable)
cptable_p<-cptable[cptable$CP<0.0005,]
cptable_p$labels<-as.character(cptable_p$nsplit)
a<-which.min(cptable_p$xerror)
x_se<-cptable_p$xerror[a]+cptable_p$xstd[a]

## complexity parameter plot
g<-ggplot(data=cptable_p,aes(x=CP,y=xerror)) +
        geom_errorbar(aes(ymin=xerror - xstd, ymax=xerror+xstd)) +
        scale_x_reverse()+ labs(x="Complexity parameter",y="cross validation error") +
        geom_point(pch=21)+ geom_line() + geom_text(aes(x=CP,y=xerror+0.005,label=labels),cex=3)+
        geom_hline(yintercept=x_se,linetype="dotted") +
        theme(strip.background = element_rect(fill = "black"),
              panel.grid.major = element_line(colour = "grey90"),
              panel.grid.minor = element_line(colour = "grey95"),
              panel.background = element_rect(fill = "transparent"))
        
g

## Pruning with 71e cp
cp.choice<-tree1$cptable[71,1]
pruned.tree<-prune(tree1, cp=cp.choice)
## Pruning with 79e cp
cp.choice2<-tree1$cptable[79,1]
pruned.tree2<-prune(tree1, cp=cp.choice2)
## checking accuracy
confus.matrix<-table(train$classe,predict(pruned.tree,newdata=train,type="class"))

##precision = 100 - (0.71563 * 0.087950)*100 cross validation
##precision = 100 - (0.71563 * 0.058610)*100 error rate

## test set accuracy
testing<-read.csv("pml-testing.csv",header = TRUE)
Test<-testing[,-(1:7)]
Test<-Test[,-bad_var]
Test<-Test[,-c(7,10,72,69,50,47)]

for (i in 1:79){
        Test[,i]<-as.numeric(Test[,i])
}

answers<-predict(pruned.tree,newdata=Test,type="class")
answers<-as.character(answers)
answers2<-predict(pruned.tree2,newdata=Test,type="class")
answers2<-as.character(answers2)