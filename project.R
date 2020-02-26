#load the dataset
dataset <-read.delim("abalone.data",sep=",",header = F)
View(dataset)
head(dataset)
colnames(dataset)=c("Sex","Length","Diameter","Height","Whole weight","Shucked weight","Viscera weight","Shell weight","Rings")
#any missing value
any(is.na(dataset))
#EDA
library(dplyr)
str(dataset)
summary(dataset)
fivenum(dataset)
library(ggplot2)
quantile(dataset$Rings)
dataset %>% ggplot(aes(x=Rings))+geom_histogram(fill="lightblue",color="Darkblue",binwidth = 2,alpha=0.5)
dataset %>% ggplot(aes(x=Rings))+geom_bar(fill="steelblue",alpha=0.6,position="dodge")
ggplot(dataset) + aes(Rings, fill = Sex) +  geom_density(alpha=0.6)

ggplot(dataset,aes(x=dataset$Rings,y=10))+geom_boxplot(aes())
boxplot(dataset$Rings,col = "steelblue",border="black",xlab=("Rings"))

ggplot(dataset,aes(x=Sex,fill=Sex))+geom_bar()
ggplot(dataset,aes(x=as.factor(Rings),y=dataset$`Whole weight`))+geom_bar()
                   
table(dataset$Sex)
hist(dataset$`Whole weight`)
ggplot(dataset,aes(y=`Whole weight`,x=2,fill="Orange"))+geom_boxplot()                            
x=summary(dataset$`Whole weight`)
hist(dataset$`Whole weight`)
a=c(min(dataset$`Whole weight`),median(dataset$`Whole weight`),max(dataset$`Whole weight`))
dataset %>% ggplot(aes(x=a,y=Rings))+geom_col()
ggplot(dataset,aes(x=Rings,y=dataset$`Whole weight`,col="lightgreen"))+geom_point()

ggplot(dataset,aes(x=as.factor(Rings),y=4))+geom_boxplot()

vect1<-round(dataset$`Whole weight`,0)    


ggplot(dataset,aes(y=factor(vect1),x=Rings,fill=factor(vect1)))+geom_col()+facet_grid(~factor(vect1))+ylab("Whole weight")

ggplot(dataset,aes(y=factor(vect1),x=Rings,fill=factor(vect1)))+geom_col()+facet_grid(~factor(vect1))+ylab("Whole weight")

ggplot(dataset,aes(y=`Shell weight`,x=2,col="lightblue"))+geom_boxplot()
vect2<-round(dataset$`Shell weight`,0)
boxplot(dataset$`Shell weight`,col = "darkgreen",border="black",ylab="Shell weight")
ggplot(dataset,aes(x=`Shell weight`,fill="Orange"))+geom_bar(binwidth = 0.02)


vect3<-round(dataset$Height,1)
vect3<-factor(vect3)
ggplot(dataset,aes(x=factor(Rings),y=dataset$Length),fill=factor(Rings))+ geom_violin()
levels(vect3)
vect4<-factor(dataset$Rings)
vect4
plot(dataset, xlim = c(1, 20), ylim = c(1,8))

library(corrplot)
corrgram(dataset[dataset$Rings,],order=T,lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Abalone data")
corrplot(dataset)
library(corrgram)


library(ggPredict3d)

ggPredict(model,digits=1)
cor(dataset$Rings,dataset$Length)
cor(dataset$Rings,dataset$Diameter)
cor(dataset$Rings,dataset$`Whole weight`)
cor(dataset$Rings,dataset$`Shell weight`)

clean_dataset<-dataset %>% mutate(age=case_when(Rings %in% 1:5~"young",Rings %in% 6:13~"adult",Rings %in% 14:30~"old"))
clean_dataset<-clean_dataset %>% select(c(2,3,5,8,10)) %>% na.omit()
clean_dataset
library(caTools)
set.seed(123)
sample=sample.split(clean_dataset,SplitRatio = 0.8)
traindata<-subset(clean_dataset,sample==T)
traindata
testdata<-subset(clean_dataset,sample==F) 
library(rpart)
model <-rpart(formula=age~.,data=traindata,method="class")
summary(model)
library(rpart.plot)
rpart.plot(model)
predicted_model<-predict(object=model,newdata=testdata,type="class")
confusion.matrix<-table(testdata$age,predicted_model)
confusion.matrix
accuracy<-sum(diag(confusion.matrix))/sum(confusion.matrix)
accuracy
