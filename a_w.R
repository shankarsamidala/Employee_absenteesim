

# importing data

data=read.csv("A_w.csv",sep=";",header = T,stringsAsFactors = F)

str(data)

class(data)

dim(data)

head(data)

tail(data)

##### converting data into original type

names(data)

names_fac=c("Reason.for.absence","Month.of.absence","Day.of.the.week","Seasons","Disciplinary.failure" 
            ,"Education", "Social.drinker","Social.smoker")

for(i in names_fac){
  data[,i]=as.factor(data[,i])
}

str(data)

## checking for missing values

sum(is.na(data))

##  checking for outliers

num_ind=sapply(data,is.numeric)

num_data=data[,num_ind]

cn=colnames(num_data)

cn


for (i in cn){
  v=data[,i][data[,i]%in%boxplot.stats(data[,i])$out]
 # print(v)
  data[,i][data[,i]%in%v]=NA
}

sum(is.na(data))

# imputing outliers

data[57,10]
 
#original = 241.476
#mean=266.70
#264.2
#271.54
data[57,10]=NA

data[57,10]

# imputing with mean

data$Work.load.Average.day[is.na(data$Work.load.Average.day)]=mean(data$Work.load.Average.day,na.rm = T)

data[57,10]

# median

data$Work.load.Average.day[is.na(data$Work.load.Average.day)]=median(data$Work.load.Average.day,na.rm = T)

data[57,10]

# knn

require(DMwR)

data_2=knnImputation(data,k=5)

##

data_2[57,10]

require(VIM)

data_3=kNN(data,c("Work.load.Average.day"),k=5,imp_var = F)

data_3[57,10]

data=knnImputation(data,k=3)

data[57,10]

# feature selection

require(corrgram)

corrgram(data[,num_ind],order = F,upper.panel = panel.pie,text.panel = panel.txt,main="correlation plot")

ano=aov(Absenteeism.time.in.hours~Reason.for.absence+Education+Month.of.absence+Day.of.the.week+Seasons+Disciplinary.failure+Social.drinker+Social.smoker,data=data)

summary(ano)

data=subset(data,select=-c(ID, Seasons, Education,Height, Body.mass.index))

data            


## normalization

cn=sapply(data,is.numeric)

cn=data[,cn]

cn=colnames(cn)

for(i in cn){
  data[,i]=(data[,i]-min(data[,i]))/(max(data[,i]-min(data[,i])))
}


install.packages("timeSeries")            

require(timeSeries)

start(data)

class(data)

data=as.ts(data)

start(data)
