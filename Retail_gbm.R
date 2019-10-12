rl_train=read.csv("store_train.csv",stringsAsFactors = F)
rl_test=read.csv("store_test.csv",stringsAsFactors = F)
rl_test$store=NA
rl_train$data="train"
rl_test$data="test"
rl_all=rbind(rl_train,rl_test)
library(dplyr)
library(tidyr)
glimpse(rl_all)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

glimpse(rl_all)
table(rl_all$store_Type)
rl_all=CreateDummies(rl_all,"store_Type")

sort(table(rl_all$state_alpha))

rl_all=CreateDummies(rl_all,"state_alpha")

sort(table(rl_all$Areaname))


rl_all=rl_all %>% 
  select(-countytownname,-country,-countyname,-storecode,-Areaname)




glimpse(rl_all)
table(rl_all$CouSub)



lapply(rl_all, function(x) sum(is.na(x)))


for(col in names(rl_all)){
  
  if(sum(is.na(rl_all[,col]))>0 & !(col %in% c("data","store"))){
    
    rl_all[is.na(rl_all[,col]),col]=mean(rl_all[rl_all$data=='train',col],na.rm=T)
  }
  
}

glimpse(rl_all)

rl_train=rl_all %>% filter(data=="train") %>% select(-data)
rl_test=rl_all %>% filter(data=="test") %>% select(-data,-store)


## separating the train data
set.seed(2)
s=sample(1:nrow(rl_train),0.8*nrow(rl_train))
rl_train1=rl_train[s,]
rl_train2=rl_train[-s,]



#  using gbm
library(gbm)
library(cvTools)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}


subset_paras=function(full_list_para,n=2){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}


param=list(interaction.depth=c(2,3,4,5,6),
           n.trees=c(100,150,200,250,300,350,400),
           n.minobsinnode=c(1,2,5,10),
           shrinkage=c(0.01,0.05,0.1,0.5),
           bag.fraction=c(0.5,0.7,0.9))

my_params=subset_paras(param,10)

myauc=0
for(i in 1:10){
  print('starting iteration')
  
  params=my_params[i,]
  
  k=cvTuning(gbm,store~.-Id,
             data =rl_train,
             tuning =params,
             args=list(distribution='bernoulli'),
             folds = cvFolds(nrow(rl_train), K=10, type = "random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="response",n.trees=params$n.trees))
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    myauc=score.this
    print(myauc)
  }
  
  print('DONE')
}


rl.gbm.final1=gbm(store~.-Id,data = rl_train,
                  n.trees = 150,
                  n.minobsinnode = 2,
                  shrinkage = 0.1, 
                  interaction.depth = 5, 
                  distribution = "bernoulli") 

test.pred=predict(rl.gbm.final1,newdata=rl_test,n.trees = 150,type = "response")
write.csv(test.pred,"Deepak_Kumar_P2_Part2.csv",row.names = F)





# now if we needed to submit probability scores for the test data we can do at this point


test.score=predict(rl.fit,newdata = rl_test,type='prob')[,2]


write.csv(test.score,'Deepak_Kumar_P2_part2.csv',row.names = F)     










