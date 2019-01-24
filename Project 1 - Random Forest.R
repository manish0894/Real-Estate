ht_train=read.csv('housing_train.csv', stringsAsFactors = F)
ht_test=read.csv('housing_test.csv', stringsAsFactors = F)

ht_test$Price=NA

ht_train$data='train'
ht_test$data='test'

ht_all=rbind(ht_train, ht_test)

glimpse(ht_all)

ht_all=ht_all[,-2]

glimpse(ht_all)

table(ht_all$CouncilArea)
length(table(ht_all$CouncilArea))

ht_all=ht_all %>% 
  mutate(TYPE_h=as.numeric(Type=='h'),
         TYPE_u=as.numeric(Type=='u')) %>% 
  select(-Type)

glimpse(ht_all)

table(ht_all$Method)

ht_all= ht_all %>% 
  mutate(METHOD_1=as.numeric(Method=='PI'),
         METHOD_2=as.numeric(Method=='S'),
         METHOD_3=as.numeric(Method=='SP'),
         METHOD_4=as.numeric(Method=='VB')) %>% 
  select(-Method)

glimpse(ht_all)

table(ht_all$Postcode)
sum(is.na(ht_all$Postcode))

table(ht_all$CouncilArea)
ht_all=ht_all %>% 
  select(-Suburb, -SellerG, -CouncilArea )

for (col in names(ht_all)) {
  if (sum(is.na(ht_all[,col]))>0 & !(col %in% c('data', 'Price'))) {
    ht_all[is.na(ht_all[,col]), col]= median(ht_all[ht_all$data=='train', col], na.rm = T)
  }
  
}

ht_train=ht_all %>% filter(data=='train') %>% select(-data)
ht_test=ht_all %>% filter(data=='test') %>% select(-data, -Price)

#----- splitting train data into further ht_train1 & ht_test1
set.seed(2)
s=sample(1:nrow(ht_train),0.75*nrow(ht_train))
ht_train1=ht_train[s,]
ht_test1=ht_train[-s,]

library(randomForest)

#----- building model using rf algorithm on ht_train1 dataset

class_rf=randomForest(Price ~ ., data=ht_train1,do.trace=T)
class_rf

#---- prediction on ht_test1 dataset

forest.pred=predict(class_rf,newdata=ht_test1)
View(ht_test1$Price)
View(forest.pred)

#----- rmse on ht_test1 dataset
rmse_test=sqrt(mean((ht_test1$Price-forest.pred)^2))

View(ht_test1$Price-forest.pred)

rmse_test

#---- as given in project instructions (assesing modelling as taught in project process guide)
212467/308646.2

#----- building model on complete ht_train dataset
class_rf1=randomForest(Price ~ ., data=ht_train,do.trace=T)
class_rf1

#---- prediction on ht_test dataset 
forest.pred1=predict(class_rf1, newdata = ht_test)
View(forest.pred1)

#---- MAPE on ht_test1
(mean(abs((ht_test1$Price-predict(class_rf,newdata=ht_test1))/ht_test1$Price)))*100