library('RMySQL');
library('sqldf');
con = dbConnect(dbDriver("MySQL"), user = "root", password = "", dbname = "movies2");
allData=dbGetQuery(con,'SELECT * FROM ratings');
avgs=dbGetQuery(con,'SELECT MovieID,count(Rating),avg(Rating) FROM ratings group by MovieID');
movies=avgs$MovieID;
users=unique(allData$UserID);
#mean=avgs[avgs$MovieID==19876,c('avg(Rating)')];
#sData=allData[(allData$MovieID==2812638&allData$UserID==24),c('Rating')];
#newTable=array(0,dim=c(length(users),length(users));
#uIte=1;
#for(u in users){
#  newTable[uIte,1]=u;
#  uIte=uIte+1;
#}

#mIte=1;
#for(m in movies){
 # uIte=1;
  #mean=avgs[avgs$MovieID==m,c('avg(Rating)')];
  #for(u in users){
  #  sData=allData[(allData$MovieID==m&allData$UserID==u),c('Rating')];
   # if(length(sData)>0){
  #  newTable[uIte,mIte+1]=sData;
  #  print("found");
  #  }else{  
  #    newTable[uIte,mIte+1]=mean;
  #  }
   # uIte=uIte+1;
  #}
  #mIte=mIte+1;
  #print(mIte);
#}

#uLen=length(users);
uLen=1000;
newTabel=array(0,dim=c(uLen,uLen));
for(i in 1:uLen-1){
  origin=users[i];
  mOrigin=allData[allData$UserID==origin,c('MovieID','Rating')];
  for(j in (i+1):uLen){
    other=users[j];
    mOther=allData[allData$UserID==other,c('MovieID','Rating')];
    inte=intersect(mOrigin$MovieID,mOther$MovieID);
   # I1=which(mOrigin$MovieId==inte);
    #I2=which(mOther$MovieId==inte);
    r1=mOrigin[mOrigin$MovieID==inte,c('Rating')];
  r2=mOther[mOther$MovieID==inte,c('Rating')];
  delta=r1-r2;
  newTabel[j,i]=sqrt(sum(delta^2));
  }
  print(i);
}

f1=newTabel[1:7,1:7];
