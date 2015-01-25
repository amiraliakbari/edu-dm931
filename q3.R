library('RMySQL');
library('sqldf');
con = dbConnect(dbDriver("MySQL"), user = "root", password = "", dbname = "movies2");
allData=dbGetQuery(con,'SELECT * FROM ratings');
avgs=dbGetQuery(con,'SELECT MovieID,count(Rating),avg(Rating) FROM ratings group by MovieID');
movies=avgs$MovieID;
users=unique(allData$UserID);
hor=array(NA,dim=c(length(users),length(movies)));
rownames(hor)=users;
colnames(hor)=movies;
rMovies=allData$MovieID;
rUsers=allData$UserID;
rRatings=allData$Rating;
len=length(rMovies);
for(i in 1:len){
  hor[toString(rUsers[i]),toString(rMovies[i])]=rRatings[i];
}




uLen=length(users);
newTabel=array(0,dim=c(uLen,uLen));
for(i in 1:uLen-1){
  for(j in (i+1):uLen){
    delta=hor[i,]-hor[j,];
    delta[is.na(delta)]=0
    newTabel[j,i]=sqrt(sum(delta^2));
  }
  print(i);
}

window=newTabel[1:60,1:60];
h=hclust(as.dist(window));
clus=cutree(h,k=6);
