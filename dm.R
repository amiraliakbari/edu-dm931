rating <- read.csv("C:/Users/Omid/Documents/DM PROJECT/rating.csv");
I=rating[((rating$MovieID==2812638)),c('UserID','MovieID','Rating')];
J=rating[,c('MovieID')];
movies=unique(J);
mean_table=array(0,dim=c(2,length(movies)));
ite=1;
for(m in movies){
  rs=rating[((rating$MovieID==m)),c('Rating')];
  r=mean(rs);
  mean_table[1,ite]=m;
  mean_table[2,ite]=r;
  ite=ite+1;
}
ply(rating, c("MovieID"), summarize, rating$Rating == mea
