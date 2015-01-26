##q1,q2,q9

library('RMySQL');
con = dbConnect(dbDriver("MySQL"), user = "root", password = "", dbname = "movies2");
allData=dbGetQuery(con,'SELECT * FROM ratings');
avgs=dbGetQuery(con,'SELECT MovieID,count(Rating),avg(Rating) FROM ratings group by MovieID');
movies=avgs$MovieID;
users=unique(allData$UserID);


#covert vertical to horizontal
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
rm(con);
rm(rMovies);
rm(rUsers);
rm(rRatings);
rm(allData);

#outlier detection
uLen=length(users);
unLike=array(0,dim=c(uLen));
mAvg=avgs[,c("avg(Rating)")];
for(i in 1:uLen){
  d=hor[i,]-mAvg;
  d[is.na(d)]=0
  unLike[i]=sqrt(sum(d^2));
}
tr=quantile(unLike,0.99);
outliers=which(unLike>tr);
rm(mAvg);
rm(avg);
rm(tr);
rm(d);

#detemine dissimilarity matrix
diff=array(0,dim=c(uLen,uLen));
for(i in 1:uLen-1){
  for(j in (i+1):uLen){
    delta=hor[i,]-hor[j,];
    delta[is.na(delta)]=0
    diff[j,i]=sqrt(sum(delta^2));
  }
  print(i);
}
rm(delta);
rm(uLen);
#remove outliers

inLiersDiff=diff;
inLierUsers=users;
it=0;
inLierUnLike=unLike;
inLiersHor=hor;
for(co in outliers){
  inLiersDiff=inLiersDiff[-(co-it),-(co-it)];
  inLierUsers=inLierUsers[-(co-it)];
  inLierUnLike=inLierUnLike[-(co-it)];
  inLiersHor=inLiersHor[-(co-it),];
  it=it+1;
  print(co-it);
}

rm(co);
rm(it);

#clustering

h2=hclust(as.dist(inLiersDiff));
clus2=cutree(h2,k=6);
rm(inLierUnLike);
rm(inLierUsers);
rm(inLiersDiff);
rm(unLike);
rm(diff);

##q3
#classificatio
library('caret');
td=as.data.frame(t(t(inLiersHor)));
tl=as.factor(as.factor(t(t(clus2))));
fit=train(td,tl,method = 'AdaBoost.M1',tuneGrid = data.frame(.maxdepth=5,.mfinal=4,.coeflearn='Breiman'));
rm(inLiersHor);
rm(users);
rm(movies);
rm(td);
rm(rl);


##q4,q6
#Movie Clustering
library('RMySQL');
con = dbConnect(dbDriver("MySQL"), user = "root", password = "", dbname = "movies2");
allTagData=dbGetQuery(con,'SELECT MovieID , Tag FROM tags ');
tagFreqs=dbGetQuery(con,'SELECT Tag , count(*) as freq FROM tags group by Tag having freq>60 order by freq desc');      
tags=tagFreqs$Tag;
rm(tagFreqs);
rm(con);
movies=unique(allTagData$MovieID);
tagHor=array(FALSE,c(length(movies),length(tags)));
rownames(tagHor)=movies;
colnames(tagHor)=tags;
rMovies=allTagData$MovieID;
rTags=allTagData$Tag;
len=length(rMovies);
for(i in 1:len){
  if ((length(which(tags==rTags[i])) > 0)) {
    tagHor[toString(rMovies[i]),toString(rTags[i])]=TRUE;
  }
  
  if ( i %% 1000 == 0) {print(i)} 
}
rm(rMovies);
rm(rTags)
rm(len);
rm(i);

mLen=length(movies);
movieDiff=array(0,dim=c(mLen,mLen));
for(i in 1:mLen-1){
  for(j in (i+1):mLen){
    delta=xor(tagHor[i,],tagHor[j,]);
    movieDiff[j,i]=length(which(delta==TRUE));
  }
  print(i);
}
h3=hclust(as.dist(movieDiff));
clus3=cutree(h3,k=5);
rm(i);
rm(j);
rm(delta);
rm(mLen);
rm(movieDiff);
rm(tagHor);

##q7
#Favorite Ganre Of A User
library('RMySQL');
con = dbConnect(dbDriver("MySQL"), user = "root", password = "", dbname = "movies2");
movie_ganre=dbGetQuery(con,'SELECT * FROM ganre');
avgs=dbGetQuery(con,'SELECT MovieID,count(Rating),avg(Rating) FROM ratings group by MovieID');
ganres=unique(movie_ganre$ganre);
movies=avgs$MovieID;
movie_ganre_hor=array(0,c(length(movies),length(ganres)));
rownames(movie_ganre_hor)=movies;
colnames(movie_ganre_hor)=ganres;
rMovies=movie_ganre$MovieID;
rGanres=movie_ganre$ganre;
len=length(rMovies);
for(i in 1:len){
  if ((length(which(movies==rMovies[i])) > 0)) {
    
    movie_ganre_hor[toString(rMovies[i]),toString(rGanres[i])]=1;
  }
  
  if ( i %% 1000 == 0) {print(i)} 
}

rm(con);
rm(movie_ganre);
rm(len);
rm(rMovies);
rm(rGanres);
rm(avgs);

hor[is.na(hor)]=0;
user_ganre=hor %*% movie_ganre_hor;
uLen=length(users);
user_favorite=array(NA,uLen);
for(i in 1:uLen){
  index=which.max(user_ganre[i,]);
  user_favorite[i]=ganres[index];
}
rm(uLen);
rm(movie_ganre_hor);
rm(ganres);
rm(i);
rm(index);



##q8
#Favorite actor Of A User
library('RMySQL');
con = dbConnect(dbDriver("MySQL"), user = "root", password = "", dbname = "movies2");
movie_actor=dbGetQuery(con,'SELECT movie_id,person_id FROM cast_info  WHERE role_id<=2');
actors=dbGetQuery(con,'SELECT person_id,count(*) as freq FROM cast_info WHERE role_id<=2 group by person_id having freq>10');
avgs=dbGetQuery(con,'SELECT MovieID,count(Rating),avg(Rating) FROM ratings group by MovieID');

actors=actors$person_id;
movies=avgs$MovieID;
movie_actor_hor=array(0,c(length(movies),length(actors)));
rownames(movie_actor_hor)=movies;
colnames(movie_actor_hor)=actors;
rMovies=movie_actor$movie_id;
rActors=movie_actor$person_id;
len=length(rMovies);
for(i in 1:len){
  if ((length(which(actors==rActors[i])) > 0)&(length(which(movies==rMovies[i])) > 0)) {
    
    movie_actor_hor[toString(rMovies[i]),toString(rActors[i])]=1;
  }
  
  if ( i %% 1000 == 0) {print(i)} 
}

rm(rMovies);
rm(rActors);
tm(len);
rm(i);
rm(con);

hor[is.na(hor)]=0;
user_actor=hor %*% movie_actor_hor;

rm(avgs);
rm(hor);
rm(movie_actor_hor);
rm(movies);
rm(actors);
rm(movie_actor);