##q1,q2,q9

library('RMySQL');
printf <- function(...) invisible(print(sprintf(...)))

print('786');
#con = dbConnect(dbDriver("MySQL"), user = "dm", password = "a", dbname = "Movie_2");
con = dbConnect(dbDriver("MySQL"), user = "root", password = "", dbname = "movies2");
allData=dbGetQuery(con,'SELECT * FROM ratings');
avgs=dbGetQuery(con,'SELECT MovieID,count(Rating),avg(Rating) FROM ratings group by MovieID');
movies=avgs$MovieID;
users=unique(allData$UserID);
test <- TRUE;
print('Initial data loaded');


#covert vertical to horizontal
createRatingsMatrix <- function (allData, users) {
  hor=array(NA,dim=c(length(users),length(movies)));
  rownames(hor)=users;
  colnames(hor)=movies;
  rMovies=allData$MovieID;
  rUsers=allData$UserID;
  rRatings=allData$Rating;
  len=length(rMovies);
  for(i in 1:len){
    hor[toString(rUsers[i]),toString(rMovies[i])]=rRatings[i];
    if ( i %% 10000 == 0) {print(i)}
    if ( test && (i >= 10000)) {break}
  }
  rm(rMovies); rm(rUsers); rm(rRatings);
  return(hor);
}


#outlier detection
detectUnlikelyUsers <- function (hor, avgs, users) {
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
  rm(mAvg); rm(tr); rm(d);
  return(c(outliers,unLike));
}


#determine dissimilarity matrix
calculateUsersDiff <- function (hor, users, outliers=FALSE, unLike=FALSE) {
  uLen=length(users);
  diff=array(0,dim=c(uLen,uLen));
  for(i in 1:uLen-1){
    for(j in (i+1):uLen){
      delta=hor[i,]-hor[j,];
      delta[is.na(delta)]=0
      diff[j,i]=sqrt(sum(delta^2));
    }
    if ( i %% 10 == 0) {print(i)}
    if ( test && (i >= 10)) {break}
  }
  rm(delta); rm(uLen);

  inLiersDiff=diff;
  if (outliers != FALSE) {
    #remove outliers
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
    rm(co); rm(it);
  }

  rm(inLierUnLike); rm(inLierUsers); rm(unLike); rm(diff);
  return(c(inLiersHor,inLiersDiff));
}


#clustering
clusterUsers <- function (inLiersDiff) {
  h2=hclust(as.dist(inLiersDiff));
  clus2=cutree(h2, k=6);
  return(clus2);
}


# Q1 & Q2
hor <- createRatingsMatrix(allData, users);
print('matrix converted');
rm(allData);
r <- detectUnlikelyUsers(hor, avgs, users);
outliers <- r[1]; unLike <- r[2];
print('outliers detected');
r <- calculateUsersDiff(hor, users, outliers=outliers, unLike=unLike);
inLiersHor <- r[1]; inLiersDiff <- r[2];
print('diff calculated');
clus2 <- clusterUsers(inLiersDiff);
rm(inLiersDiff);
print('clustering done');


#classification
library('caret');
trainClassifier <- function (inLiersHor, clusters) {
  td=as.data.frame(t(t(inLiersHor)));
  tl=as.factor(as.factor(t(t(clusters))));
  fit=train(td,tl,method = 'AdaBoost.M1',tuneGrid = data.frame(.maxdepth=5,.mfinal=4,.coeflearn='Breiman'));
  rm(td); rm(rl);
  return(fit);
}


# Q3
fit <- trainClassifier(inLiersHor, clus2);
rm(inLiersHor); rm(users); rm(movies);
print('classifier trained');


#Movie Clustering
movieCluster <- function () {
  allTagData=dbGetQuery(con,'SELECT MovieID , Tag FROM tags ');
  tagFreqs=dbGetQuery(con,'SELECT Tag , count(*) as freq FROM tags group by Tag having freq>60 order by freq desc');      
  tags=tagFreqs$Tag;
  rm(tagFreqs);
  
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
  rm(rMovies); rm(rTags) rm(len); rm(i);

  mLen=length(movies);
  movieDiff=array(0,dim=c(mLen,mLen));
  for(i in 1:mLen-1){
    for(j in (i+1):mLen){
      delta=xor(tagHor[i,],tagHor[j,]);
      movieDiff[j,i]=length(which(delta==TRUE));
    }
    #print(i);
  }
  
  h3=hclust(as.dist(movieDiff));
  clus3=cutree(h3,k=5);
  rm(i); rm(j); rm(delta); rm(mLen); rm(movieDiff); rm(tagHor);
  return(clus3);
}


##q4,q6
clus3 <- movieCluster();


# Outlier User Detection
# Find users with strange votes counts
library('outliers')
library('hash')

calculateUsersRatingsCount <- function (test=FALSE) {
  rc <- hash(keys=users, values=array(0, uLen));
  for (i in 1:len) {
    k <- toString(rUsers[i]);
    rc[[k]] <- rc[[k]] + 1;
    if ((i > 0) && (i %% 10000 == 0)) {
      printf('progress: i=%d', i);
    }
    if ((test) && (i >= 10000)) {
      break;
    }
  }
  rrc <- array(0, uLen);
  rownames(rrc) = users;
  for (i in 1:uLen) {
    k <- toString(rUsers[i]);
    rrc[[k]] <- rc[[k]];
  }
  if (!test) {
    write.table(rrc, file='data/ratingsCount');
  }
  return(rrc);
};

calculateUsersRatingsCount2 <- function (test=FALSE) {
  ur <- dbGetQuery(con,'SELECT UserID,count(*) as c FROM ratings group by UserID');
  us1 <- ur$UserID;
  counts <- ur$c;
  l <- length(us1);
  rc <- array(0, l);
  rownames(rc) <- us1;
  for (i in 1:l) {
    k <- toString(us1[i]);
    rc[[k]] <- counts[[i]];
  }
  return(rc);
}

badRatingUserDetection1 <- function (ratingsCount) {
  userOutlierScore <- scores(ratingsCount, type="iqr");   # type=z/t/chisq/iqr/mad
  badRatingUsers <- names(userOutlierScore[userOutlierScore>quantile(userOutlierScore, .99)]);
  return(badRatingUsers);
};

badRatingUserDetection2 <- function (ratingsCount) {
  normalUsers <- rm.outlier(ratingsCount);
  badRatingUsers <- setdiff(names(ratingsCount), names(normalUsers));
  return(badRatingUsers);
};

detectOutlierRaters <- function (test=FALSE) {
  print('=> Detecting Outlier Users (by ratings count)...');
  ratingsCount <- calculateUsersRatingsCount2(test=test);
  print(summary(ratingsCount));
  badRatingUsers <- badRatingUserDetection1(ratingsCount);
  print(badRatingUsers);
  underratings <- names(ratingsCount[ratingsCount<12]);
  return(c(underratings,badRatingUsers));
}


##q7
#Favorite Ganre Of A User
favGanre <- function (hor) {
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
}


##q8
#Favorite actor Of A User
favActor <- function (hor) {
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

hor[is.na(hor)]=0;
user_actor=hor %*% movie_actor_hor;

rm(avgs);
rm(hor);
rm(movie_actor_hor);
rm(movies);
rm(actors);
rm(movie_actor);
}
