myData <- read.csv2("C:/Users/Omid/Documents/DM PROJECT/Entekhabat88.csv");
x=array(0,dim=c(5,7,10));
names=c('AhmadiNejad','Moosavi','Karroobi','Rezaee','waste');


#calculate by fsd with all data;
nameIterator=0;
for(name in names){
  nameIterator=nameIterator+1;
  for(i in 0:9){
    I=which(myData[name] %% 10==i);
    x[nameIterator,1,i+1]=length(I);
    print(name);
    print(1);
    print(i)
  }
}


#calculate by ssd with all data
nameIterator=0;
for(name in names){
  nameIterator=nameIterator+1;
  for(i in 0:9){
    I=which(floor((myData[name] %% 100) / 10)==i);
    x[nameIterator,2,i+1]=length(I);
  }
}


#calculate by ssd with data greater than 10
nameIterator=0;
for(name in names){
  nameIterator=nameIterator+1;
  for(i in 0:9){
    I=which((myData[name]>=10)&(floor((myData[name] %% 100) / 10)==i));
    x[nameIterator,3,i+1]=length(I);
  }
}



#calculate by fsd with data greater than 10
nameIterator=0;
for(name in names){
  nameIterator=nameIterator+1;
  for(i in 0:9){
    I=which((myData[name]>=10)&(((myData[name] %% 10))==i));
    x[nameIterator,4,i+1]=length(I);
    print(name);
    print(4);
    print(i);
  }
}


#calculate by ssd with data greater than 100
nameIterator=0;
for(name in names){
  nameIterator=nameIterator+1;
  for(i in 0:9){
    I=which((myData[name]>=100)&(floor((myData[name] %% 100) / 10)==i));
    x[nameIterator,5,i+1]=length(I);
    print(name);
    print(5);
    print(i);
  }
}
  
  #calculate by fsd with data greater than 100
  nameIterator=0;
  for(name in names){
    nameIterator=nameIterator+1;
    for(i in 0:9){
      I=which((myData[name]>=100)&((myData[name] %% 10)==i));
      x[nameIterator,6,i+1]=length(I);
      print(name);
      print(6);
      print(i);
    }
}



#calculate by ssd with data greater than 1000
nameIterator=0;
for(name in names){
  nameIterator=nameIterator+1;
  for(i in 0:9){
    I=which((myData[name]>=1000)&(floor((myData[name] %% 100) / 10)==i));
    x[nameIterator,7,i+1]=length(I);
    print(name);
    print(7);
    print(i);
  }
}


#ks.test(0:9,x[3,3,]);

pValues=array(0,dim=c(5,7));
for(i in 1:5){
  for(j in 1:7){
   # sum=sum(x[i,j,]);
    #mean=sum/10;
    #value=0;
    #for(k in 1:10){
     # value=value+((x[i,j,k]-mean)^2)/mean;
      #pValues[i,j]=value;
    #}
    allZero=TRUE;
    for(l in 1:10){
      if(x[i,j,l]>0){
        allZero=FALSE;
      }
    }
    if(allZero){
      pValues[i,j]=NA;
      print(paste("all zero",i,j));
    }else{
    pValues[i,j]=chisq.test(x[i,j,])$p.value;
  }
 #   print(paste(i,j,chisq.test(x[i,j,])$p.value));
  }
}

karr=c();
for(j in 0:39){
I=which(myData['Karroobi']==j);
print(paste(j,length(I)));
karr[j]=length(I);
}

barplot(karr);

karr100=array(0,dim=c(20));
for(j in 100:299){
  I=which(myData['Karroobi']==j);
  print(paste(j,length(I)));
  karr100[floor((j-100)/10)]=karr100[(j-100)/10]+length(I);
}
barplot(karr100);