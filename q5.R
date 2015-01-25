printf <- function(...) invisible(print(sprintf(...)))
print('786');

# Database Data Fetch
library('RMySQL');
library('sqldf');
con = dbConnect(dbDriver("MySQL"), user = "dm", password = "a", dbname = "Movie_2");
allData=dbGetQuery(con,'SELECT * FROM ratings');
print('Fetched data from database.');


# Initial Calculations
rUsers=allData$UserID;
rRatings=allData$Rating;
rMovies=allData$MovieID;
len=length(rMovies);

users=unique(allData$UserID);
uLen=length(users);

print('Finished initial calculations.');


# Find users with strange votes counts
library('outliers')

calculateUsersRatingsCount <- function (test=FALSE) {
  ratingsCount <- array(0, dim=uLen);
  rownames(ratingsCount) <- users;
  for (i in 1:len) {
    ratingsCount[c(toString(rUsers[i]))] <- ratingsCount[c(toString(rUsers[i]))] + 1;
    if ((i > 0) && (i %% 10000 == 0)) {
      printf('progress: i=%d', i);
    }
    if ((test) && (i >= 10000)) {
      break;
    }
  }
  return(ratingsCount);
};

badRatingUserDetection1 <- function (ratingsCount) {
  userOutlierScore <- scores(ratingsCount, type="iqr");   # type=z/t/chisq/iqr/mad
  badRatingUsers = names(x[x>quantile(x, .99)]);
  return(badRatingUsers);
};

badRatingUserDetection2 <- function (ratingsCount) {
  normalUsers <- rm.outlier(ratingsCount);
  badRatingUsers <- setdiff(names(ratingsCount), names(normalUsers));
  return(badRatingUsers);
};

print('=> Detecting Outlier Users (by ratings count)...');
ratingsCount <- calculateUsersRatingsCount(test=TRUE);
badRatingUsers <- badRatingUserDetection2(ratingsCount);
print(badRatingUsers);
