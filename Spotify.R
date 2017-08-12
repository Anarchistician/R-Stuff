###################################################################
#                                                                 #
#     Playing with Spotify Data just to have something up here    #
#       to show that I have at least a basic command of R         #
#       for potential employers                                   #
#                                                                 #
###################################################################
library(corrplot);
library(GGally);

spotify = read.csv("data/Spotify.csv");
dim(spotify);
##  Not many columns, so I can do this.
summary(spotify);
##  Dude. Row names.
spotify$X = NULL;
spotify$target = NULL;

##  Dupe Check
sum(duplicated(spotify)); ## 8 dupes
spotify = spotify[!duplicated(spotify),];

metaStuff = spotify[,c("song_title","artist")];
target = spotify$target;
spotify$song_title = NULL;
spotify$artist = NULL;

##  NA Check
colSums(is.na(spotify));
##  No NAs to remove. No need to check rows.
##  This would have already shown up in the summary, but doing it again for good measure.

##  Outliers and skew
boxplot(spotify[,c(1,2,4,5,7,10,13)]); ## All between 0 and 1... This is nasty.
boxplot(spotify$loudness);

for( ind in c(1,2,4,5,7,10,13) ){
  hist(spotify[[ind]],freq=FALSE,main=names(spotify)[ind]);
  lines(density(spotify[[ind]]),col="blue");
  readline();
}

##  This might come in handy.
boxCox = function(X,lam){
  return( (X^lam - 1)/lam );
}
unBox = function(X,lam){
  return( (lam*X + 1)^(1/lam) )
}

par(mfrow=c(2,1))
hist(spotify$acousticness,freq=FALSE)
lines(density(spotify$acousticness),col="blue")
hist(scale(boxCox(spotify$acousticness,0.25)),freq=FALSE)
lines(density(scale(boxCox(spotify$acousticness,0.25))),col="blue")
par(mfrow=c(1,1))



##  Initial correlation check
corrplot(cor(spotify), type="lower");
cor(spotify[,c("acousticness","energy","loudness")]);
ggpairs(spotify[,c("acousticness","energy","loudness")]);

##  acoustic ~ energy    -> -0.65
##  acoustic ~ loudness  -> -0.56
##  energy ~ loudness    -> +0.76

scaledAEL = data.frame( acousticness = (spotify$acousticness),
                        logEnergy = (log(spotify$energy)),
                        loudness = scale(spotify$loudness));
ggpairs(scaledAEL);


