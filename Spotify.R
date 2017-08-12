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

##  Dupe Check  ----  I removed "target" first because that was somebody else's response
##                    (Do they like the song or not) and caused a few dupes with inconsistent response.
##                    Since I'm just exploring the data and not trying to predict, I don't need it.
sum(duplicated(spotify)); ## 8 dupes
spotify = spotify[!duplicated(spotify),];

## Extract Meta Data and Remove from data set
metaStuff = spotify[,c("song_title","artist")];
spotify$song_title = NULL;
spotify$artist = NULL;

##  NA Check
colSums(is.na(spotify));
##  No NAs to remove. No need to check rows.
##  This would have already shown up in the summary, but doing it again for good measure.

##  Outliers and skew
boxplot(spotify[,c(1,2,4,5,7,10,13)]); ## All between 0 and 1... This is nasty.
boxplot(spotify$loudness);

##  I want to see the histograms and density of each variable.
for( ind in 1:13 ){
  hist(spotify[[ind]],freq=FALSE,main=names(spotify)[ind]);
  lines(density(spotify[[ind]]),col="blue");
  readline("Press <enter>:");
}
##  Skew: Acousticness, energy, instrumentalness, liveness, loudness, speechiness
##  Scaling: duration, tempo, loudness
##  Categorical: key, mode (1-major/0-minor - mostly major), time_sig

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


