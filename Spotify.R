###################################################################
#                                                                 #
#     Playing with Spotify Data just to have something up here    #
#       to show that I have at least a basic command of R         #
#       for potential employers                                   #
#                                                                 #
###################################################################
library(corrplot);
library(ggplot2);
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
##  Skew: Acousticness, energy, instrumentalness, liveness, loudness (just outliers), speechiness
##  Mostly Zero: instrumentalness
##  Scaling: duration, tempo, loudness
##  Categorical: key, mode (1-major/0-minor - mostly major), time_sig

##  This might come in handy.
boxCox = function(X,lam){
  if(lam==0){ return(log(X)); } else { return((X^lam - 1)/lam); }
}
unBox = function(X,lam){
  if(lam==0){ return(exp(X)); } else { return((lam*X + 1)^(1/lam)); }
}

par(mfrow=c(2,1))
hist(spotify$acousticness,freq=FALSE)
lines(density(spotify$acousticness),col="blue")
hist(scale(boxCox(spotify$acousticness,0.25)),freq=FALSE)
lines(density(scale(boxCox(spotify$acousticness,0.25))),col="blue")
par(mfrow=c(1,1))



##  Initial correlation check (pre-FE)
corrplot(cor(spotify), type="lower");
cor(spotify[,c("acousticness","energy","loudness")]);
ggpairs(spotify[,c("acousticness","energy","loudness")]);

##  acoustic ~ energy    -> -0.65
##  acoustic ~ loudness  -> -0.56
##  energy ~ loudness    -> +0.76

scaledAEL = data.frame( acousticness = log(spotify$acousticness),
                        logEnergy = (log(spotify$energy)),
                        loudness = scale(spotify$loudness));
ggpairs(scaledAEL);
##  A deceptively high correlation between Energy and Loudness.
##    The correlation makes sense heuristically, but the outliers have too much leverage.
##    Let's see what happens if we cluster.

kmSS = function(k,datFm = spotify){
  km = kmeans(datFm,k);
  return(km$betweenss/km$totss);
}

plot( 1:8, sapply(1:8,kmSS), type="b", main="SSB/SST", xlab="k",ylab="")
km = kmeans(spotify, 4);
distFromCluster = sqrt(rowSums((spotify - km$centers[km$cluster,])^2));
boxplot(distFromCluster)
clOutliers = boxplot.stats(distFromCluster)$out;
length(clOutliers);## 81 cluster outliers. Examined, and not helpful.

plot(spotify$energy,spotify$loudness);
with(spotify[distFromCluster %in% clOutliers,],
     points(energy,loudness,pch="+",col="red"))
##  ... Nope. Not helpful.


####
##  Some basic feature engineering
####

##  Skew: Acousticness, energy, instrumentalness, liveness, loudness, speechiness
##  Mostly Zero: instrumentalness
##  Scaling: duration, tempo, loudness
##  Categorical: key, mode (1-major/0-minor - mostly major), time_sig

plot(c(-3,+3),c(0,1),col="white");
abline(h=0)
for( tL in (0:5) ){
  lines(density(scale(boxCox(spotify$instrumentalness,tL/5))),col=tL+1);
  readline(paste("lambda = ",tL/5,". Press <enter>:",sep=""));
}

##  Many of these are already normalized on a scale from 0 to 1, but after the transforms,
##    I have to restandardize them to get as close as possible to normality assumptions for
##    correlation analysis.
spotifyFE = data.frame( acousticness = scale(boxCox(spotify$acousticness,0.25)),
                        danceability = scale(spotify$danceability),
                        duration = scale(spotify$duration_ms),
                        energy = scale((spotify$energy)),
                        instr = ifelse(spotify$instrumentalness==0,0,1),
                        logLiveness = scale(log(spotify$liveness)),
                        negLogLoudness = scale(log(-spotify$loudness)),
                        mode = spotify$mode,
                        logSpeech = scale(log(spotify$speechiness)), # Not sure about this one.
                        tempo = scale(spotify$tempo),
                        valence = scale(spotify$valence),
                        time_signature = as.factor(spotify$time_signature)
                        );
corrplot(cor(spotifyFE[,-12]),type="lower");
ggpairs(spotifyFE);
ggpairs(spotifyFE[,c("acousticness","danceability","energy","negLogLoudness","valence")]);


