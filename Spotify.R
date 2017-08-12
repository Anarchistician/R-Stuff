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

metaStuff = spotify[,c("song_title","artist")];
target = spotify$target;
spotify$song_title = NULL;
spotify$artist = NULL;
spotify$target = NULL;

##  NA Check
colSums(is.na(spotify));
##  No NAs to remove. No need to check rows.

##  Dupe Check
sum(duplicated(spotify)); ## 5 dupes
spotify = spotify[!duplicated(spotify),];


##  Initial correlation check
corrplot(cor(spotify), type="lower");
cor(spotify[,c("acousticness","energy","loudness")]);
ggpairs(spotify[,c("acousticness","energy","loudness")]);

##  acoustic ~ energy    -> -0.65
##  acoustic ~ loudness  -> -0.56
##  energy ~ loudness    -> +0.76



