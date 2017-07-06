library(Hmisc)
moviedata<-rcorr(as.matrix(movie_metadata_1_[, c("num_critic_for_reviews", "num_voted_users", "imdb_score")]))
moviedata
cor.test(movie_metadata_1_$num_critic_for_reviews,movie_metadata_1_$imdb_score)
cor.test(movie_metadata_1_$num_voted_users,movie_metadata_1_$imdb_score)
moviedata.1 <- lm(num_critic_for_reviews~ imdb_score, data = movie_metadata_1_,
                   na.action = na.exclude)
summary(moviedata.1)
moviedata.2 <- lm(num_voted_users~ imdb_score, data = movie_metadata_1_,
                  na.action = na.exclude)
summary(moviedata.2)
moviedata.3 <- lm(duration~ imdb_score, data = movie_metadata_1_,
                  na.action = na.exclude)
summary(moviedata.3)
moviedata.4<- lm(gross~ imdb_score, data = movie_metadata_1_,
                  na.action = na.exclude)
summary(moviedata.4)
library(caret)
library(dplyr)
library(ggplot2)
library(GGally)
library(randomForest)
directors <- as.data.frame(table(movie_metadata_1_$director_name))
directors <- arrange(directors, desc(Freq))
ggplot(head(directors, 10), aes(x = reorder(factor(Var1), Freq), y = Freq, alpha = Freq)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Directors", y = "Number of movies") + ggtitle("Top 10 directors with most movies") + coord_flip() + theme_classic()
actors <- as.data.frame(table(movie_metadata_1_$actor_2_name))
actors <- arrange(actors, desc(Freq))
ggplot(head(actors, 20), aes(x = reorder(factor(Var1), Freq), y = Freq, alpha = Freq)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Actors", y = "Number of movies") + ggtitle("Top 20 actors with most movies") + coord_flip() + theme_dark()
country <- as.data.frame(table(movie_metadata_1_$country))
ggplot(country, aes(x = reorder(factor(Var1), Freq), y = Freq)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Countries", y = "Number of movies") + ggtitle("Total number of movies of different countries") + coord_flip() + theme_get()
year <- as.data.frame(table(movie_metadata_1_$title_year))
year <- arrange(year, desc(Freq))
ggplot(year[1:30,], aes(x = reorder(factor(Var1), Freq), y = Freq, alpha = Freq)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Years", y = "Number of movies") + ggtitle("Total number of movies every year") + coord_flip() + theme_dark()
aspect <- as.data.frame(table(movie_metadata_1_$aspect_ratio))
ggplot(aspect, aes(x = reorder(factor(Var1), Freq), y = Freq, alpha = Freq)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Aspect", y = "Number of movies") + ggtitle("Nmber of movies with different aspect ratio") +coord_flip() + theme_light()
rating <- as.data.frame(table(movie_metadata_1_$content_rating))
rating <- arrange(rating, desc(Freq))
ggplot(rating, aes(x = reorder(factor(Var1), Freq), y = Freq)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Contents", y = "Number of movies") + ggtitle("Number of movies with different content ratings") + coord_flip() + theme_light()
df <- as.data.frame(table(movie_metadata_1_$imdb_score))
df <- arrange(rating, desc(Freq))
ggplot(df, aes(x = reorder(factor(Var1)))+ geom_histogram(stat = "bin", fill = "blue") +ggtitle(paste("Distribution of", names(df)[14])) + theme_dark()
ggplot(df, aes(x = df[, 7], y = df[, 1])) + geom_point(color = "blue") + labs(x = names(df)[7], y = names(df)[1]) + stat_smooth(method = lm, se = F, color = "red") +theme_grey() + ggtitle(paste("cor:", 0.595)) + geom_smooth(color = "black")
head(movie_metadata_1_)
moviepartition<-sample(2,nrow(movie_metadata_1_),replace = TRUE,prob = c(0.8,0.2))
tdata<-movie_metadata_1_[moviepartition==1,]
vdata<-movie_metadata_1_[moviepartition==2,]
head(tdata)
head(vdata)
result<-lm(imdb_score~num_voted_users+num_critic_for_reviews,tdata)
summary(result)
result$cofficients
pred<-predict(result,vdata)
head(pred)
head(vdata)
head(vdata$imdb_score)
library(party)
library(randomForest)

# Create the forest.
output.forest <- randomForest(imdb_score~num_voted_users+num_critic_for_reviews, 
                              vdata)

# View the forest results.
print(output.forest) 

# Importance of each predictor.
print(importance(fit,type = 2))
accuracy(result,pred,threshold=20)
accuracy <- table(pred, tdata[,"imdb_score"])
sum(diag(accuracy))/sum(accuracy)
pred = predict(result, newdata=vdata)
confusionMatrix(data=pred, tdata$imdb_score)
library(caret)
omitmovie<-na.omit(movie_metadata_1_)
head(omitmovie)
library(party)
library(randomForest)
omitpartition<-sample(2,nrow(omitmovie),replace = TRUE,prob = c(0.8,0.2))
tdataomit<-omitmovie[omitpartition==1,]
vdataomit<-omitmovie[omitpartition==2,]
output.forest <- randomForest(imdb_score~num_voted_users+num_critic_for_reviews, 
                              vdataomit)

# View the forest results.
print(output.forest) 
output.forest <- randomForest(imdb_score~num_voted_users+num_critic_for_reviews, 
                              omitmovie)

# View the forest results.
print(output.forest) 
partition<-sample(2977,775)
train<-omitmovie[partition,]
test<-omitmovie[-partition,]
output.forest <- randomForest(imdb_score~num_voted_users+num_critic_for_reviews, 
                              data = train,importance=T,proximity=T)
p<-predict(output.forest,train)
p
print(output.forest) 
output.forest
table(test[,5],p)
importance(output.forest)
getTree(output.forest,500,labelVar = T)
oob.times(output.forest)
FOREST_model <- randomForest(imdb_score~num_voted_users+num_critic_for_reviews, 
                             , data=train,ntree=500, importance=TRUE, do.trace=100)
output.forest
genre<-as.data.frame(table(movie_metadata_1_$genres))
plot(genre,directors)
imdb<-as.data.frame(table(movie_metadata_1_$imdb_score))
plot(directors)
library(ggplot2)
ggplot(head(genre, 10), aes(x = reorder(factor(Var1), Freq), y = Freq, alpha = Freq)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "genre", y = "Number of movies") + ggtitle("Top 10 genre most succesful") + coord_flip() + theme_classic()
df <- movie_metadata_1_[, c(3, 4, 5, 6, 8, 9, 13, 14, 16, 19, 23, 24, 25, 26, 27, 28)]
head(df)
library(GGally)
g<-ggpairs(df, diag = list(continuous = "density", discrete = "bar"), axisLabels = "internal")
g
