library(dplyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(corrplot)
library(plotly)
library(caret)
library(rpart)
library(rpart.plot)
library(nortest)
library(tidyverse)
library(gridExtra)
library(tidymodels)
library(leaps)
library(glmnet)
library(pROC)
library(rsample)
library(correlation)
library(DataExplorer)
library(knitr)
library(outliers)
library(class)
library(grid)
library(cowplot)
library(car)
library(MASS)
library(e1071)


#### IMPORT AND READ THE DATASET

spotify_data=read.csv("C:/Users/filip/OneDrive/Desktop/UniPD/materie/statistical learning/2/esempi dataset/dataset_spotify.csv")
spotify <- subset(spotify_data, track_genre %in% c("classical", "hip-hop", 
                                                   "rock-n-roll", "reggaeton", "techno"))

#spotify_num<-spotify %>% select (-all_of(c("X","track_id","artists","album_name","track_name")))
spotify_num <- subset(spotify, select=c(popularity, duration_ms, explicit, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, time_signature,
                                             track_genre))
attach(spotify_num)

# we print the head of the dataset 
head(spotify,5)
head(spotify_num,5)


# we see the type of each variable in the dataset
str(spotify)
str(spotify_num)


#we print an overview of the dataset
summary(spotify)
summary(spotify_num)


#there aren't any NA in the dataset
anyNA(spotify)


#we transform the numerical and character variables into factor variables
spotify_num$track_genre=as.factor(spotify_num$track_genre)
spotify_num$explicit=as.factor(spotify_num$explicit)
spotify_num$key=as.factor(spotify_num$key)
spotify_num$mode=as.factor(spotify_num$mode)
spotify_num$time_signature=as.factor(spotify_num$time_signature)


#we see the number of each modality in the variable track_genre
table(spotify_num$track_genre)
prop.table(table(spotify_num$track_genre))






################################################################################
############                  EDA


#####BAR PLOT

#BAR PLOT OF VARIABLES FOR EACH GENRE
data_bar <- data.frame(track_genre, danceability, speechiness, liveness, valence, energy)
data_long <- tidyr::gather(data_bar, key = "variable", value = "value", -track_genre)
ggplot(data_long, aes(x = variable, y = value, fill = track_genre)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Variable") +
  ylab("Value") +
  ggtitle("Bar Plot of the variables for each genre") +
  theme_minimal()



#### BARPLOT EXPLICIT
df_explicit <- data.frame(explicit ,track_genre)
df_counts_explicit <- table(df_explicit$track_genre, df_explicit$explicit)
plot <- ggplot() +
  geom_col(data = as.data.frame(df_counts_explicit), aes(x = Var1, y = Freq, fill = Var2), position = "fill") +
  labs(title = "Relationship between Explicit and Track Genre", x = "Track Genre", y = "Percentage") +
  scale_fill_manual(values = c("gray", "red"), labels = c("False", "True"))+
  guides(fill = guide_legend(title = "Explicit"))
plot




#### BARPLOT KEY 1
df_key <- data.frame(key, track_genre)
df_counts_key <- table(df_key$track_genre, df_key$key)
plot <- ggplot() +
  geom_col(data = as.data.frame(df_counts_key), aes(x = Var1, y = Freq, fill = Var2), position = "fill") +
  labs(title = "Relationship between Key and Track Genre", x = "Track Genre", y = "Percentage") +
  scale_fill_manual(values = rainbow(12))+
  guides(fill = guide_legend(title = "Key"))
plot


#### BARPLOT KEY 2

df_key <- data.frame(key, track_genre)
df_counts_key <- table(df_key$key, df_key$track_genre)
plot <- ggplot(as.data.frame(df_counts_key), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_col() +
  labs(title = "Relationship between Key and Track Genre", x = "Key", y = "Frequency") +
  scale_fill_manual(values = rainbow(5)) +
  guides(fill = guide_legend(title = "Track Genre"))
plot




#### BARPLOT MODE
df_mode <- data.frame(mode, track_genre)
df_counts_mode <- table(df_mode$track_genre, df_mode$mode)
plot <- ggplot() +
  geom_col(data = as.data.frame(df_counts_mode), aes(x = Var1, y = Freq, fill = Var2), position = "fill") +
  labs(title = "Relationship between Mode and Track Genre", x = "Track Genre", y = "Percentage") +
  scale_fill_manual(values = c("gray", "red"), labels = c("0", "1"))+
  guides(fill = guide_legend(title = "Mode"))
plot




#### BARPLOT TIME_SIGNATURE
df_time_signature <- data.frame(time_signature, track_genre)
df_counts_time_signature <- table(df_time_signature$track_genre, df_time_signature$time_signature)
plot <- ggplot() +
  geom_col(data = as.data.frame(df_counts_time_signature), aes(x = Var1, y = Freq, fill = Var2), position = "fill") +
  labs(title = "Relationship between Time_signature and Track Genre", x = "Track Genre", y = "Percentage") +
  scale_fill_manual(values = c("gray", "red", "blue", "violet"), labels = c("1", "3", "4", "5"))+
  guides(fill = guide_legend(title = "Time_signature"))
plot




#####   GRAFICI A TORTA

##grafico a torta con percentuali sulla popularity rispetto al genere
sum_popularity <- aggregate(popularity ~ track_genre, data = spotify_num, FUN = sum)
sum_popularity$percent <- sum_popularity$popularity / sum(sum_popularity$popularity)
ggplot(sum_popularity, aes(x = "", y = popularity, fill = track_genre)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Sum of the popularity variable for each track_genre variable") +
  geom_text(aes(label = paste0(round(percent * 100), "%")), 
            position = position_stack(vjust = 0.5), size = 7) +
  guides(fill = guide_legend(title = "Track Genre")) +
  theme(legend.position = "right",
        legend.key.size = unit(1.5, "lines"),
        legend.text = element_text(size = 15))




##grafico a torta con percentuali sulla danceability rispetto al genere
sum_danceability <- aggregate(danceability ~ track_genre, data = spotify_num, FUN = sum)
sum_danceability$percent <- sum_danceability$danceability / sum(sum_danceability$danceability)
ggplot(sum_danceability, aes(x = "", y = danceability, fill = track_genre)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Sum of the danceability variable for each track_genre variable") +
  geom_text(aes(label = paste0(round(percent * 100), "%")), 
            position = position_stack(vjust = 0.5), size = 7) +
  guides(fill = guide_legend(title = "Track Genre")) +
  theme(legend.position = "right",
        legend.key.size = unit(1.5, "lines"),
        legend.text = element_text(size = 15))



##grafico a torta con percentuali sulla duration_ms rispetto al genere
sum_duration_ms <- aggregate(duration_ms ~ track_genre, data = spotify_num, FUN = sum)
sum_duration_ms$percent <- sum_duration_ms$duration_ms / sum(sum_duration_ms$duration_ms)
ggplot(sum_duration_ms, aes(x = "", y = duration_ms, fill = track_genre)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Sum of the duration_ms variable for each track_genre variable") +
  geom_text(aes(label = paste0(round(percent * 100), "%")), 
            position = position_stack(vjust = 0.5), size = 7) +
  guides(fill = guide_legend(title = "Track Genre")) +
  theme(legend.position = "right",
        legend.key.size = unit(1.5, "lines"),
        legend.text = element_text(size = 15))




##grafico a torta con percentuali sulla energy  rispetto al genere
sum_energy  <- aggregate(energy  ~ track_genre, data = spotify_num, FUN = sum)
sum_energy$percent <- sum_energy$energy / sum(sum_energy$energy )
ggplot(sum_energy, aes(x = "", y = energy , fill = track_genre)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Sum of the energy  variable for each track_genre variable") +
  geom_text(aes(label = paste0(round(percent * 100), "%")), 
            position = position_stack(vjust = 0.5), size = 7) +
  guides(fill = guide_legend(title = "Track Genre")) +
  theme(legend.position = "right",
        legend.key.size = unit(1.5, "lines"),
        legend.text = element_text(size = 15))




##grafico a torta con percentuali sulla loudness rispetto al genere
sum_loudness <- aggregate(loudness ~ track_genre, data = spotify_num, FUN = sum)
sum_loudness$percent <- sum_loudness$loudness / sum(sum_loudness$loudness)
ggplot(sum_loudness, aes(x = "", y = loudness, fill = track_genre)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Sum of the loudness variable for each track_genre variable") +
  geom_text(aes(label = paste0(round(percent * 100), "%")), 
            position = position_stack(vjust = 0.5), size = 7) +
  guides(fill = guide_legend(title = "Track Genre")) +
  theme(legend.position = "right",
        legend.key.size = unit(1.5, "lines"),
        legend.text = element_text(size = 15))



##grafico a torta con percentuali sulla speechiness rispetto al genere
sum_speechiness <- aggregate(speechiness ~ track_genre, data = spotify_num, FUN = sum)
sum_speechiness$percent <- sum_speechiness$speechiness / sum(sum_speechiness$speechiness)
ggplot(sum_speechiness, aes(x = "", y = speechiness, fill = track_genre)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Sum of the speechiness variable for each track_genre variable") +
  geom_text(aes(label = paste0(round(percent * 100), "%")), 
            position = position_stack(vjust = 0.5), size = 7) +
  guides(fill = guide_legend(title = "Track Genre")) +
  theme(legend.position = "right",
        legend.key.size = unit(1.5, "lines"),
        legend.text = element_text(size = 15))




##grafico a torta con percentuali sulla acousticness rispetto al genere
sum_acousticness <- aggregate(acousticness ~ track_genre, data = spotify_num, FUN = sum)
sum_acousticness$percent <- sum_acousticness$acousticness / sum(sum_acousticness$acousticness)
ggplot(sum_acousticness, aes(x = "", y = acousticness, fill = track_genre)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Sum of the acousticness variable for each track_genre variable") +
  geom_text(aes(label = paste0(round(percent * 100), "%")), 
            position = position_stack(vjust = 0.5), size = 7) +
  guides(fill = guide_legend(title = "Track Genre")) +
  theme(legend.position = "right",
        legend.key.size = unit(1.5, "lines"),
        legend.text = element_text(size = 15))



##grafico a torta con percentuali sulla liveness rispetto al genere
sum_liveness <- aggregate(liveness ~ track_genre, data = spotify_num, FUN = sum)
sum_liveness$percent <- sum_liveness$liveness / sum(sum_liveness$liveness)
ggplot(sum_liveness, aes(x = "", y = liveness, fill = track_genre)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Sum of the liveness variable for each track_genre variable") +
  geom_text(aes(label = paste0(round(percent * 100), "%")), 
            position = position_stack(vjust = 0.5), size = 7) +
  guides(fill = guide_legend(title = "Track Genre")) +
  theme(legend.position = "right",
        legend.key.size = unit(1.5, "lines"),
        legend.text = element_text(size = 15))



##grafico a torta con percentuali sulla valence rispetto al genere
sum_valence <- aggregate(valence ~ track_genre, data = spotify_num, FUN = sum)
sum_valence$percent <- sum_valence$valence / sum(sum_valence$valence)
ggplot(sum_valence, aes(x = "", y = valence, fill = track_genre)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Sum of the valence variable for each track_genre variable") +
  geom_text(aes(label = paste0(round(percent * 100), "%")), 
            position = position_stack(vjust = 0.5), size = 7) +
  guides(fill = guide_legend(title = "Track Genre")) +
  theme(legend.position = "right",
        legend.key.size = unit(1.5, "lines"),
        legend.text = element_text(size = 15))



##grafico a torta con percentuali sulla tempo rispetto al genere
sum_tempo <- aggregate(tempo ~ track_genre, data = spotify_num, FUN = sum)
sum_tempo$percent <- sum_tempo$tempo / sum(sum_tempo$tempo)
ggplot(sum_tempo, aes(x = "", y = tempo, fill = track_genre)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Sum of the tempo variable for each track_genre variable") +
  geom_text(aes(label = paste0(round(percent * 100), "%")), 
            position = position_stack(vjust = 0.5), size = 7) +
  guides(fill = guide_legend(title = "Track Genre")) +
  theme(legend.position = "right",
        legend.key.size = unit(1.5, "lines"),
        legend.text = element_text(size = 15))



####BOXPLOT


#boxplot of the variable energy for each track_genre
boxplot(popularity~track_genre, data = spotify_num,
        main = "Variation:- Popularity and  Genre",
        xlab = "Popularity",
        ylab = "Genre",
        col = "green",
        border = "blue",
        horizontal = TRUE
)



#boxplot of the variable Duration_ms for each track_genre
boxplot(duration_ms~track_genre, data = spotify_num,
        main = "Variation:- Duration_ms and  Genre",
        xlab = "Duration_ms",
        ylab = "Genre",
        col = "green",
        border = "blue",
        horizontal = TRUE
)



#boxplot of the variable danceability for each track_genre
boxplot(danceability~track_genre, data = spotify_num,
        main = "Variation:- Danceability and Genre",
        xlab = "Danceability",
        ylab = "Genre",
        col = "green",
        border = "blue",
        horizontal = TRUE
)



#boxplot of the variable energy for each track_genre
boxplot(energy~track_genre, data = spotify_num,
        main = "Variation:- Energy and  Genre",
        xlab = "Energy",
        ylab = "Genre",
        col = "green",
        border = "blue",
        horizontal = TRUE
)



#boxplot of the variable loudness for each track_genre
boxplot(loudness~track_genre, data = spotify_num,
        main = "Variation:- Loudness and Genre",
        xlab = "Loudness",
        ylab = "Genre",
        col = "green",
        border = "blue",
        horizontal = TRUE
)



#boxplot of the variable speechiness for each track_genre
boxplot(speechiness~track_genre, data = spotify_num,
        main = "Variation:- Speechiness and Genre",
        xlab = "Speechiness",
        ylab = "Genre",
        col = "green",
        border = "blue",
        horizontal = TRUE
)



#boxplot of the variable acousticness for each track_genre
boxplot(acousticness~track_genre, data = spotify_num,
        main = "Variation:- Acousticness and  Genre",
        xlab = "Acousticness",
        ylab = "Genre",
        col = "green",
        border = "blue",
        horizontal = TRUE
)



#boxplot of the variable instrumentalness for each track_genre
boxplot(instrumentalness~track_genre, data = spotify_num,
        main = "Variation:- Instrumentalness and Genres",
        xlab = "Instrumentalness",
        ylab = "Genre",
        col = "green",
        border = "blue",
        horizontal = TRUE
)



#boxplot of the variable liveness for each track_genre
boxplot(liveness~track_genre, data = spotify_num,
        main = "Variation:- Liveness and Genres",
        xlab = "Liveness",
        ylab = "Genre",
        col = "green",
        border = "blue",
        horizontal = TRUE
)



#boxplot of the variable valence for each track_genre
boxplot(valence~track_genre, data = spotify_num,
        main = "Variation:- Liveness and Genres",
        xlab = "Liveness",
        ylab = "Genre",
        col = "green",
        border = "blue",
        horizontal = TRUE
)



#boxplot of the variable valence for each track_genre
boxplot(tempo~track_genre, data = spotify_num,
        main = "Variation:- Tempo and Genres",
        xlab = "Tempo",
        ylab = "Genre",
        col = "green",
        border = "blue",
        horizontal = TRUE
)




#box plot generally of the audio features
audio_f <- gather(spotify_data,decriptive_vars, values, danceability, energy,
                  speechiness, acousticness,  liveness,valence,
                  factor_key=TRUE)
var_distribution <- ggplot(audio_f, aes(x = decriptive_vars, y = values)) +
  geom_boxplot() + 
  coord_flip() +
  labs(title = "Distribution across rest of the variables", x = "Audio features", y = "Values")
var_distribution




#Density plot of the some variables

# Density plot for popularity
popularity_density <- ggplot(spotify_num, aes(x=popularity, fill=track_genre)) +
  geom_density(alpha=.5) +
  labs(title="Density Plot of popularity ", x="Popularity", y="Density") +
  scale_fill_discrete(name="Track_genre")
plot(popularity_density)


# Density plot for durations_ms
duration_ms_density <- ggplot(spotify_num, aes(x=duration_ms, fill=track_genre)) +
  geom_density(alpha=.5) +
  labs(title="Density Plot of duration_ms ", x="Duration_ms", y="Density") +
  scale_fill_discrete(name="Track_genre")
plot(duration_ms_density)


# Density plot for explicit
explicit_density <- ggplot(spotify_num, aes(x=explicit, fill=track_genre)) +
  geom_density(alpha=.5) +
  labs(title="Density Plot of explicit ", x="Explicit", y="Density") +
  scale_fill_discrete(name="Track_genre")
plot(explicit_density)


# Density plot for loudness
loudness_density <- ggplot(spotify_num, aes(x=loudness, fill=track_genre)) +
  geom_density(alpha=.5) +
  labs(title="Density Plot of loudness ", x="Loudness", y="Density") +
  scale_fill_discrete(name="Track_genre")
plot(loudness_density)


# Density plot for instrumentalness
instrumentalness_density <- ggplot(spotify_num, aes(x=instrumentalness, fill=track_genre)) +
  geom_density(alpha=.5) +
  labs(title="Density Plot of instrumentalness ", x="Instrumentalness", y="Density") +
  scale_fill_discrete(name="Track_genre")
plot(instrumentalness_density)


# Density plot for tempo
tempo_density <- ggplot(spotify_num, aes(x=tempo, fill=track_genre)) +
  geom_density(alpha=.5) +
  labs(title="Density Plot of tempo ", x="Tempo", y="Density") +
  scale_fill_discrete(name="Track_genre")
plot(tempo_density)



#Density plot of the variable for each track_genre

# Density plot per danceability
dance_density <- ggplot(spotify_num, aes(x=danceability, fill=track_genre)) +
  geom_density(alpha=.5) +
  labs(title="Density Plot of danceability ", x="Danceability", y="Density") +
  scale_fill_discrete(name="Track_genre")

# Density plot per energy
energy_density <- ggplot(spotify_num, aes(x=energy, fill=track_genre)) +
  geom_density(alpha=.5) +
  labs(title="Density Plot of energy ", x="Energy" , y="Density") +
  scale_fill_discrete(name="Track_genre")

# Density plot per speechiness
speech_density <- ggplot(spotify_num, aes(x=speechiness, fill=track_genre)) +
  geom_density(alpha=.5) +
  labs(title="Density Plot of speechiness ", x="Speechiness",  y="Density") +
  scale_fill_discrete(name="Track_genre")

# Density plot per acousticness
acoustic_density <- ggplot(spotify_num, aes(x=acousticness, fill=track_genre)) +
  geom_density(alpha=.5) +
  labs(title="Density Plot of acousticness ", x="Acousticness",  y="Density") +
  scale_fill_discrete(name="Track_genre")

# Density plot per liveness
live_density <- ggplot(spotify_num, aes(x=liveness, fill=track_genre)) +
  geom_density(alpha=.5) +
  labs(title="Density Plot of liveness ", x="Liveness",  y="Density") +
  scale_fill_discrete(name="Track_genre")

# Density plot per valence
valence_density <- ggplot(spotify_num, aes(x=valence, fill=track_genre)) +
  geom_density(alpha=.5) +
  labs(title="Density Plot of valence ", x="Valence",  y="Density") +
  scale_fill_discrete(name="Track_genre")

grid.arrange(dance_density, energy_density, speech_density, acoustic_density, 
             live_density, valence_density, ncol=2)









########  MATRIX CORRELATION


#matrice di scatterplot con indici di pearson 
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}
pairs(~danceability+energy+loudness+acousticness+speechiness+instrumentalness+valence, data = spotify_num, lower.panel = panel.cor)







############################################################
###PREPARING THE DATASET FOR BUILDING MODELS
#Creation of dummy variables for the genres. 
#One variable is created for each genre in the dataset
#The variable will take the value 1 if the track belong to 
#that specific genre and 0 otherwise
#In this way it will be possible to train a logistic regression
#model for each genre to discriminate wether a track belongs to it or not
dummies <- model.matrix(~ track_genre + 0)
spotify_dummies <- data.frame(spotify_num, dummies)
names(spotify_dummies)[names(spotify_dummies) == "track_genreclassical" ] <- "classical"
names(spotify_dummies)[names(spotify_dummies) == "track_genrehip.hop" ] <- "hiphop"
names(spotify_dummies)[names(spotify_dummies) == "track_genrereggaeton" ] <- "reggaeton"
names(spotify_dummies)[names(spotify_dummies) == "track_genrerock.n.roll" ] <- "rock.n.roll"
names(spotify_dummies)[names(spotify_dummies) == "track_genretechno" ] <- "techno"
attach(spotify_dummies)

#The dataset with the dummy variables is splitted in a training set and in a test set
#no validation set is needed beacuse no hyperparameters need to be choosen in this section
set.seed(2222)

split <- initial_split(spotify_dummies, prop = 0.80)
train <- training(split)
test <- testing(split)


#proportion of the variable "track_genre" in the training and test set 
prop.table(table(train$track_genre))
prop.table(table(test$track_genre))

############################################################
###BUILDING THE LOGISTIC REGRESSION MODELS
#In orther to find the best subset of predictors for each genre
#we start every time by fitting the full model and then we proceed 
#to remove the predictors with the highest p-value following a backward
#elimination approach.

#####
###Classical
glm.classical <- glm(classical~ popularity + duration_ms 
                     + explicit + danceability 
                     + energy + key + loudness + mode 
                     + speechiness + acousticness 
                     + instrumentalness + valence+ tempo 
                     + liveness + time_signature,
                     data = train, family = binomial)
summary(glm.classical)

#removing explicit
glm.classical <- update(glm.classical, .~.-explicit)
summary(glm.classical)

#removing key
glm.classical <- update(glm.classical, .~.-key)
summary(glm.classical)

#removing time_signature
glm.classical <- update(glm.classical, .~.-time_signature)
summary(glm.classical)

#removing tempo
glm.classical <- update(glm.classical, .~.-tempo)
summary(glm.classical)

#removing speechiness
glm.classical <- update(glm.classical, .~.-speechiness)
summary(glm.classical)

#removing liveness
best.glm.classical <- update(glm.classical, .~.-liveness)
summary(best.glm.classical)

#####
###Hip-hop
glm.hiphop <- glm(hiphop ~ popularity + duration_ms 
                  + explicit + danceability + energy + key
                  + loudness+mode + speechiness 
                  + acousticness + instrumentalness 
                  + valence + tempo + liveness 
                  + time_signature, data = train, 
                    family = binomial)
summary(glm.hiphop)

#removing key
glm.hiphop <- update(glm.hiphop, .~.-key)
summary(glm.hiphop)

#removing time_signature
glm.hiphop <- update(glm.hiphop, .~.-time_signature)
summary(glm.hiphop)

#removing duration_ms
best.glm.hiphop <- update(glm.hiphop, .~.-duration_ms)
summary(best.glm.hiphop)

#####
###Reggaeton
glm.reggaeton <- glm(reggaeton ~ popularity + duration_ms
                      + explicit + danceability + energy
                      + key + loudness + mode + speechiness
                      + acousticness + instrumentalness 
                      + valence+tempo + liveness
                      + time_signature, 
                      data = train, 
                      family = binomial)
summary(glm.reggaeton)

#removing energy
glm.reggaeton <- update(glm.reggaeton, .~.-energy)
summary(glm.reggaeton)

#removing tempo
glm.reggaeton <- update(glm.reggaeton, .~.-tempo)
summary(glm.reggaeton)

#removing key
best.glm.reggaeton <- update(glm.reggaeton, .~.-key)
summary(best.glm.reggaeton)

#####
###Techno
glm.techno <- glm(techno ~ popularity
                  + duration_ms + explicit + danceability
                  + energy + key + loudness + mode 
                  + speechiness + acousticness 
                  + instrumentalness + valence + tempo 
                  + liveness+time_signature,
                  data = train, family = binomial)
summary(glm.techno)

#removing mode
glm.techno <- update(glm.techno, .~.-mode)
summary(glm.techno)


#removing liveness
glm.techno <- update(glm.techno, .~.-liveness)
summary(glm.techno)

#removings time_signature
glm.techno <- update(glm.techno, .~.-time_signature)
summary(glm.techno)

#removing key
best.glm.techno <- update(glm.techno, .~.-key)
summary(best.glm.techno)

#####
###Rock and roll
glm.rock_n.roll <- glm(rock.n.roll ~ popularity + duration_ms
                       + explicit + danceability
                       + energy + key + loudness + mode
                       + speechiness + acousticness 
                       + instrumentalness + valence
                       + tempo + liveness + time_signature,
                       data = train, family = binomial)
summary(glm.rock_n.roll)

##removing time_signature
glm.rock_n.roll <- update(glm.rock_n.roll, .~.-time_signature)
summary(glm.rock_n.roll)

#removing tempo
glm.rock_n.roll <- update(glm.rock_n.roll, .~.-tempo)
summary(glm.rock_n.roll)

#removing key
best.glm.rock_n.roll <- update(glm.rock_n.roll, .~.-key)
summary(best.glm.rock_n.roll)

############################################################
###DIAGNOSTIC
#We now try to understand if there are some problems in the 
#model we found and to understand how they perform

#####
###Checking for collinearity in the models
vif(best.glm.classical)
vif(best.glm.techno)
vif(best.glm.rock_n.roll)
vif(best.glm.reggaeton)
vif(best.glm.hiphop)
#It is possible to see that none of the models present problem 
#of collinearity between the predictors.
#Precisely, the Variance Inflation Factor computed on the predictors
#of each model never exceed the value 5.

#####
###Trying the best models, one at time, on the test set

###classical
logistic_prob_classical <- predict(best.glm.classical, test, type = "response")
logistic_pred_classical <- rep(0, 1000)
logistic_pred_classical[logistic_prob_classical > 0.5] <-1
classical_table <- table(logistic_pred_classical, test$classical)
classical_table <- classical_table[2:1, 2:1] #rearrange the confusion matrix to match the standar format
accuracy <- sum(diag(classical_table)) / sum(classical_table)
tpr_for_classical <- classical_table[1,1]/sum(classical_table[,1])
fpr_for_classical <- classical_table[1,2]/sum(classical_table[,2])
print(classical_table) 
print(paste("Accuracy for classical music:", accuracy))
print(paste("The True Positive Rate for classical music is: ", tpr_for_classical))
print(paste("The False Positive Rate for classical music is: ", fpr_for_classical))

###techno
logistic_prob_techno <- predict(best.glm.techno, test, type = "response")
logistic_pred_techno <- rep(0, 1000)
logistic_pred_techno[logistic_prob_techno > 0.5] <-1
techno_table <- table(logistic_pred_techno, test$techno)
techno_table <- techno_table[2:1, 2:1] 
accuracy <- sum(diag(techno_table)) / sum(techno_table)
tpr_for_techno <- techno_table[1,1]/sum(techno_table[,1])
fpr_for_techno <- techno_table[1,2]/sum(techno_table[,2])
print(techno_table)
print(paste("Accuracy for techno music:", accuracy))
print(paste("The True Positive Rate for techno music is: ", tpr_for_techno))
print(paste("The False Positive Rate for techno music is: ", fpr_for_techno))

###hip-hop
logistic_prob_hiphop <- predict(best.glm.hiphop, test, type = "response")
logistic_pred_hiphop <- rep(0, 1000)
logistic_pred_hiphop[logistic_prob_hiphop > 0.5] <-1
hiphop_table <- table(logistic_pred_hiphop, test$hiphop)
hiphop_table <- hiphop_table[2:1, 2:1] 
accuracy <- sum(diag(hiphop_table)) / sum(hiphop_table)
tpr_for_hiphop <- hiphop_table[1,1]/sum(hiphop_table[,1])
fpr_for_hiphop <- hiphop_table[1,2]/sum(hiphop_table[,2])
print(hiphop_table)
print(paste("Accuracy for hiphop music:", accuracy))
print(paste("The True Positive Rate for Hip-Hop music is: ", tpr_for_hiphop))
print(paste("The False Positive Rate for techno music is: ", fpr_for_hiphop))

###reggaeton
logistic_prob_reggaeton <- predict(best.glm.reggaeton, test, type = "response")
logistic_pred_reggaeton <- rep(0, 1000)
logistic_pred_reggaeton[logistic_prob_reggaeton > 0.5] <-1
reggaeton_table <- table(logistic_pred_reggaeton, test$reggaeton)
reggaeton_table <- reggaeton_table[2:1, 2:1] 
accuracy <- sum(diag(reggaeton_table)) / sum(reggaeton_table)
tpr_for_reggaeton <- reggaeton_table[1,1]/sum(reggaeton_table[,1])
fpr_for_reggaeton <- reggaeton_table[1,2]/sum(reggaeton_table[,2])
print(reggaeton_table)
print(paste("Accuracy for reggaeton music:", accuracy))
print(paste("The True Positive Rate for reaggaeton music is: ", tpr_for_reggaeton))
print(paste("The False Positive Rate for reggaeton music is: ", fpr_for_reggaeton))

###rock and roll
logistic_prob_rock.n.roll <- predict(best.glm.rock_n.roll, test, type = "response")
logistic_pred_rock.n.roll <- rep(0, 1000)
logistic_pred_rock.n.roll[logistic_prob_rock.n.roll > 0.5] <-1
rock.and.roll_table <- table(logistic_pred_rock.n.roll, test$rock.n.roll)
rock.and.roll_table <- rock.and.roll_table[2:1, 2:1]
accuracy <- sum(diag(rock.and.roll_table)) / sum(rock.and.roll_table)
tpr_for_rock.and.roll <- rock.and.roll_table[1,1]/sum(rock.and.roll_table[,1])
fpr_for_rnr <- rock.and.roll_table[1,2]/sum(rock.and.roll_table[,2])
print(rock.and.roll_table)
print(paste("Accuracy for rock and roll music:", accuracy))
print(paste("The True Positive Rate for rock and roll music is: ", tpr_for_rock.and.roll))
print(paste("The False Positive Rate for rock and roll music is: ", fpr_for_rnr))

#####
###Interpretation
#For classical, techno and rock and roll the accuracy is above 90%
#and for reggaeton and hiphop above 80%. At first this seems 
#like a good result but in fact it is misleading.
#In fact, we train the classifiers to recognize one target genre at time,
#comparing it with all the others genres in the dataset.
#In this way, even if all the genres are equally represented in the  
#training set, every time the target class (1) we are trying to identify is 
#is the minority class (approx 0.2 of the examples) while the "other" (0) class 
#is the majority class (approx 0.8 of the examples).
#In this context the accuracy is not a reliable metric. 
#The true prediction rate and 
#Let's chech the True and False Positive rates of the classifiers
#may be more explicative.
#While the FPR is below 10% for all the classifiers the TPR 
#does not behave very well.
#In fact, despite for classical music which is around 90%,
#it show mediocre performance for techno and rnr (around 77%)
#and poor results for reggaeton and hiphop, respectively around 56%
#and 35%

#####
####ROC curves for classifiers

###classical
roc.out.classical <- roc(test$classical, logistic_prob_classical, levels=c(0, 1))
plot(roc.out.classical, print.auc=TRUE, legacy.axes=TRUE,
     xlab="False positive rate", ylab="True positive rate")
auc(roc.out.classical)
###techno
roc.out.techno <- roc(test$techno, logistic_prob_techno, levels=c(0, 1))
plot(roc.out.techno, print.auc=TRUE, legacy.axes=TRUE,
     xlab="False positive rate", ylab="True positive rate")
auc(roc.out.techno)
###rock and roll
roc.out.rock.n.roll <- roc(test$rock.n.roll, logistic_prob_rock.n.roll, levels=c(0, 1))
plot(roc.out.rock.n.roll, print.auc=TRUE, legacy.axes=TRUE,
     xlab="False positive rate", ylab="True positive rate")
auc(roc.out.rock.n.roll)
###reggaeton
roc.out.reggaeton <- roc(test$reggaeton, logistic_prob_reggaeton, levels=c(0, 1))
plot(roc.out.reggaeton, print.auc=TRUE, legacy.axes=TRUE,
     xlab="False positive rate", ylab="True positive rate")
auc(roc.out.reggaeton)
###hiphop
roc.out.hiphop <- roc(test$hiphop, logistic_prob_hiphop, levels=c(0, 1))
plot(roc.out.hiphop, print.auc=TRUE, legacy.axes=TRUE,
     xlab="False positive rate", ylab="True positive rate")
auc(roc.out.hiphop)


############################################################
###DEALING WITH UNBLANCE OF THE TRAINING SET
#In order to overcome the unbalncing in the data we decided 
#to compare three approaches: undersampling, oversampling and a mixed one.
#As target genres for the comparison we used hiphop for which  
#his classifier had the worst TPR

#####
#Undersampling with a perfect balance between hiphop class (1)
#and "other" class (0)
other_class <- train[train$hiphop == 0, ]
subsample_size <- nrow(train[train$hiphop == 1, ]) # size of the 1 class
set.seed(123) # set a seed for reproducibility
other_class_subsample <- other_class[sample(nrow(other_class), subsample_size), ]
undersample.balanced.data.hiphop <- rbind(other_class_subsample, train[train$hiphop == 1, ])

best.glm.hiphop.undersampling <- glm(hiphop ~ popularity 
                                          + explicit + danceability
                                          + energy + +mode
                                          + speechiness 
                                          + acousticness 
                                          + instrumentalness 
                                          + valence
                                          +tempo
                                          +liveness,
                                          data=undersample.balanced.data.hiphop, 
                                          family = binomial)

#Here we check if the performance on the test set improved
logistic_prop_hiphop_under <- predict(best.glm.hiphop.undersampling, test, type = "response")
logistic_pred_hiphop_under <- rep(0, 1000)
logistic_pred_hiphop_under[logistic_prop_hiphop_under > 0.5] <-1
hiphop_table_under <- table(logistic_pred_hiphop_under, test$hiphop)
hiphop_table_under <- hiphop_table_under[2:1, 2:1] #rearrange the confusion matrix to match the standar format
tpr_for_hiphop_under <- hiphop_table_under[1,1]/sum(hiphop_table_under[,1])
fpr_for_hiphop_under <- hiphop_table_under[1,2]/sum(hiphop_table_under[,2])
print(paste("The True Positive Rate for Hip-Hop music (with undersampling) is: ", tpr_for_hiphop_under))
print(paste("The False Positive Rate for Hip-Hop music (with undersampling) is: ", fpr_for_hiphop_under))

#Using undersampling to train a new model improves a lot the TPR for hip hop
#that goes from approx 0.35 to approx. 0.90, while affects the fpr that increases from
#approx. 0.02 to approx 0.25.

#####
###Now we try the oversample of the minority class just by 
#duplicating the elemnts belonging to the hiphop class.
other_class <- train[train$hiphop == 0, ]
balanced_data <- rbind(other_class, train[train$hiphop == 1, ], train[train$hiphop == 1, ])

best.glm.hiphop_with_oversampling <- glm(hiphop~ popularity+explicit+danceability+
                                            energy+loudness+mode+speechiness+acousticness+instrumentalness+
                                            valence+tempo+liveness,data=balanced_data, family = binomial)

#Checking if perfromances improved
logistic_prop_hiphop_over <- predict(best.glm.hiphop_with_oversampling, test, type = "response")
logistic_pred_hiphop_over <- rep(0, 1000)
logistic_pred_hiphop_over[logistic_prop_hiphop_over > 0.5] <-1
hiphop_table_over <- table(logistic_pred_hiphop_over, test$hiphop)
hiphop_table_over <- hiphop_table_over[2:1, 2:1] #rearrange the confusion matrix to match the standar format
tpr_for_hiphop_over <- hiphop_table_over[1,1]/sum(hiphop_table_over[,1])
fpr_for_hiphop_over <- hiphop_table_over[1,2]/sum(hiphop_table_over[,2])
print(paste("The True Positive Rate for Hip-Hop music (with oversampling) is: ", tpr_for_hiphop_over))
print(paste("The False Positive Rate for Hip-Hop music (with oversampling) is: ", fpr_for_hiphop_over))

#Oversampling the minority class improved slightly the TPR, 
#which became around 65%, but the improvement was not comparable
#with the undersampling.
#The proportion of elements of the two class is 2000 example for class 1
#and 4000 for the 0 class.

######
#Mixed approach(?)
other_class <- train[train$hiphop == 0, ]
subsample_size <- nrow(train[train$hiphop == 1, ]) # size of the 1 class
set.seed(123) # set a seed for reproducibility
other_class_subsample <- other_class[sample(nrow(other_class), subsample_size*2), ]
balanced_data <- rbind(other_class_subsample, train[train$hiphop == 1, ], train[train$hiphop == 1, ])

best.glm.hiphop_with_mixedsampling <- glm(hiphop~ popularity+explicit+danceability+
                                             energy+loudness+mode+speechiness+acousticness+instrumentalness+
                                             valence+tempo+liveness,data=balanced_data, family = binomial)

#Checking if perfromances improved
logistic_prop_hiphop_mixed <- predict(best.glm.hiphop_with_mixedsampling, test, type = "response")
logistic_pred_hiphop_mixed <- rep(0, 1000)
logistic_pred_hiphop_mixed[logistic_prop_hiphop_mixed  > 0.5] <-1
hiphop_table_mixed <- table(logistic_pred_hiphop_mixed, test$hiphop)
hiphop_table_mixed <- hiphop_table_mixed[2:1, 2:1] #rearrange the confusion matrix to match the standar format
tpr_for_hiphop_mixed <- hiphop_table_mixed[1,1]/sum(hiphop_table_mixed[,1])
fpr_for_hiphop_mixed <- hiphop_table_mixed[1,2]/sum(hiphop_table_mixed[,2])
print(paste("The True Positive Rate for Hip-Hop music (with mixed approach) is: ", tpr_for_hiphop_mixed))
print(paste("The False Positive Rate for Hip-Hop music (with mixed approach) is: ", fpr_for_hiphop_mixed))

#The mixed approach shows perfromace pretty similar to the undersampling
#so we decide to use undersampling also for other genres
#being it the most straightforward approach.

#####
#We now use undersample to build new models for each genres, 
#despite for Classical that already had an high TPR.

#Undersampling for reggaeton
other_class <- train[train$reggaeton == 0, ]
subsample_size <- nrow(train[train$reggaeton == 1, ]) # size of the 1 class
set.seed(123) # set a seed for reproducibility
other_class_subsample <- other_class[sample(nrow(other_class), subsample_size), ]
undersample.balanced.data.reggaeton <- rbind(other_class_subsample, train[train$reggaeton == 1, ])

best.glm.reggaeton.undersampling <- glm(reggaeton ~ popularity
                                        + duration_ms
                                        + explicit
                                        + danceability
                                        + loudness
                                        + mode
                                        + speechiness
                                        + acousticness
                                        + instrumentalness
                                        + valence
                                        + liveness
                                        + time_signature,
                                        data = undersample.balanced.data.reggaeton, 
                                        family = binomial)
#Checking if perfromances improved
logistic_prop_reggaeton_under <- predict(best.glm.reggaeton.undersampling, test, type = "response")
logistic_pred_reggaeton_under <- rep(0, 1000)
logistic_pred_reggaeton_under[logistic_prop_reggaeton_under > 0.5] <-1
reggaeton_table_under <- table(logistic_pred_reggaeton_under, test$reggaeton)
reggaeton_table_under <- reggaeton_table_under[2:1, 2:1] #rearrange the confusion matrix to match the standar format
tpr_for_reggaeton_under <- reggaeton_table_under[1,1]/sum(reggaeton_table_under[,1])
fpr_for_reggaeton_under <- reggaeton_table_under[1,2]/sum(reggaeton_table_under[,2])
print(paste("The True Positive Rate for Reggaeton music (with undersampling) is: ", tpr_for_reggaeton_under))
print(paste("The False Positive Rate for Reggaeton music (with undersampling) is: ", fpr_for_reggaeton_under))


#undersampling for rock and roll
other_class <- train[train$rock.n.roll == 0, ]
subsample_size <- nrow(train[train$rock.n.roll == 1, ]) # size of the 1 class
set.seed(123) # set a seed for reproducibility
other_class_subsample <- other_class[sample(nrow(other_class), subsample_size), ]
undersample.balanced.data.rock <- rbind(other_class_subsample, train[train$rock.n.roll == 1, ])

best.glm.rock.undersampling <- glm(rock.n.roll ~ popularity
                                    + duration_ms
                                    + explicit
                                    + danceability
                                    + energy
                                    + loudness
                                    + mode
                                    + speechiness
                                    + acousticness
                                    + instrumentalness
                                    + valence
                                    + liveness,
                                    data=undersample.balanced.data.rock, 
                                    family = binomial)
#Checking if perfromances improved
logistic_prop_rnr_under <- predict(best.glm.rock.undersampling, test, type = "response")
logistic_pred_rnr_under <- rep(0, 1000)
logistic_pred_rnr_under[logistic_prop_rnr_under > 0.5] <-1
rnr_table_under <- table(logistic_pred_rnr_under, test$rock.n.roll)
rnrrnr_table_under <- rnr_table_under[2:1, 2:1] 
tpr_for_rnr_under <- rnr_table_under[1,1]/sum(rnr_table_under[,1])
fpr_for_rnr_under <- rnr_table_under[1,2]/sum(rnr_table_under[,2])
print(paste("The True Positive Rate for rock and roll music (with undersampling) is: ", tpr_for_reggaeton_under))
print(paste("The False Positive Rate for rock and roll music (with undersampling) is: ", fpr_for_reggaeton_under))


#####undersampling for techno
other_class <- train[train$techno == 0, ]
subsample_size <- nrow(train[train$techno == 1, ]) # size of the 1 class
set.seed(123) # set a seed for reproducibility
other_class_subsample <- other_class[sample(nrow(other_class), subsample_size), ]
undersample.balanced.data.techno <- rbind(other_class_subsample, train[train$techno == 1, ])

best.glm.techno.undersampling <- glm(techno ~ popularity
                                          + duration_ms
                                          + explicit
                                          + danceability
                                          + energy
                                          + loudness
                                          + speechiness
                                          + acousticness
                                          + instrumentalness
                                          + valence
                                          + tempo,
                                          data=undersample.balanced.data.techno, 
                                          family = binomial)
#checking if perfromances improved
logistic_prob_techno_under <- predict(best.glm.techno.undersampling, test, type = "response")
logistic_pred_techno_under <- rep(0, 1000)
logistic_pred_techno_under[logistic_prob_techno_under > 0.5] <-1
techno_table_under <- table(logistic_pred_techno_under, test$techno)
techno_table_under <- techno_table_under[2:1, 2:1] 
tpr_for_techno_under <- techno_table_under[1,1]/sum(techno_table_under[,1])
fpr_for_techno_under <- techno_table_under[1,2]/sum(techno_table_under[,2])
print(paste("The True Positive Rate for Techno music (with undersampling) is: ", tpr_for_techno_under))
print(paste("The False Positive Rate for Techno music (with undersampling) is: ", fpr_for_techno_under))

############################################################

############################################################

###COMPARISON
#Now we want to get for each track in the test set a unique
#prediction for its genre. To do that we decided to compare
#the probabilities given by the classifiers and to pick the 
#larger one. In this way we classify the track on the basis 
#of the classifier that has the strongest confidence for it.

#####
#if regularization == 1 uses regularized model ridge/lasso according to alpha
compare <- function(regularization, alpha){ 
  if(regularization == 0){
    classical.prob <- predict(best.glm.classical, test, type = "response")
    techno.prob <- predict(best.glm.techno.undersampling, test, type = "response")
    rnr.prob <- predict(best.glm.rock.undersampling, test, type = "response")
    reggaeton.prob <- predict(best.glm.reggaeton.undersampling, test, type = "response")
    hiphop.prob <-predict(best.glm.hiphop.undersampling, test, type = "response")
  } else if (alpha == 0){
    classical.prob <- predict(classical.ridge.cv, X.classical.test,
                              s = best.lambda.ridge.classical,
                              type = "response")
    techno.prob <- predict(techno.ridge.cv, X.techno.test,
                           s = best.lambda.ridge.techno,
                           type = "response")
    rnr.prob <- predict(rock.ridge.cv, X.rock.test,
                        s = best.lambda.ridge.rock,
                        type = "response")
    reggaeton.prob <- predict(reggaeton.ridge.cv, X.reggaeton.test,
                              s = best.lambda.ridge.reggaeton,
                              type = "response")
    hiphop.prob <-predict(hiphop.ridge.cv, X.hiphop.test,
                          s = best.lambda.ridge.hiphop,
                          type = "response")
  } else{
    classical.prob <- predict(classical.lasso.cv, X.classical.test,
                              s = best.lambda.lasso.classical,
                              type = "response")
    techno.prob <- predict(techno.lasso.cv, X.techno.test,
                           s = best.lambda.lasso.techno,
                           type = "response")
    rnr.prob <- predict(rock.lasso.cv, X.rock.test,
                        s = best.lambda.lasso.rock,
                        type = "response")
    reggaeton.prob <- predict(reggaeton.lasso.cv, X.reggaeton.test,
                              s = best.lambda.lasso.reggaeton,
                              type = "response")
    hiphop.prob <-predict(hiphop.lasso.cv, X.hiphop.test,
                          s = best.lambda.lasso.hiphop,
                          type = "response")
  }
  # Create a matrix with the five vectors as 
  #levels(spotify_dummies$track_genre)
  #classical =  1
  #hiphop =     2  
  #reggaeton =  3
  #rocknroll =  4
  #techno =     5
  prob.matrix <- cbind(classical.prob, hiphop.prob, reggaeton.prob, rnr.prob, techno.prob)
  genres <- levels(test$track_genre)
  
  track_genre.pred <- rep(0, length(test))
  
  # Loop over the indices of the vectors
  for (i in 1:length(classical.prob)) {
    # Find the maximum value at this index across all vectors
    max_value <- max(prob.matrix[i, ])
    
    for (j in 1:ncol(prob.matrix)) {
      if (prob.matrix[i, j] == max_value) {
        track_genre.pred[i] <- genres[j]
      } 
    }
  }
  
  return(track_genre.pred)
}

track_genre.pred <- compare(0,0)

cm <- table(track_genre.pred, test$track_genre)

#####
###INTERPRETING THE RESULTS
#We proceed to analyze the performance of our method
#First we compute the overall accuracy
cm.accuracy <- sum(diag(cm))/sum(cm)

#Then for each class we compute precision, recall(TPR) and F1-score

#Classical
classical.precision <- cm[1,1]/sum(cm[1,])
classical.recall <- cm[1,1]/sum(cm[,1])
classical.f1 <- 2*((classical.precision * classical.recall)/(classical.precision + classical.recall))

#Hip-hop
hiphop.precision <- cm[2,2]/sum(cm[2,])
hiphop.recall <- cm[2,2]/sum(cm[,2])
hiphop.f1 <- 2*((hiphop.precision * hiphop.recall)/(hiphop.precision + hiphop.recall))

#Reggaeton
reggaeton.precision <- cm[3,3]/sum(cm[3,])
reggaeton.recall <- cm[3,3]/sum(cm[,3])
reggaeton.f1 <- 2*((reggaeton.precision * reggaeton.recall)/(reggaeton.precision + reggaeton.recall))

#Rock and roll
rnr.precision <- cm[4,4]/sum(cm[4,])
rnr.recall <- cm[4,4]/sum(cm[,4])
rnr.f1 <- 2*((rnr.precision * rnr.recall)/(rnr.precision + rnr.recall))

#Techno
techno.precision <- cm[5,5]/sum(cm[5,])
techno.recall <- cm[5,5]/sum(cm[,5])
techno.f1 <- 2*((techno.precision * techno.recall)/(techno.precision + techno.recall))

metrics <- data.frame(
  class <- c(levels(test$track_genre)),
  precision <- round(c(classical.precision, hiphop.precision, reggaeton.precision, rnr.precision, techno.precision), 2),
  recall <- round(c(classical.recall, hiphop.recall, reggaeton.recall, rnr.recall, techno.recall), 2),
  f1 <- round(c(classical.f1, hiphop.f1, reggaeton.f1, rnr.f1, techno.f1), 2)
)
colnames(metrics) <- c("Genre", "Precision", "Recall(TPR)", "F1-score")

#Here are the performance
cm
print(paste("The overall accuracy obtained by comparing all the classifiers is: ", round(cm.accuracy, 2)))
metrics #se possibile visualizzarlo in modo più bello

###COMMENTS
#The F1 score provides and overview of the quality of the  
#classification for each class. As we can see the highest 
#scores are obtained for classical, rock and roll and
#techno while reggaeton and hiphop obtain quite poor results.
#Looking at precision and recall allow to grasp more details
#on the behaviour of our method which changes between diffrent
#genres.
#Classic music has the highest score for precision meaning that 
#almost every time the methods classify a track as classical
#it does the right choice. Recall is a bit lower (0.82) and this
#indicate that our method is missing some classical tracks.
#The consideration are pretty simlar for techno music.
#For Rock and Roll instead the behaviour is the opposite:
#recall is high (0.94) meaning that our method is classifying
#correctly most rock tracks but at the same time, being precision
#low, it can be seen that it is overestimating the presence
#of rock in the test set. In other words, it is classifying 
#more tracks as rock than what is present in our data.
#Hihop and reggaeton shows very similar score in each metrics
#moreover, by looking at the cm, it is possible to see that 
#between those genres there is the greatest misclassification.
#There are a consisten number of hiphop track that are classified 
#as reggaeton and viceversa. (richiamare grafici)

#####
par(mfrow = c(1, 3))

classical.prob <- predict(best.glm.classical, test, type = "response")
classical.pred.linear <- predict(best.glm.classical, test)
plot(classical.pred.linear, classical.prob, main = "Classical")
rnr.prob <- predict(best.glm.rock.undersampling, test, type = "response")
rnr.pred.linear <- predict(best.glm.rock.undersampling, test)
plot(rnr.pred.linear, rnr.prob, main = "Rock and Roll")

techno.prob <- predict(best.glm.techno.undersampling, test, type = "response")
techno.pred.linear <- predict(best.glm.techno.undersampling, test)
plot(techno.pred.linear, techno.prob, main = "Techno")

par(mfrow = c(1, 2))

hiphop.prob <-predict(best.glm.hiphop.undersampling, test, type = "response")
hiphop.pred.linear <- predict(best.glm.hiphop.undersampling, test)
plot(hiphop.pred.linear, hiphop.prob, main = "Hip-Hop")

reggaeton.prob <- predict(best.glm.reggaeton.undersampling, test, type = "response")
reggaeton.pred.linear <- predict(best.glm.reggaeton.undersampling, test)
plot(reggaeton.pred.linear, reggaeton.prob, main = "Reggaeton")

#####

###COMMENTS ON THE GRAPHS
#We proceeded to plot the probabilities of the predictions 
#given by each classifiers to try to understand the results 
#we got by our method. The graphs show that the genres with 
#the higher F1-scores has lot of points (predictions) 
#associated with high probabilities and also a few points 
#concentrated around the decision boundary. This could be 
#interpreted as a strong confidence of the classifier when
#making predictions.
#For hiphop and reggaeton, which have low values for the F1-score,
#the graphs are clearly different. It can be seen how there 
#are fewer points associated with high probabilities and there is 
#a great concenration of points on the middle of the curve.
#This can be seen somehow as a greater uncertainity of the classifiers
#with respect to the ones for Classical, Techno and Rock. 
#Thus, even if the classifiers by them self shows good result 
#when it is compared with the others to decide which genre
#pick among all it is more likely that there is a misclassification,
#especially between hiphop and reggaeton.

############################################################
#####RIDGE AND LASSO LOGISTIC REGRESSION
#We now want to compare whether ridge or lasso regularization
#could yeld to better models than the ones we obtained through 
#variable selections. To do so we will train a lasso and ridge
#regularized version of the logistic models we built 
#previously. For each genres we will use the same training 
#data we used in the first part, in the sense that, besides 
#for classical, we will use the balanced data used in the 
#undersampling approach. This is done to allow for a 
#fair comparison of the logistics models.

#RIDGE
#####

#Classical

#Preparing the data to be used with glmnet functions
train.classical <- train[,1:17]
test.classical <- test[,1:17]
train.classical <- train.classical[,-16]
test.classical <- test.classical[,-16]
X.classical.train <- model.matrix(classical ~ ., data = train.classical)
X.classical.test <- model.matrix(classical ~ ., data = test.classical)
X.classical.train <- X.classical.train[,-1]
X.classical.test <- X.classical.test[,-1]

#performing cv
classical.ridge.cv <- cv.glmnet(X.classical.train, train$classical, 
                                alpha = 0, family = "binomial", type.measure = "class")

plot(classical.ridge.cv)
best.lambda.ridge.classical <- classical.ridge.cv$lambda.min

#prediction 
ridge.prediction.classical <- predict(classical.ridge.cv, X.classical.test,
                                      s = best.lambda.ridge.classical,
                                      type = "class")
table(test$classical, ridge.prediction.classical)

#Techno

#Preparing the data
train.techno <- undersample.balanced.data.techno[,1:15] 
train.techno$techno <- undersample.balanced.data.techno$techno
test.techno <- test[,1:15] 
test.techno$techno <- test$techno
X.techno.train <- model.matrix(techno ~ ., data = train.techno)
X.techno.test <- model.matrix(techno ~ ., data = test.techno)
X.techno.train <- X.techno.train[,-1]
X.techno.test <- X.techno.test[,-1]

#performing cv
techno.ridge.cv <- cv.glmnet(X.techno.train, undersample.balanced.data.techno$techno, 
                             alpha = 0, family = "binomial", type.measure = "class")

plot(techno.ridge.cv)
best.lambda.ridge.techno <- techno.ridge.cv$lambda.min

#prediction 
ridge.prediction.techno <- predict(techno.ridge.cv, X.techno.test,
                                   s = best.lambda.ridge.techno,
                                   type = "class")
table(test$techno, ridge.prediction.techno)

#Hiphop

#Preparing the data
train.hiphop <- undersample.balanced.data.hiphop[,1:15] 
train.hiphop$hiphop <- undersample.balanced.data.hiphop$hiphop
test.hiphop <- test[,1:15] 
test.hiphop$hiphop <- test$hiphop
X.hiphop.train <- model.matrix(hiphop ~ ., data = train.hiphop)
X.hiphop.test <- model.matrix(hiphop ~ ., data = test.hiphop)
X.hiphop.train <- X.hiphop.train[,-1]
X.hiphop.test <- X.hiphop.test[,-1]

#performing cv
hiphop.ridge.cv <- cv.glmnet(X.hiphop.train, undersample.balanced.data.hiphop$hiphop, 
                             alpha = 0, family = "binomial", type.measure = "class")

plot(hiphop.ridge.cv)
best.lambda.ridge.hiphop <- hiphop.ridge.cv$lambda.min

#prediction 
ridge.prediction.hiphop <- predict(hiphop.ridge.cv, X.hiphop.test,
                                   s = best.lambda.ridge.hiphop,
                                   type = "class")
table(test$hiphop, ridge.prediction.hiphop)

#Reggaeton

#Preparing the data
train.reggaeton <- undersample.balanced.data.reggaeton[,1:15] 
train.reggaeton$reggaeton <- undersample.balanced.data.reggaeton$reggaeton
test.reggaeton <- test[,1:15] 
test.reggaeton$reggaeton <- test$reggaeton
X.reggaeton.train <- model.matrix(reggaeton ~ ., data = train.reggaeton)
X.reggaeton.test <- model.matrix(reggaeton ~ ., data = test.reggaeton)
X.reggaeton.train <- X.reggaeton.train[,-1]
X.reggaeton.test <- X.reggaeton.test[,-1]

#performing cv
reggaeton.ridge.cv <- cv.glmnet(X.reggaeton.train, undersample.balanced.data.reggaeton$reggaeton, 
                                alpha = 0, family = "binomial", type.measure = "class")

plot(reggaeton.ridge.cv)
best.lambda.ridge.reggaeton <- reggaeton.ridge.cv$lambda.min

#prediction 
ridge.prediction.reggaeton <- predict(reggaeton.ridge.cv, X.reggaeton.test,
                                      s = best.lambda.ridge.reggaeton,
                                      type = "class")
table(test$reggaeton, ridge.prediction.reggaeton)

#Rock

#Preparing the data
train.rock <- undersample.balanced.data.rock[,1:15] 
train.rock$rock <- undersample.balanced.data.rock$rock.n.roll
test.rock <- test[,1:15] 
test.rock$rock <- test$rock.n.roll
X.rock.train <- model.matrix(rock ~ ., data = train.rock)
X.rock.test <- model.matrix(rock ~ ., data = test.rock)
X.rock.train <- X.rock.train[,-1]
X.rock.test <- X.rock.test[,-1]

#performing cv
rock.ridge.cv <- cv.glmnet(X.rock.train, undersample.balanced.data.rock$rock, 
                           alpha = 0, family = "binomial", type.measure = "class")

plot(rock.ridge.cv)
best.lambda.ridge.rock <- rock.ridge.cv$lambda.min

#prediction 
ridge.prediction.rock <- predict(rock.ridge.cv, X.rock.test,
                                 s = best.lambda.ridge.rock,
                                 type = "class")
table(test$rock.n.roll, ridge.prediction.rock)

#Comparing the predictions

track_genre.pred.ridge <- compare(1,0)

cm.ridge <- table(track_genre.pred.ridge, test$track_genre)

#compute the metrics

#LASSO
#####
#Data has already been pre-processed for ridge regression

#Classical 

#performing cv
classical.lasso.cv <- cv.glmnet(X.classical.train, train$classical, 
                                alpha = 1, family = "binomial", type.measure = "class")

plot(classical.lasso.cv)
best.lambda.lasso.classical <- classical.lasso.cv$lambda.min

#prediction 
lasso.prediction.classical <- predict(classical.lasso.cv, X.classical.test,
                                      s = best.lambda.lasso.classical,
                                      type = "class")
table(test$classical, lasso.prediction.classical)

#Techno

#performing cv
techno.lasso.cv <- cv.glmnet(X.techno.train, undersample.balanced.data.techno$techno, 
                             alpha = 1, family = "binomial", type.measure = "class")

plot(techno.lasso.cv)
best.lambda.lasso.techno <- techno.lasso.cv$lambda.min

#prediction 
lasso.prediction.techno <- predict(techno.lasso.cv, X.techno.test,
                                   s = best.lambda.lasso.techno,
                                   type = "class")
table(test$techno, lasso.prediction.techno)

#Hiphop

#performing cv
hiphop.lasso.cv <- cv.glmnet(X.hiphop.train, undersample.balanced.data.hiphop$hiphop, 
                             alpha = 1, family = "binomial", type.measure = "class")

plot(hiphop.lasso.cv)
best.lambda.lasso.hiphop <- hiphop.lasso.cv$lambda.min

#prediction 
lasso.prediction.hiphop <- predict(hiphop.lasso.cv, X.hiphop.test,
                                   s = best.lambda.lasso.hiphop,
                                   type = "class")
table(test$hiphop, lasso.prediction.hiphop)

#Reggaeton

#performing cv
reggaeton.lasso.cv <- cv.glmnet(X.reggaeton.train, undersample.balanced.data.reggaeton$reggaeton, 
                                alpha = 1, family = "binomial", type.measure = "class")

plot(reggaeton.lasso.cv)
best.lambda.lasso.reggaeton <- reggaeton.lasso.cv$lambda.min

#prediction 
lasso.prediction.reggaeton <- predict(reggaeton.lasso.cv, X.reggaeton.test,
                                      s = best.lambda.lasso.reggaeton,
                                      type = "class")
table(test$reggaeton, lasso.prediction.reggaeton)

#Rock

#performing cv
rock.lasso.cv <- cv.glmnet(X.rock.train, undersample.balanced.data.rock$rock, 
                           alpha = 1, family = "binomial", type.measure = "class")

plot(rock.lasso.cv)
best.lambda.lasso.rock <- rock.lasso.cv$lambda.min

#prediction 
lasso.prediction.rock <- predict(rock.lasso.cv, X.rock.test,
                                 s = best.lambda.lasso.rock,
                                 type = "class")
table(test$rock.n.roll, lasso.prediction.rock)

#Comparing the predictions

track_genre.pred.lasso <- compare(1,1)

cm.lasso <- table(track_genre.pred.lasso, test$track_genre)

#compute the metrics
############################################################
#####DISCRIMINANT ANALYSIS 
#1. SW TEST FOR NORMALITY
#Both lda and qda assume that the features are normally ditributed 
#in each target class. In ordere to chec whether this assumption is 
#satisfied by our training data we perfrom the Shapiro - Wilks test.

#We define the subset of features we want to test
subset.features <- c("popularity", "duration_ms", "danceability", 
                     "energy", "loudness", "speechiness", 
                     "acousticness", "instrumentalness", "liveness", 
                     "valence", "tempo") 
#Classical
classical.data <- train[train$track_genre == "classical", subset.features]
shapiro.results <- lapply(classical.data, shapiro.test)
p.values <- sapply(shapiro.results, function(x) x$p.value)
print(p.values)

#Techno
techno.data <- train[train$track_genre == "techno", subset.features]
shapiro.results <- lapply(techno.data, shapiro.test)
p.values <- sapply(shapiro.results, function(x) x$p.value)
print(p.values)

#Hiphop
hiphop.data <- train[train$track_genre == "hip-hop", subset.features]
shapiro.results <- lapply(hiphop.data, shapiro.test)
p.values <- sapply(shapiro.results, function(x) x$p.value)
print(p.values)

#Rock
rock.data <- train[train$track_genre == "rock-n-roll", subset.features]
shapiro.results <- lapply(rock.data, shapiro.test)
p.values <- sapply(shapiro.results, function(x) x$p.value)
print(p.values)

#Reggaeton
reggaeton.data <- train[train$track_genre == "reggaeton", subset.features]
shapiro.results <- lapply(reggaeton.data, shapiro.test)
p.values <- sapply(shapiro.results, function(x) x$p.value)
print(p.values)

#2. LDA 

lda.fit <- lda(track_genre ~ popularity + duration_ms + danceability + 
                 energy + loudness + speechiness + 
                 acousticness + instrumentalness + liveness + 
                 valence + tempo, data = train)
lda.fit 
lda.predictions <- predict(lda.fit, test)
ldahist(data = lda.predictions$x[,1], g = train$track_genre)
cm.lda <- table(lda.predictions$class, test$track_genre)

#2.1 Compute metrics for cm lda
cm.lda.accuracy <- sum(diag(cm.lda))/sum(cm.lda)

#Classical
lda.classical.precision <- cm.lda[1,1]/sum(cm.lda[1,])
lda.classical.recall <- cm.lda[1,1]/sum(cm.lda[,1])
lda.classical.f1 <- 2*((lda.classical.precision * lda.classical.recall)/(lda.classical.precision + lda.classical.recall))

#Hip-hop
lda.hiphop.precision <- cm.lda[2,2]/sum(cm.lda[2,])
lda.hiphop.recall <- cm.lda[2,2]/sum(cm.lda[,2])
lda.hiphop.f1 <- 2*((lda.hiphop.precision * lda.hiphop.recall)/(lda.hiphop.precision + lda.hiphop.recall))

#Reggaeton
lda.reggaeton.precision <- cm.lda[3,3]/sum(cm.lda[3,])
lda.reggaeton.recall <- cm.lda[3,3]/sum(cm.lda[,3])
lda.reggaeton.f1 <- 2*((lda.reggaeton.precision * lda.reggaeton.recall)/(lda.reggaeton.precision + lda.reggaeton.recall))

#Rock and roll
lda.rnr.precision <- cm.lda[4,4]/sum(cm.lda[4,])
lda.rnr.recall <- cm.lda[4,4]/sum(cm.lda[,4])
lda.rnr.f1 <- 2*((lda.rnr.precision * lda.rnr.recall)/(lda.rnr.precision + lda.rnr.recall))

#Techno
lda.techno.precision <- cm.lda[5,5]/sum(cm.lda[5,])
lda.techno.recall <- cm.lda[5,5]/sum(cm.lda[,5])
lda.techno.f1 <- 2*((lda.techno.precision * lda.techno.recall)/(lda.techno.precision + lda.techno.recall))

lda.metrics <- data.frame(
  class <- c(levels(test$track_genre)),
  precision <- round(c(lda.classical.precision, lda.hiphop.precision, lda.reggaeton.precision, lda.rnr.precision, lda.techno.precision), 2),
  recall <- round(c(lda.classical.recall, lda.hiphop.recall, lda.reggaeton.recall, lda.rnr.recall, lda.techno.recall), 2),
  f1 <- round(c(lda.classical.f1, lda.hiphop.f1, lda.reggaeton.f1, lda.rnr.f1, lda.techno.f1), 2)
)
colnames(lda.metrics) <- c("Genre", "Precision", "Recall(TPR)", "F1-score")

#Here are the performance
cm.lda
print(paste("The accuracy of the LDA is: ", round(cm.lda.accuracy, 2)))
lda.metrics

#3. QDA
qda.fit <- qda(track_genre ~ popularity + duration_ms + danceability + 
                 energy + loudness + speechiness + 
                 acousticness + instrumentalness + liveness + 
                 valence + tempo, data = train)
qda.fit
qda.predictions <- predict(qda.fit, test)
cm.qda <- table(qda.predictions$class, test$track_genre)

#3.1 Compute metrics
cm.qda.accuracy <- sum(diag(cm.qda))/sum(cm.qda)

#Classical
qda.classical.precision <- cm.qda[1,1]/sum(cm.qda[1,])
qda.classical.recall <- cm.qda[1,1]/sum(cm.qda[,1])
qda.classical.f1 <- 2*((qda.classical.precision * qda.classical.recall)/(qda.classical.precision + qda.classical.recall))

#Hip-hop
qda.hiphop.precision <- cm.qda[2,2]/sum(cm.qda[2,])
qda.hiphop.recall <- cm.qda[2,2]/sum(cm.qda[,2])
qda.hiphop.f1 <- 2*((qda.hiphop.precision * qda.hiphop.recall)/(qda.hiphop.precision + qda.hiphop.recall))

#Reggaeton
qda.reggaeton.precision <- cm.qda[3,3]/sum(cm.qda[3,])
qda.reggaeton.recall <- cm.qda[3,3]/sum(cm.qda[,3])
qda.reggaeton.f1 <- 2*((qda.reggaeton.precision * qda.reggaeton.recall)/(qda.reggaeton.precision + qda.reggaeton.recall))

#Rock and roll
qda.rnr.precision <- cm.qda[4,4]/sum(cm.qda[4,])
qda.rnr.recall <- cm.qda[4,4]/sum(cm.qda[,4])
qda.rnr.f1 <- 2*((qda.rnr.precision * qda.rnr.recall)/(qda.rnr.precision + qda.rnr.recall))

#Techno
qda.techno.precision <- cm.qda[5,5]/sum(cm.qda[5,])
qda.techno.recall <- cm.qda[5,5]/sum(cm.qda[,5])
qda.techno.f1 <- 2*((qda.techno.precision * qda.techno.recall)/(qda.techno.precision + qda.techno.recall))

qda.metrics <- data.frame(
  class <- c(levels(test$track_genre)),
  precision <- round(c(qda.classical.precision, qda.hiphop.precision, qda.reggaeton.precision, qda.rnr.precision, qda.techno.precision), 2),
  recall <- round(c(qda.classical.recall, qda.hiphop.recall, qda.reggaeton.recall, qda.rnr.recall, qda.techno.recall), 2),
  f1 <- round(c(qda.classical.f1, qda.hiphop.f1, qda.reggaeton.f1, qda.rnr.f1, qda.techno.f1), 2)
)
colnames(qda.metrics) <- c("Genre", "Precision", "Recall(TPR)", "F1-score")

#Here are the performance
cm.qda
print(paste("The accuracy of the QDA is: ", round(cm.qda.accuracy, 2)))
qda.metrics

#####
#Naive bayes classifier
nb.fit <- naiveBayes(track_genre ~ ., data = train[,1:16])
nb.fit
nb.class <- predict(nb.fit, test[,1:16])
table(nb.class, test$track_genre)
#####

