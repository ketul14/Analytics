#--------------------------------
# Set Working Directory
#--------------------------------
setwd("~/Desktop/SDSU/MSIS/Fall 2018/MIS 620 Big Data/Project")

py_dir <- ("~/PycharmProject/Airbnb")
#--------------------------------
# Install Required Packages
#--------------------------------
#install.packages("chron")
#install.packages("sqldf")
#install.packages("corrgram")
#install.packages("ggplot2")
#install.packages("scales")
#install.packages("ISLR")
#install.packages("caret")
#install.packages("mice")
#install.packages("dplyr")
#install.packages("maps")
#install.packages("caTools")
#install.packages("broom")
#install.packages("recipes")
#install.packages("naniar")
#install.packages("VIM")
#install.packages("ggmap")
#install.packages("devtools")
#install.packages("googleway")
#install.packages("ggmap")
#install.packages("syuzhet")
#install.packages("stringr")
#install.packages("plotly")
#install.packages("doParallel")
#install.packages("wordcloud")
#install.packages("tm")

library(ISLR)
library(caret)
library(mice)
library(chron)
library(sqldf)
library(corrgram)
library(ggplot2)
library(scales)
library(dplyr)
library(plyr)
library(maps)
library(caTools)
library(naniar)
library(VIM)
library(devtools)
#devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
library(syuzhet)
library(stringr)
library(doParallel)
library(plotly)
library(rpart)
library(wordcloud)
library(tm)

#--------------------------------
# Step 1: Loading Airbnb Data
#--------------------------------
# Listings data
Listings = read.csv("listings.csv")
# Reviews data
Reviews = read.csv("reviews.csv")

#str(Calender)
str(Reviews)
str(Listings)

#Date Conversion
Reviews$date <- as.Date(Reviews$date)
Listings$host_since <- as.Date(Listings$host_since)
#Merge Dataframe
Airbnb <- merge(Reviews, Listings, by.x = "listing_id", by.y = "id")

#--------------------------------
# Step 2: Data Exploration 
#--------------------------------
# Dimenstions of data
dim(Airbnb)

# Geomap
library(googleway)
set_key("AIzaSyA_t7oAm06iFVep1MHvfDOVejhd8_MkEr4")
lat <- Airbnb$latitude #Seattle lat boundaries
lon <- Airbnb$longitude #Seatlle long boundaries
center = c(mean(lat), mean(lon))
points <- Listings[, c("longitude","latitude")]
newmap <- google_map(location = center, zoom = 12, data = points)
add_circles(newmap)



#Remove columns 
Airbnb[, c('reviewer_id', 'reviewer_name')] <- list(NULL)
# Removed all URL's
Airbnb[, c("listing_url", "thumbnail_url", "medium_url", "picture_url", "xl_picture_url", "host_url",
        "host_picture_url", "host_thumbnail_url")] <- list(NULL)
# Removed all Non relevant columns
Airbnb[, c("name", "summary", "description", "host_name", "host_neighbourhood", "host_verifications",
        "neighbourhood_cleansed", "amenities", "first_review", "last_review"
        )] <- list(NULL)

#Remove columns with Unique value
Airbnb[, c("scrape_id", "last_scraped", "experiences_offered", "city", "state", 
        "zipcode", "market", "smart_location", "country_code", "country",
        "calendar_last_scraped", "jurisdiction_names")] <- list(NULL)

# Remove columns
Airbnb[, c('space', "neighborhood_overview", "transit", "neighbourhood",
        "host_acceptance_rate", "host_about", "weekly_price",
        "monthly_price")] <- list(NULL)
# Remove blank columns 
Airbnb[, c("license")] <- list(NULL)
str(Airbnb)

# Study unique values of ID
unique(Airbnb$host_response_time)
unique(Airbnb$host_acceptance_rate)

#Find NA values in dataframe
colSums(is.na(Airbnb))

# Replate all N/A, NA, N A likewise string to NA
#Airbnb <- Airbnb %>% replace_with_na_all(condition = ~. %in% common_na_strings)
Airbnb <- Airbnb %>%
  replace_with_na(replace = list(comments = c('N/A', "", "na"),
                                 host_response_time = c('N/A', "", "na"),
                                 host_response_rate = c('N/A', "", "na"),
                                 host_is_superhost = c('N/A', "", "na"),
                                 host_listings_count = c('N/A', "", "na"),
                                 bathrooms = c('N/A', "", "na"),
                                 bedrooms = c('N/A', "", "na"),
                                 security_deposit = c('N/A', "", "na"),
                                 cleaning_fee = c('N/A', "", "na")
                                 ))

# Find columns with more than 80 NA
na <- (colSums(is.na(Airbnb))/nrow(Airbnb))*100
#df <- cbind(read.table(text = names(d)), d)
n <- na[na > 0]
colSums(is.na(Airbnb))

# Plot NAs
nn <- barplot(n, ylab = "% of NA")
text(x=nn, y=n, labels = n, pos = 3, cex = 0.8, col = "black")

# Remove column more than 80%
Airbnb[, c("square_feet")] <- list(NULL)
Airbnb[,c("notes")] <- list(NULL)
Airbnb[,c("host_since")] <- list(NULL)
colSums(is.na(Airbnb))

#Convert Price money column to numeric
Airbnb$price <- as.numeric(gsub('[$,]', '', Airbnb$price))
Airbnb$security_deposit <- as.numeric(gsub('[$,]', '', Airbnb$security_deposit))
Airbnb$guests_included <- as.numeric(gsub('[$,]', '', Airbnb$guests_included))
Airbnb$cleaning_fee <- as.numeric(gsub('[$,]', '', Airbnb$cleaning_fee))
Airbnb$extra_people <- as.numeric(gsub('[$,]', '', Airbnb$extra_people))


#-----------------------------
# Step 4: Data  Cleaning
#-----------------------------

# Missing value and impuatation
md.pattern(Airbnb)
colSums(is.na(Airbnb))

# Mice imputation
mice_plot <- aggr(Airbnb, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Airbnb), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


imp.col <- c("review_scores_rating", "review_scores_accuracy", "review_scores_cleanliness",
             "review_scores_checkin", "review_scores_communication",
             "review_scores_location", "review_scores_value", 
             "host_response_time", "host_response_rate", "bathrooms", "bedrooms",
             "security_deposit", "cleaning_fee")

Airbnb.i <- mice(Airbnb[, imp.col], m=5, maxit = 1, method = 'pmm', seed = 500)
Airbnb.i.df <- complete(Airbnb.i, 1)
Airbnb <- Airbnb[, !(names(Airbnb) %in% imp.col)]
Airbnb <- cbind(Airbnb, Airbnb.i.df)

colSums(is.na(Airbnb))
Airbnb <-Airbnb[is.na(Airbnb$comments) == FALSE,]

mice_plot <- aggr(Airbnb, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Airbnb), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

pairs(Airbnb[, c("price", "bedrooms", "bathrooms", "security_deposit",
                 "cleaning_fee", "availability_365", "accommodates",
                 "accommodates", "room_type",
                 "guests_included", "number_of_reviews",
                 "reviews_per_month", "extra_people", 
                 "host_listings_count", "availability_90", "beds", 
                 "review_scores_rating")])


#--------------------------------------------
# Step 5: Sentiment Analysis of Reviews
#--------------------------------------------

# Sentiment on comments using Python
command <-"/Users/ketulpatel/PycharmProjects/Airbnb/venv/bin/python"
py_sentiment <- "/Users/ketulpatel/PycharmProjects/Airbnb/AirbnbSentiment.py"
Sentiment <- system2(command = command, py_sentiment, stdout = TRUE)

Review.sentiment <- read.csv("reviews_sentiment.csv")
Review.sentiment[, c("listing_id", "date", "reviewer_id", "reviewer_name")] <- NULL
Airbnb <- merge(Airbnb, Review.sentiment, by.x = "id", by.y = "id")

# Sentiment (Emotions) of reviews using R
Reviews.review <- Reviews
# to lowercase
Reviews.review$clean_comments <- tolower(Reviews.review$comments)

# Remove &amp
Reviews.review$clean_comments = gsub("&amp", "", Reviews.review$clean_comments)
# Remove names RT |
Reviews.review$clean_comments = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", Reviews.review$clean_comments)
# Remove @person
Reviews.review$clean_comments = gsub("@\\w+", "", Reviews.review$clean_comments)
# Remove punctuations
Reviews.review$clean_comments = gsub("[[:punct:]]", "", Reviews.review$clean_comments)
# Remove numbers
Reviews.review$clean_comments = gsub("[[:digit:]]", "", Reviews.review$clean_comments)
# Remove URLs http
Reviews.review$clean_comments = gsub("http\\w+", "", Reviews.review$clean_comments)
# Remove tabs
Reviews.review$clean_comments = gsub("[ \t]{2,}", "", Reviews.review$clean_comments)
# Remove other \n \t \s
Reviews.review$clean_comments = gsub("^\\s+|\\s+$", "", Reviews.review$clean_comments)

# Calculate sentiment score
Reviews.review$senti_score <- get_nrc_sentiment((Reviews.review$clean_comments))
Reviews.review <- Reviews.review[!Reviews.review$clean_comments == "",]

# Visualize the emotions from NRC sentiments
senti_bar = colSums(Reviews.review$senti_score)
senti_sum = data.frame(count = senti_bar, senti = names(senti_bar))
senti_sum$senti = factor(senti_sum$senti, levels=senti_sum$senti[order(senti_sum$count, decreasing = TRUE)])
library(plotly)
plot_ly(senti_sum, x=~senti, y=~count, type="bar", color=~senti) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion of Reviews")

# Create comparison word cloud data

wordcloud_airbnb = c(
  paste(Reviews.review$clean_comments[Reviews.review$senti_score$anger > 0], collapse=" "),
  paste(Reviews.review$clean_comments[Reviews.review$senti_score$anticipation > 0], collapse=" "),
  paste(Reviews.review$clean_comments[Reviews.review$senti_score$disgust > 0], collapse=" "),
  paste(Reviews.review$clean_comments[Reviews.review$senti_score$fear > 0], collapse=" "),
  paste(Reviews.review$clean_comments[Reviews.review$senti_score$joy > 0], collapse=" "),
  paste(Reviews.review$clean_comments[Reviews.review$senti_score$sadness > 0], collapse=" "),
  paste(Reviews.review$clean_comments[Reviews.review$senti_score$surprise > 0], collapse=" "),
  paste(Reviews.review$clean_comments[Reviews.review$senti_score$trust > 0], collapse=" ")
)

# create corpus
corpus = Corpus(VectorSource(wordcloud_airbnb))

# remove punctuation, convert every word in lower case and remove stop words
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
corpus = tm_map(corpus, stemDocument)

# create document term matrix
tdm = TermDocumentMatrix(
    corpus,
    control = list(
    wordLengths=c(0,Inf),
    removePunctuation = TRUE,
    stopwords = c("prayformh370", "prayformh", stopwords("english")),
    removeNumbers = TRUE, tolower = TRUE) )

# convert as matrix
tdm = as.matrix(tdm)
tdmnew <- tdm[nchar(rownames(tdm)) < 11,]
tm_map(corpus, removePunctuation)
# column name binding
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdmnew) <- colnames(tdm)
comparison.cloud(tdmnew, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4), rot.per=0.4)

str(corpus)
# Explore outlier of price
boxplot(Airbnb$price ~ Airbnb$bedrooms, names(Airbnb$bedrooms))
bwplot(Airbnb$price ~ Airbnb$room_type)

# Final remove columns
Airbnb[, c("comments", "id", "cleansed_comments", "listing_id", "date", "host_id",
        "host_location", "street", "Sentiment", "X")] <- NULL


#Correlation
corrgram(Airbnb)
# detailed correlation matrix
corrgram(Airbnb, lower.panel=panel.shade, upper.panel=panel.pie)

# Ser parallel run
library(doParallel)
cl <- makePSOCKcluster(3)
clusterSetRNGStream(cl, 99) #set seed for everymember of cluster
registerDoParallel(cl)


#---------------------------
# Step 6: Model Building
#---------------------------

# temp delete neighborhood delete
Airbnb[, c("neighbourhood_cleansed", "requires_license", "has_availability")] <- NULL
Airbnb$host_response_rate <- as.numeric(Airbnb$host_response_rate)
str(Airbnb)

saveRDS(Airbnb, "Airbnb.rds")
# Encode categorical values
Airbnb.dmy <- dummyVars( "~ ." , Airbnb, fullRank = T)
Airbnb.d <- data.frame(predict(Airbnb.dmy, newdata = Airbnb))
head(Airbnb.d)
str(Airbnb.d)

saveRDS(Airbnb.d, "Airbnb_dmy.rds")

Airbnb.d <- readRDS("Airbnb_dmy.rds")
#####Data splitting for numerical 
set.seed(99)
trainIndex.d <- createDataPartition(Airbnb.d$price, p=.7, list=F)
nrow(trainIndex.d)

Airbnb.d[, c("host_response_time.N.A")] <-  NULL
Airbnb.train.d <- Airbnb.d[trainIndex.d,]
Airbnb.test.d <- Airbnb.d[-trainIndex.d,]
str(Airbnb.train.d)
str(Airbnb.test.d)
nrow(Airbnb.test.d)
colnames(Airbnb.train.d)
colnames(Airbnb.test.d)


## Create test data split for train
y.Airbnb.train.d <- Airbnb.train.d[, "price"]
x.Airbnb.train.d <- Airbnb.train.d[, -52]
head(y.Airbnb.train.d)
colnames(y.Airbnb.train.d)


## Create test data split for test
y.Airbnb.test.d <- Airbnb.test.d[, 52]
x.Airbnb.test.d <- Airbnb.test.d[,-52]
head(y.Airbnb.test.d)
colnames(Airbnb.d)

#some parameters to control the sampling during parameter tuning and testing
#10 fold crossvalidation
set.seed(99)
ctrl <- trainControl(method="cv", number=50,
                     allowParallel =  TRUE) 

### Find the function of RMSE###
#rmse function to calculate training error on residuals
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# A: LINEAR REGRESSION MODELS
#------------------------------
m.Airbnb.d.lm <-  train(y=y.Airbnb.train.d, x=x.Airbnb.train.d,
                     method = "lm",
                     trControl = ctrl)
m.Airbnb.d.lm
saveRDS(m.Airbnb.d.lm, "m.Airbnb.d.lm.rds")
m.Airbnb.d.lm <- readRDS("m.Airbnb.d.lm.rds")
r.Airbnb.d.train.lm <-getTrainPerf(m.Airbnb.d.lm)
r.Airbnb.d.train.lm
summary(r.Airbnb.d.train.lm)
rmse(resid(m.Airbnb.d.lm))
plot(density(resid(m.Airbnb.d.lm))) #A density plot
qqnorm(resid(m.Airbnb.d.lm)) # A quantile normal plot - good for checking normality
qqline(resid(m.Airbnb.d.lm))
p.Airbnb.d.lm <- predict(m.Airbnb.d.lm, newdata = Airbnb.test.d)
r.Airbnb.d.test.lm <- postResample(p.Airbnb.d.lm, Airbnb.d$price)
r.Airbnb.d.test.lm
# VarImp
varImp(m.Airbnb.d.lm)

# 1. RIDGE Regression
#------------------------
#install.packages("elasticnet")
library(elasticnet)

m.Airbnb.d.ridge <-  train(y=y.Airbnb.train.d, x=x.Airbnb.train.d,
                     method = "ridge",
                     trControl = ctrl,
                     tuneLength=10)

saveRDS(m.Airbnb.d.ridge, "m.Airbnb.d.ridge.rds")
m.Airbnb.d.ridge <- readRDS("m.Airbnb.d.ridge.rds")
m.Airbnb.d.ridge
r.Airbnb.d.train.ridge <-getTrainPerf(m.Airbnb.d.ridge)
r.Airbnb.d.train.ridge
summary(r.Airbnb.d.train.ridge)
rmse(resid(m.Airbnb.d.ridge))
plot(density(resid(m.Airbnb.d.ridge))) #A density plot
qqnorm(resid(m.Airbnb.d.ridge)) # A quantile normal plot - good for checking normality
qqline(resid(m.Airbnb.d.ridge))
p.Airbnb.d.ridge <- predict(m.Airbnb.d.ridge, newdata = Airbnb.test.d)
r.Airbnb.d.test.ridge <- postResample(p.Airbnb.d.ridge, Airbnb.d$price)
r.Airbnb.d.test.ridge
# Variable importance
varImp(m.Airbnb.d.ridge)


# 2. LASSO Regression
#-----------------------
m.Airbnb.d.lasso <-  train(y=y.Airbnb.train.d, x=x.Airbnb.train.d,
                        method = "lasso",
                        trControl = ctrl,
                        tuneLength=10)
saveRDS(m.Airbnb.d.lasso, "m.Airbnb.d.lasso.rds")
m.Airbnb.d.lasso <- readRDS("m.Airbnb.d.lasso.rds")
m.Airbnb.d.lasso
r.Airbnb.d.train.lasso <-getTrainPerf(m.Airbnb.d.ridge)
r.Airbnb.d.train.lasso
summary(r.Airbnb.d.train.lasso)
rmse(resid(m.Airbnb.d.lasso))
plot(density(resid(m.Airbnb.d.lasso))) #A density plot
qqnorm(resid(m.Airbnb.d.lasso)) # A quantile normal plot - good for checking normality
qqline(resid(m.Airbnb.d.lasso))
p.Airbnb.d.lasso <- predict(m.Airbnb.d.lasso, newdata = Airbnb.test.d)
r.Airbnb.d.test.lasso <- postResample(p.Airbnb.d.lasso, Airbnb.d$price)
r.Airbnb.d.test.lasso
# Variable importance
varImp(m.Airbnb.d.lasso)


# 3. GLM -  combination of ridge and lasso
#------------------------------
m.Airbnb.d.glm <- train(y=y.Airbnb.train.d, x=x.Airbnb.train.d,
                     method = "glm",
                     metric = "RMSE",
                     trControl = ctrl)

saveRDS(m.Airbnb.d.glm, "m.Airbnb.d.glm.rds")
m.Airbnb.d.glm <- readRDS("m.Airbnb.d.glm.rds")
r.Airbnb.d.train.glm <-getTrainPerf(m.Airbnb.d.glm)
r.Airbnb.d.train.glm
summary(r.Airbnb.d.train.glm)
rmse(resid(m.Airbnb.d.glm))
plot(density(resid(m.Airbnb.d.glm))) #A density plot
qqnorm(resid(m.Airbnb.d.glm)) # A quantile normal plot - good for checking normality
qqline(resid(m.Airbnb.d.glm))
p.Airbnb.d.glm <- predict(m.Airbnb.d.glm, newdata = Airbnb.test.d)
r.Airbnb.d.test.glm <- postResample(p.Airbnb.d.glm, Airbnb.d$price)
r.Airbnb.d.test.glm
# Variable importance
varImp(m.Airbnb.d.glm)

# 4.1. PCR(method = "kernelpls")
#-------------------------------------------
#install.packages("pls")
library(pls)
m.Airbnb.d.kpls <- train(y=y.Airbnb.train.d, x=x.Airbnb.train.d,
                        method = "kernelpls",
                        metric = "RMSE",
                        trControl = ctrl)

saveRDS(m.Airbnb.d.kpls, "m.Airbnb.d.kpls.rds")
m.Airbnb.d.kpls <- readRDS("m.Airbnb.d.kpls.rds")
r.Airbnb.d.train.kpls <-getTrainPerf(m.Airbnb.d.kpls)
r.Airbnb.d.train.kpls
summary(r.Airbnb.d.train.kpls)
rmse(resid(m.Airbnb.d.kpls))
plot(density(resid(m.Airbnb.d.kpls))) #A density plot
qqnorm(resid(m.Airbnb.d.kpls)) # A quantile normal plot - good for checking normality
qqline(resid(m.Airbnb.d.kpls))
p.Airbnb.d.kpls <- predict(m.Airbnb.d.kpls, newdata = Airbnb.test.d)
r.Airbnb.d.test.kpls <- postResample(p.Airbnb.d.kpls, Airbnb.d$price)
r.Airbnb.d.test.kpls
# Variable importance
varImp(m.Airbnb.d.kpls)


# 4.2. PCR(method = "pls")
#---------------------------
m.Airbnb.d.pls <- train(y=y.Airbnb.train.d, x=x.Airbnb.train.d,
                         method = "pls",
                         metric = "RMSE",
                         trControl = ctrl)

saveRDS(m.Airbnb.d.pls, "m.Airbnb.d.pls.rds")
m.Airbnb.d.pls <- readRDS("m.Airbnb.d.pls.rds")
r.Airbnb.d.train.pls <-getTrainPerf(m.Airbnb.d.pls)
r.Airbnb.d.train.pls
summary(r.Airbnb.d.train.pls)
rmse(resid(m.Airbnb.d.pls))
plot(density(resid(m.Airbnb.d.pls))) #A density plot
qqnorm(resid(m.Airbnb.d.pls)) # A quantile normal plot - good for checking normality
qqline(resid(m.Airbnb.d.pls))
p.Airbnb.d.pls <- predict(m.Airbnb.d.pls, newdata = Airbnb.test.d)
r.Airbnb.d.test.pls <- postResample(p.Airbnb.d.pls, Airbnb.d$price)
r.Airbnb.d.test.pls
# Variable importance
varImp(m.Airbnb.d.pls)

# 4.3. PCR(method = "pcr")
#---------------------------
m.Airbnb.d.pcr <- train(y=y.Airbnb.train.d, x=x.Airbnb.train.d,
                        method = "pcr",
                        metric = "RMSE",
                        trControl = ctrl)

saveRDS(m.Airbnb.d.pcr, "m.Airbnb.d.pcr.rds")
m.Airbnb.d.pcr <- readRDS("m.Airbnb.d.pcr.rds")
r.Airbnb.d.train.pcr <-getTrainPerf(m.Airbnb.d.pcr)
r.Airbnb.d.train.pcr
summary(r.Airbnb.d.train.pcr)
rmse(resid(m.Airbnb.d.pcr))
plot(density(resid(m.Airbnb.d.pcr))) #A density plot
qqnorm(resid(m.Airbnb.d.pcr)) # A quantile normal plot - good for checking normality
qqline(resid(m.Airbnb.d.pcr))
p.Airbnb.d.pcr <- predict(m.Airbnb.d.pcr, newdata = Airbnb.test.d)
r.Airbnb.d.test.pcr <- postResample(p.Airbnb.d.pcr, Airbnb.d$price)
r.Airbnb.d.test.pcr
# Variable importance
varImp(m.Airbnb.d.pcr)


# B. NON-LINEAR REGRESSION MODEL
#--------------------------------

# 1. DECISION TREE
#-------------------
set.seed(99)
m.Airbnb.d.dt <-  train(x=x.Airbnb.train.d, y=y.Airbnb.train.d,
                        method = "rpart",
                        trControl = ctrl,
                        tuneLength=10)
saveRDS(m.Airbnb.d.dt, "m.Airbnb.d.dt.rds")
plot(m.Airbnb.d.dt,main='Decision Tree')
m.Airbnb.d.dt <- readRDS("m.Airbnb.d.dt.rds")
plot(density(resid(m.Airbnb.d.dt))) #A density plot
qqnorm(resid(m.Airbnb.d.dt)) # A quantile normal plot - good for checking normality
qqline(resid(m.Airbnb.d.dt))

p.Airbnb.d.dt <- predict(m.Airbnb.d.dt, newdata = Airbnb.test.d)
r.Airbnb.test.d.dt <- postResample(p.Airbnb.d.dt, Airbnb.test.d$price )
r.Airbnb.test.d.dt
# Variable importance
varImp(m.Airbnb.d.dt)

#using rpart for regression tree
library(rpart) #faster than tree
#install.packages("tree")
library(tree) #has useful functions to use with rpart

#create tree
m.Airbnb.d.dt <- rpart(price ~ ., data=Airbnb.train.d)

#rather than using default lets use new library
#install.packages("rpart.plot")
library(rpart.plot)

#very readable defaults
rpart.plot(m.Airbnb.d.dt)


# 2. BAGGING TREE
#-------------------
set.seed(99)

m.Airbnb.d.bt <-  train(x=x.Airbnb.train.d, y=y.Airbnb.train.d,
                     method = "treebag",
                     trControl = ctrl,
                     tuneLength=10)
m.Airbnb.d.bt
saveRDS(m.Airbnb.d.bt, "m.Airbnb.d.bt.rds")
m.Airbnb.d.bt <- readRDS("m.Airbnb.d.bt.rds")
plot(density(resid(m.Airbnb.d.bt))) #A density plot
qqnorm(resid(m.Airbnb.d.bt)) # A quantile normal plot - good for checking normality
qqline(resid(m.Airbnb.d.bt))
p.Airbnb.d.bt <- predict(m.Airbnb.d.bt, newdata = Airbnb.test.d)
r.Airbnb.test.d.bt <- postResample(p.Airbnb.d.bt, Airbnb.test.d$price )
r.Airbnb.test.d.bt
# Variable importance
varImp(m.Airbnb.d.bt)


# 3. RANDOM FOREST
#-------------------
set.seed(99)
m.Airbnb.d.rf <-  train(x=x.Airbnb.train.d, y=y.Airbnb.train.d,
                     method = "rf",
                     trControl = ctrl,
                     tuneLength=10)
m.Airbnb.d.rf
m.Airbnb.d.rf <- readRDS("m.Airbnb.d.rf.rds")

plot(density(resid(m.Airbnb.d.rf))) #A density plot
qqnorm(resid(m.Airbnb.d.bt)) # A quantile normal plot - good for checking normality
qqline(resid(m.Airbnb.d.bt))

p.Airbnb.d.rf <- predict(m.Airbnb.d.rf, newdata = Airbnb.test.d)
r.Airbnb.test.d.rf <- postResample(p.Airbnb.d.rf, Airbnb.test.d$price )
r.Airbnb.test.d.rf
# Variable importance
varImp(m.Airbnb.d.dt)

# 4. BOOSTING
#-------------------
set.seed(99)
#install.packages("gbm")
library(gbm)
m.Airbnb.d.boost <-  train(x=x.Airbnb.train.d, y=y.Airbnb.train.d,
                     method = "gbm",
                     trControl = ctrl,
                     tuneLength=10)
m.Airbnb.d.boost
saveRDS(m.Airbnb.d.boost, "m.Airbnb.d.boost.rds")
m.Airbnb.d.boost <- readRDS("m.Airbnb.d.boost.rds")
plot(density(resid(m.Airbnb.d.boost))) #A density plot
qqnorm(resid(m.Airbnb.d.boost)) # A quantile normal plot - good for checking normality
qqline(resid(m.Airbnb.d.boost))

p.Airbnb.d.boost <- predict(m.Airbnb.d.boost, newdata = Airbnb.test.d)
r.Airbnb.test.d.boost <- postResample(p.Airbnb.d.boost, Airbnb.test.d$price )
r.Airbnb.test.d.boost
# Variable importance
varImp(m.Airbnb.d.boost)

# Read all rds files to integrate
m.Airbnb.d.lm <- readRDS("m.Airbnb.d.lm.rds")
m.Airbnb.d.ridge <- readRDS("m.Airbnb.d.ridge.rds")
m.Airbnb.d.lasso <- readRDS("m.Airbnb.d.lasso.rds")
m.Airbnb.d.kpls <- readRDS("m.Airbnb.d.pcr.rds")
m.Airbnb.d.pls <- readRDS("m.Airbnb.d.pls.rds")
m.Airbnb.d.pcr <- readRDS("m.Airbnb.d.pcr.rds")
m.Airbnb.d.dt <- readRDS("m.Airbnb.d.dt.rds")
m.Airbnb.d.bt <- readRDS("m.Airbnb.d.bt.rds")


#---------------------------
# Step 7: Model Evaluation
#---------------------------

# 1. Linear Models
#---------------------
models.linear<- list("linear"=m.Airbnb.d.lm, "Ridge" = m.Airbnb.d.ridge, 
                     "Lasso"=m.Airbnb.d.lasso, "GLM" = m.Airbnb.d.glm,
                     "PCR(kernelpls)" = m.Airbnb.d.kpls, "PCR(pls)" = m.Airbnb.d.pls, 
                     "PCR(pcr)" = m.Airbnb.d.pcr)

# 2. Non-Linear Models
#---------------------
models.nonlinear<- list( "DecisionTree"=m.Airbnb.d.dt, "Bagging" = m.Airbnb.d.bt, #, "Boosting" = m.Airbnb.d.boost,
                         "Boosting"=m.Airbnb.d.boost)


# Result matrix: LINEAR
Airbnb.d.resamples.linear<- resamples(models.linear)
summary(Airbnb.d.resamples.linear)

# Plot model performances
bwplot(Airbnb.d.resamples.linear, metric="MAE")
bwplot(Airbnb.d.resamples.linear, metric="RMSE")
bwplot(Airbnb.d.resamples.linear, metric="Rsquared")


# Result matrix: NON-LINEAR
Airbnb.d.resamples.nonlinear <- resamples(models.nonlinear)
summary(Airbnb.d.resamples.nonlinear)

# Plot model performances
bwplot(Airbnb.d.resamples.nonlinear, metric="MAE")
bwplot(Airbnb.d.resamples.nonlinear, metric="RMSE")
bwplot(Airbnb.d.resamples.nonlinear, metric="Rsquared")


# MODEL Performances- ALL MODELS COMBINED
models.all<- list("lm" = m.Airbnb.d.lm,
                  "Ridge" = m.Airbnb.d.ridge, "Lasso"=m.Airbnb.d.lasso, 
                  "GLM" = m.Airbnb.d.glm,
                  "PCR(kernelpls)" = m.Airbnb.d.kpls, "PCR(pls)" = m.Airbnb.d.pls, 
                  "PCR(pcr)" = m.Airbnb.d.pcr, "DecisionTree"=m.Airbnb.d.dt, 
                  "Bagging" = m.Airbnb.d.bt, "RandomForest"=m.Airbnb.d.boost, 
                  "Boosting"=m.Airbnb.d.boost)

Airbnb.d.resamples.all<- resamples(models.all)
summary(Airbnb.d.resamples.all)

bwplot(Airbnb.d.resamples.all, metric="MAE")
bwplot(Airbnb.d.resamples.all, metric="RMSE")
bwplot(Airbnb.d.resamples.all, metric="Rsquared")


# Test Results
r.Airbnb.d.test.lm
r.Airbnb.d.test.ridge
r.Airbnb.d.test.lasso
r.Airbnb.d.test.glm
r.Airbnb.d.test.kpls
r.Airbnb.d.test.pls
r.Airbnb.d.test.pcr
r.Airbnb.test.d.dt
r.Airbnb.test.d.bt
r.Airbnb.test.d.rf
r.Airbnb.test.d.boost

stopCluster(cl)



#---------------------------
# Ends
#---------------------------





















