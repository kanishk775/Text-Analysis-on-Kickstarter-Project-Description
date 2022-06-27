

### Course: Text Analysis
### Lecturer: Dr. Jochen Hartmann
### Assignment: "Predicting online success from text data"


### Data Source: https://www.pnas.org/doi/10.1073/pnas.2026045118 
### Reference: Markowitz, D. M., & Shulman, H. C. (2021). The predictive utility of word familiarity for online engagements and funding. Proceedings of the National Academy of Sciences, 118(18).
### Please see the ReadMe file for variable descriptions: https://osf.io/rym7z/


# check and set working directory
getwd()
setwd("C:/Users/kanishk/Desktop/Semester 3/Text Analysis/Kickstarter_Assignment")


# set random seed to make results replicable

set.seed(42)

# load libraries
library(dplyr)
library(car)
library(anytime)
library(tm)
library(stargazer)
library(randomForest)
library(esquisse)
library(data.table)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(rio)
library(ngram)
library(tm)
library(rpart)
library(rpart.plot)
library(caret)


# load data
d1<- read.csv("kickstarter_data_OSF_simplified.csv", stringsAsFactors = F)
d <- read.csv("kickstarter_data_OSF_simplified.csv", stringsAsFactors = F)

#Sentiment classification done using Roberta - Hugging Face code on Google Colab
d_roberta <- read.csv("kickstarter_labelled_roberta.csv", stringsAsFactors = F)
df0 <- cbind(d,d_roberta)
colnames(df0)
df0$text <- NULL
df0$label <- NULL


#Selecting random rows from dataset
class(d)
sampled_data = d[sample(nrow(d), 1000), ]

#Creating a CSV file to label the data
#Making the command a comment so the labelled data is not overwritten
#write.csv(sampled_data, file = "sampled_data_kickstarter.csv", row.names = FALSE)


#Checking the absurdness in dataset
#d[which(d$uid == 149011), 1:5] 


#Reading the labelled file
df <- read.csv("sampled_data_kickstarter_labelled.csv", stringsAsFactors = F)

# explore data
head(df, 3)
View(df[1:10,])
table(df$Plead.No.plead)
table(df$state)
length(unique(df$country))  # check number of countries
sort(table(d$category), decreasing = T)  # check distribution of categories

#Removing Class Imbalance and creating equal class dataset
df_class1 <- subset(df, Plead.No.plead ==1)
df_class0 <- subset(df, Plead.No.plead ==0)
df_class1_sample <-  df_class1[sample(nrow(df_class1), 165), ]

df2_bind <- rbind(df_class0, df_class1_sample)
#Shuffling The data frame
df2 <-  df2_bind[sample(nrow(df2_bind), 330), ]


# transform variables
library(dplyr)

df2 <- df2 %>% dplyr::rename(text = blurb)  # rename text column
d <- d %>% dplyr::rename(text = blurb)  # rename text column
summary(df2$WC)
df2$WC_mc <- as.numeric(scale(df2$WC, scale = F))  # mean-center word count (WC)
summary(df2$WC_mc)  # check mean-centering
df2$campaign_success <- ifelse(df2$state == "failed", 0, 1)
df2$usd_pledged_ln <- log(df2$usd_pledged + 1)
df2$goal_ln <- log(df2$goal + 1)
df2$backers_count_ln <- log(df2$backers_count+1)
df2$start_year <- format(as.Date(anytime(df2$unix_startdate), format="%d/%m/%Y"),"%Y")


#Data Visualization

###Loading Esquisse to plot ggplots

esquisse::esquisser()

library(ggplot2)

## 1. What proportion of projects managed to raise the desired amount of money?
ggplot(df2) +
 aes(x = state, y = usd_pledged_ln) +
 geom_boxplot(shape = "circle", fill = "#112446") +
 theme_minimal()


#Values in Million for the below summary
df2 %>% group_by(state) %>% summarize(usd_pledged_ln = mean(usd_pledged_ln))


## 2. What is the median funding goal? What is the median of funds raised?
df2 %>% summarize(goal_ln = median(goal_ln))
df2 %>% summarize(usd_pledged_ln = median(usd_pledged_ln))

ggplot(df) +
 aes(x = state) +
 geom_bar(fill = "#112446") +
  labs(
    title = "Kickstarter Projects Success/Failure"
  ) +
 theme_minimal()

ggplot(df) +
 aes(x = state, y = Plead.No.plead) +
 geom_violin(adjust = 1L, scale = "area", fill = "#112446") +
  labs(
    title = "State vs. Plead/No Plead"
  ) +
 theme_minimal()


meanpledge <- df %>% group_by(country) %>% summarise_at(vars(usd_pledged),funs(mean(.,na.rm=TRUE)))
ggplot(meanpledge) +
  aes(x = country, weight = usd_pledged) +
  geom_bar(fill = "#112446") +
  labs(
    x = "Country",
    y = "Mean Pledge",
    title = "Mean Pledge in Each Country"
  ) +
  theme_minimal()

##3. How many different categories are included in the dataset? How do they differ in terms of funding objectives?
length(unique(df2$category))
ggplot(df) +
  aes(x = category) +
  geom_bar(fill = "#A3313B") +
  labs(title = "Number of projects by Category") +
  theme_minimal() + coord_flip()

df2 %>% group_by(category) %>% summarize(usd_pledged_ln = mean(usd_pledged_ln))
ggplot(df2) +
  aes(x = category, y = usd_pledged_ln) +
  geom_boxplot(shape = "circle", fill = "#112446") +
  theme_minimal() + coord_flip()

df2 %>% group_by(category) %>% summarize(goal_mean = mean(goal_ln))
ggplot(df2) +
  aes(x = category, y = goal_ln) +
  geom_boxplot(shape = "circle", fill = "#112446") +
  theme_minimal() + coord_flip()


ggplot(df) +
  aes(x = state, y = i) +
  geom_violin(adjust = 1L, scale = "area", fill = "#7D7B2D") +
  labs(title = "Usage of \"I\" vs. State of Project") +
  theme_minimal()


##data visualization with balanced class
#Enlarge this image for a better view or change the ggtheme to minimal
ggplot(df2) +
 aes(x = state, y = Plead.No.plead) +
 geom_violin(adjust = 1L, scale = "area", fill = "#112446") +
 labs(title = "State vs. Plead/No Plead in df2") +
 ggthemes::theme_par()

## 4. How many words does the average text contain? What are the 10 most frequent words?

df2 %>% summarize(WC = mean(WC))

## 7. Create at least one dictionary, i.e., custom word list, that you expect to 
## be associated with campaign success. Pursue a top-down approach, ideally 
## basing your selection of words on theoretical considerations to justify the 
## hypothesized linguistic patterns.

## Creating Dictionary
# create additional text-based variables
df2$i <- grepl("\\bi\\b|\\bme\\b|\\bmyself\\b|\\bmy\\b", tolower(df2$text))
round(prop.table(table(df2$i)), 3)  # explore new variable
df2 %>% group_by(i) %>% summarize(usd_pledged_mean = mean(usd_pledged))  # explore model-free evidence
#df2 %>% filter(i == T) %>% select(text) %>% sample_n(5)

#Creating dictionary for Full dataset
d$i <- grepl("\\bi\\b|\\bme\\b|\\bmyself\\b|\\bmy\\b", tolower(d$text))
round(prop.table(table(d$i)), 3)
d$dictionary <- grepl("\\bmoney\\b|\\bfund\\b|\\bhelp\\b|\\blove\\b|\\bneed\\b|\\bseek\\b|\\bjoin us\\b|
                      \\bsupport\\b|\\bappreciate\\b|\\breinvestment\\b|\\bfundraise\\b|\\bfundraising\\b|\\bdonation\\b|\\bbe part of community\\b", tolower(d$text))

round(prop.table(table(d$dictionary)), 3)
# create additional text-based variables - creating dictionary for pleading words
df2$dictionary <- grepl("\\bmoney\\b|\\bfund\\b|\\bhelp\\b|\\blove\\b|\\bneed\\b|\\bseek\\b|\\bjoin us\\b|
                        \\bsupport\\b|\\bappreciate\\b|\\breinvestment\\b|\\bfundraise\\b|\\bfundraising\\b|\\bdonation\\b|\\bbe part of community\\b", tolower(df2$text))
round(prop.table(table(df2$dictionary)), 3)  # explore new variable
df2 %>% group_by(dictionary) %>% summarize(usd_pledged_mean = mean(usd_pledged))  # explore model-free evidence
#df2 %>% filter(i == T) %>% select(text) %>% sample_n(5)

# visualize data
plot(table(df2$start_year), main = "Number of Projects over Time")  # plot distribution of years
par(mfrow = c(1,2))
hist(df2$usd_pledged_ln, main = "Amount Pledged (in USD, ln)")
hist(df2$goal_ln, main = "Funding Goal (in USD, ln)")
round(cor(df2$usd_pledged_ln, df2$goal_ln), 3)  # check correlation
hist(df2$WC, main = "Word Count")
hist(df2$WC_mc, main = "Word Count (mean-centered)")

esquisse::esquisser()

ggplot(df2) +
 aes(x = dictionary, y = Plead.No.plead) +
 geom_violin(adjust = 1L, scale = "area", fill = "#662E1F") +
 labs(title = "Using Pleading words and classififcation as such") +
 theme_minimal()

ggplot(df2) +
 aes(x = usd_pledged_ln, y = WC_mc) +
 geom_point(shape = "circle", size = 1.65, colour = "#662E1F") +
 geom_smooth(span = 0.1) +
 theme_minimal()

ggplot(df2) +
 aes(x = usd_pledged_ln, y = WC_mc) +
 geom_point(shape = "circle", size = 1.65, colour = "#662E1F") +
 geom_smooth(span = 0.1) +
 labs(title = "Log of USD Pledged vs. Word Count") +
 theme_minimal()

ggplot(df2) +
 aes(x = dictionary, y = usd_pledged_ln) +
 geom_violin(adjust = 1L, scale = "area", fill = "#662E1F") +
 labs(title = "Use of pleading words and USD funded") +
 theme_minimal()


##Create additional textual features based on different off-the-shelf lexicons

library("syuzhet")
colnames(df2)

selected_examples <- df2$text  # select examples

# extract sentiment
sentiment_score <- get_sentiment(selected_examples, 
                                 method = "afinn")
df2$Lexicon <- data.frame(example_sentence = selected_examples, sentiment_score, 
           polarity = ifelse(sentiment_score > 0, "positive", "negative"))  # assign sentiment to sentences

View(df2$Lexicon)
head(df2$Lexicon)
selected_examples_full_dataset <- d$text  # select examples

# extract sentiment
sentiment_score_full_dataset <- get_sentiment(selected_examples_full_dataset, 
                                 method = "afinn")
d$Lexicon <- data.frame(example_sentence = selected_examples_full_dataset, sentiment_score_full_dataset, 
                          polarity = ifelse(sentiment_score_full_dataset > 0, "positive", "negative"))  # assign sentiment to sentences

View(d$Lexicon)

#Running Script 2 from class
# pre-process data
corpus2 <- VCorpus(VectorSource(df2$text))
corpus_clean2 <- tm_map(corpus2, content_transformer(tolower))
corpus_clean2 <- tm_map(corpus_clean2, removePunctuation)  # remove punctuation
#corpus_clean2 <- tm_map(corpus_clean, removeWords, stopwords('english')) # remove stopwords like 'and'
corpus_clean2 <- tm_map(corpus_clean2, removeWords, stopwords('english')[-which(stopwords('english') == "not")])  # remove stopwords like 'and', except for 'not'

# print stopwords
stopwords('english')  
"the" %in% stopwords('english')  # check if a word occurs in the stopwords

# specify tokenization limits
min_uni <- 3
min_chars <- 2
max_chars <- 20
min_bi <- 5
min_tri <- 3


## 5. How can you visualize the overall distribution of words in the data in a compelling way?
## Overall distribution can be visualized in a compelling way using a DTM


# tokenize, create document-term-matrix (dtm) - DECISION TREE
UnigramTokenizer2 <- function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
dtm_unigram2 <- DocumentTermMatrix(corpus_clean2, control = list(tokenize = UnigramTokenizer2, 
                                                               wordLengths=c(min_chars,max_chars), 
                                                               bounds = list(global = c(min_uni,Inf))))
dtm_unigram2 <- weightBin(dtm_unigram2)
colnames(dtm_unigram2)

BigramTokenizer2 <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
dtm_bigram2 <- DocumentTermMatrix(corpus_clean2, control = list(tokenize = BigramTokenizer2, wordLengths=c(min_chars*2,20), bounds = list(global = c(min_bi,Inf))))
dtm_bigram2 <- weightBin(dtm_bigram2)
colnames(dtm_bigram2)

#Not sufficent trigram words
TrigramTokenizer2 <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
dtm_trigram2 <- DocumentTermMatrix(corpus_clean2, control = list(tokenize = TrigramTokenizer2, wordLengths=c(min_chars*2,20), bounds = list(global = c(min_tri,Inf))))
dtm_trigram2 <- weightBin(dtm_trigram2)
colnames(dtm_trigram2)

dtm_combined <- (as.data.frame(cbind(as.matrix(dtm_unigram2), as.matrix(dtm_bigram2))))
# explore dtm
dtm_unigram2
head(as.matrix(dtm_combined)[,1:15])

# add labels to dtm
labeled_dtm2 <- as.data.frame(cbind(df2$Plead.No.plead, as.data.frame(as.matrix(dtm_unigram2))))
str(labeled_dtm2)
labeled_dtm2[,-1] <- apply(labeled_dtm2[,-1], 2, function(x) as.numeric(as.character(x)))
colnames(labeled_dtm2)[1] <- "y"

# split data
partition <- .8
set.seed(0)
trainIndex <- createDataPartition(labeled_dtm2$y, p = partition, list = FALSE)
train2 <- labeled_dtm2[trainIndex,]
test2 <- labeled_dtm2[-trainIndex,]
nrow(train2) + nrow(test2) == nrow(labeled_dtm2)

# train decision tree (DT)
model_dt <- rpart(y ~ ., data = train2, cp = 0.00, method = "class")  # optional: Let's vary the complexity parameter (cp)
model_dt

# plot decision tree
rpart.plot(model_dt, 
           box.palette="Blues",
           tweak = 1,
           fallen.leaves = TRUE,
           round = 0,
           type=1)

# predict on hold-out test data
preds_dt <- predict(model_dt, newdata = test2, type = "class")
head(preds_dt)
length(preds_dt)

# evaluate accuracy
round(mean(preds_dt == test2$y), 4)


#Running Script 3 from Class

pre_process_data <- function(dataset){
  
  processed_dataset <- VCorpus(VectorSource(dataset$text))
  processed_dataset <- tm_map(processed_dataset, content_transformer(tolower))
  processed_dataset <- tm_map(processed_dataset, removeNumbers)
  processed_dataset <- tm_map(processed_dataset, removePunctuation)
  processed_dataset <- tm_map(processed_dataset, stripWhitespace)
  processed_dataset <- tm_map(processed_dataset, removeWords, stopwords('english')[-which(stopwords('english') == "not")])  # remove stopwords like 'and', except for 'not'
  min_uni <- 3; min_bi <- 7; min_chars <- 2
  
  UnigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
  dtm_unigram <- DocumentTermMatrix(processed_dataset, control = list(tokenize = UnigramTokenizer, wordLengths=c(min_chars,20), bounds = list(global = c(min_uni,Inf))))
  dtm_unigram <- weightBin(dtm_unigram)
  
  BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  dtm_bigram <- DocumentTermMatrix(processed_dataset, control = list(tokenize = BigramTokenizer, wordLengths=c(min_chars*2,20), bounds = list(global = c(min_bi,Inf))))
  dtm_bigram <- weightBin(dtm_bigram)
  
  print(class(dtm_bigram))
  print(class(dtm_unigram))

    
  return(as.data.frame(cbind(as.matrix(dtm_unigram), as.matrix(dtm_bigram))))
  
}



corpus_clean <- pre_process_data(df2)
str(corpus_clean)
object.size(corpus_clean)
View(head(corpus_clean, 20))
which.max(colSums(corpus_clean))

# Split labeled DTM into training set (80% of data) and hold-out test set (20% of data)

partition <- .8

labeled_dtm <- as.data.frame(cbind(df2$Plead.No.plead, corpus_clean))
labeled_dtm[,-1] <- apply(labeled_dtm[,-1], 2, function(x) as.numeric(as.character(x)))
colnames(labeled_dtm)[1] <- "class_dv"

set.seed(128); trainIndex <- createDataPartition(df2$Plead.No.plead, p = partition, list = FALSE)
train_labeled <- labeled_dtm[trainIndex,]
test_labeled <- labeled_dtm[-trainIndex,]

# Define trainControl functions

cv_tune <- 5; rep_tune <- 1
cv_final <- 10; rep_final <- 5

ctrl_tune <- trainControl(method = "repeatedcv", number = cv_tune, repeats = rep_tune, selectionFunction = "best", 
                          verboseIter = TRUE, savePredictions = "final", classProbs = FALSE)
ctrl_final <- trainControl(method = "repeatedcv", number = cv_final, repeats = rep_final, selectionFunction = "best", 
                           verboseIter = TRUE, savePredictions = "final", classProbs = FALSE)

# Set parameter grids

grid_knn <- expand.grid(k = c(1,15,30,45,65))
grid_rf <- expand.grid(mtry = c(round(sqrt(ncol(train_labeled))/2),round(sqrt(ncol(train_labeled)))), 
                       splitrule = "gini", min.node.size = 1)
grid_svm <- expand.grid(C = c(0.01,0.1,1,10,100))

# Create set of models and combine grids

set_of_models <- c("knn", "ranger", "svmLinear")
model_parameter_grids <- as.data.frame(matrix(nrow = length(set_of_models), ncol = 2))

colnames(model_parameter_grids) <- c("model", "parameter_grid")
model_parameter_grids$model = set_of_models
model_parameter_grids$parameter_grid = list(grid_knn, grid_rf, grid_svm)
model_parameter_grids

df_train_results <- as.data.frame(matrix(nrow = length(set_of_models), ncol = 5))
colnames(df_train_results) <- c("final_model", "model", "train_acc", "tuned_parameters", "runtime")

# Initialize lists

models = list()
final_model_list = list()
tuned_parameters = list()
models_final = list()
final_model_list_final = list()

# Train models

set.seed(128); system.time(
  for(i in 1:length(set_of_models)) {
    
    method_train <- model_parameter_grids$model[i]
    grid <- model_parameter_grids$parameter_grid[i]
    grid <- grid[[1]]
    
    fitted <- caret::train(y = factor(train_labeled[,1]), x = train_labeled[,-1], method = method_train, metric = "Accuracy",
                           tuneGrid = grid, trControl = ctrl_tune)
    
    final_model <- fitted
    train_acc <- caret::confusionMatrix(fitted$pred$pred, fitted$pred$obs)
    
    final_model_list[[i]] <- final_model
    models[[i]] <- fitted
    tuned_parameters[[i]] <- fitted$bestTune
    
    df_train_results$train_acc[i] <- round(train_acc$overall[1],4)
    
    # Fit tuned model on full dataset
    
    fitted_final <- caret::train(y = factor(labeled_dtm[,1]), x = labeled_dtm[,-1], method = method_train, metric = "Accuracy",
                                 tuneGrid = fitted$bestTune, trControl = ctrl_final)
    
    final_model_final <- fitted_final
    repeated_acc <- caret::confusionMatrix(fitted_final$pred$pred, fitted_final$pred$obs)
    
    final_model_list_final[[i]] <- final_model_final
    models_final[[i]] <- fitted_final
    
    df_train_results$repeated_acc[i] <- round(repeated_acc$overall[1],4)
    
    
  }
)

# Save models and tuned parameters

df_train_results$final_model <- final_model_list
df_train_results$model <- models
df_train_results$tuned_parameters <- tuned_parameters

parameters <- data.frame(df_train_results$tuned_parameters[[1]]$k,
                         df_train_results$tuned_parameters[[2]]$mtry,
                         df_train_results$tuned_parameters[[3]]$C)
colnames(parameters) <- c("kNN_k", "RF_mtry", "SVM_C")

# Compute standard deviations and standard errors

std <- function(x) sd(x)/sqrt(length(x))
std_dev <- vector(mode="numeric", length=0)
std_err <- vector(mode="numeric", length=0)

for(l in 1:length(set_of_models)) {
  
  std_dev[l] <- sd(final_model_list_final[[l]]$resample$Accuracy)
  std_err[l] <- std(final_model_list_final[[l]]$resample$Accuracy)
  
}

# explore variance of accuracy for rf
final_model_list_final[[2]]$resample$Accuracy

df_train_results$std_dev <- round(std_dev,4); df_train_results$std_err <- round(std_err,4)

# Predict on hold-out test set

df_train_results$test_acc = NA
predictions <- as.data.frame(matrix(nrow = nrow(test_labeled), ncol = length(final_model_list)))
colnames(predictions) <- c("kNN", "RF", "SVM")
head(predictions)

for(j in 1:length(final_model_list)) {
  
  method_train <- model_parameter_grids$model[j]
  
  pred_i <- predict(final_model_list[[j]], test_labeled[, -1], type = "raw")
  
  test_acc <- caret::confusionMatrix(pred_i, as.factor(test_labeled[,1]))
  df_train_results$test_acc[j] <- round(test_acc$overall[1],4)
  predictions[, j] = pred_i 
  
}

# Consolidate results

results_cols <- c("test_acc", "std_dev", "std_err")
results <- df_train_results[,results_cols]
rownames(results) <- c("kNN", "RF", "SVM")

# Print results

results





# apply text classifier to full data, important: run "SentimentML_Script.R" before running this
# transform variables
library(dplyr)

#d <- d %>% dplyr::rename(text = blurb)  # rename text column
summary(d$WC)
d$WC_mc <- as.numeric(scale(d$WC, scale = F))  # mean-center word count (WC)
summary(d$WC_mc)  # check mean-centering
d$campaign_success <- ifelse(d$state == "failed", 0, 1)
d$usd_pledged_ln <- log(d$usd_pledged + 1)
d$goal_ln <- log(d$goal + 1)
d$backers_count_ln <- log(d$backers_count+1)
d$start_year <- format(as.Date(anytime(d$unix_startdate), format="%d/%m/%Y"),"%Y")

class(dtm_unigram2)
class(dtm_combined)
corpus2 <- VCorpus(VectorSource(d$text))
dtm2 <- DocumentTermMatrix(corpus2, control = list(dictionary = Terms(dtm_unigram2), 
                                                   weighting = function(x) weightBin(x)))  
#dtm2  # inspect document-term-matrix (number of columns should be identical to the document-term-matrix based on which the classifier is trained)
d$pleadornoplead <- predict(model_dt, as.data.frame(as.matrix(dtm2)), type = "class")  # create new column based on predictions from classifier
table(d$pleadornoplead)  # important: this is a weak sentiment measure and just for illustration purposes


colnames(d)
# analyze data
m <- list()
m[[1]] <- glm(campaign_success ~ pleadornoplead + sentiment_score_full_dataset + pred + dictionary + WC_mc+ i + goal_ln + date_difference + country + category, data = d, family = "binomial")  # logistic regression (for binary data)
m[[2]] <- lm(usd_pledged_ln ~ pleadornoplead + sentiment_score_full_dataset + pred + dictionary + WC_mc+ i + goal_ln + date_difference + country + category, data = d)  # linear regression (for continuous data)
m[[3]] <- update(m[[2]], "usd_pledged ~ .")  # change to non-ln transformed usd_pledged to compare model fit
m[[4]] <- update(m[[2]], "backers_count_ln ~ .")
m[[5]] <- glm(backers_count ~ dictionary + WC_mc + i + goal_ln + date_difference + country + category, data = d, family = "poisson")  # poisson regression (for count data)
summary(m[[1]])
.vif(m[[1]])  # check vif values
     
# report results
stargazer(m,
          title = "Regression Results",
          omit = c("country", "category"),
          no.space = F,
          initial.zero = F,
          notes.align = "l",
          notes = "",
          star.cutoffs = c(.05, .01, .001),
          add.lines=list(c('Country Fixed Effects', rep('Yes', length(m))),
                         c('Category Fixed Effects', rep('Yes', length(m)))),
          omit.stat = "aic",
          type = "text")

# plot curves in relevant value range
par(mfrow = c(1,3))
START = quantile(d$WC_mc, probs = .05, na.rm = T)  # define 90% value range for WC from START to END
START
END = quantile(d$WC_mc, probs = .95, na.rm = T)
END

# plot campaign success
b1 = coef(m[[1]])["WC_mc"]
c = coef(m[[1]])["(Intercept)"]
curve(b1 * x + c, from = START, to = END, 
      ylab="Campaign Success", xlab = "Word Count (mean-centered)")

# plot usd pledged (ln)
b1 = coef(m[[2]])["WC_mc"]
c = coef(m[[2]])["(Intercept)"]
curve(b1 * x + c, from = START, to = END, 
      ylab="USD Pledged (ln)", xlab = "Word Count (mean-centered)")

# plot backers count
b1 = coef(m[[5]])["WC_mc"]
c = coef(m[[5]])["(Intercept)"]
curve(b1 * x + c, from = START, to = END, 
      ylab="Backers Count", xlab = "Word Count (mean-centered)")


# THE END

View(df2)
#Limitation -  WordCloud is not forming
wordcloud <- VCorpus(VectorSource(d$text))
wordcloud <- tm_map(wordcloud, content_transformer(tolower))
wordcloud <- tm_map(wordcloud, removeNumbers)
wordcloud <- tm_map(wordcloud, removePunctuation)
wordcloud <- tm_map(wordcloud, stripWhitespace)
wordcloud <- tm_map(wordcloud, removeWords, stopwords('english')[-which(stopwords('english') == "not")])
#Non linear relationship - add full dataset columns (reqd. only)
library(wordcloud)
wordcloud(wordcloud, scale = c(2, 1), min.freq = 75, colors = rainbow(30))



library(tidytext)
top10_df <- data_frame(Text = d$text) # tibble aka neater data frame
top10_words <- top10_df %>% 
  unnest_tokens(output = word, input = Text)

top10_words <- top10_words %>%
  anti_join(stop_words) # Remove stop words in peter_words

top10_wordcounts <- top10_words %>% count(word, sort = TRUE)

top10 <- head(top10_wordcounts,10)

top10 %>% 
  filter(n > 70) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Top 10 Words \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12))


#Non Linear Visualization
colnames(d)
cols <- c("text")

library(GGally)
ggpair(df2[cols], method = c("everything", "pearson"))

nonlinear <- glm(campaign_success ~ goal_ln + date_difference + I(WC_mc^10), data = d, family = "binomial")
summary(nonlinear)
plot(nonlinear)
