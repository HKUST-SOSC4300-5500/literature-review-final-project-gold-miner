### @author LI, Jingchen
# This file is mainly for the statistic analysis of sentiment. The data was preprocessed in the python file.
# import data
sentiment_matched <- read.csv("C:/Users/lijin/seventh_expedition_expedition_data_prediction_sentiment_matched.csv")
sentiment <- read.csv("C:/Users/lijin/seventh_expedition_expedition_data_prediction_sentiment.csv")
table(sentiment$group)
colnames(sentiment)
# change the column name for future visualization
colnames(sentiment_matched)[which(names(sentiment_matched) == "positive_probs_y")] <- "positive_probs"
# linear regression on comment and reply positiveness
positive <- lm(formula = positive_probs_x ~ positive_probs,
          data = sentiment_matched )
summary(positive)

# keep only level2 comments using simplified Chinese
sentiment2=sentiment[sentiment$level== 2,]
sentiment2=sentiment2[sentiment2$simplified == 'True',]
table(sentiment2$prediction)

# linear regression on reply count and comment positiveness
count_positive <- lm(formula = comment_count ~ positive_probs,
          data = sentiment2 )
summary(count_positive)

# linear regression on like count and comment positiveness
like_positive <- lm(formula = like_count ~ positive_probs,
                     data = sentiment2 )

summary(like_positive)

# ttest on the four related response behavior against the predicted camp
t.test(sentiment2$positive_probs ~ sentiment2$prediction)
t.test(sentiment2$like_count ~ sentiment2$prediction)
t.test(sentiment2$comment_count ~ sentiment2$prediction)
t.test(sentiment_matched$positive_probs_x ~ sentiment_matched$prediction_y)

# output the regression result
library(stargazer)
stargazer(positive,like_positive, count_positive,type="html", out="like_positive.html",
          covariate.labels=c("Comment Positive Probablity"),dep.var.labels=c("Reply Positive Probablity","Like count", "Reply count"))
