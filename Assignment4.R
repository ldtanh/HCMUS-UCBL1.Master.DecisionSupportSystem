seg.df <- read.csv("http://goo.gl/qw303p")
summary(seg.df)

mean(seg.df$income[seg.df$Segment=="Moving up"])
mean(seg.df$income[seg.df$Segment=="Moving up" & seg.df$subscribe == "subNo"])

by(seg.df$income, seg.df$Segment, mean)
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)

aggregate(income ~ Segment, data=seg.df, mean)
aggregate(income ~ Segment + ownHome, data=seg.df, mean)

agg.data <- aggregate(income ~ Segment + ownHome,
                      data=seg.df, mean)
str(agg.data)
agg.data[2, ]
agg.data[2, 3]

aggregate(kids ~ Segment, data=seg.df, sum)

eg.mean <- aggregate(income ~ Segment,
                     data=seg.df, mean)
library(lattice)
barchart(income ~ Segment, data=seg.df,
         col="grey")


bwplot(Segment ~ income, data=seg.df,
       horizontal=TRUE, xlab = "Income")

###################
seg.raw <- read.csv("http://goo.gl/qw303p") 
seg.df <- seg.raw[ , -7] # remove the known segment assignments
summary(seg.df)

seg.df.num <- seg.df
seg.df.num$gender <- ifelse(seg.df$gender=="Male", 0, 1)
seg.df.num$ownHome <- ifelse(seg.df$ownHome=="ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe=="subNo", 0, 1)
summary(seg.df.num)
seg.df <- seg.df.num

seg.summ <- function(data, groups) { aggregate(data,
                                               list(groups), function(x) mean(as.numeric(x))) }
seg.summ(seg.df, seg.raw$Segment)


set.seed(96743) # because starting assignments are random
seg.k <- kmeans(seg.df.num, centers=4)
seg.summ(seg.df, seg.k$cluster)

boxplot(seg.df.num$income ~ seg.k$cluster, xlab="Income",
        ylab="Segment", horizontal=TRUE)

library(cluster)
clusplot(seg.df, seg.k$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0, main="K-means cluster plot")

###################
set.seed(04625) # make it repeatable
train.prop <- 0.65 # train on 65% of data. Hold 35% for testing
train.cases <- sample(nrow(seg.raw), nrow(seg.raw)*train.prop)
seg.df.train <- seg.raw[ train.cases, ]
seg.df.test <- seg.raw[-train.cases, ]

install.packages("e1071")
library(e1071) # install.packages("e1071") if required
seg.nb <- naiveBayes(Segment ~ ., data=seg.df.train)
seg.nb.class <- predict(seg.nb, seg.df.test)
prop.table(table(seg.nb.class))

clusplot(seg.df.test[, -7], seg.nb.class, color=TRUE,
         shade=TRUE, labels=4, lines=0, main="Naive Bayes classification, holdout data")

mean(seg.df.test$Segment==seg.nb.class) 
install.packages("mclust")
library(mclust)
adjustedRandIndex(seg.nb.class, seg.df.test$Segment)


####################
install.packages(c("arules", "arulesViz"))
library(arules)
data("Groceries")
summary(Groceries)

inspect(head(Groceries))
groc.rules <- apriori(Groceries, parameter=list(supp=0.01, conf=0.3, target="rules"))
inspect(subset(groc.rules, lift > 3))


retail.raw <- readLines("http://goo.gl/FfjDAO")
summary(retail.raw)
head(retail.raw, 5)
tail(retail.raw, 3)

retail.list <- strsplit(retail.raw, " ")
names(retail.list) <- paste("Trans", 1:length(retail.list), sep="")
str(retail.list)

retail.trans <- as(retail.list, "transactions")
summary(retail.trans)

retail.rules <- apriori(retail.trans,
                        parameter=list(supp=0.001, conf=0.4))

install.packages("arulesViz")
install.packages("glue")
library(arulesViz)
plot(retail.rules)

retail.hi <- head(sort(retail.rules, by="lift"), 50) # top 50
inspect(retail.hi)

plot(retail.hi, method="graph", control=list(type="items"))
