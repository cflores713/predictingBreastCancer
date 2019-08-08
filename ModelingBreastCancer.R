library(ggplot2)      # for ggplot
library(GGally)       # for ggpairs
library(highcharter)  # HTML plots
library(class)        # KNN
library(magrittr)     # Pipe Operators
library(caret)        # KMeans
library(factoextra)   # Plotting KMeans
library(e1071)        # SVM
library(dplyr)        # Data Preprocessing
#require(graphics)     # Plotting Dendrograms


# Import Dataset
wbcd <- read.csv("~/Desktop/MLProject2/biopsy.csv", header=T, stringsAsFactors=F)
wbcd$X <- NULL
wbcd <- wbcd[,-1]
wbcd$diagnosis <- factor(ifelse(wbcd$diagnosis=="B","Benign","Malignant"))

# Arrange the patients by Diagnosis (Benign then Malignant)
wbcd <- data.frame(wbcd %>% dplyr::arrange(diagnosis))
# Create a copy of the data set with only 10 features
wbcd10 <- wbcd[,c(1:11)]
# Create a dataset of only the labels
wbcd.Labels = wbcd[1]
# Remove the labels (for clustering)
wbcd.30.cluster = wbcd[2:31]
wbcd.10.cluster = wbcd10[2:11]
# Scale the Data
wbcd.30.scale.cluster <- scale(wbcd.30.cluster)
wbcd.10.scale.cluster <- scale(wbcd.10.cluster)

# Perform a 80/20 split for the Supervised Models
nrows <- NROW(wbcd)
set.seed(1)                          
index <- sample(1:nrows, 0.8 * nrows)   
#train with 20 predictors               ## 569 test data (100%)
train <- wbcd[index,]                   ## 456 test data (80%)
test <- wbcd[-index,]                   ## 113 test data (20%)
#train with 10 predictors
train10 <- wbcd10[index,]                   ## 456 test data (80%)
test10 <- wbcd10[-index,]                   ## 113 test data (20%)

### KNN
#library(class)
#knn.acc is an array to store values
knn.acc <- numeric() 
for(i in 2:30){
  set.seed(1)
  knn.predict <- knn(train=train[,-1], test=test[,-1], cl=train[,1], k=i, prob=T)
  #Store the average number of correctly classified points
  knn.acc <- c(knn.acc,mean(knn.predict==test[,1]))
}

project.acc <- data.frame(k= seq(2,30), cnt = knn.acc)
opt_k <- subset(project.acc, cnt==max(cnt))[1,]
subtitle <- paste("Optimal k:", opt_k$k, 
                  "(Test Error :", 1-opt_k$cnt,").")

#library(magrittr)
#library(highcharter)
hchart(project.acc, 'line', hcaes(k, cnt)) %>%
  hc_title(text = "80/20 Split With 30 Predictors (KNN)") %>%
  hc_subtitle(text = subtitle) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_xAxis(title = list(text = "Number of Neighbors(k)")) %>%
  hc_yAxis(title = list(text = "Accuracy"))


#KNN for 10 features
knn.acc.10 <- numeric() 
for(i in 2:30){
  set.seed(1)
  knn.predict.10 <- knn(train=train10[,-1], test=test10[,-1], 
                        cl=train10[,1], k=i, prob=T)
  #Store the average number of correctly classified points
  knn.acc.10 <- c(knn.acc.10,mean(knn.predict.10==test10[,1]))
}

project.acc.10 <- data.frame(k= seq(2,30), cnt = knn.acc.10)
opt_k.10 <- subset(project.acc.10, cnt==max(cnt))[1,]
subtitle10 <- paste("Optimal k:", opt_k.10$k, 
                    "(accuracy :", 1-opt_k.10$cnt,").")

hchart(project.acc.10, 'line', hcaes(k, cnt)) %>%
  hc_title(text = "80/20 Split With 10 Predictors (KNN)") %>%
  hc_subtitle(text = subtitle10) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_xAxis(title = list(text = "Number of Neighbors(k)")) %>%
  hc_yAxis(title = list(text = "Accuracy"))

# 30 predictors 4 neighbors 80/20 split @ 96%
set.seed(1)
pre_knn <- knn(train = train[,-1], test = test[,-1], 
               cl = train[,1], k=opt_k$k, prob=T)
cm_knn  <- confusionMatrix(pre_knn, test$diagnosis)
# 30 predictors with 4 neighbors Full Set @ 95%
set.seed(1)
knn.pre.30.full <- knn(train = wbcd[,-1], test = wbcd[,-1], 
                       cl = wbcd[,1], k=opt_k$k, prob=T)
knn.cm.30.full  <- confusionMatrix(knn.pre.30.full, wbcd$diagnosis)
mean(knn.pre.30.full != wbcd[,1])
# 10 predictors 10 neighbors @ 96%
set.seed(1)
knn.pre10 <-knn(train = train[,-1], test = test[,-1], 
                cl = train[,1], k=opt_k.10$k, prob=T)
knn.cm.10  <- confusionMatrix(knn.pre10, test$diagnosis)
# 10 predictors with 4 neighbors Full Set @ 91%
set.seed(1)
knn.pre.10.full <-knn(train = wbcd10[,-1], test = wbcd10[,-1], 
                      cl = wbcd10[,1], k=opt_k.10$k, prob=T)
knn.cm.10.full  <- confusionMatrix(knn.pre.10.full, wbcd10$diagnosis)

# Confusion Matrices and Visualizations
col <- c("plum3", "purple4")
par(mfrow=c(2,2))
fourfoldplot(cm_knn$table, color = col, conf.level = 0, margin = 1, 
             main=paste("30p 80/20 KNN (",round(cm_knn$overall[1]*100),"%)",sep=""))
fourfoldplot(knn.cm.10$table, color = col, conf.level = 0, margin = 1, 
             main=paste("10p 80/20 KNN (",round(knn.cm.10$overall[1]*100),"%)",sep=""))
fourfoldplot(knn.cm.30.full$table, color = col, conf.level = 0, margin = 1, 
             main=paste("30p Full Set KNN (",round(knn.cm.30.full$overall[1]*100),"%)",sep=""))
fourfoldplot(knn.cm.10.full$table, color = col, conf.level = 0, margin = 1, 
             main=paste("10p Full Set KNN (",round(knn.cm.10.full$overall[1]*100),"%)",sep=""))

# Create the optimal model on the full data set
set.seed(1)
pre_knn.30.split.mod <- knn(train[,-1], test[,-1], train[,1], k=opt_k$k, prob=T)
1-mean(pre_knn.30.split.mod != test[,1])

#### KMeans
#library(factoextra)
## WSS on 30 Predictors & Raw Data
set.seed(1)
fviz_nbclust(wbcd.30.cluster, kmeans,
             nstart = 50,
             method = "wss")
# Gap Statistic on 30 Predictors & Raw Data
set.seed(1)
fviz_nbclust(wbcd.30.cluster, kmeans,
             nstart = 50,
             nboot =20,
             method = "gap_stat")
# Average Silhouette Width on 30 Predictors & Raw Data
set.seed(1)
fviz_nbclust(wbcd.30.cluster, kmeans,
             nstart = 50,
             nboot =20,
             method = "silhouette")

## WSS on 30 predictors & Scaled
set.seed(1)
fviz_nbclust(wbcd.30.scale.cluster, kmeans,
             nstart = 50,
             method = "wss")
# WSS on 30 predictors & Scaled
set.seed(1)
fviz_nbclust(wbcd.30.scale.cluster, kmeans,
             nstart = 50,
             nboot =20,
             method = "gap_stat")
# Average Silhouette Width on 30 predictors & Scaled
set.seed(1)
fviz_nbclust(wbcd.30.scale.cluster, kmeans,
             nstart = 50,
             nboot =20,
             method = "silhouette")

## WSS on 10 Predictors & Raw Data
set.seed(1)
fviz_nbclust(wbcd.10.cluster, kmeans,
             nstart = 50,
             method = "wss")
# Gap Statistic on 10 Predictors & Raw Data
set.seed(1)
fviz_nbclust(wbcd.10.cluster, kmeans,
             nstart = 50,
             nboot =20,
             method = "gap_stat")
# Average Silhouette Width on 10 Predictors & Raw Data
set.seed(1)
fviz_nbclust(wbcd.10.cluster, kmeans,
             nstart = 50,
             nboot =20,
             method = "silhouette")

## WSS on 10 predictors & Scaled
set.seed(1)
fviz_nbclust(wbcd.10.scale.cluster, kmeans,
             nstart = 50,
             method = "wss")
# WSS on 10 predictors & Scaled
set.seed(1)
fviz_nbclust(wbcd.10.scale.cluster, kmeans,
             nstart = 50,
             nboot =20,
             method = "gap_stat")
# Average Silhouette Width on 10 predictors & Scaled
set.seed(1)
fviz_nbclust(wbcd.10.scale.cluster, kmeans,
             nstart = 50,
             nboot =20,
             method = "silhouette")

### Creating Confusion Matrices for KMeans:
# Create optimal KMeans models:
# Optimal KMeans with 30 predictors & Raw Data
set.seed(1)
km.30 <- eclust(wbcd.30.cluster, FUNcluster = "kmeans", k=2, nstart=50)
# Obtain the cluster assignments from your object
km.30.raw.assign <- km.30$cluster
# Convert cluster assignments from numerical to it's categorical equivalent
km.30.raw.test <- factor(ifelse(km.30.raw.assign ==1,"Benign","Malignant"))
# Create a confusion Matrix
km.30.raw.cm <- confusionMatrix(km.30.raw.test, wbcd10$diagnosis)
km.30.raw.caption <- paste("30p Raw KMeans (",round(km.30.raw.cm$overall[1]*100),"%)",sep="")



# Do the same for the rest of the models
# Optimal KMeans with 30 predictors & Scaled Data
set.seed(1)
km.30.scale <- eclust(wbcd.30.scale.cluster, FUNcluster = "kmeans", k=2, nstart=50)
km.30.scale.assign <- km.30.scale$cluster
# Cluster 2 is Benign! Make sure to check for the correct dummy variable.
km.30.scale.test <- factor(ifelse(km.30.scale.assign ==2,"Benign","Malignant"))
km.30.scale.cm <- confusionMatrix(km.30.scale.test, wbcd$diagnosis)
km.30.scale.caption <- paste("30p Scaled KMeans (",round(km.30.scale.cm$overall[1]*100),"%)",sep="")

# Optimal KMeans with 10 predictors & Raw Data
set.seed(1)
km.10 <- eclust(wbcd.10.cluster, FUNcluster = "kmeans", k=2, nstart=50)
km.10.raw.assign <- km.10$cluster
km.10.raw.test <- factor(ifelse(km.10.raw.assign ==1,"Benign","Malignant"))
km.10.raw.cm <- confusionMatrix(km.10.raw.test, wbcd10$diagnosis)
km.10.raw.caption <- paste("10p Raw KMeans (",round(km.10.raw.cm$overall[1]*100),"%)",sep="")


# Optimal KMeans with 10 predictors & Scaled Data
set.seed(1)
km.10.scale <- eclust(wbcd.10.scale.cluster, FUNcluster = "kmeans", k=2, nstart=50)
km.10.scale.assign <- km.10.scale$cluster
km.10.scale.test <- factor(ifelse(km.10.scale.assign ==1,"Benign","Malignant"))
km.10.scale.cm <- confusionMatrix(km.10.scale.test, wbcd10$diagnosis)
km.10.scale.caption <- paste("10p Scaled KMeans (",round(km.10.scale.cm$overall[1]*100),"%)",sep="")

# Confusion Matrices:
par(mfrow=c(2,2))
fourfoldplot(km.30.raw.cm$table, color = col, conf.level = 0, margin = 1, main=km.30.raw.caption)
fourfoldplot(km.30.scale.cm$table, color = col, conf.level = 0, margin = 1, main=km.30.scale.caption)
fourfoldplot(km.10.raw.cm$table, color = col, conf.level = 0, margin = 1, main=km.10.raw.caption)
fourfoldplot(km.10.scale.cm$table, color = col, conf.level = 0, margin = 1, main=km.10.scale.caption)

### Hierarchical Clustering
hc.scale <- scale(p10)
hc.sd <- hclust(dist(hc.scale), method = "complete")
plot(hc.sd)

### SVM
# library(e1071)
# 30 predictors 4 neighbors 80/20 split
# Initialize Grid Values for CV:
gamma <- seq(0,0.1,0.005)
cost <- 2^(0:5)
deg <- c(2:4)
parms <- expand.grid(cost=cost, gamma=gamma)

set.seed(1)
# Find the best model:
svm.linear.tune.30 <- tune(svm, diagnosis~., data=train, 
                             kernel = "linear",
                             ranges = list(cost = cost))
summary(svm.linear.tune.30)
svm.linear.tune.30.predict <- predict(svm.linear.tune.30$best.model, train)
table(pred = svm.linear.tune.30.predict, true = train$diagnosis)
mean(svm.linear.tune.30.predict != train$diagnosis)
# Create the best model
svm.linear.full <- svm(diagnosis~., data=wbcd, 
                       cost=svm.linear.tune.30$best.model$cost, 
                       kernel = "linear")
svm.linear.predict <- predict(svm.linear.full, wbcd[,-1])
# Create a Confusion Matrix
svm.linear.cm <- confusionMatrix(svm.linear.predict, wbcd$diagnosis)

set.seed(1)
svm.poly.tune.30 <- tune(svm, diagnosis~., data=train, 
                           kernel = "polynomial",
                           ranges = list(cost = cost, degree = deg))
summary(svm.poly.tune.30)
svm.poly.tune.30.predict <- predict(svm.poly.tune.30$best.model, train)
table(pred = svm.poly.tune.30.predict, true = train$diagnosis)
mean(svm.poly.tune.30.predict != train$diagnosis)

svm.poly.full <- svm(diagnosis~., data=wbcd, 
                     cost=svm.poly.tune.30$best.model$cost, 
                     kernel = "polynomial",
                     degree = svm.poly.tune.30$best.model$degree)
svm.poly.predict <- predict(svm.poly.full, wbcd[,-1])
svm.poly.cm <- confusionMatrix(svm.poly.predict, wbcd$diagnosis)

set.seed(1)
svm.radial.tune.30 <- tune(svm, diagnosis~., data=train, 
                         kernel = "radial",
                         ranges = list(cost = cost, gamma = gamma))
summary(svm.radial.tune.30)
svm.radial.tune.30.predict <- predict(svm.radial.tune.30$best.model, train)
table(pred = svm.radial.tune.30.predict, true = train$diagnosis)
mean(svm.radial.tune.30.predict != train$diagnosis)

svm.radial.full <- svm(diagnosis~., data=wbcd, 
                     cost=svm.radial.tune.30$best.model$cost, 
                     kernel = "radial",
                     gamma = svm.radial.tune.30$best.model$gamma)
svm.radial.predict <- predict(svm.radial.full, wbcd[,-1])
svm.radial.cm <- confusionMatrix(svm.radial.predict, wbcd$diagnosis)

mean(svm.linear.tune.30.predict != train$diagnosis)
mean(svm.poly.tune.30.predict != train$diagnosis)
mean(svm.radial.tune.30.predict != train$diagnosis)

par(mfrow=c(1,3))
fourfoldplot(svm.linear.cm$table, color = col, conf.level = 0, margin = 1, main=paste("Linear SVM (",round(svm.linear.cm$overall[1]*100),"%)",sep=""))
fourfoldplot(svm.poly.cm$table, color = col, conf.level = 0, margin = 1, main=paste("Poly SVM (",round(svm.poly.cm$overall[1]*100),"%)",sep=""))
fourfoldplot(svm.radial.cm$table, color = col, conf.level = 0, margin = 1, main=paste("Radial SVM (",round(svm.radial.cm$overall[1]*100),"%)",sep=""))

# Run the optimal model on the entire data set
svm.opt <- svm(diagnosis~., data=wbcd, kernel="radial", gamma=0.01, cost=32)
# Find the best predictors to plot the model:
# Correlation Matrix for Mean
ggpairs(wbcd[,c(2:11,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Cancer Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

# Plot the best radial model in 2D space by using (area, radius)
plot(svm.radial.full, wbcd, area_mean~radius_mean)

# Plot the Radial Tune Out Summary:
svm.split.30.test <- numeric()
svm.split.30.accuracy1 <- NULL 
svm.split.30.accuracy2 <- NULL

for(i in 1:NROW(parms)){        
  set.seed(1)
  svm.split.30 <- svm(diagnosis~., data=train, gamma=parms$gamma[i], cost=parms$cost[i])
  svm.split.30.predict <- predict(svm.split.30, test[,-1])
  svm.split.30.accuracy1 <- confusionMatrix(svm.split.30.predict, test$diagnosis)
  svm.split.30.accuracy2[i] <- svm.split.30.accuracy1$overall[1]
}

svm.split.30.acc <- data.frame(cvTest= seq(1,NROW(parms)), Accuracy = accuracy2)
opt_p <- subset(svm.split.30.acc, Accuracy==max(Accuracy))[1,]
radial.30.split.sub <- paste("CV Test #9: Cost:", parms[126,1], ", Gamma:", parms[126,2]," (Test Error :", 1-opt_p$Accuracy,")")

#library(highcharter)
hchart(svm.split.30.acc, 'line', hcaes(cvTest, Accuracy)) %>%
  hc_title(text = "Optimal SVM Model- 80/20 Split on 30 Predictors") %>%
  hc_subtitle(text = radial.30.split.sub) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_xAxis(title = list(text = "Number of Tests for Cross Validation")) %>%
  hc_yAxis(title = list(text = "Accuracy"))


par(mfrow=c(1,3))
fourfoldplot(knn.cm.30.full$table, color = col, conf.level = 0, margin = 1, 
             main=paste("30p Full Set KNN (",round(knn.cm.30.full$overall[1]*100),"%)",sep=""))
fourfoldplot(svm.radial.cm$table, color = col, conf.level = 0, 
             margin = 1, main=paste("Radial SVM (",round(svm.radial.cm$overall[1]*100),"%)",sep=""))
fourfoldplot(km.30.raw.cm$table, color = col, conf.level = 0, 
             margin = 1, main=km.30.raw.caption)
