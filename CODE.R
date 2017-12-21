adult <- read.csv(file = "adult.txt", sep = ",", na.strings = " ?", stringsAsFactors = FALSE)
names(adult) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship","race","sex","capital_gain","capital_loss","hours_per_week","native_country","Salary")
str(adult)
sum(complete.cases(adult))
sum(complete.cases(adult))/nrow(adult)
for (i in 1:ncol(adult)){
  print(paste("Total NAs in column ",i," is ",sum(is.na(adult[,i]))))
}

sum(adult[,15]==" <=50K")
sum(adult[,15]== " >50K")

sum(adult[,15]== " >50K")/(sum(adult[,15]== " >50K")+sum(adult[,15]==" <=50K"))

adult1 <- as.data.frame(adult)
adult1$workclass <- as.factor(adult1$workclass)
adult1$education <- as.factor(adult1$education)
adult1$marital_status <- as.factor(adult1$marital_status)
adult1$occupation <- as.factor(adult1$occupation)
adult1$relationship <- as.factor(adult1$relationship)
adult1$race <- as.factor(adult1$race)
adult1$sex <- as.factor(adult1$sex)
adult1$native_country <- as.factor(adult1$native_country)
adult1$Salary <- as.factor(adult1$Salary)
adult1 <- adult1[complete.cases(adult1),]
str(adult1)
library(randomForest)
rf1 <- randomForest(Salary~.-Salary, data = adult1, mtry = 4, ntree = 50,importance = TRUE)
importance(rf1)
work <- adult[complete.cases(adult[,2]),]
incompwork <- adult[is.na(adult[,2]),]
wobs <- sample(nrow(work), 0.75*nrow(work), replace = FALSE)
work_train <- work[wobs,]
work_test <- work[-wobs,]
work_train$workclass <- as.factor(work_train$workclass)
work_train$marital_status <- as.factor(work_train$marital_status)
work_train$relationship <- as.factor(work_train$relationship)
work_test$workclass <- as.factor(work_test$workclass)
work_test$marital_status <- as.factor(work_test$marital_status)
work_test$relationship <- as.factor(work_test$relationship)
work_test$Salary <- as.factor(work_test$Salary)
str(work_test)
rf1 <- randomForest(workclass~age+fnlwgt+marital_status+relationship+capital_gain, data = work_train,ntree = 500, mtry = 3)
prd1 <- predict(rf1,work_test, type = "class")
tab1 <- table(work_test$workclass,prd1)
acc1 <- sum(diag(tab1))/sum(tab1)
str(incompwork)
incompwork$workclass <- as.factor(incompwork$workclass)
incompwork$marital_status <- as.factor(incompwork$marital_status)
incompwork$relationship <- as.factor(incompwork$relationship)
incompwork$Salary <- as.factor(incompwork$Salary)
prd2<- predict(rf1,incompwork, type = "class")
work2 <- cbind(incompwork[,-c(2)],prd2)
View(work2)
names(work2)[15] <- "workclass"
work2 <- work2[,c(1,15,2,3,4,5,6,7,8,9,10,11,12,13,14)]

library(Boruta)

boruta.train <- Boruta(Salary~.-Salary, data = new2, doTrace = 2)
print(boruta.train)


work$workclass <- as.factor(work$workclass)
work$marital_status <- as.factor(work$marital_status)
work$relationship <- as.factor(work$relationship)



str(work)
occ <- adult[complete.cases(adult[,7]),]
incomocc <- adult[is.na(adult[,7]),]
oobs <- sample(nrow(occ), 0.75*nrow(occ), replace = FALSE)
occ_train <- occ[oobs,]
occ_test <- occ[-oobs,]
occ_train$occupation <- as.factor(occ_train$occupation)
occ_train$marital_status <- as.factor(occ_train$marital_status)
occ_train$relationship <- as.factor(occ_train$relationship)
str(occ_train)
occ_test$occupation <- as.factor(occ_test$occupation)
occ_test$marital_status <- as.factor(occ_test$marital_status)
occ_test$relationship <- as.factor(occ_test$relationship)
str(occ_test)
rf2 <- randomForest(occupation~age+fnlwgt+marital_status+relationship+capital_gain, data = occ_train,ntree = 1000, mtry = 3)
prd2 <- predict(rf2,occ_test, type = "class")
tab2 <- table(occ_test$occupation,prd2)
acc2 <- sum(diag(tab2))/sum(tab2)
incomocc$occupation <- as.factor(incomocc$occupation)
incomocc$marital_status <- as.factor(incomocc$marital_status)
incomocc$relationship <- as.factor(incomocc$relationship)
prd3<- predict(rf2,work2, type = "class")
incomocc
occ2 <- cbind(work2[,-c(7)],prd3)
View(occ)
names(occ2)[15] <- "occupation"
occ2 <- occ2[,c(1,2,3,4,5,6,15,7,8,9,10,11,12,13,14)]
new <- rbind(adult1, occ2)
str(new)

levels(new$Salary) <- c(0,1)


new2 <- new[complete.cases(new),]
for (i in 1:ncol(new)){
  print(paste("Total NAs in column ",i," is ",sum(is.na(new[,i]))))
}
sum(new2[,15]==0)
sum(new2[,15]== 1)

sum(new2[,15]== 0)/(sum(new2[,15]== 1)+sum(new2[,15]==0))

library(DMwR)

new3<-SMOTE(Salary ~ ., new2, perc.over = 220,perc.under=150)



rf3 <- randomForest(Salary~.-Salary, data = new2, mtry = 5, ntree = 200, importance = TRUE)
importance(rf3)

newsam <- sample(nrow(new2), 0.75*nrow(new2), replace = FALSE) 
new_train <- new2[newsam,]
new_test <- new2[-newsam,]


rfm1 <- randomForest(Salary~age+fnlwgt+marital_status+occupation+relationship+capital_gain, data = new_train, mtry = 2, ntree = 1000)
prdrf <- predict(rfm1, new_test, type = "class")

View(prdrf)
tabrf <- table(new_test$Salary, prdrf)
tabrf
accrf <- sum(diag(tabrf))/sum(tabrf)

library(pROC)
library(ROCR)

str(prdrf)
roc <- roc(as.numeric(new_test$Salary),as.numeric(prdrf))
plot(roc, type = "b")
auc(roc)

rf <- function(ntree = 250, mtry = 4){
  require(randomForest)
  rfm <- randomForest(Salary~age+fnlwgt+marital_status+occupation+relationship+capital_gain, data = new_train, mtry = mtry, ntree = ntree)
  require(pROC)
  
  prd <- predict(rfm,new_test, type = "class")
  tab <- table(new_test$Salary, prd)
  acc <- sum(diag(tab))/sum(tab)
  rc <- roc(as.numeric(new_test$Salary),as.numeric(prd))
  return(print(paste("The Accuracy for Random Forest Model is: ",acc," And the Area under the curve is ", auc(rc))))
}

rf2 <- function(ntree = 250, mtry = 4){
  require(randomForest)
  rfm <- randomForest(Salary~age+fnlwgt+marital_status+occupation+relationship+capital_gain, data = new_train, mtry = mtry, ntree = ntree)
  require(pROC)
  
  prd <- predict(rfm,new_test, type = "class")
  tab <- table(new_test$Salary, prd)
  acc <- sum(diag(tab))/sum(tab)
  rc <- roc(as.numeric(new_test$Salary),as.numeric(prd))
  return(plot(rc, type = "b"))
}

varImpPlot(rfm1)



for (i in 2:5){
  require(randomForest)
  rfm <- randomForest(Salary~age+fnlwgt+marital_status+occupation+relationship+capital_gain, data = new_train, mtry = i, ntree = 500)
  require(pROC)
  prd <- predict(rfm,new_test, type = "class")
  tab <- table(new_test$Salary, prd)
  acc <- sum(diag(tab))/sum(tab)
  rc <- roc(as.numeric(new_test$Salary),as.numeric(prd))
  print(paste("The Accuracy for Random Forest Model is: ",acc," And the Area under the curve is ", auc(rc)))
}

tree <- function(ntree = NULL,mtry= NULL){
  treeModel<-rpart(Salary~age+fnlwgt+marital_status+occupation+relationship+capital_gain, method="class", data=new_train)
  pred <- predict(treeModel, new_test, type = "class")
  tb <- table(new_test$Salary, pred)
  acc <- sum(diag(tb))/sum(tb)
  rc <- roc(as.numeric(new_test$Salary),as.numeric(pred))
  return(c(plot(treeModel),text(treeModel)))
}

tree2 <- function(ntree = NULL,mtry= NULL){
  treeModel<-rpart(Salary~age+fnlwgt+marital_status+occupation+relationship+capital_gain, method="class", data=new_train)
  pred <- predict(treeModel, new_test, type = "class")
  tb <- table(new_test$Salary, pred)
  acc <- sum(diag(tb))/sum(tb)
  rc <- roc(as.numeric(new_test$Salary),as.numeric(pred))
  return(print(paste("The Accuracy for Decision Tree Model is: ",acc," And the Area under the curve is ", auc(rc))))
}

install.packages("e1071")
library(e1071)

library(rpart)

nb1 <- function(ntree= NULL,mtry= NULL){
  nbModel<-naiveBayes(Salary~age+fnlwgt+marital_status+occupation+relationship+capital_gain, data = new_train)
  pred <- predict(nbModel, new_test, type = "class")
  tb <- table(new_test$Salary, pred)
  acc <- sum(diag(tb))/sum(tb)
  rc <- roc(as.numeric(new_test$Salary),as.numeric(pred))
  return(print(paste("The Accuracy for Naive Bayes Model is: ",acc," And the Area under the curve is ", auc(rc))))
  
}


nb2 <- function(ntree= NULL,mtry= NULL){
  nbModel<-naiveBayes(Salary~age+fnlwgt+marital_status+occupation+relationship+capital_gain, data = new_train)
  pred <- predict(nbModel, new_test, type = "class")
  tb <- table(new_test$Salary, pred)
  acc <- sum(diag(tb))/sum(tb)
  rc <- roc(as.numeric(new_test$Salary),as.numeric(pred))
  return(plot(rc))
  
}


library(shiny)
ui <- fluidPage(sliderInput(inputId = "ntree", label = "Enter the Number of Trees", min = 100, max = 1000, value = 250),
                sliderInput(inputId = "mtry", label = "Enter the value of Mtry", min = 2, max = 6, value = 3),
                plotOutput(outputId = "roc"),
                textOutput(outputId = "text"),
                radioButtons("algo", "Choose the Algorithm:",
                             c("Random Forest" = "rf",
                               "Naive Bayes" = "nb",
                               "Decision Trees" = "dt")))
server <- function(input,output){
  output$roc <- renderPlot({ algo <- switch(input$algo,
                                            rf = rf2,
                                            dt = tree, nb = nb2)
  
  algo(input$ntree,input$mtry)})
  output$text <- renderPrint({algo <- switch(input$algo,
                                             rf = rf,
                                             dt = tree2, nb = nb1)
  
  algo(input$ntree,input$mtry)})
}
shinyApp(ui = ui, server = server)









