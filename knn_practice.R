setwd("C:/BAIS Summer/Machine-Learning-with-R-datasets-master/Machine-Learning-with-R-datasets-master")
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
wbcd[1,]
str(wbcd)
wbcd <- wbcd[-1]
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
install.packages("class")
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred<-knn(train = wbcd_train, test = wbcd_test,cl= wbcd_train_labels,k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)
