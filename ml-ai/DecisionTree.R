library(C50)
#library(caret)
library(mltools)


tab <- read.table(file = "lymphography.data", sep = ",", header = TRUE)
podzial <- folds(tab$Klasa, nfolds = 10, stratified = TRUE)
tab <- cbind(tab, podzial)

for (i in 1:1)
{
  trenig <- tab [tab$podzial != i,]
  test <- tab [tab$podzial == i,]
  
  
}

  b <-as.factor(trenig[,c(19)])
  
  drzewo <- C5.0(x = trenig[,-c(19, 20)], y = as.factor(trenig[, c(19)]))
 # plot(drzewo)

  
  predykcja <- predict(drzewo, test[,-c(20)])
  wyniki <- cbind(test, predykcja)
  pomylki <- table(predicted = predykcja, actual = test$Klasa)
  
  
  poprawne <- pomylki[1,1]+pomylki[2,2]
  niepoprawne <- pomylki[1,2]+pomylki[2,1]
  
  blad <- (niepoprawne/(poprawne+niepoprawne))*100  # liczenie błędu procentowo
  cat("Błąd wynosi",blad,"% \n")
  
 
  
  


