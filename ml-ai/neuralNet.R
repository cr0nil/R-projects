library(neuralnet)

data <- read.table("lymphography.csv",  header = TRUE,sep = ",")

scaleddata <- scale(data)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf <- as.data.frame(lapply(data, normalize))

myfun <- function(x) {
  tmp <- x
  cat(tmp)
  return(tmp)
}

samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample(seq_len (nrow (data)), size = samplesize)

# Create training and test set
datatrain = maxmindf[1:89, ]
datatest = maxmindf[89:148, ]

#df[nrow(df) + 1,] = c("v1","v2")
trainNN = maxmindf[1:110, ]
testNN = maxmindf[110:148, ]

set.seed(3)
NN = neuralnet(
  Klasa ~ limfatyczne + blokada + blLimfyC + blLimfyS + bypass + wynaczynienia +
    regeneracja + wczesneWch + zwezlyLimm + lym + zmianyLym + wadaWwezle + zminayWwezle +
    zmainyStru + formySpec + zwichniecie + wykluczenie + nieWezlowW,
  trainNN,
  hidden = 3 ,
  linear.output = TRUE
)
NN$result
# plot neural network
plot(NN)
predict_testNN = compute(NN, testNN[, c(1:18)])
predict_testNN2 = (predict_testNN$net.result * (max(data$Klasa) - min(data$Klasa))) + min(data$Klasa)
round(predict_testNN2)

results <- data.frame(actual = testNN$Klasa, prediction = predict_testNN$net.result)
actual=results$actual * abs(diff(range(data$Klasa))) + min(data$Klasa)

deviation=((actual-predict_testNN2)/actual)

accuracy=1-abs(mean(deviation))
cat("Acuracy",accuracy,"%")
