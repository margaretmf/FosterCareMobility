
# set working directory
setwd("~/Desktop/Mobility in Foster Care Children")

# libraries
library(readr)
library(randomForest)
library(rpart)

# spell and placement data
spell <- read.csv("spell_clean.csv")
placement <- read.csv("rp_placements_clean.csv")
removal <- read.csv("rp_removal_clean.csv")

# merge data
spells <- merge(merge(spell[,c(2,11:15,40,45,46,34,52:62)],placement[,c(2,7,14,40)],by.x="DWID",by.y="MCI_ID"),
                removal[,c(1,2,7)],by.x=c("CLIENT_ID","HOME_RMVL_SEQ_NBR"),by.y=c("CL_ID","SEQ_NBR"))

# group exits by desirability
ix <- which(spells$EXIT%in% c("XRF","XLC","XRL","XCA") )
ix2 <- which(spells$EXIT%in% c("XOT","XOP","XRM","XRY","XJP") )
ix3 <- which(spells$EXIT%in% c("ZTC","XUK") )

# create new col for move reason desirability
spells$MoveReasonDesirability <- "NA"
spells$MoveReasonDesirability[ix] <- "Good"
spells$MoveReasonDesirability[ix2] <- "Bad"
spells$MoveReasonDesirability[ix3] <- "Unknown"

# exclude unknowns
spells <- spells[spells$MoveReasonDesirability!="Unknown",]
spells <- spells[,c(4:9,11:23,25:26)]

# categorial variables as factors
for (i in 1:4) {
  spells[,i] <- as.factor(spells[,i])
}
spells[,7] <- as.factor(spells[,7])
for (i in 20:21) {
  spells[,i] <- as.factor(spells[,i])
}

# randomize the order of the vector
randomized_spells <- spells[sample(1:nrow(spells)),]
# training set
training_set <- randomized_spells[1:(nrow(randomized_spells)*.8),]
# testing set
testing_set <- randomized_spells[(nrow(randomized_spells)*.8+1):nrow(randomized_spells),]

# random forest model built on training set
set.seed(4543)
spells.rf <- randomForest(MoveReasonDesirability ~ ., data=training_set[,c(1:6,8:21)], ntree=50,
                          keep.forest=TRUE,importance=TRUE,doBest=TRUE)
print(spells.rf)
data.frame(importance(spells.rf))[order(data.frame(importance(spells.rf))[,3],decreasing=TRUE),]
varImpPlot(spells.rf) 

# variables with high MeanDecreaseGini: DURAT,STARTAGE, P_SG, RMVL_REASON
# variables with high MeanDecreaseAccuracy: DURAT, RMVL_REASON, NPLACES
# DURAT, STARTAGE, P_SG, RMVL_REASON, NPLACES
spells.rf2 <- randomForest(MoveReasonDesirability ~., data=training_set[,c(6,5,17,20,8,21)],
                           ntree=50,keep.forest=TRUE,importance=TRUE,doBest=TRUE)
data.frame(importance(spells.rf2))[order(data.frame(importance(spells.rf2))[,4],decreasing=TRUE),]

# predict on testing set
spellsrf.pred2 <- predict(spells.rf2, testing_set[,c(6,5,7,17,20,2,8,12,21)], type="response",
                         norm.votes=TRUE)
summary(spellsrf.pred2)

# accuracy rate
accuracy_rate <- (table(spellsrf.pred,testing_set$MoveReasonDesirability)[1,1]+
                    table(spellsrf.pred,testing_set$MoveReasonDesirability)[2,2])/nrow(testing_set)*100
print(accuracy_rate) # 92% accuracy rate

# NEXT STEP: cross validation, check duration vs other factors
# DURATION vs. OUTCOME


