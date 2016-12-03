
# set working directory
setwd("~/Desktop/Mobility in Foster Care Children")

# libraries
library(readr)
library(randomForest)

# spell and placement data
spell <- read.csv("spell_clean.csv")
placement <- read.csv("rp_placements_clean.csv")
removal <- read.csv("rp_removal_clean.csv")

# merge data
spells <- merge(merge(spell[,c(2,11:15,45)],placement[,c(2,7,14,40)],by.x="DWID",by.y="MCI_ID"),
                removal[,c(1,2,7)],by.x=c("CLIENT_ID","HOME_RMVL_SEQ_NBR"),by.y=c("CL_ID","SEQ_NBR"))
spells <- spells[,c(3:8,11,9,10)]

# group exits by desirability
ix <- which(spells$EXIT%in% c("XRF","XLC","XRL","XCA") )
ix2 <- which(spells$EXIT%in% c("XOT","XOP","XRM","XRY","XJP") )
ix3 <- which(spells$EXIT%in% c("ZTC","XUK") )

# create new col for move reason desirability
spells$MoveReasonDesirability <- "NA"
spells$MoveReasonDesirability[ix] <- "Good"
spells$MoveReasonDesirability[ix2] <- "Bad"
spells$MoveReasonDesirability[ix3] <- "Unknown"

# categorial variables as factors
for (i in 2:5) {
  spells[,i] <- as.factor(spells[,i])
}
for (i in 7:10) {
  spells[,i] <- as.factor(spells[,i])
}

# randomize the order of the vector
randomized_spells <- spells[sample(1:nrow(spells)), ]
# training set
training_set <- randomized_spells[1:(nrow(randomized_spells)*.8),c(2:7,10)]
# testing set
testing_set <- randomized_spells[(nrow(randomized_spells)*.8+1):nrow(randomized_spells),c(2:7,10)]

# random forest model with training set
set.seed(4543)
spells.rf <- randomForest(MoveReasonDesirability ~ ., data=training_set[,1:7], ntree=1000,
                          keep.forest=TRUE, importance=TRUE)
importance(spells.rf, type=1)

# predict on testing set
spellsrf.pred <- predict(spells.rf, testing_set[,1:7], type="response",
                       norm.votes=TRUE)
summary(spellsrf.pred)

# accuracy rate
(table(spellsrf.pred,testing_set$MoveReasonDesirability)[1,1]+
  table(spellsrf.pred,testing_set$MoveReasonDesirability)[2,2]+
  table(spellsrf.pred,testing_set$MoveReasonDesirability)[3,3])/nrow(testing_set) # accuracy rate = 0.65

# NEXT STEPS: 5-fold cross validation with this data, then include # of placements per spell
# and P_AL, P_FC, P_GH, P_IL, P_KC, P_RC, P_RT, P_SF, P_SG, P_SK, P_UK
