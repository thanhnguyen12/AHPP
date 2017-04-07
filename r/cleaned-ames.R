
# ---------------- Summary of work done in this code ----------------------

# Factors with known values from the data description were replaced.
# That leaves us with lotfrontage, garage year built, masonry veneer/area and electrical
# Its probably best to replace NA's with 0 for lotfrontage, assuming the property is not
# connected to a street. We might want to create new variables to describe the garage and
# I replaced masonry veneer type with no veneer and masonry area with 0, the respective
# mode and median. The final cleaned dataset is named "ames"

library(tidyverse)
library(caret)

# this assumes there is a data folder in the parent folder of your working directory
# ask thanh about naming and location conventions

ames <- read.csv("../data/train.csv", stringsAsFactors = F)
ames <- ames %>% as_data_frame()
names(ames) <- tolower(names(ames))
ames$saleprice <- log(ames$saleprice)


glimpse(ames)
dim(ames)


sort(colSums(is.na(ames)), decreasing = T)


# some basement qualities actually have NA's
# houses with id 333 and 949 have an NA value for a basement feature
# not sure what to do with those



# the following code replaces NA's that have known values from the data description

repl.ls <- list(bsmtqual = "nobsmt", bsmtcond = "nobsmt", bsmtexposure = "nobsmt",
                bsmtfintype1 = "nobsmt", bsmtfintype2 = "nobsmt", 
                fireplacequ = "nofire", alley = "noalley",
                garagetype = "nogar", garagefinish = "nogar",
                garagequal = "nogar", garagecond = "nogar", poolqc = "nopool", 
                fence = "nofence", miscfeature = "none")


ames[ ,names(repl.ls)] <- ames[ ,names(repl.ls)] %>% replace_na(repl.ls)


sort(colSums(is.na(ames)), decreasing = T)



# now we have just five variables with NA's, lotfrontage, garage year built,
# masonry area, masonry type, and electrical. Electrical only has 1 na so we can
# probably just remove it

barchart(ames$electrical)



#---------------- lot frontage ---------------------

summary(ames$lotfrontage)
boxplot(ames$lotfrontage, main = "lot frontage")

# perhaps an NA in lotfrontage really means 0?
# or perhaps they just skipped measuring it and we should replace with median

# code for both follows

# replace with median

    # lotfrontage.med  <- median(ames$lotfrontage[!is.na(ames$lotfrontage)])
    # ames[,"lotfrontage"][is.na(ames$lotfrontage),] <- lotfrontage.med


# replace with 0

ames[,"lotfrontage"][is.na(ames$lotfrontage),] <- 0



# ---------------- garage year built ------------------------

summary(ames$garageyrblt)
boxplot(ames$garageyrblt, main = "garage year")


# We  have a quantitative variable (year) but 81 houses do not have garages
# What to do with NA's? I'm going to turn garageyrblt into a categorical variable
# to capture the age-related information while allowing us to account for no garage
# I will just bin the ages by quartile and replace NA's with 'nogar'

brkyr <- c(1899, 1961, 1980, 2002, 2011)
yrlabel <- c("pre1961","pre1980","pre2002","pre2010")


ames$garageyrblt <- as.character(cut(ames$garageyrblt, breaks = brkyr, label = yrlabel))
ames[,"garageyrblt"][is.na(ames$garageyrblt),] <- "nogar"


# code to check things add up
sum(ames$garageyrblt == "nogar")
unique(ames$garageyrblt)




# ----------------- masonry veneer ---------------------------

# this code replaces veneer type with the mode (none) and veneer area with 0

summary(ames$masvnrarea)
boxplot(ames$masvnrarea, main = "masvnarea")
barchart(ames$masvnrtype)


ames$masvnrtype[is.na(ames$masvnrtype)] <- "None"
ames$masvnrarea[is.na(ames$masvnrarea)] <- 0



# finally we can remove the one observation where the electric system is unknown

ames <- ames %>% na.omit()

sort(colSums(is.na(ames)), decreasing = T)
dim(ames)
glimpse(ames)

# The above code does it's best to retain as much information as possible from the original
# dataset. Now we can focus on feature engineering, dimension reduction, or simply fitting models
# We will probably need to wrap this code up in a function and apply it to the test dataframe


#
#
# ------------------------------- test data -------------------------------------
#
#


test <- read.csv("../data/test.csv", stringsAsFactors = F)

test <- test %>% as_data_frame()
names(test) <- tolower(names(test))
sort(colSums(is.na(test)), decreasing = T)

# replace known
# some issues with this

repl.ls <- list(bsmtqual = "nobsmt", bsmtcond = "nobsmt", bsmtexposure = "nobsmt",
                bsmtfintype1 = "nobsmt", bsmtfintype2 = "nobsmt", 
                fireplacequ = "nofire", alley = "noalley", saletype = "WD",
                garagetype = "nogar", garagefinish = "nogar", bsmtunfsf = 0,
                garagequal = "nogar", garagecond = "nogar", garagecars = 0,
                garagearea = 0, poolqc = "nopool", fence = "nofence",
                miscfeature = "none", mszoning = "RL", utilities = "AllPub",
                bsmtfullbath = 0, bsmthalfbath = 0, functional = "Typ",
                exterior1st = "VinylSd", exterior2nd = "VinylSd", bsmtfinsf1 = 0,
                bsmtfinsf2 = 0, totalbsmtsf = 0, kitchenqual = "TA")


# I filled in all the missing values with the modes read from the below barcharts

barchart(test$saletype)
barchart(test$mszoning)
barchart(test$utilities)
barchart(test$functional)
barchart(test$exterior1st)
barchart(test$exterior2nd)
barchart(test$kitchenqual)

test[ ,names(repl.ls)] <- test[ ,names(repl.ls)] %>% replace_na(repl.ls)


test$lotfrontage[is.na(test$lotfrontage)] <- 0

test$masvnrtype[is.na(test$masvnrtype)] <- "None"
test$masvnrarea[is.na(test$masvnrarea)] <- 0

# this takes care of an error where 2007 was entered as 2207

brkyr <- c(1890, 1961, 1980, 2002, 2300)
yrlabel <- c("pre1961","pre1980","pre2002","pre2010")


test$garageyrblt <- as.character(cut(test$garageyrblt, breaks = brkyr, label = yrlabel))
test$garageyrblt[is.na(test$garageyrblt)] <- "nogar"


sort(colSums(is.na(test)), decreasing = T)

### fit a model and make submission 

tc <- trainControl(method = "repeatedcv", number = 5, repeats = 2)

gbm <- caret::train(saleprice ~ ., data=ames, method = "gbm", trControl = tc)




submission <- data.frame(id =test$id, saleprice =exp(predict(gbm, newdata=test)))

write.csv(submission, "01_submission.csv", row.names = F)
