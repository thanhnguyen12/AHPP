
# ---------------- Summary of work done in this code ----------------------

# Factors with known values from the data description were replaced.
# That leaves us with lotfrontage, garage year built, masonry veneer/area and electrical
# Its probably best to replace NA's with 0 for lotfrontage, assuming the property is not
# connected to a street. We might want to create new variables to describe the garage and
# I replaced masonry veneer type with no veneer and area 0, the mode and median
# the final cleaned dataset is named "ames"

library(tidyverse)
library(caret)

setwd("~/Git/r")

# this assumes there is a data folder in your working directory
# ask thanh about naming and location conventions

ames <- read.csv("../data/train.csv", stringsAsFactors = F)
ames <- ames %>% as_data_frame()
names(ames) <- tolower(names(ames))
ames$saleprice <- log(ames$saleprice)


glimpse(ames)

ames$bsmtfinsf1 <- as.numeric(ames$bsmtfinsf1)
ames$bsmtfinsf2 <- as.numeric(ames$bsmtfinsf2)

dim(ames)

sort(colSums(is.na(ames)), decreasing = T)


# some basement qualities actually have NA's
# houses with id 333 and 949 have an NA value for a basement feature
# not sure what to do with those



# the following code replaces NA's that have known values from the data description

repl.ls <- list(bsmtqual = "nobsmt", bsmtcond = "nobsmt", bsmtexposure = "nobsmt",
                bsmtfinsf1 = "nobsmt", bsmtfinsf2 = "nobsmt", bsmtfintype1 = "nobsmt",
                bsmtfintype2 = "nobsmt", fireplacequ = "nofire", alley = "noalley",
                garagetype = "nogar", garagefinish = "nogar",
                garagequal = "nogar", garagecond = "nogar", poolqc = "nopool", 
                fence = "nofence", miscfeature = "none")


ames[ ,names(repl.ls)] <- ames[ ,names(repl.ls)] %>% replace_na(repl.ls)


sort(colSums(is.na(ames)), decreasing = T)



# now we have just four variables with NA's


summary(ames$lotfrontage)
summary(ames$masvnrarea)
summary(ames$garageyrblt)

boxplot(ames$lotfrontage, main = "lot frontage")
boxplot(ames$garageyrblt, main = "garage year")
boxplot(ames$masvnrarea, main = "masvnarea")


barchart(ames$electrical)
barchart(ames$masvnrtype)

#---------------- lot frontage ---------------------


# perhaps an NA in lotfrontage really means 0?
# or perhaps they just skipped measuring it and we should replace with median

# code for both follows

# replace with median

    # lotfrontage.med  <- median(ames$lotfrontage[!is.na(ames$lotfrontage)])
    # ames[,"lotfrontage"][is.na(ames$lotfrontage),] <- lotfrontage.med


# replace with 0

     ames[,"lotfrontage"][is.na(ames$lotfrontage),] <- 0


# ---------------- garage year built ------------------------

# We  have a quantitative variable (year) but 81 houses do not have garages
# What to do with NA's? I think we should create a variable "garage" that is yes/no
# or create a categorical variable with categories for years
# and possibly a variable that is how many cars the garage fits



# ----------------- masonry veneer ---------------------------

# this code replaces veneer type with the mode (none) and veneer area with 0

ames$masvnrtype[is.na(ames$masvnrtype)] <- "None"
ames$masvnrarea[is.na(ames$masvnrarea)] <- 0

unique(ames$masvnrtype) # check that the capital N in 'none' matches

sort(colSums(is.na(ames)), decreasing = T)


# if you just want to run models without worry about NA's you can run the following command

# ames <- ames %>% na.omit()
