# Manual feature selection
#
# @author: Thanh Nguyen
# Apr, 2017

train_data <- read.csv('data/train_no_na.csv')
test_data <- read.csv('data/test_no_na.csv')

# Created list of useless features by human judgement

# MSSubClass is the type of dwelling which domain is ordinal, but it does make no sense as higher values 
# (e.g. 30	1-STORY 1945 &amp; OLDER and 40	1-STORY W/FINISHED ATTIC ALL AGES) do not imply higher quality
# that their sale prices say

rm_variables <- c("mssubclass")

# "Street", "Alley", "Utilities" may be useless as well, but it can be removed due to their near-zero variances
rm_variables <- c(rm_variables, "street", "alley", "utilities", "condition1", "condition2")

# "Condition1" and "Condition2"

# Month, Year sale make no sense too. If any, knowing inflation rate may help something but not pratical
rm_variables <- c(rm_variables, "mosold", "yrsold")

# New data

train_data[, rm_variables] <- NULL
test_data[, rm_variables] <- NULL
