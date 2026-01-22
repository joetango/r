### (a)

college <- read.csv("phc6097/College.csv")

### (b)

rownames(college) <- college[, 1]
college <- college[, -1]
View(college)

### (c)

  ## i. 
  summary(college)

  ##ii.
  pairs(college[,2:11])
