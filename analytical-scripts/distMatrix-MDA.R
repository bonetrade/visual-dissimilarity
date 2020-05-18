
# Load packages

library(mda)
library(ggplot2)

# Load input data

skulls <- read.csv("../raw-data/square-matrix-results.csv", header = TRUE, sep=",")

row.names(skulls) <- skulls[, 1] # assign labels as the row names

# The data contains categorical variables (sex, origin, provenance, split, sample)
# and the squared distance matrix of all skulls versus all skulls.

# set criterion for filtering the distance matrix
distMatrixColumns <- grep("ref", names(skulls))

# set criterion for filtering by provenance status
isSecured <- skulls$provenance == "secure provenance"

# set criterion for training set distances
refColumns <- c(match("origin", names(skulls)), distMatrixColumns[isSecured])

# exclude one so mda doesn't generate a row of NAs
refColumns <- refColumns[-length(refColumns)]

# creating a loose variables for color-coded categories
color.category <- factor(
  ifelse(isSecured, 
         as.character(skulls$origin), 
         "not confirmed"),
  levels = c(levels(skulls$origin), "not confirmed")
)

color.category.fine <- factor(
  ifelse(isSecured,
         as.character(skulls$origin),
         paste("NC:", skulls$origin)),
  levels = c(levels(skulls$origin), paste("NC:", levels(skulls$origin)))
)

# plot first 6 columns of matrix to show that they are not correlated
png(filename = paste0("output/pairs-dist-matrix.png"), 
    width = 1000, height = 1000)

pairs(skulls[, refColumns[2:7]],
      col = c(rainbow(nlevels(color.category) - 1), "black")[color.category],
      pch = ifelse(isSecured, 19, 3),
      oma=c(10,3,3,3), cex.labels = 1.2)
par(xpd = TRUE)
legend("bottomleft", legend = levels(color.category), fill = c(rainbow(nlevels(color.category) - 1), "black"), horiz = T)
legend("bottomright", legend = levels(skulls$provenance), pch = c(3, 19), horiz = T)

dev.off()

# because variable names (observation labels) can be too long for mda to handle,
# we have to keep track of code names for all of them

# refNamesTable <- data.frame(
#   code = paste0("v", 1:(length(refColumns) - 1)),
#   name = names(skulls)[refColumns[-1]]
# )
# refNamesTable$code <- as.character(refNamesTable$code)
# refNamesTable$name <- as.character(refNamesTable$name)
# 
# names(skulls)[refColumns[-1]] <- refNamesTable$code 

# export operational dataset (distances to the "references" or 
# the skulls with confirmed origin, minus one)
write.csv(skulls[, c(1:(distMatrixColumns[1] - 1), distMatrixColumns[isSecured])], 
          file = "output/operational-data.csv")

# NOTE: with this approach, we are not using the distances to the unprovenanced skulls.

# MDA ============================================================================

# fit model

setRNG::setRNG(seed = 0)
model.mda <- mda(origin ~ .,
  # as.formula(paste("origin", 
  #                  paste(names(skulls[, refColumns])[c(-1)], collapse=" + "), 
  #                  sep=" ~ ")), 
  data = skulls[isSecured, refColumns], 
  iter = 2000,
  trace = F) # activate this to see deviance of each iteration

model.mda

# using 'mda' package to plot the distribution of subgroups in canonical space 
# (similar to PCA in that it is reducing the dimensionality of data)
# see: https://rdrr.io/cran/mda/man/plot.fda.html
png(filename = "output/model.dist-mda.png", width = 1000, height = 1200)
layout(matrix(c(5, 6, 1, 2, 3, 4), ncol = 2, byrow = TRUE), heights = c(1, 20, 20))
par(cex = 1.5)

plot(model.mda, skulls[isSecured, refColumns])
plot(model.mda, skulls[, refColumns])
plot(model.mda, skulls[isSecured, refColumns], group = "predicted")
plot(model.mda, skulls[, refColumns], group = "predicted")

par(cex = 2, mar = rep(0, 4))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.55, y = 0.5, font = 4, cex = 1.2,
     labels = "training data")
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.55, y = 0.5, font = 4, cex = 1.2,
     labels = "all data")

dev.off()

# summarize the fit for secure
summary(model.mda)

# make predictions
predicted.train <- predict(model.mda, skulls[isSecured, refColumns])

# summarize accuracy
model.fitness <- 
  table(predicted.train, skulls[isSecured, "origin"]) # or skulls[isSecured,]$origin

model.fitness # perfect model fitness using n - 1 dimensions

# Make predictions

predicted.test <- predict(model.mda, skulls[!isSecured, refColumns])

# summarize accuracy
socmed.predictions <- table(predicted.test, skulls[!isSecured, "origin"])

# Match between model predicted and social media claim origins
mean(predicted.test == skulls[!isSecured, "origin"])

# compare model fitness and match with social media claims
#======================================
model.fitness
#======================================
socmed.predictions
#======================================

# confusion matrix plot
# #==================================
df <- as.data.frame(socmed.predictions)
names(df) <- c("x", "y", "Freq")

png(filename = "output/dist-mda-confusion-matrix.png", width = 800, height = 800)

ggplot(data =  df, mapping = aes(x = x, y = y)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1, size = 10) +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("Predicted") + ylab("Given in Social media") +
  theme_bw() + theme(legend.position = "none", text = element_text(size = 40))

dev.off()


