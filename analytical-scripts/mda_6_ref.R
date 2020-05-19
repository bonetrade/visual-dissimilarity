library(caret) # this package is required to use 'preProcess()'
library(klaR)
library(magrittr)
library(pander)
library(MASS)
library(dplyr)
library(ggplot2)
library(mda) # best place every library/require commands at the start

skulls <- read.csv("../raw-data/input-table.csv", header = TRUE, sep=",")

isSecured <- skulls$provenance == "secure provenance"
# However, this way can be difficult to apply while using dplyr 'pipeline' structure

# In this case, it is also a good idea to define in a single place the reference 
# to the variables holding the quantitative data, to avoid 'magic numbers' down below.
# Using variable name instead of index give readers a better sense of what they are.
distancesToRefs <- c("ref.female.afr.american", 
                     "ref.female.asian", 
                     "ref.female.european", 
                     "ref.male.afr.american",
                     "ref.male.asian",
                     "ref.male.european")

# It's always good practice to visualise the data first
pairs(skulls[, distancesToRefs]) 

# creating a loose variable for color-coded categories
color.category <- factor(
  ifelse(isSecured, 
         as.character(skulls$origin), 
         "not confirmed"),
  levels = c(levels(skulls$origin), "not confirmed")
)

#==================================
pairs(skulls[, distancesToRefs],
      col = rainbow(nlevels(color.category))[color.category],
      pch = ifelse(isSecured, 19, 3),
      oma=c(10,3,3,3))
par(xpd = TRUE, cex = 0.7)
legend("bottom", legend = levels(color.category), fill = rainbow(nlevels(color.category)), horiz = T)
#==================================

# fit model

setRNG::setRNG(seed = 0)
fit <- mda(
  origin ~ 
    ref.female.afr.american + 
    ref.female.asian + 
    ref.female.european + 
    ref.male.afr.american + 
    ref.male.asian + 
    ref.male.european, 
  data = skulls[isSecured,], # I can't help using the classic R way of filtering instead of dplyr
  iter = 2000,
  trace = F) # activate this to see deviance of each iteration

fit

# summarize the fit for secure
summary(fit)

# make predictions
predicted.train <- predict(fit, skulls[isSecured, distancesToRefs])

# summarize accuracy
model.fitness <- 
  table(predicted.train, skulls[isSecured, "origin"]) # or skulls[isSecured,]$origin

skulls_scaled <- cbind(
  setNames(
    data.frame(scale(skulls[, distancesToRefs])), # data.frame is needed because scale() returns a matrix
    paste0("scaled_", distancesToRefs) # these are the names of the new scaled variables
  ),
  origin = skulls$origin
)

# keep reference of new scaled variables
scaledDistancesToRefs <- paste0("scaled_", distancesToRefs)

setRNG::setRNG(seed = 0)
train.mda <- mda(
  origin ~ 
    scaled_ref.female.afr.american + 
    scaled_ref.female.asian + 
    scaled_ref.female.european + 
    scaled_ref.male.afr.american + 
    scaled_ref.male.asian + 
    scaled_ref.male.european, 
  data = skulls_scaled[isSecured,], # I can't help using the classic R way of filtering instead of dplyr
  iter = 2000,
  trace = F) # activate this to see deviance of each iteration

train.mda

# Make predictions
#predicted.classes <- train.mda %>% predict(skulls[])
predicted.test <- predict(fit, skulls[!isSecured, distancesToRefs])

# summarize accuracy
socmed.predictions <- table(predicted.test, skulls[!isSecured, "origin"])
socmed.predictions

# Match between model predicted and social media claim origins
mean(predicted.test == skulls[!isSecured, "origin"])

#### append predictions
#test.transformed$predictions <- predicted.classes

# compare model fitness and match with social media claims
model.fitness
#======================================
socmed.predictions
#======================================

# confusion matrix plot
df <- as.data.frame(socmed.predictions)
names(df) <- c("x", "y", "Freq")

ggplot(data =  df, mapping = aes(x = x, y = y)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("Predicted") + ylab("Given in Social media") +
  theme_bw() + theme(legend.position = "none")


skulls$confirmed.origin <- ifelse(isSecured,
                                  predicted.train == skulls$origin[isSecured],
                                  predicted.test == skulls$origin[!isSecured])

write.csv(skulls, file = "output/input-table-with-predicted.csv")


#---
# fit model

setRNG::setRNG(seed = 0)
model.mda <- mda(
  origin ~ 
    ref.female.afr.american + 
    ref.female.asian + 
    ref.female.european + 
    ref.male.afr.american + 
    ref.male.asian + 
    ref.male.european, 
  data = skulls[isSecured,], # I can't help using the classic R way of filtering instead of dplyr
  iter = 2000,
  trace = F) # activate this to see deviance of each iteration

model.mda

# using 'mda' package to plot the distribution of subgroups in canonical space 
# (similar to PCA in that it is reducing the dimensionality of data)
# see: https://rdrr.io/cran/mda/man/plot.fda.html
png(filename = "output/model.mda.png", width = 1000, height = 1200)
layout(matrix(c(5, 6, 1, 2, 3, 4), ncol = 2, byrow = TRUE), heights = c(1, 20, 20))
par(cex = 1.5)

plot(model.mda, skulls[isSecured,])
plot(model.mda, skulls)
plot(model.mda, skulls[isSecured,], group = "predicted")
plot(model.mda, skulls, group = "predicted")

par(cex = 2, mar = rep(0, 4))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.55, y = 0.5, font = 4, cex = 1.2,
     labels = "training data")
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.55, y = 0.5, font = 4, cex = 1.2,
     labels = "all data")

dev.off()
