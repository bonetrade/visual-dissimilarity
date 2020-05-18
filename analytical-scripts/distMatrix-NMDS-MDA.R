
# Load packages

# required for Non-metric Multidimensional Scaling with Stable Solution 
# from Random Starts, Axis Scaling and Species Scores: metaMDS()
library(vegan) 

library(biplot2d3d)

library(mda)
library(ggplot2)

# Load input data

skulls <- read.csv("raw-data/square-matrix-results.csv", header = TRUE, sep=",")

row.names(skulls) <- skulls[, 1] # assign labels as the row names

# The data contains categorical variables (sex, origin, provenance, split, sample)
# and the squared distance matrix of all skulls versus all skulls.

# set criterion for filtering the distance matrix
distMatrixColumns <- grep("ref", names(skulls))

# set criterion for filtering by provenance status
isSecured <- skulls$provenance == "secure provenance"

# set criterion for training set distances
refColumns <- distMatrixColumns[isSecured]

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

# Perform NMDS (all rows, but only ref columns)

skulls_nmds <- metaMDS(as.matrix(skulls[, refColumns]),
                       k = 35, # this is the number of NMDS dimensions
                       trymax = 1000)

# do this if you want to visualise arrows in the biplot
#names(skulls_nmds)[names(skulls_nmds) == "species"] <- "loadings"

# plot results

biplot_2d(skulls_nmds,
          ordination_method = "NMDS",
          show_arrows = FALSE, # because we are inputting distances directly, there is no need for arrows
          groups = color.category,
          group_color = NULL, # setting this argument to NULL defaults to a rainbow palette
          group_ellipse_cex = 0, # the groups are quite spread, so ellipses are not so useful
          x_title = paste0("confirmed origin and unknowns (",
                           skulls_nmds$ndim, " dimensions)"),
          fitAnalysis_fig = c(0.05, 0.4, 0.05, 0.25),
          # display and file saving
          width = 1000, height = 1000,
          x_title_cex = 2, group_label_cex = 2, fitAnalysis_stress_cex = 2,
          directory = "output",
          file_name = paste0("skulls.nmds-", skulls_nmds$ndim),
          output_type = c("preview", "png")
          )

biplot_2d(skulls_nmds,
          ordination_method = "NMDS",
          show_arrows = FALSE, # because we are inputting distances directly, there are no arrows
          groups = color.category.fine,
          group_color = NULL, # setting this argument to NULL defaults to a rainbow palette
          group_ellipse_cex = 0, # the groups are quite spread, so ellipses are not so useful
          x_title = paste0("confirmed and claimed origin (",
                           skulls_nmds$ndim, " dimensions)"),
          fitAnalysis_fig = c(0.05, 0.4, 0.05, 0.25),
          # display and file saving
          width = 1000, height = 1000,
          x_title_cex = 2, group_label_cex = 2, fitAnalysis_stress_cex = 2,
          directory = "output",
          file_name = paste0("skulls.nmds.fine-", skulls_nmds$ndim),
          output_type = c("preview", "png")
)

# extract the NMDS dimensions as a a new input data frame
# and add origin as a categorical variable
newSkulls <- cbind(origin = skulls$origin,
                   data.frame(skulls_nmds$points))

# some preliminary visualisation

#==================================
png(filename = paste0("output/nmds.pairs-", skulls_nmds$ndim, ".png"), 
                      width = 1000, height = 1000)
pairs(newSkulls[, c(-1)],
      col = rainbow(nlevels(color.category))[color.category],
      pch = ifelse(isSecured, 19, 3),
      oma=c(10,3,3,3), cex.labels = 1.2)
par(xpd = TRUE)
legend("bottom", legend = levels(color.category), fill = rainbow(nlevels(color.category)), horiz = T)
dev.off()
# Interpretation: as expected, there is little correlation between NMDS dimensions 
# (i.e., as in PCA, the goal is to represent the most variance with every new dimension)
#==================================

# MDA ============================================================================

# fit model

setRNG::setRNG(seed = 0)
model.mda <- mda(origin ~ ., 
  data = newSkulls[isSecured,], 
  iter = 2000,
  trace = F) # activate this to see deviance of each iteration

model.mda

# using 'mda' package to plot the distribution of subgroups in canonical space 
# (similar to PCA in that it is reducing the dimensionality of data)
# see: https://rdrr.io/cran/mda/man/plot.fda.html
png(filename = paste0("output/model.nmds-mda-", skulls_nmds$ndim, ".png"), 
    width = 1000, height = 1200)
layout(matrix(c(5, 6, 1, 2, 3, 4), ncol = 2, byrow = TRUE), heights = c(1, 20, 20))
par(cex = 1.5)

plot(model.mda, newSkulls[isSecured,])
plot(model.mda, newSkulls)
plot(model.mda, newSkulls[isSecured,], group = "predicted")
plot(model.mda, newSkulls, group = "predicted")

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
predicted.train <- predict(model.mda, newSkulls[isSecured, c(-1)])

# summarize accuracy
model.fitness <- 
  table(predicted.train, newSkulls[isSecured, "origin"]) # or skulls[isSecured,]$origin

model.fitness

# quite good but not perfect

# Make predictions

predicted.test <- predict(model.mda, newSkulls[!isSecured, c(-1)])

# summarize accuracy
socmed.predictions <- table(predicted.test, newSkulls[!isSecured, "origin"])

# Match between model predicted and social media claim origins
mean(predicted.test == newSkulls[!isSecured, "origin"])

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

png(filename = paste0("output/nmds-confusion-matrix-", skulls_nmds$ndim, ".png"), 
    width = 800, height = 800)

ggplot(data =  df, mapping = aes(x = x, y = y)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1, size = 10) +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("Predicted") + ylab("Given in Social media") +
  theme_bw() + theme(legend.position = "none", text = element_text(size = 40))

dev.off()


#==================================================================================
# Create a plot showing the fitness of mda model for each number of NMDS dimensions 
#==================================================================================

# function assumes data contains only predictor variables (both training and testing sets)
fitnessCurvePerNMDS <- function(data, variableToPredict, isTraining, maxNumberOfDimensions)
{
  curve.training <- c()
  curve.testing <- c()
  
  setRNG::setRNG(seed = 0)
  
  for (i in 1:maxNumberOfDimensions)
  {
    data.nmds <- cbind(variableToPredict = variableToPredict,
                       data.frame(metaMDS(data, k = i, trymax = 1000)$points))
    
    mda.model <- mda(variableToPredict ~ ., 
                     data = data.nmds[isTraining,], 
                     iter = 2000)
    
    prediction.training <- predict(mda.model, data.nmds[isTraining, c(-1)])
    
    curve.training <- c(curve.training, mean(prediction.training == variableToPredict[isTraining]))
    
    prediction.testing <- predict(mda.model, data.nmds[!isTraining, c(-1)])
    
    curve.testing <- c(curve.testing, mean(prediction.testing == variableToPredict[!isTraining]))
  }
  
  return(data.frame(numberOfDimensions = 1:maxNumberOfDimensions, 
                    fitness.training = curve.training,
                    fitness.testing = curve.testing))
}

fitnessCurves <- fitnessCurvePerNMDS(data = as.matrix(skulls[, refColumns],),
                                     variableToPredict = skulls$origin,
                                     isTraining = isSecured,
                                     maxNumberOfDimensions = 35)

write.csv(fitnessCurves, file = "output/nmds-mda-fitness-curves.csv")

fitnessCurves <- read.csv(file = "output/nmds-mda-fitness-curves.csv", row.names = 1)

fitnessCurvesMelted <- reshape2::melt(fitnessCurves, id.var = 'numberOfDimensions')

# convert into %
fitnessCurvesMelted$value <- fitnessCurvesMelted$value * 100

png(filename = paste0("output/nmds-fitness-curves.png"), 
    width = 1000, height = 800)

ggplot(data = fitnessCurvesMelted,
       aes(x = numberOfDimensions, y = value, color = variable)) +
  geom_line(size = 1.5) +
  geom_smooth(size = 1.5) +
  ylab("MDA model fitness (%)") + xlab("number of NMDS dimensions") +
  scale_color_discrete(labels = c("confirmed origin (training set)", 
                                "social media claim (test set)"),
                       name = NULL) +
  scale_y_continuous(breaks = seq(0, 100, by = 25), 
                     minor_breaks = seq(0, 100, by = 5),
                     limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 35, by = 5),
                     minor_breaks = 0:35,
                     limits = c(0, 36)) +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 25),
        legend.text = element_text(size = 25),
        legend.position = "bottom")

dev.off()

#----
## print out predictions per row
unprovenance_predictions <- (skulls[!isSecured, c(-1)])
unprovenance_predictions$predicted <- predicted.test
unprovenance_predictions$labels <- rownames(unprovenance_predictions)
write.csv(unprovenance_predictions, "output/unprovenance_predictions.csv")

