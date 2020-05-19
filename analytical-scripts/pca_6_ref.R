library(mda) # best place every library/require commands at the start
library(ggplot2)

#devtools::install_github("Andros-Spica/biplot2d3d")
library(biplot2d3d)

#setwd("~/Desktop/triplet-loss/results-2020")

# load data
skulls <- read.csv("./raw-data/input-table.csv", header = TRUE, sep=",")

isSecured <- skulls$provenance == "secure provenance"

distancesToRefs <- c("ref.female.afr.american", 
                     "ref.female.asian", 
                     "ref.female.european", 
                     "ref.male.afr.american",
                     "ref.male.asian",
                     "ref.male.european")

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

# some preliminary visualisation

barplot(table(skulls[isSecured, "origin"]))

#==================================
png(filename = "output/pairs.png", width = 1000, height = 1000)
pairs(skulls[, distancesToRefs],
      col = rainbow(nlevels(color.category))[color.category],
      pch = ifelse(isSecured, 19, 3),
      oma=c(10,3,3,3), cex.labels = 1.2)
par(xpd = TRUE)
legend("bottom", legend = levels(color.category), fill = rainbow(nlevels(color.category)), horiz = T)
dev.off()
# Interpretation: data is highly positively correlated, 
# i.e. the more distant to ONE reference, the more distant to ANY reference.


# PCA ============================================================================

# only secured origin
skullsSecure.pca <- prcomp(skulls[isSecured, distancesToRefs],
                           center = T,
                           scale. = T) 

biplot_2d(skullsSecure.pca, # without log transformation
          groups = color.category[isSecured], 
          group_color = NULL, # setting this argument to NULL defaults to a rainbow palette
          xlim = c(-2, 2), # zooms out
          x_title = "secured",
          # better fit of the arrows (labels are probably too long for nice plots)
          arrow_fig = c(0.5, 1, 0, 0.5),
          arrow_label_adj = 1,
          # display and file saving
          width = 500, height = 500,
          directory = "output",
          file_name = "skullsSecure.pca",
          output_type = c("preview", "png"))

# only not secured origin

skullsNotSecure.pca <- prcomp(skulls[!isSecured, distancesToRefs],
                              center = T,
                              scale. = T)

biplot_2d(skullsNotSecure.pca, 
          x_title = "not secured",
          # invert x coordinates to easy the comparison with skullsSecure.pca
          invert_coordinates = c(TRUE, FALSE), 
          # also use the same arrow config than before to spot differences
          xlim = c(-2, 2), # zooms out
          arrow_fig = c(0.5, 1, 0, 0.5),
          arrow_label_adj = 1,
          # display and file saving
          width = 500, height = 500,
          directory = "output",
          file_name = "skullsNotSecure.pca",
          output_type = c("preview", "png"))

# all

skulls.pca <- prcomp(skulls[, distancesToRefs],
                     center = T,
                     scale. = T) 

# grouped by confirmed origin or not confirmed status
biplot_2d(skulls.pca, # without log transformation
          groups = color.category, 
          group_color = NULL, # setting this argument to NULL defaults to a rainbow palette
          x_title = "all",
          # invert x coordinates to easy the comparison with skullsSecure.pca
          invert_coordinates = c(TRUE, FALSE), 
          # also use the same arrow config than before to spot differences
          xlim = c(-2, 2), # zooms out
          arrow_fig = c(0.5, 1, 0, 0.5),
          arrow_label_adj = 1,
          # display and file saving
          width = 500, height = 500,
          directory = "output",
          file_name = "skulls.pca",
          output_type = c("preview", "png"))

# grouped by confirmed and not confirmed origin 
biplot_2d(skulls.pca, # without log transformation
          groups = color.category.fine, 
          group_color = NULL, # setting this argument to NULL defaults to a rainbow palette
          x_title = "all",
          show_group_legend = T,
          group_label_cex = 0,
          group_legend_fig = c(0.7, 0.95, 0.7, 0.98),
          group_legend_title = NULL,
          # invert x coordinates to easy the comparison with skullsSecure.pca
          invert_coordinates = c(TRUE, FALSE), 
          # also use the same arrow config than before to spot differences
          xlim = c(-2, 2), # zooms out
          arrow_fig = c(0.5, 1, 0, 0.5),
          arrow_label_adj = 1,
          # display and file saving
          width = 500, height = 500,
          directory = "output",
          file_name = "skulls.pca_finerGroups",
          output_type = c("preview", "png"))

# different view
g2 <- ggbiplot(skulls.pca, obs.scale = .05, var.scale = 1,labels.size=2, 
               groups = skulls$origin, ellipse = TRUE, 
               circle = FALSE,
               labels = skulls$label)
g2 <- g2 + scale_color_discrete(name = '')
g2 <- g2 + theme(legend.direction = 'horizontal', 
                 legend.position = 'top') +
  theme_linedraw(base_size = 11, base_family = "")

g2


