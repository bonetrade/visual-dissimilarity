# Load library
library(VennDiagram)

# a table where there are four columns: label, claimed origin, and the predicted origins from MDA and ludwig
input <- read.csv("../raw-data/inputForVenn.csv")


names(input) <- c("label", "claimed", "MDA", "ludwig")

input[] <- lapply(input, as.character) # reassure everything is text (not factors)

# draw and save plot


fontFamily = "sans" # if saving in png
png("output/confusion_VennDiagram.png", width = 500, height = 500)

draw.triple.venn(
  area1 = 28, area2 = 28, area3 = 28, 
  n12 = sum(input$claimed == input$MDA), # claimed & MDA 
  n13 = sum(input$claimed == input$ludwig), # claimed & ludwig
  n23 = sum(input$MDA == input$ludwig), # MDA & ludwig
  n123 = sum(input$claimed == input$MDA & input$claimed == input$ludwig), # all
  category = c("claimed", "MDA", "ludwig"),
  lty = rep("blank", 3),
  fill = c("red", "green", "blue"),
  alpha = rep(0.3, 3),
  cex = 3,
  fontface = "bold",
  fontfamily = fontFamily,
  cat.pos = c(330, 30, 180),
  cat.dist = c(0.08, 0.08, 0.07),
  cat.cex = c(3, 3, 3),
  cat.fontface = "bold",
  cat.fontfamily = fontFamily,
  #cat.col = c("red", "green", "blue")
  )

dev.off()


# to create a eps version, it needs to be slightly different 
# because postcript doesn't allow transparency

fontFamily = "Helvetica" # if saving in eps
extrafont::loadfonts(device = "postscript") # this is to embbed the fonts into the eps file
postscript("output/confusion_VennDiagram.eps", width = 5, height = 5, family = fontFamily)

draw.triple.venn(
  area1 = 28, area2 = 28, area3 = 28, 
  n12 = sum(input$claimed == input$MDA), # claimed & MDA 
  n13 = sum(input$claimed == input$ludwig), # claimed & ludwig
  n23 = sum(input$MDA == input$ludwig), # MDA & ludwig
  n123 = sum(input$claimed == input$MDA & input$claimed == input$ludwig), # all
  category = c("claimed", "MDA", "ludwig"),
  lwd = rep(2, 3),
  col = c("red", "green", "blue"),
  cex = 3,
  fontface = "bold",
  fontfamily = fontFamily,
  cat.pos = c(340, 30, 180),
  cat.dist = c(0.08, 0.08, 0.07),
  cat.cex = c(3, 3, 3),
  cat.fontface = "bold",
  cat.fontfamily = fontFamily,
  #cat.col = c("red", "green", "blue")
)

dev.off()
