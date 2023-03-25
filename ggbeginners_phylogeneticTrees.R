# Author: Corey C. Holt
# Date: 2023-03-24
# Purpose: Plot a phylogenetic tree with metadata

# Load required libraries
library(tidyverse)
library(ape)
library(ggtree)
library(ggtreeExtra)
library(ggnewscale)
library(gginnards)
library(treeio)

# Define color palette for BUSCO status
pal.BUSCO <- c(Complete = "#36AE7C",
               Duplicated = "#187498",
               Fragmented = "#F9D923",
               Missing = "#EB5353")

# Read in tree file. Use read.iqtree for .contree files. 
# Alternatively, use path to treefile 
tree <- read.tree(text='((A, B)100, ((C, D)89, (E, F)90)70)100;')

# Or use read.iqtree from the treeio package for .contree files using the path to the file


# Create ggtree object of the tree and add tip labels and node labels
ggtree(tree)+ 
  geom_tiplab()+ # Add tip labels
  geom_nodelab() # Add node labels

# Read in metadata for BUSCO analysis
meta.busco.df<- read_csv("meta.busco.df.csv")

# Define metadata for genes for each tip of the tree (or read file using read_csv/read_table)
meta.df <- data.frame(Tip=c("A", "B", "C", "D", "E", "F"),
                      gene1 = c("Y","Y","Y","Y","N","N"),
                      gene2 = c("Y","N","N","Y","Y","N"),
                      gene3 = c("N","N","Y","Y","Y","N"))

# Reshape metadata for gene values into long format 
meta.df.m <- meta.df %>%  
  pivot_longer(cols = -Tip, # Exclude the Tip column
               names_to = "variable", # Rename the column with the gene names to "variable"
               values_to = "value") # Rename the column with the gene values to "value"

# Create ggtree object of the tree with a roundrect layout and add tip labels and node labels
p1 <- ggtree(tree,
       layout="roundrect")+ 
  geom_tiplab(size=6, 
              fontface="bold")+ 
  geom_nodelab(aes(subset = !is.na(as.numeric(label)) & as.numeric(label) != 100), # Add node labels for nodes that are not 100
               nudge_x = 0.1)+ # Nudge node labels to the right
  geom_nodepoint(
    aes(subset = !is.na(as.numeric(label)) & as.numeric(label) == 100), # Add node points for nodes that are 100
    colour = "black", 
    size = 4 
  )
p1

#Add a fruit plot to the ggtree object to show the percentage of BUSCO genes found for each tip
p2 <- p1+
  geom_fruit(
  data = meta.busco.df,
  geom = geom_bar,
  mapping = aes(x = p, 
                y = Tip, 
                fill = Status),
  colour="black",
  stat="identity", # Use stat="identity" to plot the data as is
  orientation="y", # Plot the fruit plot vertically
  pwidth = 0.5, # Set the width of the fruit plot
  offset = 0.1, # Set the offset of the fruit plot
  width=0.5, # Set the width of the bars in the fruit plot
  inherit.aes = FALSE, 
  axis.params=list( 
    axis="x",
    title = "% BUSCOs",
    title.size=5,
    text.size = 0,
    line.size = 0)
) +
  scale_fill_manual(values = pal.BUSCO) 
p2

# Add a new fill scale to the ggtree object to show the presence or absence of each gene for each tip
p3 <- p2 +
  new_scale_fill() +
  geom_fruit(
    data = meta.df.m,
    geom = geom_tile,
    mapping = aes(x = variable, 
                  y = Tip, 
                  fill = value),
    show.legend = FALSE,
    colour="black",
    size=0.5,
    height=0.5, 
    width=0.5,
    pwidth = 0.5,
    offset = 0.05,
    inherit.aes = FALSE,
    axis.params=list(
      axis="x",
      title = "Genes",
      title.size=5,
      text.size = 4,
      line.size = 0,
      vjust = -3
    )
) +
  scale_fill_manual(values = c(Y="khaki", N="white"))
p3

# Alternatively, add a fruit plot to the ggtree object to show the presence or absence of each gene for each tip using points instead of tiles
p3 <- p2 +
  new_scale_fill() + # Add a new fill scale
  geom_fruit(
    data = meta.df.m,
    geom = geom_point,
    mapping = aes(x = variable, 
                  y = Tip, 
                  fill = value),
    pch=21, # Set the shape of the points to a filled circle
    stroke=0.75, # Set the stroke of the points to 0.75
    show.legend = FALSE, # Do not show a legend
    colour="black",
    size=15,
    pwidth = 0.5,
    offset = 0.05,
    inherit.aes = FALSE,
    axis.params=list(
      axis="x",
      title = "Genes",
      title.size=5,
      text.size = 4,
      line.size = 0,
      vjust = -3
    )
  ) +
  scale_fill_manual(values = c(Y="khaki", N="white"))
p3

# Create a separate plot to add node labels to the ggtree object 
# Make a note of nodes you wish to highlight
nodemap <- ggtree(tree)+
  geom_tiplab()+
  geom_label2(
    aes(subset = !isTip, label = node), # Add node labels for nodes that are not tips
    size = 6, 
    color = "darkred"
  )
nodemap

# Add a highlight to the ggtree object to highlight the node of interest
p4 <- p3 + 
  geom_hilight(node = 10,
               fill = "#C3C5C5",
               extendto = 7.2, # Extend the highlight to the node with the label "7.2"
               roundrect=TRUE, # Make the highlight a rounded rectangle
               roundrect.r=0.1) # Set the radius of the rounded rectangle to 0.1
p4

# Add a clade label to the ggtree object to label the clade of interest
p5 <- p4 + geom_cladelabel(
  10,
  "Interesting taxa", 
  offset = 4.3, # Offset the label from the node with the label "4.3"
  barsize = 1, # Set the size of the bar to 1
  angle = -90, # Set the angle of the label to -90
  offset.text = 0.20, # Offset the label text from the bar
  hjust = 0.5, # Set the horizontal justification of the label text to 0.5
  fontsize = 4)
p5

# Move the highlight to the bottom of the ggtree object
p6<- move_layers(p5, 'GeomHilightRect', position = "bottom")
p6

