# Author: Corey C. Holt
# Date: 2023-03-24
# Purpose: Plot BUSCO output

# Load the tidyverse package for data manipulation and plotting
library(tidyverse)

# Read in the busco data for each cell using the read_tsv function from the tidyverse package, skipping the first two rows of metadata
busco.cell1 <- read_tsv("~/Bioinformatics/scTranscriptomes/busco/1/full_table.tsv", skip=2)
busco.cell2 <- read_tsv("~/Bioinformatics/scTranscriptomes/busco/2/full_table.tsv", skip=2)
busco.cell3 <- read_tsv("~/Bioinformatics/scTranscriptomes/busco/3/full_table.tsv", skip=2)

# Add a column to each dataset indicating the cell it belongs to
busco.cell1$cell <- "cell1"
busco.cell2$cell <- "cell2"
busco.cell3$cell <- "cell3"

# Combine the three datasets into a single dataframe using rbind
busco.df <- rbind(busco.cell1,
                  busco.cell2,
                  busco.cell3)

# Group the dataframe by cell and BUSCO status, calculate the number of occurrences of each status, and add a column indicating the percentage of BUSCOs with each status for each cell
busco.df.count <- busco.df %>%
  group_by(cell, Status) %>%
  tally() %>%
  mutate(percent = n / sum(n))

# Define a color palette for the different BUSCO statuses
pal.BUSCO <- c(Complete = "#36AE7C",
               Duplicated = "#187498",
               Fragmented = "#F9D923",
               Missing = "#EB5353")

# Reorder factors
busco.df.count$Status.o <- factor(busco.df.count$Status,
                                  levels=c("Missing",
                                           "Fragmented",
                                           "Duplicated",
                                           "Complete"))

# Create a stacked bar chart showing the percentage of BUSCOs with each status for each cell, using the percent and cell columns of the busco.df.count dataframe, and coloring each status with the corresponding color from the pal.BUSCO palette
ggplot(data=busco.df.count, aes(x=percent, 
                                y=cell, 
                                fill=Status.o))+
  geom_bar(stat="identity", 
           colour="black",
           width=0.75)+
  scale_y_discrete(expand=c(0,0.5))+
  scale_fill_manual(values=pal.BUSCO)+ 
  theme(plot.margin=margin(4,1,4,1, unit="cm"),
        panel.grid.major.y = element_blank(), # Remove the y-axis grid lines
        panel.grid.major.x = element_line(colour="grey60",
                                          linetype="dashed"), # Add dashed x-axis grid lines
        axis.ticks = element_blank(), # Remove the y-axis ticks
        axis.title.y = element_blank(), # Remove the y-axis title
        axis.text = element_text(size=10), # Set the font size of the axis text
        axis.title = element_text(size=12, 
                                 face="bold"), # Set the font size and boldness of the axis title
        axis.text.y = element_text(margin = margin(r = -15)), # Move the y-axis text to the right
        panel.background = element_blank())+ # Remove the panel background
  xlab("% BUSCOs") # Add an x-axis label

# Create a dodged bar chart showing the percentage of BUSCOs with each status for each cell, and limit the x-axis range to 0-0.25
ggplot(data=busco.df.count, aes(x=percent, 
                                y=cell, 
                                fill=Status.o))+
  geom_bar(stat="identity", 
           colour="black",
           width=0.75,
           position="dodge")+ # Use the dodge position to create a dodged bar chart
  scale_y_discrete(expand=c(0,0.5))+
  scale_fill_manual(values=pal.BUSCO)+
  theme(plot.margin=margin(4,1,4,1, unit="cm"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60",
                                          linetype="dashed"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12,
                                  face="bold"),
        axis.text.y = element_text(margin = margin(r = -15)),
        panel.background = element_blank())+
  xlab("% BUSCOs")+
  coord_cartesian(xlim=c(0,0.25)) # Limit the x-axis range to 0-0.25

# Save the plot as a png file
ggsave(device="png", filename = "BUSCO_barplot.png",
        path="~/path/",
        dpi=320,
        height=7, width=7, units="in")