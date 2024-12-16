# Create your own plot style
henrik_plot <- function(xlim, ylim, xlab, ylab, title=NULL) {
  #par(font.lab = 2, oma = c(0, 0, 0, 0), mar =c(4, 4, 2, 2))
  require(Hmisc)
  plot(NULL,
       # Set the plot boundaries
       xlim = xlim, ylim=ylim,
       # Set the plot labels
       xlab=xlab, ylab = ylab,
       # Rotate the y-labels
       las = 1,
       # Change the axis text
       family = "serif",
       # Change the plot expansion
       xaxs="i", yaxs="i")
  # Add minor ticks
  minor.tick(nx = 5, ny = 5, tick.ratio = 0.5)
  
  # Make the title
  title(title, line = 0.5)
  
}

# Colours
colours <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
