# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.

# Go to the discussions to
# share your thoughts and to discover
# what other people found.

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.

# SUBMIT YOUR CODE BELOW THIS LINE
# ===================================================================
library(ggplot2)
library(gridExtra)

data(diamonds)

p1 = qplot(x = color, y = price/carat, data = subset(subset(diamonds, !is.na(price)),!is.na(carat)), geom = 'boxplot', xlab = 'Color', ylab = 'Price/Carat ($)') + coord_cartesian(ylim = c(1000, 7000))
p2 = qplot(x = carat, y = price, data = diamonds, geom = 'point', color = color, xlab = 'Carat', ylab = 'Price ($)') + scale_y_log10() + scale_x_continuous(limits = c(0,2)) + stat_smooth(method="lm", se=FALSE)

grid.arrange(p1, p2, ncol=1)
