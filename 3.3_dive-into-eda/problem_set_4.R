# ======================================================
# In this problem set, you'll continue
# to explore the diamonds data set.

# Your first task is to create a
# scatterplot of price vs x.
# using the ggplot syntax.

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ======================================================

library(ggplot2)
data(diamonds)
summary(diamonds)

ggplot(aes(x = price, y = x), data = diamonds) + geom_point()

# Find correlations

cor(diamonds$price,diamonds$x)
cor(diamonds$price,diamonds$y)
cor(diamonds$price,diamonds$z)

# ======================================================
# Create a simple scatter plot of price vs depth.

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ======================================================
ggplot(aes(x = price, y = depth), data = diamonds) + geom_point()

# ======================================================
# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units. See the instructor notes
# for two hints.

# This assignment is not graded and
# will be marked as correct when you submit.

# ALTER THE CODE BELOW THIS LINE
# ======================================================
ggplot(data = diamonds, aes(x = depth, y = price)) +
  geom_point(alpha=1/100) +
  scale_x_continuous(breaks = seq(0,max(diamonds$depth),2))

cor(diamonds$depth,diamonds$price)

# ======================================================
# Create a scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ======================================================
ggplot(diamonds, aes(carat,price)) +
  geom_point() +
  xlim(0, quantile(diamonds$carat, 0.99)) +
  ylim(0, quantile(diamonds$price, 0.99))


# ======================================================
# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.

# Create a new variable for volume in the diamonds data frame.
# This will be useful in a later exercise.

# Don't make any adjustments to the plot just yet.

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ======================================================
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(diamonds,aes(volume,price)) + geom_point()

# find corelations... by first subsetting diamonds
# into a new data frame to only examine diamonds
# with volume between 0 and 800
diamonds2 <- subset(diamonds,diamonds$volume <= 800 & diamonds$volume > 0)
cor(diamonds2$volume, diamonds2$price)

# ======================================================
# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot. (See the
# Instructor Notes or look up the documentation of
# geom_smooth() for more details about smoothers.)

# We encourage you to think about this next question and
# to post your thoughts in the discussion section.

# Do you think this would be a useful model to estimate
# the price of diamonds? Why or why not?

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ======================================================
ggplot(diamonds2,aes(volume,price)) + geom_point(alpha=1/20) + geom_smooth(method = 'lm')

# ======================================================
# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity.

# Name the data frame diamondsByClarity

# The data frame should contain the following
# variables in this order.

#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n

# where n is the number of diamonds in each
# level of clarity.

# This assignment WILL BE automatically
# graded!

# DO NOT ALTER THE NEXT THREE LINES OF CODE.
# ======================================================
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
data(diamonds)

# ENTER YOUR CODE BELOW THIS LINE
# ======================================================
clarity_groups <- group_by(diamonds, clarity)
diamondsByClarity <- summarise(clarity_groups,
                      mean_price = mean(price),
                      median_price = median(price),
                      min_price = min(price),
                      max_price = max(price),
                      n = n())

# ======================================================
# We've created summary data frames with the mean price
# by clarity and color. You can run the code in R to
# verify what data is in the variables diamonds_mp_by_clarity
# and diamonds_mp_by_color.

# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.

# This assignment is not graded and
# will be marked as correct when you submit.

# See the Instructor Notes for more info on bar charts
# and for a hint on this task.

# DO NOT DELETE THE LINES OF CODE BELOW
# ======================================================
data(diamonds)
library(dplyr)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

# ENTER YOUR CODE BELOW THIS LINE
# ======================================================
library(gridExtra)
p1 <- ggplot(diamonds_mp_by_clarity,aes(clarity, mean_price)) + geom_boxplot()
p2 <- ggplot(diamonds_mp_by_color,aes(color, mean_price)) + geom_boxplot()
grid.arrange(p1,p2)

# ======================================================
# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to continue the investigation you did at the
# end of Problem Set 3 or you can start fresh and choose a different
# data set from Gapminder.

# If you're feeling adventurous or want to try some data munging see if you can
# find a data set or scrape one from the web.

# In your investigation, examine pairs of variable and create 2-5 plots that make
# use of the techniques from Lesson 4.

# You can find a link to the Gapminder website in the Instructor Notes.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. the variable(s) you investigated, your observations, and any summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.
# ======================================================
# TBD