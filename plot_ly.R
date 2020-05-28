# load the plotly R package
library(plotly) 

# load the diamonds dataset from the ggplot2 package 
data(diamonds, package = "ggplot2")
diamonds

# create three visualizations of the diamonds dataset

plot_ly(diamonds, x = ~cut)
plot_ly(diamonds, x = ~cut, y = ~clarity)
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")

