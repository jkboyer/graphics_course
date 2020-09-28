# annotation ####

#fancy text in axis labels ####
#greek letters, italics, symbols, subscript etc.
#expression()lets you write mathematical symbols and expressions
#paste() pastes multiple character strings together
#substitute()

#below code will show you what expression() can create
demo(plotmath) #run this line and hit enter to run through examples


# Gray text shows what you write inside expression(), black text shows output
# paste() combines multiple elements (i.e., math symbols and regular text)
# substitute() lets you sub values from your R session into math expressions
example.plot <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  theme_minimal(base_size = 20)

example.plot +
  #an axis label with a greek letter
  xlab(expression(x[i] + y^2)) +#label with subscript and superscript
  ylab(expression(paste(mu, L))) #paste() combines multiple elements into label

example.plot +
  xlab(expression("Temperature " (degree*C)))  + #temperature label
  # use expression(paste()) to bold or italicize certain parts of label
  ylab(expression(Discharge ~~(m^3/s)))


#if your expression for a label or annotation is expecially long or complex, it
#may be easier to define it first, then add to plot
temp.label <- expression("Temperature " ( degree*C))

example.plot +
  scale_y_continuous(temp.label)

#annotate in plot panel
# A FEW GENERAL NOTES
# 1) ggplot stacks objects in the order they are written, so if you want your
#    data to be in front of annotations, put annotation first and plot type
#    (geom_point, geom_line etc.) after.  This will ensure annotated rectangles
#    etc. do not cover data
# 3) there are two ways to annotate in ggplot2: with annotate() and with
#    geoms (geom_rect(), geom_text() etc.).  Either method will work for a few
#    simple annotations, but geoms are better to use if you are annotating
#    multiple objects because they let you reference a new dataframe, and
#    annotate() does not
# 2) a few annotations are easy to add to directly plot code, but for multiple,
#    similar annotations, or for annotating multiple facets of a plot, the best
#    approach is to to first make a dataframe containing all info (xmin, xmax,
#    ymin, ymax, labels, etc.), and then reference the dataframe when annotating
#    your plot to make multiple annotations with one geom_..() in your plot code

#annotating lines
example.plot +
  #use size, lty, and color to change appearance of lines
  #geom_segment is very customizable
  geom_segment(aes(x = 6, y = 40, xend = 4.2, yend = 25), #x, y, starts and ends
               size = 1.2, #appearance
               arrow = arrow()) + #add arrow to line. arrow will be at line end
  geom_segment(aes(x = 2, xend = 3.5, y = 47, yend = 47)) +
  #geom_hline and geom_vline make horizontal or vertical lines the length of plot
  geom_hline(yintercept = 20, linetype  = "longdash") + #horizontal
  geom_vline(xintercept = 4, colour = "red") +#vertical
  #geom_abline lets you plot lines using intercept and slope
  geom_abline(intercept = 40, slope = -4, linetype = "dotted") +
  # you can also add lines with annotate("segment" ...)
  #but annotate is more annoying, I suggest you use geom... instead
  annotate("segment", x = 2, y = 2, xend = 6, yend = 10, color = "blue")

#ANNOTATE RECTANGLES
# make a simple scatter plot to annotate
example.plot +
  #put annotation before plot type (geom_...), otherwise shapes will cover data
  #annotate() is one way to annotate shapes
  annotate("rect",  #type of shape
           alpha = 0.3, #alpha useful to make shapes transparent and lighter
           fill = "blue", #change color of fill
           xmin = 2, xmax = 3, ymin = 26, ymax = 30) + #location of shape
  #geom_rect() is another way
  geom_rect(xmin = 4, xmax = 5, ymin =15, ymax = 20, #location
            fill = "gray", alpha = 0.8) + #appearance
  geom_point() #actual data to plot


#ANNOTATE TEXT
example.plot +
  # the annotate() method of adding text
  annotate("text", label = "outlier", #text to insert
           x = 2.9, y = 44.5, #location (center of text)
           family = "serif", color = "gray50", size = 6) + #appearance
  #and the geom_text method of adding text
  geom_text(label = "A", x = 4.2, y = 13, size = 10,
            hjust = 0, vjust = 0)  #label, location, appearance
  #hjust and vjust can be used for alignment
  #alignment is relative to x and y locations, NOT plot margins
  #hjust: 0 = left, 1 = right; vjust: 0 = top, 1 = bottom
  #change these to demonstrate

# ANNOTATE MULTIPLE OBJECTS AT ONCE
# first, make data frame with information for annotations
# make a vector for each column of dataframe
xstart = c(1.5, 2.6, 5.2) #position information for each annotated object
xend = c(2.1, 4, 7.2)
ystart = c(24, 15, 22)
yend = c(45, 30, 28)
#can make vectors of various appearance attributes (fill, alpha, etc.) if you
#want to vary appearance among annotated objects
colors = c("cornflowerblue", "navy", "darkcyan")
labels = c("A", "B", "C") #text labels
#combine above vectors into dataframe
df.rectangles <- data.frame(xstart, xend, ystart, colors, yend, labels)

example.plot +
  geom_rect(data = df.rectangles, #dataframe to use
            inherit.aes=FALSE, #dont use dataframe from points in plot
            aes(xmin = xstart, xmax = xend, ymin = ystart, ymax = yend), #position
            alpha = 0.3, fill = colors) + #appearance
  geom_text(data = df.rectangles, inherit.aes = FALSE,
            #position text labels relative to corresponding rectangles
            #the below example places them centered and above rectangles
            aes(x = xstart + (xend - xstart)/2, y = yend + 2.5),
            label = labels, #dataframe column with labels to use
            size = 10) #appearance


# ANNOTATE A FACETED PLOT (example in MWF movement graph I think)

#if we just annotate as for a single anntation, it's
#the same on all facets
example.plot +
  geom_hline(yintercept = 24, color = "purple") + #appearance
  facet_wrap(~drv)


# first, make data frame with information for annotations
# make a vector for each column of dataframe
drv <- c("4", "f", "r") #varible you will facet plot by
yintercept = c(24, 40, 18) #position information
colors = c("orange", "green", "purple") #appearence info if you want each
# to have different appearance attributes. If all will have same color, linetype
# etc. can just specity that in plot code, not dataframe
df.lines <- data.frame(drv, yintercept, colors)

example.plot +
  geom_hline(data = df.lines, #dataframe to use
               inherit.aes=FALSE, #dont use dataframe from points in plot
               aes(yintercept = yintercept), #position
               color = colors) + #appearance
  facet_wrap(~drv)

#statistics annotation
#manually annotate model fits
glm.cars <- glm(vs ~ wt, data = mtcars,
                family = binomial(link = "logit"))
summary(glm.cars)

#predict
predicted <- data.frame(wt = seq(min(mtcars$wt),
                                 max(mtcars$wt),
                                 by = 0.01))
predicted$vs <- predict(glm.cars, predicted,
                        type = "response")

ggplot(mtcars, aes(x = wt, y = vs)) +
  geom_point() +
  geom_line(data = predicted)

#To annotate statistics stuff (equations, predicted values, etc.)
#many packages have shortcuts that are easier than adding stats
#annotation manually with ggplot geoms
##Statistics annotation packages:
#ggpmisc, ggpubr, ggstatsplot, ggfortify, jtools

library(ggpmisc)
#simulate some data
df <- data.frame(x = c(1:100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)
my.formula <- y ~ x
ggplot(df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  geom_point()

#packages
require(tidyverse)
# Secondary axes ####
head(beaver1)
beaver1 %>%
  arrange(time) %>%
ggplot(aes(x = time, y = temp)) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous("Celsius",
                     sec.axis = sec_axis(~ .*(9/5) + 32,
                                         name = "Fahrenheit"))
#just duplicate axis
beaver1 %>%
  arrange(time) %>%
  ggplot(aes(x = time, y = temp)) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous("Celsius",
                     sec.axis = dup_axis())

#mention reverse transforming variable (WARNING)

# complex layouts #####
install.packages("cowplot")
require(cowplot)
# multipanel

#cowplot::plot_grid() lets us make multipanel layouts

#first make each individual graph
plot.1 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  theme_cowplot() #cowplot also comes with nice, simple themes

plot.2 <- ggplot(iris, aes(Sepal.Length)) +
  geom_density(alpha = 0.5) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  theme_cowplot()

#by default, plots are not aligned
#use align = "h" or align = "v" to align horiz or vertically
plot_grid(plot.1, plot.2, align = "h")

#add labels automatically
plot_grid(plot.1, plot.2, align = "h", labels = "AUTO")

#adjust label font
plot_grid(plot.1, plot.2, align = "h", labels = "AUTO",
          label_fontfamily = "serif", label_fontface = "plain", label_colour = "blue")

#vertically align

#add labels automatically
plot_grid(plot.1, plot.2, align = "v", ncol = 1, labels = "AUTO")

#different widths
plot_grid(plot.1, plot.2, align = "h", labels = "AUTO",
          rel_widths = c(1, 2))

#extra complex layouts: combine multiple plot_grids
plot.grid.1 <- plot_grid(plot.1, plot.2, align = "h", labels = "AUTO", rel_widths = c(1, 2))
plot.grid.2 <- plot_grid(plot.1, plot.2, align = "v", ncol = 1, labels = c('B', 'C'))

plot_grid(plot.grid.1, plot.grid.2, align = "v", ncol = 1, rel_heights = c(1, 3))

#shared legends
plot.1a <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  theme_cowplot() #cowplot also comes with nice, simple themes

plot.2a <- ggplot(iris, aes(Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.5) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  theme_cowplot()

plot_grid(plot.1a, plot.2a, align = "h", labels = "auto")
#well, 2 legends looks silly


p.nolegend <- plot_grid(plot.1a + theme(legend.position = "none"),
          plot.2a + theme(legend.position = "none"),
          align = "h", labels = "auto")

legend <- get_legend(plot.1a)

# add the legend to the row we made earlier. Give it one-third of
# the width of one plot (via rel_widths).
plot_grid(p.nolegend, legend, rel_widths = c(3, .6))

# inset
##ggdraw() draws stuff on your plot

p <- ggplot(mpg, aes(displ, cty)) +
  geom_point() +
  theme_cowplot()

ggdraw(p) +
    draw_label("Draft", alpha = 0.3,
               size = 100, angle = 45)

p.to.inset <- ggplot(mpg, aes(drv)) +
  geom_bar() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  theme_cowplot()

#draw inset plot on main plot
#ggdraw()
 ggdraw(p) + #main plot
  draw_plot(p.to.inset, #inset plot
            .45, .45, .5, .5) #position

# facet hacks ####
#when ggplot lacks functionality for something...
#you can often use facets as a hack or workaround

# nested axis labels
#ggplot doesn't have a way to do nested, multilevel axis labels
 #but, we can fake it with facets

#make a simple dataframe with nested levels
df.nested <- data.frame(category = c("control", "control",
                                   "treatment", "treatment"),
                      time = c("c1", "c2", "t1", "t2"),
                      time2 = factor(c(1,2,1,2)),
                      height = c(2.3, 4.3, 2.3, 8.4))
ggplot(df.nested, aes(x = time, y = height)) +
  geom_col() +
  facet_wrap(~category, scales = "free_x",
             strip.position = "bottom" ) +
   theme(panel.spacing = unit(0, "lines"),
         strip.background = element_blank(),
         strip.placement = "outside")
# Themes Revisited ####

# more theme packages

#writing your own theme


# automation (for loops)####
# graphing function
diamonds <- diamonds
PlotManyPlots <- function(df, na.rm = TRUE, ...){
  color.vec <- unique(diamonds$color)
  for (i in seq_along(color.vec)) { #loop to create graphs for each cut

    # plot code
    p <- df %>%
      filter(color == color.vec[i]) %>%
      ggplot(aes(x = carat, y = price, group = color)) +
      geom_point(alpha = 0.2, color = "seagreen4") +
      facet_wrap( ~ cut, ncol = 2) +
      ggtitle(paste("Diamonds: color", color.vec[i])) +
      theme_minimal()

    #save plots as file.type defined at top of script
    ggsave(p,
           file = paste0("./output/figures/plot_color", color.vec[i],
                        ".jpg"),
           width = 6.5, height = 5, dpi = 300)

    # print plots to screen
    print(p)
  }
}

#run function to create plots
PlotManyPlots(diamonds)
