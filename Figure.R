source("makecaplot.R")
library(ca)
ca.fit <- ca(freqtab)
ca.plot <- plot(ca.fit)


ca.plot.df <- make.ca.plot.df(ca.plot,
                              row.lab = "Food Groups",
                              col.lab = "Location")
ca.plot.df$Size <- ifelse(ca.plot.df$Variable == "Location", 2, 1)
ca.sum <- summary(ca.fit)
dim.var.percs <- ca.sum$scree[,"values2"]

library(ggplot2)
library(ggrepel)

p <- ggplot(ca.plot.df, aes(x = Dim1, y = Dim2,
                            col = Variable, shape = Variable,
                            label = Label, size = Size)) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = .5) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = .5) +
  geom_point() +
  scale_x_continuous(limits = range(ca.plot.df$Dim1) + c(diff(range(ca.plot.df$Dim1)) * -0.2,
                                                         diff(range(ca.plot.df$Dim1)) * 0.2)) +
  scale_y_continuous(limits = range(ca.plot.df$Dim2) + c(diff(range(ca.plot.df$Dim2)) * -0.2,
                                                         diff(range(ca.plot.df$Dim2)) * 0.2)) +
  scale_size(range = c(4, 7)) +
  geom_label_repel(show.legend = FALSE, segment.alpha = .5, point.padding = unit(5, "points")) +
  labs(x = paste0("Dimension 1 (", signif(dim.var.percs[1], 3), "%)"),
       y = paste0("Dimension 2 (", signif(dim.var.percs[2], 3), "%)"),
       col = "", shape = "") +
  theme_minimal() + 
  theme(plot.caption = element_text(size = 12,
                                    face = "bold"), 
        panel.grid.major = element_line(colour = "lightgray",  size = 0.6, linetype = "dotted"), 
        panel.grid.minor = element_line(size = 1.6, linetype = "dotted"), 
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 15, colour = "gray0"), 
        plot.title = element_text(size = 21, 
                                                    face = "bold", hjust = 0.5, vjust = 2), 
                          panel.background = element_rect(fill = "white", 
                                                          size = 0), legend.position = "none") +
  labs(title = "Correspondence Analysis of food groups and locations", 
       colour = NULL, shape = NULL, caption = "Coordinates in symmetric") + 
  theme(plot.caption = element_text(hjust = 0))
plot(p)
