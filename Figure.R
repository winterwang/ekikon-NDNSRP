
# CA use own program ------------------------------------------------------
TableFoogGroup <- Food1_9_teen %>% 
  ungroup() %>% 
  group_by(mfgLab) %>% 
  summarise(n = n(), meanHpoint = mean(H_points, na.rm = T), mfgCalories = sum(Energykcal)) %>% 
  # arrange(-mfgCalories) %>% 
  # mutate(n.freq = paste0(round(100 * n/sum(n), 2), "%"))  %>% 
  # mutate(cal.Prop = paste0(round(100 * mfgCalories/sum(mfgCalories), 2), "%"))  %>% 
  mutate(calprop = mfgCalories/sum(mfgCalories)) %>% 
  # mutate(calcumprop = paste0(round(100 * cumsum(calprop), 3), "%")) %>% 
  # mutate(Hpoint = round(meanHpoint, 2)) %>% 
  mutate(healthy     = meanHpoint < -2, 
         lesshealthy = meanHpoint > 4, 
         neutral     = (meanHpoint <= 4) & (meanHpoint >= -2)) 

TableFoogGroup <- TableFoogGroup %>% 
  mutate(HealthPoints3g = ntile(meanHpoint, 3))

# Total food and locations ------------------------------------------------


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
  theme(plot.caption = element_text(size = 16,
                                    face = "bold"), 
        panel.grid.major = element_line(colour = "lightgray",  size = 0.6, linetype = "dotted"), 
        panel.grid.minor = element_line(size = 1.6, linetype = "dotted"), 
        axis.title = element_text(size = 26), 
        axis.text = element_text(size = 26, colour = "gray0"), 
        plot.title = element_text(size = 30,  face = "bold", hjust = 0.5, vjust = 2), 
                          panel.background = element_rect(fill = "white", 
                                                          size = 0), legend.position = "none") +
  labs(title = "Correspondence Analysis of food groups and Time", 
       colour = NULL, shape = NULL) +
  scale_color_manual(values = c("#e41a1c", "#377eb8")) + 
       # , caption = "Coordinates in symmetric") + 
  theme(plot.caption = element_text(hjust = 0))
plot(p)



# stratification by Hpoint ------------------------------------------------

G1Foods <- as.data.frame.matrix(freqtab)[rownames(as.data.frame.matrix(freqtab)) %in% TableFoogGroup$mfgLab[TableFoogGroup$HealthPoints3g == 1],]
G2Foods <- as.data.frame.matrix(freqtab)[rownames(as.data.frame.matrix(freqtab)) %in% TableFoogGroup$mfgLab[TableFoogGroup$HealthPoints3g == 2],]
G3Foods <- as.data.frame.matrix(freqtab)[rownames(as.data.frame.matrix(freqtab)) %in% TableFoogGroup$mfgLab[TableFoogGroup$HealthPoints3g == 3],]

library(FactoMineR)
library(factoextra)
G1.ca <- CA(G1Foods, graph = FALSE)
fviz_ca_biplot(G1.ca, 
               repel = TRUE, title = "Biplot of Correspondence analysis for food group 1 (1st 20 healthy foods).") 
G2.ca <- CA(G2Foods, graph = FALSE)
fviz_ca_biplot(G2.ca, 
               repel = TRUE, title = "Biplot of Correspondence analysis for food group 1 (2nd 20 healthy foods).") 
G3.ca <- CA(G3Foods, graph = FALSE)
fviz_ca_biplot(G3.ca, 
               repel = TRUE, title = "Biplot of Correspondence analysis for food group 1 (2nd 20 healthy foods).") 


# G1 foods ----------------------------------------------------------------

ca.fit <- ca(G1Foods)
ca.plot <- plot(ca.fit)


ca.plot.df <- make.ca.plot.df(ca.plot,
                              row.lab = "Food Groups",
                              col.lab = "Location")
ca.plot.df$Size <- ifelse(ca.plot.df$Variable == "Location", 1.5, 1)
ca.sum <- summary(ca.fit)
dim.var.percs <- ca.sum$scree[,"values2"]

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
  theme(plot.caption = element_text(size = 23,
                                    face = "bold"), 
        panel.grid.major = element_line(colour = "lightgray",  size = 0.6, linetype = "dotted"), 
        panel.grid.minor = element_line(size = 1.6, linetype = "dotted"), 
        axis.title = element_text(size = 24), 
        axis.text = element_text(size = 24, colour = "gray0"), 
        plot.title = element_text(size = 27,  face = "bold", hjust = 0.5, vjust = 2), 
        panel.background = element_rect(fill = "white", 
                                        size = 0), legend.position = "none") +
  labs(title = "Correspondence Analysis of 1st 20 food groups and locations",
       colour = NULL, shape = NULL) +
  scale_color_manual(values = c("#e41a1c", "#377eb8")) + 
  # , caption = "Coordinates in symmetric") + 
  theme(plot.caption = element_text(hjust = 0))
plot(p)




# G2 foods ----------------------------------------------------------------

ca.fit <- ca(G2Foods)
ca.plot <- plot(ca.fit)


ca.plot.df <- make.ca.plot.df(ca.plot,
                              row.lab = "Food Groups",
                              col.lab = "Location")
ca.plot.df$Size <- ifelse(ca.plot.df$Variable == "Location", 2, 1)
ca.sum <- summary(ca.fit)
dim.var.percs <- ca.sum$scree[,"values2"]

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
  theme(plot.caption = element_text(size = 23,
                                    face = "bold"), 
        panel.grid.major = element_line(colour = "lightgray",  size = 0.6, linetype = "dotted"), 
        panel.grid.minor = element_line(size = 1.6, linetype = "dotted"), 
        axis.title = element_text(size = 24), 
        axis.text = element_text(size = 24, colour = "gray0"), 
        plot.title = element_text(size = 27,  face = "bold", hjust = 0.5, vjust = 2), 
        panel.background = element_rect(fill = "white", 
                                        size = 0), legend.position = "none") +
  labs(title = "Correspondence Analysis of 2nd 20 food groups and locations",
       colour = NULL, shape = NULL) +
  scale_color_manual(values = c("#e41a1c", "#377eb8")) + 
  # , caption = "Coordinates in symmetric") + 
  theme(plot.caption = element_text(hjust = 0))
plot(p)



# G3 foods ----------------------------------------------------------------

ca.fit <- ca(G3Foods)
ca.plot <- plot(ca.fit)


ca.plot.df <- make.ca.plot.df(ca.plot,
                              row.lab = "Food Groups",
                              col.lab = "Location")
ca.plot.df$Size <- ifelse(ca.plot.df$Variable == "Location", 2, 1)
ca.sum <- summary(ca.fit)
dim.var.percs <- ca.sum$scree[,"values2"]

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
  theme(plot.caption = element_text(size = 23,
                                    face = "bold"), 
        panel.grid.major = element_line(colour = "lightgray",  size = 0.6, linetype = "dotted"), 
        panel.grid.minor = element_line(size = 1.6, linetype = "dotted"), 
        axis.title = element_text(size = 24), 
        axis.text = element_text(size = 24, colour = "gray0"), 
        plot.title = element_text(size = 27,  face = "bold", hjust = 0.5, vjust = 2), 
        panel.background = element_rect(fill = "white", 
                                        size = 0), legend.position = "none") +
  labs(title = "Correspondence Analysis of 3rd 20 food groups and locations",
       colour = NULL, shape = NULL) +
  scale_color_manual(values = c("#e41a1c", "#377eb8")) + 
  # , caption = "Coordinates in symmetric") + 
  theme(plot.caption = element_text(hjust = 0))
plot(p)


# ca for food and location among teens ------------------------------------

library(FactoMineR)
library(factoextra)
library(ca)
res.ca <- CA(as.data.frame.matrix(freqtab), graph = FALSE)
# res.ca <- ca(as.data.frame.matrix(freqtab), graph = FALSE)
ca.plot <- plot(res.ca)

fviz_ca_biplot(res.ca, repel = TRUE) +  
  theme(plot.caption = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_line(colour = "lightgray",  size = 0.6, linetype = "dotted"), 
        panel.grid.minor = element_line(size = 1.6, linetype = "dotted"), 
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 15, colour = "gray0"), 
        plot.title = element_text(size = 21, 
                                  face = "bold", hjust = 0.5, vjust = 2), 
        panel.background = element_rect(fill = "white", size = 0), legend.position = "none") +
  labs(title = "Correspondence Analysis of food groups and locations") + 
  theme(plot.caption = element_text(hjust = 0)) 
