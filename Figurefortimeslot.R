
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
  scale_color_manual(values = c("#e41a1c", "#377eb8")) + 
  # labs(title = "Correspondence Analysis of food groups and Time Slots.", 
  #      colour = NULL, shape = NULL) +
  # , caption = "Coordinates in symmetric") + 
  theme(plot.caption = element_text(hjust = 0)) #+ 
  # scale_x_reverse() + 
  # scale_y_reverse()
plot(p)


# stratification by Hpoint ------------------------------------------------

TableFoogGroup <- Food1_9_adlt %>% 
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

G1Foods <- as.data.frame.matrix(freqtab)[rownames(as.data.frame.matrix(freqtab)) %in% TableFoogGroup$mfgLab[TableFoogGroup$HealthPoints3g == 1],]
G2Foods <- as.data.frame.matrix(freqtab)[rownames(as.data.frame.matrix(freqtab)) %in% TableFoogGroup$mfgLab[TableFoogGroup$HealthPoints3g == 2],]
G3Foods <- as.data.frame.matrix(freqtab)[rownames(as.data.frame.matrix(freqtab)) %in% TableFoogGroup$mfgLab[TableFoogGroup$HealthPoints3g == 3],]

library(FactoMineR)
library(factoextra)
G1.ca <- CA(G1Foods, graph = FALSE)
fviz_ca_biplot(G1.ca, 
               repel = TRUE, 
               title = "Biplot of Correspondence analysis for food group 1 (1st 20 healthy foods).") + 
  scale_x_reverse() + 
  scale_y_reverse()

G2.ca <- CA(G2Foods, graph = FALSE)
fviz_ca_biplot(G2.ca, 
               repel = TRUE, title = "Biplot of Correspondence analysis for food group 1 (2nd 20 healthy foods).") 
G3.ca <- CA(G3Foods, graph = FALSE)
fviz_ca_biplot(G3.ca, 
               repel = TRUE, title = "Biplot of Correspondence analysis for food group 1 (3rd 20 healthy foods).") 


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
  labs(title = "Correspondence Analysis of 1st 20 food groups and Time Slots",
       colour = NULL, shape = NULL) +
  scale_color_manual(values = c("#e41a1c", "#377eb8")) + 
  # , caption = "Coordinates in symmetric") + 
  theme(plot.caption = element_text(hjust = 0)) + 
  scale_y_reverse()
  
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
  labs(title = "Correspondence Analysis of 2nd 20 food groups and Time slots",
       colour = NULL, shape = NULL) +
  scale_color_manual(values = c("#e41a1c", "#377eb8")) + 
  # , caption = "Coordinates in symmetric") + 
  theme(plot.caption = element_text(hjust = 0)) + 
  scale_x_reverse() #+ 
  # scale_y_reverse()
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
  labs(title = "Correspondence Analysis of 3rd 20 food groups and Time slots",
       colour = NULL, shape = NULL) +
  scale_color_manual(values = c("#e41a1c", "#377eb8")) + 
  # , caption = "Coordinates in symmetric") + 
  theme(plot.caption = element_text(hjust = 0))+ 
  scale_x_reverse() #+ 
# scale_y_reverse()
plot(p)




# stratified by DM status -------------------------------------------------
load("../CA-NDNSRP/HFood.Rdata")

Foodhghlght <- c("Puddings",
                 "Reg soft drinks",
                 "Sugar confectionery",
                 "Chocolate",
                 "Spirits and liqueurs",
                 "Beer lager",
                 "Ice cream",
                 "Biscuits",
                 "Crisps")

## diagnosed DM ---------------
DiagDM <- as.logical(HFood$DM4cat.y == 3)
DiagDMtab <- xtabs(~HFood$mfgLab + HFood$MealTimeSlot, subset = DiagDM)
DiagDMmatrix <- matrix(data = DiagDMtab, nrow = 59, ncol = 7, 
                       dimnames = list(rownames(DiagDMtab), colnames(DiagDMtab)))

DiagDM.ca <- CA(DiagDMmatrix, graph = FALSE)
fviz_ca_biplot(DiagDM.ca, 
               repel = TRUE, title = "Biplot of Correspondence analysis among diagnosed DM.") 

## non-diabetes -----------

DiagDM <- as.logical(HFood$DM4cat.y == 0)
DiagDMtab <- xtabs(~HFood$mfgLab + HFood$MealTimeSlot, subset = DiagDM)
DiagDMmatrix <- matrix(data = DiagDMtab, nrow = 60, ncol = 7, 
                       dimnames = list(rownames(DiagDMtab), colnames(DiagDMtab)))

DiagDM.ca <- CA(DiagDMmatrix, graph = FALSE)
fviz_ca_biplot(DiagDM.ca, 
               repel = TRUE, title = "Biplot of Correspondence analysis among non-diabetes participants.") 


## undiagnosed DM ---------------


DiagDM <- as.logical(HFood$DM4cat.y == 2)
DiagDMtab <- xtabs(~HFood$mfgLab + HFood$MealTimeSlot, subset = DiagDM)
DiagDMmatrix <- matrix(data = DiagDMtab, nrow = 59, ncol = 7, 
                       dimnames = list(rownames(DiagDMtab), colnames(DiagDMtab)))

DiagDM.ca <- CA(DiagDMmatrix, graph = FALSE)
fviz_ca_biplot(DiagDM.ca, 
               repel = TRUE, title = "Biplot of Correspondence analysis among undiagnosed DM participants.") 

## Prediabetes --------

DiagDM <- as.logical(HFood$DM4cat.y == 1)
DiagDMtab <- xtabs(~HFood$mfgLab + HFood$MealTimeSlot, subset = DiagDM)
DiagDMmatrix <- matrix(data = DiagDMtab, nrow = 59, ncol = 7, 
                       dimnames = list(rownames(DiagDMtab), colnames(DiagDMtab)))

DiagDM.ca <- CA(DiagDMmatrix, graph = FALSE)
fviz_ca_biplot(DiagDM.ca, 
               repel = TRUE, title = "Biplot of Correspondence analysis among Prediabetes participants.") 

## Total Sample --------
Totaltab <- xtabs(~HFood$mfgLab + HFood$MealTimeSlot)
Totalmatrix <- matrix(data = Totaltab, nrow = 60, ncol = 7, 
                       dimnames = list(rownames(Totaltab), colnames(Totaltab)))


## People without DM information --------

MissDM <- is.na(HFood$DM4cat.y)
MissDMtab <- xtabs(~HFood$mfgLab + HFood$MealTimeSlot, subset = MissDM)
MissDMmatrix <- matrix(data = MissDMtab, nrow = 60, ncol = 7, 
                       dimnames = list(rownames(MissDMtab), colnames(MissDMtab)))

ca.fit <- ca(DiagDMmatrix)
ca.fit <- ca(Totalmatrix)
ca.fit <- ca(MissDMmatrix)
ca.plot <- plot(ca.fit)


ca.plot.df <- make.ca.plot.df(ca.plot,
                              row.lab = "Food Groups",
                              col.lab = "Location")
ca.plot.df$StrVariable <- ifelse(ca.plot.df$Label %in% Foodhghlght, 
                              "highlight", ca.plot.df$Variable)

ca.plot.df$Size <- ifelse(ca.plot.df$Variable == "Location", 2, 1)
ca.sum <- summary(ca.fit)
dim.var.percs <- ca.sum$scree[,"values2"]

p <- ggplot(ca.plot.df, aes(x = Dim1, y = Dim2,
                            col = StrVariable, shape = Variable,
                            label = Label, size = Size)) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = 1) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = 1) +
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
  scale_color_manual(values = c("#4daf4a", "#e41a1c", "#377eb8")) + 
  # labs(title = "Correspondence Analysis of food groups and Time Slots\n in participants without DM information.",
  # labs(title = "Correspondence Analysis of food groups and Time Slots\n in total sample. ",
  # labs(title = "Correspondence Analysis of food groups and Time Slots\n in pre-diabetics. ",
  # labs(title = "Correspondence Analysis of food groups and Time Slots\n in non-diabetics. ",
  # labs(title = "Correspondence Analysis of food groups and Time Slots\n in diabetics. ",
  labs(title = "Correspondence Analysis of food groups and Time Slots\n in undiagnosed diabetics. ",
  colour = NULL, shape = NULL) +
  # , caption = "Coordinates in symmetric") +
  theme(plot.caption = element_text(hjust = 0))  + 
# scale_x_reverse() + 
   scale_y_reverse()
plot(p)

