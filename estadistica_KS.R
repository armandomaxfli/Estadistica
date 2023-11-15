getwd()

KS <- read.delim("documents/posdoc_Gymnodinium_catenatum/qPCR/expresion_relativa/expresion_KS.txt")

KS$salinidad <- ordered(KS$salinidad,
                        levels = c("20", "25", "30", "35", "40"))

levels(KS$salinidad)

library(dplyr)

medias <- group_by(KS, salinidad) %>%
  summarise(count = n(),
            mean = mean(fold, na.rm = TRUE),
            sd = sd(fold, na.rm = TRUE))

library(ggplot2)
library(ggpubr)

ggboxplot(KS, x = "salinidad", y = "fold",
          color = "salinidad",
          order = c("20", "25", "30", "35", "40"),
          ylab = "Fold", xlab = "salinidad")

res.aov <- aov(fold ~ salinidad, data = KS)

summary(res.aov)

library(agricolae)

Tukey <- HSD.test(res.aov, "salinidad", group = TRUE)

plot(Tukey)

grupos <- Tukey$groups

ggbarplot(medias, x = "salinidad", y = "mean",
          fill = "salinidad",
          xlab = "Salinity", ylab = "Fold") +
  geom_errorbar(data = medias, aes(y = mean, ymin = mean - sd, ymax = mean + sd),
                width = 0.3,
                position = position_dodge(0.9)) +
  ylim(0, 3.5)
