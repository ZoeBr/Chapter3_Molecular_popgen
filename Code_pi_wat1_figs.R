# Pixy output 

#plotting nucleotide diversity
#WAT1 gene region across all populations

library (ggplot2)
library (dplyr)
install.packages("nlme")
library(mgcv)

Chrom1_pi_dat <- read.table("wgs_test2_1500window_pi.txt", header = TRUE, sep = "\t")
head(Chrom1_pi_dat)
#remove NA's
cleaned_chrom1_pi <- subset(Chrom1_pi_dat, !is.na(avg_pi))
head(cleaned_chrom1_pi)
#Wat1 region between 35853322-35856354
Wat1_pi_subset <- subset(cleaned_chrom1_pi, window_pos_1 >= 35800501 & window_pos_1 <= 35900000)
head(Wat1_pi_subset)
#create dotplot
ggplot(Wat1_pi_subset, aes(x = window_pos_1, y = avg_pi, color = pop)) +
  geom_point(size = 3) +
  labs(
    x = "genome pos",
    y = "Ave Pi",
    color = "population"
  ) +
  theme_minimal()


#subset for D01H01
D01H01_Wat1_pi <- subset(Wat1_pi_subset, pop %in% c("D01", "H01"))
head(D01H01_Wat1_pi)

Plot_1 <- ggplot(D01H01_Wat1_pi, aes(x = window_pos_1, y = avg_pi, color = pop, group = pop)) +
  geom_point(size = 1.5) +
  labs(
    title = "WAT1 pi D01H01",
    x = "Chromosome 1 position",
    y = "Average nucleotide diversity (Pi)",
    color = "population"
  ) +
  xlim(35820000, 35900000) +
  ylim(0, 0.25) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
Plot_1

#D03H02
D03H02_Wat1_pi <- subset(Wat1_pi_subset, pop %in% c("D03", "H02"))

Plot_2 <- ggplot(D03H02_Wat1_pi, aes(x = window_pos_1, y = avg_pi, color = pop, group = pop)) +
  geom_point(size = 1.5) +
  labs(
    title = "WAT1 pi D03H02",
    x = "Chromosome 1 position",
    y = "Average nucelotide diversity (Pi)",
    color = "Population"
  ) +
  xlim(35820000, 35900000) +
  ylim(0, 0.25) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

#D04H05
D04H05_Wat1_pi <- subset(Wat1_pi_subset, pop %in% c("D04", "H05"))

Plot_3 <- ggplot(D04H05_Wat1_pi, aes(x = window_pos_1, y = avg_pi, color = pop, group = pop)) +
  geom_point(size = 1.5) +
  labs(
    title = "WAT1 pi D04H05",
    x = "Chromosome 1 position",
    y = "Average nucelotide diversity (Pi)",
    color = "Population"
  ) +
  xlim(35820000, 35900000) +
  ylim(0, 0.3) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

#D05H06
D05H06_Wat1_pi <- subset(Wat1_pi_subset, pop %in% c("D05", "H06"))

Plot_4 <- ggplot(D05H06_Wat1_pi, aes(x = window_pos_1, y = avg_pi, color = pop, group = pop)) +
  geom_point(size = 1.5) +
  labs(
    title = "WAT1 pi D05H06",
    x = "Chromosome 1 position",
    y = "Average nucelotide diversity (Pi)",
    color = "Population"
  ) +
  xlim(35820000, 35900000) +
  ylim(0, 0.25) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

#D32H12
D32H12_Wat1_pi <- subset(Wat1_pi_subset, pop %in% c("D32", "H12"))

Plot_5 <- ggplot(D32H12_Wat1_pi, aes(x = window_pos_1, y = avg_pi, color = pop, group = pop)) +
  geom_point(size = 1.5) +
  #geom_line() +
  labs(
    title = "WAT1 pi D32H12",
    x = "Chromosome 1 position",
    y = "Average nucelotide diversity (Pi)",
    color = "Population"
  ) +
  xlim(35820000, 35900000) +
  ylim(0, 0.25) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

#D00H00
D00H00_Wat1_pi <- subset(Wat1_pi_subset, pop %in% c("D00", "H00"))

Plot_6 <- ggplot(D00H00_Wat1_pi, aes(x = window_pos_1, y = avg_pi, color = pop, group = pop)) +
  geom_point(size = 1.5) +
  #geom_line() +
  labs(
    title = "WAT1 pi D00H00",
    x = "Chromosome 1 position",
    y = "Average nucelotide diversity (Pi)",
    color = "Population"
  ) +
  xlim(35820000, 35900000) +
  ylim(0, 0.25) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

#Combine into a big plot
install.packages("patchwork")
library (patchwork)
combined_plot <- (Plot_6 | Plot_1 | Plot_2) / (Plot_3 | Plot_4 | Plot_5) +
  plot_layout(guides = "collect")
combined_plot


#DXY pixy output


  