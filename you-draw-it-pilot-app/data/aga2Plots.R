library(tidyverse)
library(plotly)
library(scales)

# Aga2 (Soybean Canopy Cover) --------------------------------------------------------
aga2 <- read.csv("~/GitHub/Perception-of-Log-Scales/PotentialData/Aga2.csv")
names(aga2)

aga2Long <- aga2 %>%
  pivot_longer(
    cols = X14:X70,
    values_to = "y"
  ) %>%
  mutate(x = (as.numeric(substr(name,2,3))-14),
         Yr = as.factor(Yr)) %>%
  select(NUID, Yr, Strain, x, y)
 
agaPlot_linear <- aga2Long %>%
              filter(NUID == 1) %>%
              # filter(NUID <= 50) %>%
            ggplot(aes(x = x, y = y, color = Strain)) +
              geom_line() +
              theme_bw() +
              scale_color_brewer(palette = "Paired")
agaPlot_linear
ggplotly(agaPlot_linear)


agaPlot_log <- aga2Long %>%
  # filter(NUID == 1) %>%
  filter(NUID <= 50) %>%
  ggplot(aes(x = x, y = y, color = Strain)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Paired") +
  scale_y_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x))
agaPlot_log

