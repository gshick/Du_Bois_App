# All Data Input from Original Charts can be found at 
# https://medium.com/nightingale/w-e-b-du-bois-staggering-data-visualizations-are-as-powerful-today-as-they-were-in-1900-64752c472ae4

# Load Required Libraries
##############################################################################
library(data.table)
library(ggplot2)
library(dplyr)

# Load Data From Nightingale
##############################################################################
g0 <- data.table(year = rep(c(1790,1800,1810,1820,1830,1840,1850,1860,1870),2),
                 group = c(rep("Slaves",9),rep("Free",9)),
                 per = c(0.987,0.983,0.983,0.988,0.992,0.991,0.993,0.992,0,
                         0.013,0.017,0.017,0.012,0.008,0.009,0.007,0.008,1))

# Create Chart
##############################################################################
ggplot(g0, aes(x=year, y=per, fill=group)) +
  geom_area(alpha = 0.8, size=1) +
  coord_flip() +
  scale_x_reverse(breaks = as.numeric(seq(1790, 1870, by=10))) +
  scale_y_reverse(position = "top", breaks = as.numeric(seq(0, 0.03, by=0.01))) +
  theme_minimal() +
  scale_fill_manual(values=c('#CB0432','#101010')) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "SLAVES AND FREE MEN .",
       x="", y="")