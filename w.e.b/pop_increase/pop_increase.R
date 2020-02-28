
# All Data Input from Original Charts can be found at 
# https://medium.com/nightingale/w-e-b-du-bois-staggering-data-visualizations-are-as-powerful-today-as-they-were-in-1900-64752c472ae4

# Load Required Libraries
##############################################################################
library(data.table)
library(plotly)
library(ggplot2)

# Load Data From Nightingale
##############################################################################
w0 <- data.table(year = c(1750,1760,1770,1780,1790,1800,1810,1820,1830,1840,1850,1860,1870,1880,1890), 
                 pop = c(220000,310000,462000,562000,757208,1002037,1377808,1771646, 2328642, 2873648, 3638808, 4441830, 4880009, 6580793, 7470040))
w0$labs = paste(w0$year, format(w0$pop,big.mark=","), sep = "  -  ")

# Create Chart
##############################################################################

ggplot(w0, aes(x=reorder(labs, order(-year)), y=pop)) +
  geom_bar(stat = 'identity', width = 0.5, fill = '#CB0432', alpha = 0.9) +
  coord_flip() + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill = '#ECDED1', colour = '#ECDED1'),
        panel.background = element_rect(fill = '#ECDED1', colour = '#ECDED1'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(face = "bold")) +
  labs(title = "Increase of the Black population in the United States of America.",
       subtitle = "Accroissement de la population Noirs aux Etats Unis d' Amerique. \n \n Done by Atlanta Univeristy.",
       x = " ", y ="")
