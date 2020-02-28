
# All Data Input from Original Charts can be found at 
# https://medium.com/nightingale/w-e-b-du-bois-staggering-data-visualizations-are-as-powerful-today-as-they-were-in-1900-64752c472ae4

# Load Required Libraries
##############################################################################
library(data.table)
library(ggplot2)
library(dplyr)

# Load Data From Nightingale
##############################################################################
s0 <- data.table(year = rep(c(1790,1800,1810,1820,1830,1840,1850,1860,1870),2),
                 group = c(rep("Slaves",9),rep("Free",9)),
                 per = c(0.92,0.89,0.865,0.87,0.86,0.87,0.88,0.89,0,
                         0.08,0.11,0.135,0.13,0.14,0.13,0.12,0.11,1))

s1 <- s0 %>% 
  filter(group =="Free")
s1$label1 = ifelse(s1$per<1,paste(s1$per * 100,"%"),"")
s1$label2 = ifelse(s1$per==1,paste(s1$per * 100,"%"),"")


# Create Chart
##############################################################################
ggplot(s0, aes(x=year, y=per, fill=group)) + 
  geom_area(alpha = 0.8, size=1) +
  annotate(geom="text", x=s1$year, y=1-s1$per, fontface = 2,
           label = s1$label1, size = 4, vjust=-0.5,
           colour = "black") +
  annotate(geom="text", x=s1$year, y=s1$per-0.1, fontface = 2,
           label = s1$label2, size = 4, vjust=-0.5,
           colour = "black") +
  # aes(color=group) +
  # geom_text(data = filter(s0, group == "Free"), show.legend = FALSE,
  #           aes_string(label = s0$per), vjust=-1, size=2) +
  theme_minimal() +
  scale_fill_manual(values=c('#217748','#101010')) + 
  scale_x_continuous(position = "top", 
                     breaks = as.numeric(seq(1790, 1870, by=10))) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face = "bold", vjust = 50),
        axis.text.y = element_blank()) +
  annotate("text",  x = 1830, y = 0.5, fontface = 2,
           label = "SLAVES\n ESCLAVES", size = 5, color = "white") +
  annotate("text",  x = 1830, y = 0.95, fontface = 2, 
           label = "FREE - LIBRE", size = 5, color = "black") +
  labs(title = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN BLACKS .",
       subtitle = "PROPORTION DES NOIR LIBRES ET DES ESCLAVES EN AMERIQUE . \n \n DONE BY ATLANTA UNIVERSITY.",
       x="", y="")

myplot

png("myplot.png")
ggsave("C:/Users/gregs/Hugo/bin/greg_shick/content/post/DuBois-Part_3/myplot.png")