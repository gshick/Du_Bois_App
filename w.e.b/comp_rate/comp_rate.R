
# Load Required Libraries
##############################################################################
library('rvest')
library('dplyr')
library('stringr')
library('ggplot2')
library('reshape2')
library('scales')
library('grid')
library('tidyr')

# Wikipedia data source
url <- 'https://en.wikipedia.org/wiki/Historical_racial_and_ethnic_demographics_of_the_United_States'

# Read HTML code from the URL
webpage <- read_html(url)

# Using CSS selectors to scrape the table
tbls <- html_nodes(webpage,'table')

# Pull data from 1760-1840
##############################################################################
w0 <- html_table(tbls[grep("1760 and 1840",tbls,ignore.case = T)],fill = T)[[1]] %>%
  filter(`Race/Ethnic Group` %in% c("White", "Black (also called Negro)")) %>%
  select(-`1760`, -`1770`) %>%
  rename('ethnicity' = `Race/Ethnic Group`) %>%
  mutate(ethnicity = str_replace(ethnicity,"Black [(]also called Negro[)]","Black"))


# Pull data from 1850-1920
##############################################################################  
w1 <- html_table(tbls[grep("1850 and 1920",tbls,ignore.case = T)],fill = T)[[1]] %>%
  filter(`Race/Ethnic Group` %in% c("White", "Black")) %>%
  select(-`1900`, -`1910`, -`1920`) %>%
  rename('ethnicity' = `Race/Ethnic Group`)

# Combine data into single frame, years 1790-1890
############################################################################## 
w2 <- inner_join(w0,w1,by='ethnicity')

# Reshape data and prepare for graph
############################################################################## 
w3 <- melt(w2, id.vars = "ethnicity") %>%
  arrange(ethnicity) %>%
  rename(pop=value, year=variable)
w3$pop <- as.numeric(gsub(",","",w3$pop))

# Calculate % change values
############################################################################## 
w4 <- w3 %>%
  gather(key = key, value = value, pop) %>%
  group_by(ethnicity, key) %>%
  mutate(lag = lag(value)) %>%
  mutate(pct = (value - lag) / lag) %>%
  ungroup() %>%
  select(-key, -lag) %>%
  rename(pop=value) %>%
  filter(year!=1780)

# Create Chart
############################################################################## 
ggplot(data=w4, aes(x=year, y=pop, group=ethnicity, color=ethnicity)) +
  geom_line(size=3) +
  geom_text(label = scales::percent(w4$pct), vjust=-2, show_guide = FALSE, size=2) + 
  theme_minimal() +
  scale_color_manual(values=c('#CB0432','#354733'),
                     guide = guide_legend(reverse=TRUE, label.position = "left"),
                     labels=c("BLACK-NOIRE-","WHITE-BLANCHE-")) + 
  scale_y_continuous(labels = as.numeric(seq(5, 50, by=5)),
                     breaks = as.numeric(seq(5000000, 50000000, by=5000000))) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title=element_blank(),
        legend.position = c(0.2, 0.75),
        plot.background = element_rect(fill = '#ECDED1', colour = '#ECDED1'),
        panel.background = element_rect(fill = '#ECDED1', colour = '#ECDED1')) +
  annotate("text",  x = 3, y = 35000000, 
           label = "SMALL FIGURES = PERCENTAGE OF \n
           INCREASE PER DECADE \n
           LES PETITS CHIFFRES INDIQUENT L'ACROISSEMENT \n
           PROPORTIONAL PAR DIX ANS", size = 1.5) + 
  annotate("text",  x = 3, y = 19000000, 
           label = "SUPRESSION OF SLAVE TRADE", size = 1.5, angle = -90) +
  annotate("text",  x = 6, y = 50000000, 
           label = "EUROPEAN IMMIGRATION", size = 1.5, angle = -90, vjust = -3) +
  annotate("text",  x = 8, y = 52000000, 
           label = "EMANCIPATION", size = 1.5, angle = -90, vjust = -3) +
  labs(title = "Comparative rate of increase of the White and Black elements of the \n \n population of the United States.",
       subtitle = "Accroissement proportionnel des elements blancs et noirs aux Etats Unis \n \n Done by Atlanta Univeristy.",
       x="", y="")
