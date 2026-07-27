library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
library(ggiraph)
library(htmlwidgets)


metadata <- read.csv("disk_usage_metadata.txt", sep = '\t', header = F)
names(metadata) <- c('user', 'group')


#df <- read.csv('disk_usage_gxxiao.txt', sep = ' ')
df <- read.csv('/u/project/gxxiao/giovas/sea', sep = ' ')

names(df) <- c('weekday', 'month',  'day', 'time', 'timezone', 'year', 'user', 'user_id', 'mem', 'mem_quota', 'files', 'filequota')
df$datetime <- as.POSIXct(paste(df$month, df$day, df$time, df$year), format = "%b %d %H:%M:%S %Y", tz = "America/Los_Angeles")

df$mem <- df$mem/1024
df <- df[df$mem > 2, ]
max_mem <- max(df$mem, na.rm = TRUE) + 1
df <- df %>% left_join(metadata, by = 'user')

labdf <- df %>% group_by(user) %>% slice_max(order_by = datetime, n = 1, with_ties = FALSE) %>% ungroup()


g <- ggplot(df, aes(x = datetime, y = mem))+
	geom_line(aes(color = user, group = user))+
	geom_label_repel(data = labdf, aes(label = user, color = user), max.overlaps = 16, direction = "x") + 
	labs(x = "date", y = "usage in Tb") +
	facet_grid(group~., scales = 'free')+
	scale_y_continuous(breaks = seq(2, max_mem, 2)) + 
	theme_bw() +
	theme(legend.position = "none") 



g <- girafe(ggobj = g)

saveWidget(g, "interactive_plot.html")


pdf('/u/home/g/giovas/gxxiao/disk_usage_gxxiao.pdf', width = 10, height = 19)
print(g)
dev.off()






