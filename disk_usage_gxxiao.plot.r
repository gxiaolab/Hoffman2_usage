library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
library(ggiraph)
library(htmlwidgets)



#df <- read.csv('disk_usage_gxxiao.txt', sep = ' ')
df <- read.csv('/u/project/gxxiao/giovas/sea', sep = ' ')

names(df) <- c('weekday', 'month',  'day', 'time', 'timezone', 'year', 'user', 'user_id', 'mem', 'mem_quota', 'files', 'filequota')
df$datetime <- as.POSIXct(paste(df$month, df$day, df$time, df$year), format = "%b %d %H:%M:%S %Y", tz = "America/Los_Angeles")

df$mem <- df$mem/1024
df <- df[df$mem > 2, ]
max_mem <- max(df$mem, na.rm = TRUE) + 1

labdf <- df %>% group_by(user) %>% slice_max(order_by = datetime, n = 1, with_ties = FALSE) %>% ungroup()



g <- ggplot(df, aes(x = datetime, y = mem, data_id = user, tolltip = user))+
	geom_line_interactive(aes(color = user, group = user))+
	geom_label_interactive(data = labdf, aes(label = user, color = user), size = 2.5) + 
	labs(x = "date", y = "usage in Tb") +
	scale_y_continuous(breaks = seq(2, max_mem, 2)) + 
	theme_bw() +
	theme(legend.position = "none") 



g <- girafe(ggobj = g, options = list(
        opts_hover(css = "stroke-width:1;"),
        opts_hover_inv(css = "opacity:0.25;")
    ))

saveWidget(g, "index.html", selfcontained = TRUE)
