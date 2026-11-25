#!/u/local/apps/R/4.5.1/gcc10.2.0_intel2023.1.0/bin/Rscript

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
library(ggiraph)
library(htmltools)
library(htmlwidgets)
library(rmarkdown)
library(lubridate)

#rmarkdown::find_pandoc(version = "2.9.1")

args <- commandArgs(trailingOnly = TRUE)

disk.file <- args[1]
df0 <- read.csv(disk.file, sep = ' ')

names(df0) <- c('weekday', 'month',  'day', 'time', 'timezone', 'year', 'user', 'user_id', 'mem', 'mem_quota', 'files', 'filequota')
df0$datetime <- as.POSIXct(paste(df0$month, df0$day, df0$time, df0$year), 
    format = "%b %d %H:%M:%S %Y", 
    tz = "America/Los_Angeles")
df0$mem <- df0$mem/1024 # in Tb

df <- df0 %>%
  filter(mem >= 1) %>%
  filter(datetime >= today() - days(14))


max_mem <- max(df$mem, na.rm = TRUE) + 1
latest_time <- max(df$datetime, na.rm = TRUE)


df_latest <- df %>%
  filter(datetime == latest_time) %>%
  mutate(pct = mem/(318000/1024) * 100,
    label = sprintf("%.1f%%", pct)) %>%
  arrange(desc(pct)) %>%
  mutate(user = factor(user, levels = user))

cum_usage <- sum(df_latest$mem)
capacity_pct <- cum_usage/(318000/1024) * 100
capacity_status <- if (capacity_pct >= 90) {
  "High utilization"
} else if (capacity_pct >= 75) {
  "Approaching capacity"
} else {
  "Within capacity"
}
capacity_class <- if (capacity_pct >= 90) "high" else if (capacity_pct >= 75) "moderate" else "normal"

g1 <- ggplot(df, aes(x = datetime, y = mem, data_id = user, tooltip = user))+
	geom_line_interactive(aes(color = user, group = user))+
	geom_label_interactive(data = df_latest, aes(label = user, color = user), size = 2.5) + 
	labs(x = "date", y = "usage in Tb") +
	scale_y_continuous(breaks = seq(2, max_mem, 2)) + 
	theme_bw() +
	theme(legend.position = "none")


g_line <- girafe(
  ggobj = g1, 
  width_svg = 5, height_svg = 5,
  options = list(
      opts_hover(css = "stroke-width:1;"),
      opts_hover_inv(css = "opacity:0.25;")
  )
)

g2 <- ggplot(df_latest, aes(x = '1', y = pct)) +
  geom_col_interactive(aes(
      fill = user,
      tooltip = paste0(user, ": ", label),
      data_id = user), width = 0.5) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  labs(x = NULL, y = "Total usage") +
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = NA, colour = NA),
    panel.background = element_rect(fill = NA, colour = NA),
    legend.position = "none",
    axis.text.x = element_blank()
  )

g_bar <- girafe(
  ggobj = g2,
  width_svg = 2, height_svg = 5,
  options = list(
    opts_hover(css = "stroke-width:1;"),
    opts_hover_inv(css = "opacity:0.25;")
  )
)

# Inject the keyframe animation properly (opts_add_css() is not a real function)
g_bar <- htmlwidgets::prependContent(
  g_bar,
  htmltools::tags$style(HTML("
    .ggiraph-svg rect {
      transform-box: fill-box;
      transform-origin: bottom;
      animation: fillBar 1.5s ease-out forwards;
    }
    @keyframes fillBar {
      from { transform: scaleY(0); }
      to   { transform: scaleY(1); }
    }
  "))
)

page <- tagList(
  tags$head(
    tags$style(HTML("
      html, body {
        margin: 0;
        padding: 0;
        min-height: 100%;
        background: #f4f7fb;
        color: #172033;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }
      .page-shell {
        width: min(1440px, 100%);
        box-sizing: border-box;
        margin: 0 auto;
        padding: 32px clamp(18px, 4vw, 56px) 48px;
      }
      .page-header {
        display: flex;
        align-items: flex-end;
        justify-content: space-between;
        gap: 24px;
        margin-bottom: 24px;
        padding: 26px 30px;
        background: #fff;
        border: 1px solid #e4eaf2;
        border-radius: 18px;
        box-shadow: 0 8px 26px rgba(27, 45, 75, 0.06);
      }
      .header-copy { min-width: 0; }
      .eyebrow {
        margin: 0 0 8px;
        color: #526785;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.12em;
        text-transform: uppercase;
      }
      .page-title {
        margin: 0;
        color: #14213d;
        font-size: clamp(26px, 3vw, 36px);
        line-height: 1.15;
        letter-spacing: -0.035em;
      }
      .page-description {
        margin: 9px 0 0;
        color: #62718a;
        font-size: 15px;
        line-height: 1.5;
      }
      .header-meta {
        display: flex;
        align-items: flex-end;
        flex-direction: column;
        gap: 10px;
        flex: 0 0 auto;
      }
      .capacity-badge {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        padding: 9px 13px;
        border-radius: 999px;
        font-size: 13px;
        font-weight: 700;
        white-space: nowrap;
      }
      .capacity-badge::before {
        width: 8px;
        height: 8px;
        border-radius: 50%;
        background: currentColor;
        content: '';
      }
      .capacity-badge.high { color: #a33a32; background: #fff0ed; }
      .capacity-badge.moderate { color: #8a5a08; background: #fff6df; }
      .capacity-badge.normal { color: #21744e; background: #eaf7ef; }
      .updated-at {
        color: #526785;
        font-size: 15px;
        font-weight: 700;
      }
      .charts-row {
        display: flex;
        align-items: stretch;
        flex-direction: row;
        gap: 20px;
      }
      .chart-card {
        min-width: 0;
        padding: 12px;
        background: #fff;
        border: 1px solid #e4eaf2;
        border-radius: 16px;
        box-shadow: 0 5px 20px rgba(27, 45, 75, 0.04);
      }
      @media (max-width: 700px) {
        .page-shell { padding-top: 18px; }
        .page-header { align-items: flex-start; flex-direction: column; padding: 22px; }
        .header-meta { align-items: flex-start; }
        .charts-row { flex-direction: column; }
      }"))
  ),
  tags$div(class = "page-shell",
    tags$header(class = "page-header",
      tags$div(class = "header-copy",
        tags$p(class = "eyebrow", "Storage dashboard"),
        tags$h1(class = "page-title", "Shared disk usage"),
        tags$p(class = "page-description", "A current view of group storage use and its recent trend.")
      ),
      tags$div(class = "header-meta",
        tags$span(class = paste("capacity-badge", capacity_class),
          paste(capacity_status, "·", sprintf("%.1f%% used", capacity_pct))),
        tags$span(class = "updated-at", paste("Latest data:", format(latest_time, "%b %d, %Y · %H:%M:%S %Z")))
      )
    ),
    tags$div(class = "charts-row",
      tags$div(class = "chart-card", style = "flex: 3;", g_line),
      tags$div(class = "chart-card", style = "flex: 1;", g_bar)
    )
  )
)

save_html(page, "/u/project/gxxiao/giovas/disk_usage/index.html")

