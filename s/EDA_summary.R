# 0) Load the tidyverse (dplyr + tidyr + ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(cowplot)

alldata = read.csv("./data/processed/alldat_combined_20250827.csv")

# ————————————————————————————————————————————————
# 1) Create logical flags for each criterion, row‐by‐row
# ————————————————————————————————————————————————
pal <- c(Instagram = "#962fbf", Flickr = "#FF0084", iNaturalist = "#74AC00", GBIF = "#175CA1")

pipeline_summary <- alldata %>%
  group_by(source_type) %>%
  summarize(
    Total = n(),
    Shark = sum(sd_is_shark, na.rm = TRUE),
    Spatiotemporal = sum(sd_is_shark & has_spatiotemporal, na.rm = TRUE),
    Wild = sum(sd_is_shark & has_spatiotemporal & is_wild, na.rm = TRUE),
    Validated = sum(cs_is_shark & has_spatiotemporal & is_wild & is_validated, na.rm = TRUE)
  ) %>%
  ungroup()

pipeline_summary[2,2] = 2635400

plot_data <- pipeline_summary %>%
  pivot_longer(
    cols      = c(Total, Shark, Spatiotemporal, Wild, Validated),
    names_to  = "step_name",
    values_to = "count"
  )

Y_LIMIT <- 1e6
fmt_m   <- function(x) scales::label_number(accuracy = 0.1, scale = 1e-6, suffix = " M")(x)

# lock source order for plotting & legend
source_order <- c("GBIF", "Instagram", "Flickr", "iNaturalist")
plot_data <- plot_data %>%
  mutate(
    source_type = factor(source_type, levels = source_order),
    step_name   = factor(step_name, levels = c("Total","Shark","Spatiotemporal","Wild","Validated")),
    count_cap   = pmin(count, Y_LIMIT)
  )

# rows that exceed window -> label at top with per-source x jitter
tops <- plot_data %>%
  filter(count > Y_LIMIT) %>%
  mutate(
    x_num = as.numeric(step_name),
    # per-source horizontal jitter (left/right); tweak as needed
    x_off = case_when(
      source_type == "Flickr" ~ -0.1,
      source_type == "GBIF"   ~  0.3,
      TRUE                    ~  0.00
    ),
    x_lab = x_num + x_off,
    # arrow tail a bit below the cap, head at cap
    y0 = Y_LIMIT * 0.92,
    y1 = Y_LIMIT,
    # label text a bit below the cap and slightly past the arrow head
    y_lab = Y_LIMIT - Y_LIMIT*0.06,
    label = scales::label_number(accuracy = 0.1, scale = 1e-6, suffix = " M")(count)
  )

p_pipeline <- ggplot(plot_data,
                     aes(x = step_name, y = count_cap, group = source_type, color = source_type)
) +
  geom_line(size = 1.5) +
  geom_point(size = 3.5, shape = 16) +
  
  # top labels: jittered left/right by source; don't affect legend
  # 1) arrow at the top window
  geom_segment(
    data = tops,
    aes(x = x_lab, xend = x_lab, y = y0, yend = y1, color = source_type),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(6, "pt"), type = "closed"),
    linewidth = 0.9,
    show.legend = FALSE
  ) +
  
  # 2) jittered text (side + down)
  geom_text(
    data = tops,
    aes(x = x_lab, y = y_lab, label = label, color = source_type),
    inherit.aes = FALSE,
    hjust = ifelse(tops$x_off < 0, 1.1, ifelse(tops$x_off > 0, -0.1, 0.5)),  # flip alignment by side
    vjust = 0.5,
    size = 3.4,
    fontface = "plain",
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = pal,
    breaks = source_order,   # lock legend order
    drop   = FALSE
  ) +
  scale_x_discrete(labels = c(
    "Total","Shark","Geolocation","Naturally Occurring","Human Validated"
  )) +
  scale_y_continuous(
    limits = c(0, Y_LIMIT),
    breaks = scales::pretty_breaks(n = 10),  # aim for ~10 ticks
    labels = function(x) paste0(x / 1000, "k"),
    expand = expansion(add = c(5000, 5000))
  ) +
  labs(
    x = "Filtering Step",
    y = "Posts and Records",
    color = "Open Data Source",
    title = "Retention of Posts after Automatic Filtering"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    panel.grid  = element_blank(),
    plot.margin = margin(t = 14, r = 10, b = 10, l = 10)
  ) +
  coord_cartesian(ylim = c(0, Y_LIMIT), clip = "off")
p_pipeline

# ─────────────────────────────────────────────────────────────────────────────
# EXAMPLE: Create a simple monthly time‐series data.frame.
#    Assume you have a data.frame `alldat4` with at least:
#      • source_type  (factor/string: "iNaturalist", "Flickr", "Instagram")
#      • date         (a Date or POSIXt stamp for each post)
#
#    We’ll bucket by Year‐Month and count posts per network.
# ─────────────────────────────────────────────────────────────────────────────

time_series_df <- alldata %>%
  # Extract Year‐Month as the first day of each month:
  mutate(yearmon = floor_date(as.Date(date), unit = "month")) %>%
  group_by(source_type, yearmon) %>%
  filter(!is.na(yearmon)) %>%
  filter(is_wild) %>%
  summarize(monthly_count = n(), .groups = "drop")

# (Optional) If you want to “densify” because some months have zero posts:
# ensure every network has a row for every yearmon in the overall range:
all_yearmon <- seq(
  from = min(time_series_df$yearmon),
  to   = max(time_series_df$yearmon),
  by   = "1 month"
)
networks   <- unique(time_series_df$source_type)
complete_grid <- expand.grid(source_type = networks, yearmon = all_yearmon)

time_series_df <- complete_grid %>%
  left_join(time_series_df, by = c("source_type", "yearmon")) %>%
  mutate(monthly_count = replace_na(monthly_count, 0)) 

time_series_df$source_type <- factor(
  time_series_df$source_type,
  levels = c("GBIF", "Instagram", "Flickr", "iNaturalist")
)


# ─────────────────────────────────────────────────────────────────────────────
# 2) Build the “time‐series” ggplot (p_ts):
#    • x = yearmon (monthly), y = monthly_count
#    • color = source_type with the same custom colors
# ─────────────────────────────────────────────────────────────────────────────
p_ts = ggplot(time_series_df, aes(x = yearmon, y = monthly_count, color = source_type)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = pal,
    breaks = c("GBIF", "Instagram", "Flickr", "iNaturalist")  # ensure legend follows this order too
  ) +
  scale_y_continuous(
    limits = c(0, 10000),
    labels = function(x) paste0(x/1000, "k"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_date(
    # set breaks every 2 years (for readability), with labels “Jan YY”
    date_breaks = "2 years",
    date_labels = "%Y",
    limits     = c(as.Date("1998-01-01"), as.Date("2025-01-01"))
  ) +
  labs(
    x     = NULL,
    y     = "Posts",
    color = NULL,
    title = "Total Observation Potential",
    subtitle = "Confirmed (by human) or automatic observation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title  = element_text(hjust = 0.5, face = "bold", size = 12.75),
    plot.subtitle = element_text(hjust = 0.5, size = 12.25),
    axis.text.x.bottom = element_text(size = 10),
    axis.text.y.left = element_text(size = 10),
    axis.title.y = element_text(size = 11.5),
    # legend.position = "none",
    panel.grid = element_blank()
  )
p_ts


# ─────────────────────────────────────────────────────────────────────────────
# 4) Inset `p_ts` into `p_pipeline` using cowplot::ggdraw()
# ─────────────────────────────────────────────────────────────────────────────

combined_plot <- ggdraw() +
  draw_plot(p_pipeline,  x = 0,   y = 0,   width = 1,   height = 1) + 
  draw_plot(p_ts,        x = 0.3, y = 0.4, width = 0.50, height = 0.50)

# Save to disk:
ggsave(
  filename = "./figures/summary.png",
  plot     = combined_plot,
  width    = 10,    # inches
  height   = 6,     # inches
  dpi      = 300
)

ggsave(
  filename = "./figures/summary_retention.png",
  plot     = p_pipeline,
  width    = 9,    # inches
  height   = 6,     # inches
  dpi      = 300
)

ggsave(
  filename = "./figures/summary_p_ts.png",
  plot     = p_ts,
  width    = 9,    # inches
  height   = 6,     # inches
  dpi      = 300
)
