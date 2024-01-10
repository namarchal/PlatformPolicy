library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
#display.brewer.all()

df_categories <- readRDS("~/Dropbox/PRODIGI Team/Platform_Policy/figures/df_plot.rds")


# Figure 1
df_categories <- 
  df_categories %>%
  mutate(pubDateTime = ymd(pubDateTime),
         pub_year = year(pubDateTime),
         pub_month = month(pubDateTime),
         pub_quarter = quarter(pubDateTime),
         category = ifelse(category == "Politics&Regulation", "Politics & Regulation", category))
  
df_categories %>%
  filter(pub_year < 2021) %>%
  group_by(pub_year, pub_quarter, category) %>%
  summarise(cat_freq = sum(cat_freq)) %>%
  mutate(pub_quarter_label = paste(pub_year, " Q", as.character(pub_quarter), sep = ""),
         pub_quarter_date = ymd(paste(pub_year, 3*pub_quarter, 30, sep = "-")) - days(45)) %>%
  
  ggplot(aes(x = pub_quarter_date, y = cat_freq, fill = category)) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(legend.position = c(0.22, 0.75),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 7)
        ) +
  labs(x = "", y = "Number of Articles\n")

ggsave("figures/PNAS_figure1.pdf", width = 11, height = 11, units = "cm", dpi = 300)


# Figure 2
data <- readRDS("~/Dropbox/PRODIGI Team/Platform_Policy/df2021.rds")

ROLLING_WINDOW_WIDTH <- 20 # in weeks

important_dates_df <- tibble(
  id = c("Facebook", "YouTube", "Facebook", "YouTube", "Facebook", "Facebook", "Twitter"),
  event_dates = ymd(c("2011-09-01", "2017-03-01", "2018-03-17", "2018-03-10", "2019-07-19", "2020-10-01", "2020-10-01")),
  happening = c(
    "The Social Network released",
    "YouTube faces boycott from\nadvertisers over extremist material",
    "Cambridge Analytica scandal breaks",
    "Publication of 'The Great Radicalizer'",
    "FTC imposes 5bn\nfine on Facebook",
    "Facebook faces\nconsorship allgations",
    "Twitter faces\ncensorship allegations"
  ),
  y_nudge = c(0, -1, 0, 2, -2, 1, -3) * 0.05
)

ANNOTATION_HEIGHT <- 0.7
data %>%
  group_by(id) %>%
  #select(id, year, week, n_plat, n_neg) %>%
  mutate(n_plat = ifelse(is.na(n_plat), 0, n_plat),
         n_neg = ifelse(is.na(n_neg), 0, n_neg)) %>%
  mutate(plot_date = ymd(paste(as.character(year), "01", "01", sep = "-")) + weeks(week),
         prop = ifelse(n_plat == 0, 0, n_neg/n_plat)) %>%
  arrange(id, plot_date) %>%
  mutate(prop_smooth = zoo::rollmean(prop, k = 30, fill = NA)) %>%
  
  #select(id, year, week, month, plot_date, n_plat, n_neg, y)
  
  ggplot(mapping = aes(x = plot_date)) +
  
  geom_segment(mapping = aes(x = event_dates, y = 0,
                             xend = event_dates, yend = y_nudge + ANNOTATION_HEIGHT),
               size = 0.4,
               linetype = "dashed",
               data = important_dates_df) +
  geom_label(mapping = aes(x = event_dates, y = y_nudge + ANNOTATION_HEIGHT, label = happening),
             lineheight = 0.7,
             size = 1.5,
             data = important_dates_df) +
  
  geom_line(aes(y = prop_smooth, color = id),
            size = 0.6,
            alpha = 1) +
  
  scale_color_manual(breaks = c("Facebook", "YouTube", "Twitter"),
                     values = c("#3b5998", "#FF0000", "#1DA1F2")) +
  scale_y_continuous(
    labels = scales::percent,
    name = "Relative Negative Coverage\n",
    limits = c(-0.025, ANNOTATION_HEIGHT + 0.1)
  ) +
  scale_x_date(name = "", expand = c(0.085, 0.085)) +
  
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        legend.position = c(0.15, 0.85),
        legend.background = element_rect(fill = "white", colour = "black"),
        text = element_text(size = 7),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  
  geom_point(aes(y = -0.025, alpha = change, color = id), shape = "|", size = rel(2), show.legend = FALSE) +
  scale_alpha(range = c(0, 1))

ggsave("figures/PNAS_figure2_large.pdf", width = 22, height = 18, units = "cm", dpi = 300)
ggsave("figures/PNAS_figure2_medium.pdf", width = 11, height = 11, units = "cm", dpi = 300)
