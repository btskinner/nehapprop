## -----------------------------------------------------------------------------
##
## [ PROJ ] Plot NEH appropriations
## [ FILE ] appropriations.R
## [ AUTH ] Benjamin Skinner
## [ INIT ] 21 April 2025
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("tidyverse", "rvest", "fredr", "ggtext")
sapply(libs, require, character.only = TRUE)

## paths
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path(".."), args)
fig_dir <- file.path(root, "figures")
scr_dir <- file.path(root, "scripts")

## -----------------------------------------------------------------------------
## FRED
## -----------------------------------------------------------------------------

## -------------------------------------
## inflation adjustment
## -------------------------------------
cpi <- fredr(series_id = "USACPIALLAINMEI",
             observation_start = as.Date("1965-01-01"),
             observation_end = as.Date("2025-01-01")) |>
  mutate(year = year(date),
         adj = value / value[year == 2024]) |>
  select(year, adj)

## -----------------------------------------------------------------------------
## NEH appropriations
## -----------------------------------------------------------------------------

## -------------------------------------
## scrape
## -------------------------------------

df <- read_html("https://www.neh.gov/neh-appropriations-history") |>
  html_node(".field-body-rte") |>
  html_table() |>
  rename(fy = `Fiscal Year`,
         ap = `Appropriations`)

## -------------------------------------
## wrangle
## -------------------------------------|>

df <- df |>
  left_join(cpi, by = c("fy" = "year")) |>
  mutate(ap = parse_number(ap)) |>
  mutate(ap_adj = ap / adj) |>
  pivot_longer(cols = starts_with("ap"),
               names_to = "type") |>
  ungroup()

## -----------------------------------------------------------------------------
## plot
## -----------------------------------------------------------------------------

## -------------------------------------
## consistent labels
## -------------------------------------

title_text <- paste(
  "Annual appropriations to the National Endowment",
  "for the Humanities, 1966\u20132024"
)
x_text <- "Fiscal year"
y_text <- "Appropriations (millions)"

## -------------------------------------
## nominal
## -------------------------------------

## plot
g <- df |>
  filter(type %in% c("ap")) |>
  ggplot(aes(x = fy, y = value / 1000000, color = type, linetype = type)) +
  geom_line() +
  geom_textbox(aes(x = 1973, y = 20,
                   label = paste(
                     "The National Endowment for the Humanities was initially",
                     "appropriated **$5.9 million** in 1966."
                   )),
               fill = "white",
               orientation = "upright",
               hjust = 0,
               vjust = 1,
               width = unit(.235, "npc"),
               color = "black") +
  geom_textbox(aes(x = 1983, y = 210,
                   label = paste(
                     "Appropriations to NEH increased from 1966 to the",
                     "mid-1990s, reaching **$177 million** in 1995.",
                     "From this peak, the agency lost nearly 40% of its",
                     "funding in FY1996, dropping to **$110 million**."
                    )),
               fill = "white",
               orientation = "upright",
               hjust = 0,
               vjust = 1,
               width = unit(.3, "npc"),
               color = "black") +
   geom_textbox(aes(x = 2005, y = 120,
                   label = paste(
                     "NEH received **$207 million** in FY2024,",
                     "which represented the highest nominal",
                     "appropriation amount in the agency's history."
                   )),
               fill = "white",
               orientation = "upright",
               hjust = 0,
               vjust = 1,
               width = unit(.3, "npc"),
               color = "black") +
  scale_x_continuous(breaks = c(1966, seq(1970, 2020, 5), 2024),
                     minor_breaks = seq(1966, 2024, 1)) +
  scale_y_continuous(breaks = seq(0, 700, 50),
                     minor_breaks = seq(0, 700, 10),
                     labels = scales::label_dollar()) +
  scale_linetype_manual(values = c("solid")) +
  scale_color_manual(values = c("#F8766D")) +
  labs(x = x_text,
       y = y_text,
       title = title_text,
       caption = c(expression(
         italic(paste("Data sources: ",
                      "https://www.neh.gov/neh-appropriations-history"))),
         "https://github.com/btskinner/nehapprop"
         )) +
  theme_bw(base_family = "Avenir", base_size = 18) +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = c(1,0), size = 9))

## save
ggsave(filename = file.path(fig_dir, "neh_appropriations_nom.png"),
       plot = g,
       width = 16,
       height = 9,
       units = "in",
       dpi = "retina",
       bg = "white")

## -------------------------------------
## nominal plus real (2024)
## -------------------------------------

## plot
g <- df |>
  filter(type %in% c("ap", "ap_adj")) |>
  mutate(type = ifelse(type == "ap",
                       "Nominal dollars",
                       "Inflation-adjusted dollars (2024)")) |>
  ggplot(aes(x = fy, y = value / 1000000, color = type, linetype = type)) +
  geom_line() +
  geom_textbox(aes(x = 1983, y = 620,
                   label = paste(
                     "Adjusted for inflation, annual appropriations to",
                     "the National Endowment for the Humanities were highest",
                     "in the late 1970s, reaching over **$600 million in 2024",
                     "dollars** in 1979."
                   )),
               fill = "white",
               orientation = "upright",
               hjust = 0,
               vjust = 1,
               width = unit(.295, "npc"),
               color = "black") +
  geom_textbox(aes(x = 2004, y = 310,
                   label = paste(
                     "In real terms, NEH appropriations have been",
                     "relatively flat for the past three decades."
                   )),
               fill = "white",
               orientation = "upright",
               hjust = 0,
               vjust = 1,
               width = unit(.22, "npc"),
               color = "black") +
  scale_x_continuous(breaks = c(1966, seq(1970, 2020, 5), 2024),
                     minor_breaks = seq(1966, 2024, 1)) +
  scale_y_continuous(breaks = seq(0, 700, 100),
                     minor_breaks = seq(0, 700, 10),
                     labels = scales::label_dollar()) +
  scale_linetype_manual(values = c("longdash", "solid")) +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) +
  labs(x = x_text,
       y = y_text,
       title = title_text,
       caption = c(expression(
         italic(paste("Data sources: ",
                      "https://www.neh.gov/neh-appropriations-history; ",
                      "FRED"))),
         "https://github.com/btskinner/nehapprop"
         )) +
  guides(color = guide_legend(position = "inside"),
         linetype = guide_legend(position = "inside")) +
  theme_bw(base_family = "Avenir", base_size = 18) +
  theme(legend.margin = margin(0,0,0,0),
        legend.justification.inside = c(1,0),
        legend.position.inside = c(0.99,0.01),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = c(1,0), size = 9))

## save
ggsave(filename = file.path(fig_dir, "neh_appropriations_real.png"),
       plot = g,
       width = 16,
       height = 9,
       units = "in",
       dpi = "retina",
       bg = "white")

## -----------------------------------------------------------------------------
## end script
################################################################################
