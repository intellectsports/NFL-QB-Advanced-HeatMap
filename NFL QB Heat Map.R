library(tidyverse)
library(nflfastR)
library(ggtext)


pbp <- load_pbp(2025)

# Enter QB's Heat Map to view
qb_name <- "S.Darnold" 

qb_pass <- pbp %>%
  filter(
    passer == qb_name,
    pass == 1,
    !is.na(air_yards),
    !is.na(pass_location)
  )


# Depth categories
qb_pass <- qb_pass %>%
  mutate(
    depth = case_when(
      air_yards <= 10 ~ "Short",
      air_yards <= 20 ~ "Medium",
      air_yards > 20  ~ "Deep"
    ),
    depth = factor(depth, c("Short","Medium","Deep")),
    pass_location = factor(pass_location,
                           c("left","middle","right"),
                           c("Left","Middle","Right"))
  )


# Summary: CPOE, EPA, Attempts
heat_df <- qb_pass %>%
  group_by(depth, pass_location) %>%
  summarise(
    mean_cpoe = mean(cpoe, na.rm = TRUE),
    mean_epa  = mean(qb_epa,  na.rm = TRUE),
    attempts  = n(),
    .groups = "drop"
  ) %>%
  mutate(
    label_big   = sprintf("%.1f", mean_cpoe),
    label_small = sprintf("EPA: %.2f", mean_epa),
    label_n     = sprintf("n = %d", attempts)
  )

# ---------------------------
# Plot
# ---------------------------
ggplot(heat_df, aes(pass_location, depth)) +
  geom_tile(aes(fill = mean_cpoe), color = "white", size = 1.2) +
  
  # Big CPOE text (top-center)
  geom_text(
    aes(label = label_big),
    size = 10,
    fontface = "bold",
    vjust = -0.5
  ) +
  
  # EPA text (middle)
  geom_text(
    aes(label = label_small),
    size = 4,
    vjust = 0.8
  ) +
  
  # Attempts (n)
  geom_text(
    aes(label = label_n),
    size = 4,
    vjust = 2.8,
    color = "gray20"
  ) +
  
  scale_fill_gradient2(
    low = "#b2182b",
    mid = "white",
    high = "#2166ac",
    midpoint = 0,
    name = "CPOE"
  ) +
  
  labs(
    title = paste("QB Throw Heatmap –", qb_name),
    subtitle = "Comp pct over exp (big) | EPA/play | Attempts (n)",
    x = "Location",
    y = "Depth",
    caption = "Author: Kabir Devgun| Data: nflfastR"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face="bold", size=20),
    axis.title = element_text(face="bold"),
    plot.caption = element_text(size=10, color="gray30", hjust=0.5, margin=margin(t=10))
  )



