### ----------------------------------------------------------------------------
### ----------------------------------------------------------------------------
### Temporal evolution
### ----------------------------------------------------------------------------
### ----------------------------------------------------------------------------

incidence <- cohort[,
  .(tests = .N),
  by = .(date, covid)][order(da te)]

incidence <- incidence[,
  .(date,
    tests,
    avg7 = frollmean(tests, 7, fill = NA, align = "center")),
  by = covid]

### ----------------------------------------------------------------------------
### Exposed vs. Positive
### ----------------------------------------------------------------------------

exposed_vs_positive <- ggplot(
  cohort,
  aes(
    x = date,
    fill = covid
  )
) +
  geom_histogram(
    color = "black",
    position = "dodge",
    binwidth = 7
  ) +
  scale_x_date(
    breaks = scales::breaks_width("1 months"),
    labels = scales::label_date("%b '%y")
  ) +
  theme_hc(base_size = 12, base_family = 'arial') +
  xlab("") +
  ylab("Subjects per week") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)
  ) +
  scale_fill_lancet(name = "")

### ----------------------------------------------------------------------------
### Positive (obese vs non-obese)
### ----------------------------------------------------------------------------

obesity_plot <- ggplot(
  cohort,
  aes(
    x = date,
    fill = obesity
  )
) +
  geom_histogram(
    color = "black",
    position = "dodge",
    binwidth = 7
  ) +
  scale_x_date(
    breaks = scales::breaks_width("1 months"),
    labels = scales::label_date("%b '%y")
  ) +
  xlab("") +
  ylab("Subjects per week") +
  theme_hc(base_size = 12, base_family = 'arial') +
  theme(
    axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)
  ) +
  scale_fill_manual(values = pal_lancet()(9)[8:7], name = "", labels = c("non-obese", "obese"))

obesity_plot + facet_grid(vars(covid), scales = "free_y")

### ----------------------------------------------------------------------------
### Odds per plot
### ----------------------------------------------------------------------------
odds_week_plot <- ggplot(
  weekly_odds,
  aes(
    x = date,
    y = odds,
    color = p_value < 0.05)
) +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  xlab("") +
  scale_x_date(
    breaks = scales::breaks_width("1 months"),
    labels = scales::label_date("%b '%y")
  ) +
  ylab("OR per week") +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  theme_hc(base_size = 12, base_family = "arial") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)
  ) +
  scale_color_manual(
    values = c("gray70", "black"),
    name = "",
    labels = c("non-significant", "significant")
  )


## Final evolution plot

temporal_evo <- exposed_vs_positive /
  (obesity_plot + facet_grid(vars(covid), scales = "free_y")) /
  odds_week_plot +
  plot_annotation(tag_levels = 'A') +
  plot_layout(heights = 1, widths = 1)

ggsave(
  "temporal_evo.png",
  temporal_evo,
  width = 7.5,
  height = 8.5,
  units = "in",
  dpi = 300,
  scale = 1.5
)


## MASS vs MGB plot

incidence_unfold[source == "MASS", source := "MA"]

mass_mgb_vs_plot <- ggplot(
  incidence_unfold,
  aes(
    x = date,
    fill = source
  )
) +
  geom_histogram(
    color = "black",
    position = "dodge",
    binwidth = 7
  ) +
  scale_x_date() +
  xlab("") +
  scale_x_date(
    breaks = scales::breaks_width("1 months"),
    labels = scales::label_date("%b '%y")
  ) +
  ylab("Positive per week") +
  theme_hc(base_size = 12, base_family = "arial") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
    legend.position
  ) +
  scale_fill_manual(
    values = pal_lancet()(4)[1:2],
    name = "",
    labels = c("MA", "MGB")) +
  facet_grid(vars(source), scales = "free_y")

mass_mgb_vs_plot

pal <- wes_palette("Zissou1", 100, type = "continuous")

## Towns plot
mass_mgb_vs_town_plot <- ggplot(
  towns_df %>% filter(positive > 0),
  aes(
    x = positive_mass,
    y = positive,
    size = POP2010,
    color = ratio
  )
) +
  geom_smooth(
    method = "lm",
    se = F,
    color = "black",
    linetype = "dashed",
    show.legend = FALSE
    ) +
  geom_label_repel(aes(label = TOWN), size = 2) +
  xlab("Positive MA") +
  ylab("Positive MGB") +
  scale_size_continuous(
    labels = scales::label_number_si(big.mark = ",", accuracy = 1),
    name = "Population"
  ) +
  scale_color_gradientn(
    ## colors = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 100),
    colors = rev(nord("victory_bonds", 100)),
    labels = scales::percent_format(accuracy = 1),
    trans = "log10",
    name = "Ratio"
  ) +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_point() +
  theme_minimal(base_size = 12, base_family = "arial") +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(0.3, "cm")
  ) +
  guides(
    color = guide_colorbar(title.vjust = 1)
  )

mass_mgb_vs_town_plot

## Correlation plot
data_cor_town <- as.data.table(towns_df)[,
  .(
    mgb = log10(positive),
    ma = log10(positive_mass))
  ][
    ma >= 0 &
    !is.na(ma) &
    mgb >= 0 &
    !is.na(mgb)
  ]

summary(glm(ma ~ mgb, data = data_cor_town))

cor.test(data_cor_town$ma, data_cor_town$mgb)

towns_plot <- ggplot(towns_df) +
  theme_map(base_size = 12, base_family = "arial") +
  theme(
    legend.position = c(-0.1, 0.2),
    legend.direction = "horizontal",
    legend.title = element_text(
      vjust = 1.0
    ),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm")
  ) +
  coord_fixed()


map_inf <- towns_plot +
  geom_sf(
    aes(
      fill = positive
    ),
    color = "black",
    size = 0.2
  ) +
  scale_fill_gradientn(
    na.value = rev(nord("victory_bonds", 100))[1],
    colors = rev(nord("victory_bonds", 100)),
    name = "MGB Positives",
    labels = scales::label_number_si(big.mark = ",", accuracy = 1),
    trans = "log10",
    lim = c(1,10000),
    breaks = c(1, 10, 100, 1000, 10000)
  ) +
  guides(
    fill = guide_colorbar(title.vjust = 1, draw.ulim = T)
  )

positive_mass_towns_plot <- towns_plot +
  geom_sf(
    aes(
      fill = positive_mass
    ),
    color = "black",
    size = 0.2
  ) +
  scale_fill_gradientn(
    na.value = rev(nord("victory_bonds", 100))[1],
    colors = rev(nord("victory_bonds", 100)),
    name = "MA Positives",
    labels = scales::label_number_si(big.mark = ",", accuracy = 1),
    trans = "log10",
    lim = c(1, 100000),
    breaks = c(1, 10, 100, 1000, 10000, 100000)
    ) +
  guides(
    fill = guide_colorbar(
      title.vjust = 1
    )
  )

figure2 <- positive_mass_towns_plot +
  map_inf +
  mass_mgb_vs_town_plot +
  mass_mgb_vs_plot +
  plot_annotation(tag_levels = "A")

ggsave("mass_mgb.png", figure2, width = 7.5, height = 6.5, scale = 1.5, dpi = 400)

## Odds plot

odds_df[
  population == "total",
  population := "Total"
]

odds_df[
  population == "sex",
  population := "Sex"
]

odds_df[
  population == "hypertension",
  population := "Hypertension"
]

odds_df[
  population == "age",
  population := "Age"
]

odds_df[
  population == "dm2",
  population := "T2DM"
]

odds_df$population <- ordered(odds_df$population, c("Total", "Age", "Sex", "T2DM", "Hypertension"))

odds_df$group2 <- ordered(odds_df$group,
                          c(
                            "total",
                            "Male",
                            "Female",
                            "Y",
                            "N",
                            ">64",
                            "40-64",
                            "20-39",
                            "13-19",
                            "3-12"
                          )
                          )

odds_plot <- ggplot(
  odds_df,
  aes(
    y = group2,
    x = odds,
    color = p_value < 0.05
  )
) +
  geom_point(size = 4) +
  geom_label(
    aes(
      label = sprintf(
        "OR = %.2f (%.2f - %.2f) p-value = %.2e",
        odds,
        lower,
        upper,
        p_value
      ),
      x = odds,
      y = group2
    ),
    vjust = -1.,
    hjust = 0.1,
    show.legend = F
  ) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbar(aes(xmin = lower, xmax = upper), size = 1.5, width = 0.1) +
  scale_x_continuous(breaks = scales::breaks_width(0.1)) +
  theme_calc(base_size = 15, base_family = "arial") +
  xlab("Odds Ratio") +
  ylab("") +
  facet_grid(
    vars(population),
    scale = "free_y",
    space = "free_y"
  ) +
  scale_color_manual(values = c("gray70", "black"), name = "", labels = c("non-significant", "significant")) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.background = element_blank(),
  )

ggsave("odds_plot.png", odds_plot, width = 7.5, height = 8.5, dpi = 300, scale = 1.5)
