

# Boxplot -----------------------------------------------------------------
ice_labels <- c(yes = "Ice-on", no = "Ice-off")

ggplot(final_no_middle, aes(x = site_type, y = delta_spc_daily, fill = site_type)) +
  geom_boxplot() +
  facet_wrap(~ ice_cover, labeller = labeller(ice_cover = ice_labels)) +
  labs(x = "Site Type",
       y = "Delta Specific Conductance (µS/cm)",
       fill = "Site Type") + #legend label
  scale_fill_manual(values = c("deephole" = "springgreen3", "inflow" = "skyblue2")) + #change fill colors
  theme_bw() +
  theme(axis.title.x = element_text(size = 14, margin = margin(t = 15)), #larger, bold axis titles w/space bw margins
        axis.title.y = element_text(size = 14, margin = margin(r = 15)),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1), #rotate x-axis labels
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 13, face = "bold"), #legend title
        legend.text = element_text(size = 12), #legend text
        strip.text = element_text(size = 13, face = "bold")) #facet labels

ggsave("outputs/boxplot.png", width = 5, height = 5)



# Interaction Plot --------------------------------------------------------
ggplot(final_all, aes(x = site_type, y = delta_spc_daily, color = ice_cover, group = ice_cover)) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun = mean, geom = "line", linewidth = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, linewidth = 1) +
  scale_color_manual(values = c("yes" = "springgreen3", "no" = "skyblue2")) +  #custom colors
  labs(x = "Site Type",
       y = "Mean Delta Specific Conductance (µS/cm)",
       color = "Ice Cover") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14, margin = margin(t = 15)), #larger, bold axis titles w/space bw margins
        axis.title.y = element_text(size = 14, margin = margin(r = 15)),
        axis.text.x = element_text(size = 10), #rotate x-axis labels
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 13, face = "bold"), #legend title
        legend.text = element_text(size = 12), 
        legend.position=c(0.75,0.85)) +
  scale_x_discrete(expand = expansion(add = 0.2)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.07)))

ggsave("outputs/interactionplot.png", width = 5, height = 5)




# Stratification Plots ----------------------------------------------------

ggplot(me_mo_spc_daily_by_depth, 
       aes(x = date, y = depths, fill = spc_daily)) +
  geom_tile() +
  scale_y_reverse() +   # deep water at bottom
  scale_fill_viridis_c(option = "plasma", name = "SPC (µS/cm)") +
  facet_wrap(~ site, ncol = 1, scales = "free_x") +
  labs(
    title = "SPC Stratification Over Time",
    x = "Date",
    y = "Depth (m)"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.key.height = unit(1.2, "cm")
  )
