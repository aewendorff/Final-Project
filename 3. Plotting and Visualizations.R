#Load Packages
library(ggplot2)
library(sf)
library(tidyverse) #for filtering
library(ggspatial) #for background map



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



# Interaction with emmeans Plot --------------------------------------------------------
ggplot(emm_df, aes(x = site_type, y = emmean, color = ice_cover, group = ice_cover)) +
  geom_point(size = 4) +
  geom_line(linewidth = 1.5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1, linewidth = 1) +
  scale_color_manual(values = c("yes" = "springgreen3", "no" = "skyblue2"),
                     labels = c("Ice-on", "Ice-off"),
                     name = "Ice Cover") +
  labs(x = "Site Type", y = "Predicted ΔSPC (µS/cm)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14, margin = margin(t = 15)),
        axis.title.y = element_text(size = 14, margin = margin(r = 15)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = c(0.75, 0.85)) +
  scale_x_discrete(expand = expansion(add = 0.2)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.07)))

ggsave("outputs/interaction_emmeans.png", width = 5, height = 5)



# Stratification Plots (Did not use) ----------------------------------------------------
ggplot(me_mo_spc_daily_by_depth, 
       aes(x = date, y = depths, fill = spc_daily)) +
  geom_tile() +
  scale_y_reverse() +   # deep water at bottom
  scale_fill_viridis_c(option = "plasma", name = "SPC (µS/cm)") +
  facet_wrap(~ site, ncol = 1, scales = "free_x") +
  labs(x = "Date", y = "Depth (m)") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.key.height = unit(1.2, "cm"))



# Buoy Location Map -------------------------------------------------------
#Read Wisconsin open water shapefile *NEEDS TO BE DOWNLOADED SEPARATELY*
path <- "data/shapefile/24k_Hydro_Waterbodies_(Open_Water).shp"
madison_lakes <- st_read(path)
    #st_read() reads a shapefile

#Read CSV file
buoy_cords <- read.csv("data/Buoy_Coordinate_Data.csv")

#View attributes (column names)
names(madison_lakes)

#Filter for only the lakes within my sampling frame
my_lakes <- madison_lakes %>%
  filter(WATERBODY_ %in% c("Lake Mendota", "Lake Monona", "Lake Wingra"))
      #using filter this way will ONLY match with text in the waterbody column that is exactly the same

#Check the geographic coordinate system of the shapefile and buoy coordinates
st_crs(my_lakes)
st_crs(buoy_cords)

#Convert the buoy data to an sf
buoy_sf <- st_as_sf(buoy_cords, 
                    coords = c("Y.coordinate", "X.coordinate"), 
                    crs = 4326)
      #st_as_sf() converts dataframe into a sf
      #coords = c("insert x cord column", "insert y cord column")
      #crs = 4326, refers to WGS84 geographic coordinates, which is what the cord's are in


#Transform to match the CRS of the lake shapefile
  buoy_transform <- st_transform(buoy_sf, 
                                 st_crs(madison_lakes))
          #re-projects the buoy coords to the same crs as madison_lakes

#Check the coordinates of the boudning boxes to very transformation
st_bbox(my_lakes)
st_bbox(buoy_transform)


#Plot a map with the shapefile, coordinate points, and background map
ggplot() +
  #Add background map
  annotation_map_tile(type = "osm",  #background map (OpenStreetMap)
                      zoomin = 0, 
                      alpha = 1) + 
  #Add lake shapefiles to the map
  geom_sf(data = my_lakes,
          fill = NA,
          color = "black", 
          linewidth = 0.6,
          aes(fill = WATERBODY_)) +
  #Add buoy coordinates to the map
  geom_sf(data = buoy_transform, 
          aes(fill = Buoy), #unique fill color for each buoy
          shape = 21, #point with outline
          color = "black", #color outline black
          stroke = 1, #width of outline
          size = 3) +
  #Add map scale
  annotation_scale(location = "bl", #bottom left placement
                   width_hint = 0.5) + #scale bar takes up 50% of the map width
  #Add north arrow
  annotation_north_arrow(location = "tr", #top right placement
                         which_north = "true") +  #points to geographic north pole
  #Add Axes Labels
  labs(x = "Longitude", y = "Latitude") +
  #Add bw theme
  theme_bw() +
  #Change text and legend aesthetics 
  theme(axis.title.x = element_text(size = 13, 
                                    margin = margin(t = 10), 
                                    face = "bold"),
        axis.title.y = element_text(size = 13, 
                                    margin = margin(r = 10),
                                    face = "bold"),
        plot.title = element_text(size = 15, 
                                  margin = margin(b = 10), 
                                  face = "bold",
                                  hjust = 0.5), #centers title
        legend.title = element_text(face = "bold",
                                    hjust = 0.5),
        legend.box.background = element_rect(color = "black", #makes box around legend 
                                             linewidth = 0.5))

#Save the Map
ggsave(filename = "outputs/Overwinter_Buoy_Locations.pdf", 
       width = 6, height = 6, units = "in")






