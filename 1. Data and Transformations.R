# Load Packages -----------------------------------------------------------
library(tidyverse)



# Clean and Transform Data ------------------------------------------------
#Load Dataset and Fix 2025 Data
master <- read_csv("data/Yahara_Buoys_master_20250527.csv")



# Initial Filtering ---------------------------------------------------------------
#Filter just Mendota and Monona Sites
  me_mo <- master %>% 
  filter(site %in% c("Mendota", "Monona", "Starkweather", "Pheasant", "Yahara")) %>% 
#Filter just Oct through May (Timeframe of buoy deplpyment except in 2022-2023)
  filter(month(date_time) %in% c(10, 11, 12, 1, 2, 3, 4, 5)) %>% 

    

# Create Deephole and Wateryear Variables ----------------------------------------------------
#Create a new category: Deephole or Inflow
  mutate(site_type = case_when(site %in% c("Mendota", "Monona") ~ "deephole",
                                         site %in% c("Starkweather", "Pheasant", "Yahara") ~ "inflow")) %>% 
#Create a water year variable (2022-2023, 2023-2024, 2024-2025) from date_time
  mutate(water_year = if_else(month(date_time) >= 10, #extract month, if >= 10 then paste0
                              paste0(year(date_time), "-", year(date_time) + 1), #extract first year then year + 1, format is year-year+1
                              paste0(year(date_time) - 1, "-", year(date_time))), #if < 10 then subtract 1 from year, year-1-year
                              year = year(date_time)) #extracts just year from date_time
#Make year a factor
  me_mo$year <- as.factor(me_mo$year)
  

# Calculate Delta SPC -----------------------------------------------------
  me_mo_spc_all <- me_mo %>% 
  #Create new depth label
    group_by(site, water_year, date_time) %>% #treats each site AND year independently
    mutate(depth_label = case_when(depths == min(depths) ~ "surface", #Min depth labelled surface
                                 depths == max(depths) ~ "deep", #Max depth labelled deep
                                 TRUE ~ "middle")) %>% #Everything else  labelled middle
  #Filter only deep/surface AND filter out any days that don't have both deep and surface
    filter(any(depth_label == "surface") & any(depth_label == "deep")) %>%
  #Calculate Delta SPC (deep spc - surface spc)
    mutate(delta_spc = spc[depth_label == "deep"] - spc[depth_label == "surface"]) %>%  #create delta_spc column
    ungroup()


# Calculate Daily Delta SPC -----------------------------------------------------
#Daily Delta SPC by depth_label (Surface, Middle, Deep)
  me_mo_spc_daily_all <- me_mo_spc_all %>%
  mutate(date = as_date(date_time)) %>% #create daily date
    group_by(site, site_type, water_year, date, depth_label) %>%
    summarise(spc_daily = mean(spc, na.rm = TRUE),
              delta_spc_daily = mean(delta_spc, na.rm = TRUE)) %>%
  ungroup()

#Daily Delta SPC by each depth 
me_mo_spc_daily_by_depth <- me_mo_spc_all %>%
    mutate(date = as_date(date_time)) %>%   #create daily date
    group_by(site, site_type, water_year, date, depths) %>%
    summarise(spc_daily = mean(spc, na.rm = TRUE)) %>%
    ungroup()

#Make depth numeric rather than a factor
me_mo_spc_daily_by_depth <- me_mo_spc_daily_by_depth %>%
  mutate(depths = as.numeric(depths))



# Create Ice Cover variable -----------------------------------------------
#Create ice-cover column using daily SPC averages by surface and deep ONLY
  final_all <- me_mo_spc_daily_all %>%
    mutate(ice_cover = case_when(
      #2022-2023
      site %in% c("Mendota", "Pheasant", "Yahara") &
        water_year == "2022-2023" &
        date >= as_date("2022-12-25") &
        date <= as_date("2023-04-02") ~ "yes",
      site %in% c("Monona", "Starkweather") &
        water_year == "2022-2023" &
        date >= as_date("2022-12-19") &
        date <= as_date("2023-03-20") ~ "yes",
      
      #2023-2024
      site %in% c("Mendota", "Pheasant", "Yahara") &
        water_year == "2023-2024" &
        date >= as_date("2024-01-15") &
        date <= as_date("2024-02-28") ~ "yes",
      site %in% c("Monona", "Starkweather") &
        water_year == "2023-2024" &
        date >= as_date("2024-01-15") &
        date <= as_date("2024-02-28") ~ "yes",
      
      #2024-2025
      site %in% c("Mendota", "Pheasant", "Yahara") &
        water_year == "2024-2025" &
        ((date >= as_date("2024-12-25") & date <= as_date("2024-12-27")) |
           (date >= as_date("2025-01-07") & date <= as_date("2025-03-15"))) ~ "yes",
      site %in% c("Monona", "Starkweather") &
        water_year == "2024-2025" &
        ((date >= as_date("2024-12-13") & date <= as_date("2024-12-17")) |
           (date >= as_date("2024-12-22") & date <= as_date("2024-12-28")) |
           (date >= as_date("2025-01-05") & date <= as_date("2025-03-15"))) ~ "yes",
      TRUE ~ "no")) #anything else will be ice-off
  

# Filter out just middle values -------------------------------------------
final_no_middle <- final_all %>%
    filter(depth_label %in% c("surface", "deep"))  


  