library(tidyverse)
library(tidylog)
library(rvest)
library(gt)

setwd("~/")

botw_armor <- read.csv('BOTW Armor Sets.csv',
                       fileEncoding="UTF-8-BOM")

base_select <- botw_armor %>%
  select(DLC,
         Amiibo,
         Armor,
         Gear,
         Set,
         Base, 
         Full.Set)

head_armor <- dplyr::filter(botw_armor, Gear == 'Head')
body_armor <- dplyr::filter(botw_armor, Gear == 'Torso')
leg_armor <- dplyr::filter(botw_armor, Gear == 'Leg')

armor_combinations <- expand.grid(head_armor$Armor, 
                                  body_armor$Armor,
                                  leg_armor$Armor)

total_combos <- armor_combinations %>%
  select(Head_Armor = Var1,
         Body_Armor = Var2,
         Leg_Armor = Var3) %>%
  left_join(base_select,
            by = c('Head_Armor' = 'Armor')) %>%
  select(Head_Armor,
         Head_Armor_Base = Base,
         Head_Set = Set,
         Head_DLC = DLC,
         Head_Amiibo = Amiibo,
         Head_Full_Set = Full.Set,
         Body_Armor,
         Leg_Armor) %>%
  left_join(base_select,
            by = c('Body_Armor' = 'Armor')) %>%
  select(Head_Armor,
         Head_Armor_Base,
         Head_Set,
         Head_DLC,
         Head_Amiibo,
         Head_Full_Set,
         Body_Armor,
         Body_Armor_Base = Base,
         Body_Set = Set,
         Leg_Armor) %>%
  left_join(base_select,
            by = c('Leg_Armor' = 'Armor')) %>%
  select(Head_Armor,
         Head_Armor_Base,
         Head_Set,
         Head_DLC,
         Head_Amiibo,
         Head_Full_Set,
         Body_Armor,
         Body_Armor_Base,
         Body_Set,
         Leg_Armor,
         Leg_Armor_Base = Base,
         Leg_Set = Set) %>%
  mutate(Total_Defense = Head_Armor_Base + Body_Armor_Base + Leg_Armor_Base,
         Avg_Defense = mean(Total_Defense),
         Median_Defense = median(Total_Defense),
         Effective = Total_Defense/Avg_Defense,
         #Head_Set = ifelse(Head_DLC == TRUE | Head_Amiibo == TRUE, paste(Head_Set, '*', sep = ''), Head_Set),
         Set = case_when(Head_Set == Body_Set & Body_Set == Leg_Set ~ Head_Set,
                         Head_Set != Body_Set | Body_Set != Leg_Set ~ ''),
         Set = ifelse(Head_Armor == 'No Head Armor' & Body_Armor == 'Old Shirt' & Leg_Armor == 'Well-Worn Trousers', 'Worn', Set),
         Head_Armor = gsub('No Head Armor', '', Head_Armor),
         Set = ifelse(Set == 'Unique', '', Set)) %>%
  arrange(desc(Total_Defense)) %>%
  filter(Set != '') %>%
  select(Set,
         Head_Armor,
         Body_Armor,
         Leg_Armor,
         Set_Adv = Head_Full_Set,
         Total_Defense)

gt(total_combos) %>%
  cols_label(
    Set = 'Set',
    Head_Armor = 'Head Armor',
    Body_Armor = 'Body Armor',
    Leg_Armor = 'Leg Armor',
    Set_Adv = 'Set Advantage',
    Total_Defense = 'Total Defense'
  ) %>%
  data_color(
    columns = 6,
    colors = scales::col_numeric(
      palette = c("white", "#30a2da"),
      domain = c(0, 24)
    )
  ) %>% 
  espnscrapeR::gt_theme_538() %>% 
  tab_source_note(
    source_note = md("\n**Table**: Alex Elfering | **Data**: Nintendo, IGN, Nintendo Life")
  ) %>% 
  tab_header(
    title = "Wearing Phantom Armor Provides the Most Defense in The Legend of Zelda: Breath of the Wild",
    subtitle = "Adding together the head, body, and leg base stats, the Phantom Set provides the strongest defense against enemy attacks. The Ancient Set, however, provides extra protection against Ancient attacks and additional advantages depending on the head gear worn. This table assumes that no upgrades have been made."
  )




