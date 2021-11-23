library(shiny)
library(tidyverse)
library(rtrim)
library(shinythemes)
#install.packages("ps")

# Datasets ----------------------------------------------------------------

#dataset for overall results
dataset <- read.csv("indices_final.csv")
dataset <- dataset %>% mutate(spp = case_when(
  spp == "ASW" ~ "Atherton Scrubwren",
  spp == "BFMON" ~ "Black-faced Monarch",
  spp == "BGER" ~ "Brown Gerygone",
  spp == "BHE" ~ "Bridled Honeyeater",
  spp == "BPIG" ~ "Brown Cuckoo-Dove",
  spp == "BST" ~ "Bower's Shrike-Thrush",
  spp == "CAT" ~ "Spotted Catbird",
  spp == "CC" ~ "Chowchilla",
  spp == "CURR" ~ "Pied Currawong",
  spp == "EWB" ~ "Eastern Whipbird",
  spp == "FTCUC" ~ "Fan-tailed Cuckoo",
  spp == "FW" ~ "Fernwren",
  spp == "GFAN" ~ "Grey Fantail",
  spp == "GHE" ~ "Graceful Honeyeater",
  spp == "GHR" ~ "Grey-headed Robin",
  spp == "GOLDBB" ~ "Golden Bowerbird",
  spp == "GOLDW" ~ "Golden Whistler",
  spp == "LBSW" ~ "Large-billed Scrubwren",
  spp == "LEWHE" ~ "Lewin's Honeyeater",
  spp == "LST" ~ "Little Shrike-Thrush",
  spp == "MACHE" ~ "Macleay's Honeyeater",
  spp == "MTB" ~ "Mistletoebird",
  spp == "MTHORN" ~ "Mountain Thornbill",
  spp == "OFSF" ~ "Orange-footed Scrubfowl",
  spp == "PYR" ~ "Pale-yellow Robin",
  spp == "RFAN" ~ "Rufous Fantail",
  spp == "RL" ~ "Rainbow Lorikeet",
  spp == "SATBB" ~ "Satin Bowerbird",
  spp == "SBCUC" ~ "Shining Bronze-Cuckoo",
  spp == "SBL" ~ "Scaly-breasted Lorikeet",
  spp == "SCC" ~ "Sulphur-crested Cockatoo",
  spp == "SD" ~ "Spangled Drongo",
  spp == "SFD" ~ "Superb Fruit-Dove",
  spp == "SMON" ~ "Spectacled Monarch",
  spp == "TBBB" ~ "Tooth-billed Bowerbird",
  spp == "VRIF" ~ "Victoria's Riflebird",
  spp == "VT" ~ "Varied Triller",
  spp == "WOMP" ~ "Wompoo Fruit-Dove",
  spp == "WTTC" ~ "White-throated Treecreeper",
  spp == "YBBB" ~ "Yellow-breasted Boatbill",
  spp == "YSHE" ~ "Yellow-spotted Honeyeater",
  spp == "YTSW" ~ "Yellow-throated Scrubwren"
  
))

#dataset for statistics
bird <- read.csv("birds_awt_abnormalities.csv") 
bird$plot_name <- factor(bird$plot_name, levels = c("WT200", "WT400", "WT600", "WT800", "WT1000", "WT1200"))
bird <- bird %>% select(-site) %>% filter(subregion_id != "KU")
bird <- bird %>% filter(subregion_id %in% c("AU", "CU", "WU", "SU"))
birds_wt <- bird %>% rename(site = plot_name) %>% filter(!year %in% c(1996, 1997, 1998, 1999))
birds_wt <- birds_wt %>% select(year, site, ASW:YTSW)
birds_wt_gather <- birds_wt %>% gather(spp, count, ASW:YTSW)
birds_wt_gather <- birds_wt_gather %>% arrange(year) %>% mutate(spp = case_when(
  spp == "ASW" ~ "Atherton Scrubwren",
  spp == "BFMON" ~ "Black-faced Monarch",
  spp == "BGER" ~ "Brown Gerygone",
  spp == "BHE" ~ "Bridled Honeyeater",
  spp == "BPIG" ~ "Brown Cuckoo-Dove",
  spp == "BST" ~ "Bower's Shrike-Thrush",
  spp == "CAT" ~ "Spotted Catbird",
  spp == "CC" ~ "Chowchilla",
  spp == "CURR" ~ "Pied Currawong",
  spp == "EWB" ~ "Eastern Whipbird",
  spp == "FTCUC" ~ "Fan-tailed Cuckoo",
  spp == "FW" ~ "Fernwren",
  spp == "GFAN" ~ "Grey Fantail",
  spp == "GHE" ~ "Graceful Honeyeater",
  spp == "GHR" ~ "Grey-headed Robin",
  spp == "GOLDBB" ~ "Golden Bowerbird",
  spp == "GOLDW" ~ "Golden Whistler",
  spp == "LBSW" ~ "Large-billed Scrubwren",
  spp == "LEWHE" ~ "Lewin's Honeyeater",
  spp == "LST" ~ "Little Shrike-Thrush",
  spp == "MACHE" ~ "Macleay's Honeyeater",
  spp == "MTB" ~ "Mistletoebird",
  spp == "MTHORN" ~ "Mountain Thornbill",
  spp == "OFSF" ~ "Orange-footed Scrubfowl",
  spp == "PYR" ~ "Pale-yellow Robin",
  spp == "RFAN" ~ "Rufous Fantail",
  spp == "RL" ~ "Rainbow Lorikeet",
  spp == "SATBB" ~ "Satin Bowerbird",
  spp == "SBCUC" ~ "Shining Bronze-Cuckoo",
  spp == "SBL" ~ "Scaly-breasted Lorikeet",
  spp == "SCC" ~ "Sulphur-crested Cockatoo",
  spp == "SD" ~ "Spangled Drongo",
  spp == "SFD" ~ "Superb Fruit-Dove",
  spp == "SMON" ~ "Spectacled Monarch",
  spp == "TBBB" ~ "Tooth-billed Bowerbird",
  spp == "VRIF" ~ "Victoria's Riflebird",
  spp == "VT" ~ "Varied Triller",
  spp == "WOMP" ~ "Wompoo Fruit-Dove",
  spp == "WTTC" ~ "White-throated Treecreeper",
  spp == "YBBB" ~ "Yellow-breasted Boatbill",
  spp == "YSHE" ~ "Yellow-spotted Honeyeater",
  spp == "YTSW" ~ "Yellow-throated Scrubwren"
  
))
list_df <- split(birds_wt_gather, birds_wt_gather$spp)
mean_counting <- map(list_df, ~ .x %>% group_by(year, site) %>% summarise(count = mean(count, na.rm = T)) %>% ungroup() %>% mutate(count = count*100))
models <- map(mean_counting, ~ trim(count ~ site + year, data = .x, model = 2, overdisp = F, serialcor = F, autodelete=TRUE, changepoints = "all"))

#dataset for indicators

MSI_results <- read.csv("MSI_results.csv")


#dataset for elevation range

elevation <- read.csv("abundance_trends.csv")
elevation <- elevation %>% mutate(spp = case_when(
  spp == "ASW" ~ "Atherton Scrubwren",
  spp == "BFMON" ~ "Black-faced Monarch",
  spp == "BGER" ~ "Brown Gerygone",
  spp == "BHE" ~ "Bridled Honeyeater",
  spp == "BPIG" ~ "Brown Cuckoo-Dove",
  spp == "BST" ~ "Bower's Shrike-Thrush",
  spp == "CAT" ~ "Spotted Catbird",
  spp == "CC" ~ "Chowchilla",
  spp == "CURR" ~ "Pied Currawong",
  spp == "EWB" ~ "Eastern Whipbird",
  spp == "FTCUC" ~ "Fan-tailed Cuckoo",
  spp == "FW" ~ "Fernwren",
  spp == "GFAN" ~ "Grey Fantail",
  spp == "GHE" ~ "Graceful Honeyeater",
  spp == "GHR" ~ "Grey-headed Robin",
  spp == "GOLDBB" ~ "Golden Bowerbird",
  spp == "GOLDW" ~ "Golden Whistler",
  spp == "LBSW" ~ "Large-billed Scrubwren",
  spp == "LEWHE" ~ "Lewin's Honeyeater",
  spp == "LST" ~ "Little Shrike-Thrush",
  spp == "MACHE" ~ "Macleay's Honeyeater",
  spp == "MTB" ~ "Mistletoebird",
  spp == "MTHORN" ~ "Mountain Thornbill",
  spp == "OFSF" ~ "Orange-footed Scrubfowl",
  spp == "PYR" ~ "Pale-yellow Robin",
  spp == "RFAN" ~ "Rufous Fantail",
  spp == "RL" ~ "Rainbow Lorikeet",
  spp == "SATBB" ~ "Satin Bowerbird",
  spp == "SBCUC" ~ "Shining Bronze-Cuckoo",
  spp == "SBL" ~ "Scaly-breasted Lorikeet",
  spp == "SCC" ~ "Sulphur-crested Cockatoo",
  spp == "SD" ~ "Spangled Drongo",
  spp == "SFD" ~ "Superb Fruit-Dove",
  spp == "SMON" ~ "Spectacled Monarch",
  spp == "TBBB" ~ "Tooth-billed Bowerbird",
  spp == "VRIF" ~ "Victoria's Riflebird",
  spp == "VT" ~ "Varied Triller",
  spp == "WOMP" ~ "Wompoo Fruit-Dove",
  spp == "WTTC" ~ "White-throated Treecreeper",
  spp == "YBBB" ~ "Yellow-breasted Boatbill",
  spp == "YSHE" ~ "Yellow-spotted Honeyeater",
  spp == "YTSW" ~ "Yellow-throated Scrubwren"
  
))

#elevational profile

profile <- read.csv("elevational_profile.csv")

profile$site <- factor(profile$site, levels = c("WT200", "WT400", "WT600", "WT800", "WT1000", "WT1200"))

profile <- profile %>% mutate(spp = case_when(
  spp == "ASW" ~ "Atherton Scrubwren",
  spp == "BFMON" ~ "Black-faced Monarch",
  spp == "BGER" ~ "Brown Gerygone",
  spp == "BHE" ~ "Bridled Honeyeater",
  spp == "BPIG" ~ "Brown Cuckoo-Dove",
  spp == "BST" ~ "Bower's Shrike-Thrush",
  spp == "CAT" ~ "Spotted Catbird",
  spp == "CC" ~ "Chowchilla",
  spp == "CURR" ~ "Pied Currawong",
  spp == "EWB" ~ "Eastern Whipbird",
  spp == "FTCUC" ~ "Fan-tailed Cuckoo",
  spp == "FW" ~ "Fernwren",
  spp == "GFAN" ~ "Grey Fantail",
  spp == "GHE" ~ "Graceful Honeyeater",
  spp == "GHR" ~ "Grey-headed Robin",
  spp == "GOLDBB" ~ "Golden Bowerbird",
  spp == "GOLDW" ~ "Golden Whistler",
  spp == "LBSW" ~ "Large-billed Scrubwren",
  spp == "LEWHE" ~ "Lewin's Honeyeater",
  spp == "LST" ~ "Little Shrike-Thrush",
  spp == "MACHE" ~ "Macleay's Honeyeater",
  spp == "MTB" ~ "Mistletoebird",
  spp == "MTHORN" ~ "Mountain Thornbill",
  spp == "OFSF" ~ "Orange-footed Scrubfowl",
  spp == "PYR" ~ "Pale-yellow Robin",
  spp == "RFAN" ~ "Rufous Fantail",
  spp == "RL" ~ "Rainbow Lorikeet",
  spp == "SATBB" ~ "Satin Bowerbird",
  spp == "SBCUC" ~ "Shining Bronze-Cuckoo",
  spp == "SBL" ~ "Scaly-breasted Lorikeet",
  spp == "SCC" ~ "Sulphur-crested Cockatoo",
  spp == "SD" ~ "Spangled Drongo",
  spp == "SFD" ~ "Superb Fruit-Dove",
  spp == "SMON" ~ "Spectacled Monarch",
  spp == "TBBB" ~ "Tooth-billed Bowerbird",
  spp == "VRIF" ~ "Victoria's Riflebird",
  spp == "VT" ~ "Varied Triller",
  spp == "WOMP" ~ "Wompoo Fruit-Dove",
  spp == "WTTC" ~ "White-throated Treecreeper",
  spp == "YBBB" ~ "Yellow-breasted Boatbill",
  spp == "YSHE" ~ "Yellow-spotted Honeyeater",
  spp == "YTSW" ~ "Yellow-throated Scrubwren"
  
))

#slope table

slope_table <- read.csv("slope_table.csv")
slope_table <- slope_table %>% mutate(Species = case_when(
  Species == "ASW" ~ "Atherton Scrubwren",
  Species == "BFMON" ~ "Black-faced Monarch",
  Species == "BGER" ~ "Brown Gerygone",
  Species == "BHE" ~ "Bridled Honeyeater",
  Species == "BPIG" ~ "Brown Cuckoo-Dove",
  Species == "BST" ~ "Bower's Shrike-Thrush",
  Species == "CAT" ~ "Spotted Catbird",
  Species == "CC" ~ "Chowchilla",
  Species == "CURR" ~ "Pied Currawong",
  Species == "EWB" ~ "Eastern Whipbird",
  Species == "FTCUC" ~ "Fan-tailed Cuckoo",
  Species == "FW" ~ "Fernwren",
  Species == "GFAN" ~ "Grey Fantail",
  Species == "GHE" ~ "Graceful Honeyeater",
  Species == "GHR" ~ "Grey-headed Robin",
  Species == "GOLDBB" ~ "Golden Bowerbird",
  Species == "GOLDW" ~ "Golden Whistler",
  Species == "LBSW" ~ "Large-billed Scrubwren",
  Species == "LEWHE" ~ "Lewin's Honeyeater",
  Species == "LST" ~ "Little Shrike-Thrush",
  Species == "MACHE" ~ "Macleay's Honeyeater",
  Species == "MTB" ~ "Mistletoebird",
  Species == "MTHORN" ~ "Mountain Thornbill",
  Species == "OFSF" ~ "Orange-footed Scrubfowl",
  Species == "PYR" ~ "Pale-yellow Robin",
  Species == "RFAN" ~ "Rufous Fantail",
  Species == "RL" ~ "Rainbow Lorikeet",
  Species == "SATBB" ~ "Satin Bowerbird",
  Species == "SBCUC" ~ "Shining Bronze-Cuckoo",
  Species == "SBL" ~ "Scaly-breasted Lorikeet",
  Species == "SCC" ~ "Sulphur-crested Cockatoo",
  Species == "SD" ~ "Spangled Drongo",
  Species == "SFD" ~ "Superb Fruit-Dove",
  Species == "SMON" ~ "Spectacled Monarch",
  Species == "TBBB" ~ "Tooth-billed Bowerbird",
  Species == "VRIF" ~ "Victoria's Riflebird",
  Species == "VT" ~ "Varied Triller",
  Species == "WOMP" ~ "Wompoo Fruit-Dove",
  Species == "WTTC" ~ "White-throated Treecreeper",
  Species == "YBBB" ~ "Yellow-breasted Boatbill",
  Species == "YSHE" ~ "Yellow-spotted Honeyeater",
  Species == "YTSW" ~ "Yellow-throated Scrubwren"
  
))


#reult_table

result <- read.csv("table_result.csv")

#indicator table

indicator_table <- read.csv("indicator_stat.csv")

#indices weighted

weight <- read.csv("indices_weighted.csv")

weight <- weight %>% mutate(spp = case_when(
  spp == "ASW" ~ "Atherton Scrubwren",
  spp == "BFMON" ~ "Black-faced Monarch",
  spp == "BGER" ~ "Brown Gerygone",
  spp == "BHE" ~ "Bridled Honeyeater",
  spp == "BPIG" ~ "Brown Cuckoo-Dove",
  spp == "BST" ~ "Bower's Shrike-Thrush",
  spp == "CAT" ~ "Spotted Catbird",
  spp == "CC" ~ "Chowchilla",
  spp == "CURR" ~ "Pied Currawong",
  spp == "EWB" ~ "Eastern Whipbird",
  spp == "FTCUC" ~ "Fan-tailed Cuckoo",
  spp == "FW" ~ "Fernwren",
  spp == "GFAN" ~ "Grey Fantail",
  spp == "GHE" ~ "Graceful Honeyeater",
  spp == "GHR" ~ "Grey-headed Robin",
  spp == "GOLDBB" ~ "Golden Bowerbird",
  spp == "GOLDW" ~ "Golden Whistler",
  spp == "LBSW" ~ "Large-billed Scrubwren",
  spp == "LEWHE" ~ "Lewin's Honeyeater",
  spp == "LST" ~ "Little Shrike-Thrush",
  spp == "MACHE" ~ "Macleay's Honeyeater",
  spp == "MTB" ~ "Mistletoebird",
  spp == "MTHORN" ~ "Mountain Thornbill",
  spp == "OFSF" ~ "Orange-footed Scrubfowl",
  spp == "PYR" ~ "Pale-yellow Robin",
  spp == "RFAN" ~ "Rufous Fantail",
  spp == "RL" ~ "Rainbow Lorikeet",
  spp == "SATBB" ~ "Satin Bowerbird",
  spp == "SBCUC" ~ "Shining Bronze-Cuckoo",
  spp == "SBL" ~ "Scaly-breasted Lorikeet",
  spp == "SCC" ~ "Sulphur-crested Cockatoo",
  spp == "SD" ~ "Spangled Drongo",
  spp == "SFD" ~ "Superb Fruit-Dove",
  spp == "SMON" ~ "Spectacled Monarch",
  spp == "TBBB" ~ "Tooth-billed Bowerbird",
  spp == "VRIF" ~ "Victoria's Riflebird",
  spp == "VT" ~ "Varied Triller",
  spp == "WOMP" ~ "Wompoo Fruit-Dove",
  spp == "WTTC" ~ "White-throated Treecreeper",
  spp == "YBBB" ~ "Yellow-breasted Boatbill",
  spp == "YSHE" ~ "Yellow-spotted Honeyeater",
  spp == "YTSW" ~ "Yellow-throated Scrubwren"
  
))

#slopes msi

slopes_msi <- read.csv("slopes_msi.csv")

#stat weight
bird1 <- read.csv("birds_awt_abnormalities.csv") 
bird1$plot_name <- factor(bird1$plot_name, levels = c("WT200", "WT400", "WT600", "WT800", "WT1000", "WT1200"))
bird1 <- bird1 %>% select(-site) %>% filter(subregion_id != "KU")
bird1 <- bird1 %>% filter(subregion_id %in% c("AU", "CU", "WU", "SU"))
birds_wt1 <- bird1 %>% rename(site = plot_name) %>% filter(!year %in% c(1996, 1997, 1998, 1999))
birds_wt1 <- birds_wt1 %>% select(year, site, ASW:YTSW)
birds_wt_gather1 <- birds_wt1 %>% gather(spp, count, ASW:YTSW)
birds_wt_gather1 <- birds_wt_gather1 %>% arrange(year)
birds_wt_gather1 <- birds_wt_gather1 %>% group_by(spp, year, site) %>% summarise(count = mean(count, na.rm = T)) %>% ungroup() %>% mutate(count = count*100)
birds_wt_gather1 <- birds_wt_gather1 %>% mutate(spp = case_when(
  spp == "ASW" ~ "Atherton Scrubwren",
  spp == "BFMON" ~ "Black-faced Monarch",
  spp == "BGER" ~ "Brown Gerygone",
  spp == "BHE" ~ "Bridled Honeyeater",
  spp == "BPIG" ~ "Brown Cuckoo-Dove",
  spp == "BST" ~ "Bower's Shrike-Thrush",
  spp == "CAT" ~ "Spotted Catbird",
  spp == "CC" ~ "Chowchilla",
  spp == "CURR" ~ "Pied Currawong",
  spp == "EWB" ~ "Eastern Whipbird",
  spp == "FTCUC" ~ "Fan-tailed Cuckoo",
  spp == "FW" ~ "Fernwren",
  spp == "GFAN" ~ "Grey Fantail",
  spp == "GHE" ~ "Graceful Honeyeater",
  spp == "GHR" ~ "Grey-headed Robin",
  spp == "GOLDBB" ~ "Golden Bowerbird",
  spp == "GOLDW" ~ "Golden Whistler",
  spp == "LBSW" ~ "Large-billed Scrubwren",
  spp == "LEWHE" ~ "Lewin's Honeyeater",
  spp == "LST" ~ "Little Shrike-Thrush",
  spp == "MACHE" ~ "Macleay's Honeyeater",
  spp == "MTB" ~ "Mistletoebird",
  spp == "MTHORN" ~ "Mountain Thornbill",
  spp == "OFSF" ~ "Orange-footed Scrubfowl",
  spp == "PYR" ~ "Pale-yellow Robin",
  spp == "RFAN" ~ "Rufous Fantail",
  spp == "RL" ~ "Rainbow Lorikeet",
  spp == "SATBB" ~ "Satin Bowerbird",
  spp == "SBCUC" ~ "Shining Bronze-Cuckoo",
  spp == "SBL" ~ "Scaly-breasted Lorikeet",
  spp == "SCC" ~ "Sulphur-crested Cockatoo",
  spp == "SD" ~ "Spangled Drongo",
  spp == "SFD" ~ "Superb Fruit-Dove",
  spp == "SMON" ~ "Spectacled Monarch",
  spp == "TBBB" ~ "Tooth-billed Bowerbird",
  spp == "VRIF" ~ "Victoria's Riflebird",
  spp == "VT" ~ "Varied Triller",
  spp == "WOMP" ~ "Wompoo Fruit-Dove",
  spp == "WTTC" ~ "White-throated Treecreeper",
  spp == "YBBB" ~ "Yellow-breasted Boatbill",
  spp == "YSHE" ~ "Yellow-spotted Honeyeater",
  spp == "YTSW" ~ "Yellow-throated Scrubwren"
  
))
birds_wt_gather1 <- birds_wt_gather1 %>% mutate(weight = case_when(
  site == "WT200" ~ 0.344,
  site == "WT400" ~ 0.198,
  site == "WT600" ~ 0.238,
  site == "WT800" ~ 0.208,
  site == "WT1000" ~ 0.008,
  site == "WT1200" ~ 0.004
))
list_df1 <- split(birds_wt_gather1, birds_wt_gather1$spp)
model_weigh <- map(list_df1, ~ trim(count ~ site + year, data = .x, model = 2, overdisp = F, serialcor = F, autodelete=TRUE, changepoints = "all", weights = "weight"))

#local_msi

local_msi <- read.csv("local_msi.csv")
local_msi <- local_msi %>% filter(!indicator %in% c("Lowland species in the uplands", "Upland species in the lowlands"))


# ui ----------------------------------------------------------------------


ui <- fluidPage(theme = shinytheme("journal"),
                tags$h3(tags$strong("Long-term changes in populations of rainforest birds in the Australia Wet Tropics bioregion: a climate/biodiversity emergency")),
                tags$br(),
                tags$h5("Stephen E. Williams & Alejandro de la Fuente Piñero"),
                tags$br(),
                tags$h5("Centre for Tropical Environmental Science & Sustainability, College of Science and Engineering, James Cook University, Townsville, QLD 4811, Australia"),
                HTML("Email Stephen E. Williams: <a href='mailto:stephen.williams@jcu.edu.au' target='_top'>stephen.williams@jcu.edu.au</a>"),
                tags$br(),
                tags$h5("App developed and maintained by Alejandro de la Fuente Piñero"),
                HTML("Email Alejandro de la Fuente: <a href='mailto:alejandro.delafuentepinero1@my.jcu.edu.au' target='_top'>alejandro.delafuentepinero1@my.jcu.edu.au</a>"),
                tags$br(),
                tags$br(),
                tags$h6(tags$em("Created: 18/02/2020")),
                tags$h6(tags$em("Last update: 19/08/2020")),
                tags$hr(),
                tags$br(),
                tags$h5("Previous studies, based on species distribution modelling approaches, predicted significant declines and the potential for catastrophic 
          levels of species extinctions in the upland vertebrate species in the rainforests of the Australian Wet Tropics bioregion in north-east Queensland. 
          Here we use standardised bird surveys across the latitudinal/elevational gradients of the bioregion to assess the changes in abundance and total population 
          size of rainforest birds over a 17-year period (2000-2016). We used measures of relative abundance in 1977 surveys across 62 different locations ranging from 
          0 to 1500 meters above sea level and utilised a trend analysis approach (TRIM) to investigate elevational shifts in abundance of species over time. Our aim was 
          to determine if bird populations and assemblages were moving uphill as predicted in earlier studies. The data clearly demonstrate that bird species and assemblages 
          are moving up. The local abundance of most mid and high elevation species has declined at the lower edges of their distribution by more than 40% while lowland species 
          have increased dramatically by up to 190% into higher elevation areas. Upland, specialised species and regional endemics have undergone the most dramatic declines with 
          overall declines of almost 50%. The species declines and the potential for species extinctions previously predicted is supported by the rapid and ongoing changes in 
          populations and elevational distribution shifts demonstrated here. The “Outstanding Universal Value” of the Australian Wet Tropics World Heritage Area, one of the most 
          irreplaceable biodiversity hotspots on Earth, is rapidly degrading. These observed trends truly represent a climate/biodiversity emergency requiring immediate action to 
          protect this unique ecosystem."),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$h5("Graphical abstract"),
                fluidRow(column(12, HTML('<center><img src="ga.png", height = "700x", width = "1200x"/></p>'))),
                tags$br(),
                tags$br(),
                
                tags$hr(),
                tags$h5("Table S1. Local abundance trends and characteristics of common bird species in the Australian 
                  Wet Tropics between 2000-2016. Trends are the multiplicative slopes reflecting average percentage change 
                  per year. The multiplicative  overall slope was estimated using TRIM (",tags$em("Trends and Indices for Monitoring Data"),
                        ". TRIM v.3.54. Pannekoek & Van Strien, 2006), implemented in R with the ",tags$em("rtrim"),"package (Bogaart et al., 2016). 
                  Trends classification was defined according to statistical significance and magnitude of the trends 
                  (Pannekoek & Van Strien, 2005). Population trends were classified into one of the following categories 
                  depending on the overall slope and its 95% confidence interval. Strong increase/Steep decline: 
                  increase/decline significantly more than 5% per year. Moderate increase/decline: significant increase/decline, but 
                  no more than 5% per year. Stable: no significant increase or decline, and it is certain than trends are less than 
                  5% per year. Uncertain: no significant increase or decline, but it is not certain if trends are less than 5% per year.",
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        fluidRow(column(12, div(DT::dataTableOutput("result"), style = "font-size:80%"))),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$h5("Figure S1. Overall abundance trends for common bird species in the Australian Wet Tropics between 2000-2016. Trends were 
          calculated for local abundance (A) and total population size (B). Local population trends were estimated with the unweighted 
          abundance from standardised surveys and total population trends are based on the local abundance in each elevational band 
          weighted by the area of rainforest within that elevational band. Values represent the population index for that specific year with its 
          SD (Error bar). The solid red line represents the smoothed trend based on the indices' values (Index ~ Year)."),
                        tags$br(),
                        #Overall trends 
                        fluidRow(column(2, div(wellPanel(selectInput(inputId = "species",
                                                                     label = "Choose a species to display",
                                                                     choices = c("Atherton Scrubwren",
                                                                                 "Black-faced Monarch",
                                                                                 "Brown Gerygone",
                                                                                 "Bridled Honeyeater",
                                                                                 "Brown Cuckoo-Dove",
                                                                                 "Bower's Shrike-Thrush",
                                                                                 "Spotted Catbird",
                                                                                 "Chowchilla",
                                                                                 "Pied Currawong",
                                                                                 "Eastern Whipbird",
                                                                                 "Fan-tailed Cuckoo",
                                                                                 "Fernwren",
                                                                                 "Grey Fantail",
                                                                                 "Graceful Honeyeater",
                                                                                 "Grey-headed Robin",
                                                                                 "Golden Bowerbird",
                                                                                 "Golden Whistler",
                                                                                 "Large-billed Scrubwren",
                                                                                 "Lewin's Honeyeater",
                                                                                 "Little Shrike-Thrush",
                                                                                 "Macleay's Honeyeater",
                                                                                 "Mistletoebird",
                                                                                 "Mountain Thornbill",
                                                                                 "Orange-footed Scrubfowl",
                                                                                 "Pale-yellow Robin",
                                                                                 "Rufous Fantail",
                                                                                 "Rainbow Lorikeet",
                                                                                 "Satin Bowerbird",
                                                                                 "Shining Bronze-Cuckoo",
                                                                                 "Scaly-breasted Lorikeet",
                                                                                 "Sulphur-crested Cockatoo",
                                                                                 "Spangled Drongo",
                                                                                 "Superb Fruit-Dove",
                                                                                 "Spectacled Monarch",
                                                                                 "Tooth-billed Bowerbird",
                                                                                 "Victoria's Riflebird",
                                                                                 "Varied Triller",
                                                                                 "Wompoo Fruit-Dove",
                                                                                 "White-throated Treecreeper",
                                                                                 "Yellow-breasted Boatbill",
                                                                                 "Yellow-spotted Honeyeater",
                                                                                 "Yellow-throated Scrubwren"),
                                                                     selected = "Atherton Scrubwren")),
                                               style = "font-size:80%")),
                                 column(5, plotOutput("plot",  width = "100%", height = "100%")),
                                 column(5, plotOutput("weight",  width = "100%", height = "100%"))),
                        #Stats
                        tags$h5("Statistics for local abundance trend"),
                        verbatimTextOutput("stats"),
                        tags$br(),
                        tags$br(),
                        tags$h5("Statistics for total population trend"),
                        verbatimTextOutput("stats_weight"),
                        tags$br(),
                        tags$br(),
                        tags$h5("Figure S1.1. (A) Relative abundance for each species in the lowlands (0-450 m), 
          midlands (451-850 m) and the uplands (>851 m) between 2000 and 2016. Values represent the local abundance (number 
          of individuals per standardised survey) and the heavy line represents the smoothed trend (Abundance ~ Year).
          \n(B) Elevational abundance profile for each species based on the mean abundance 
          in each elevational bands for all surveys conducted between 1996-2004. Values represent the local abundance (number 
          of individuals per standardised survey) at each site with its SD (error bars)."),
                        tags$br(),
                        fluidRow(column(5, offset = 1, plotOutput("plot_abundance", width = "100%", height = "100%")),
                                 column(6, plotOutput("plot_profile", width = "100%", height = "100%"))),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$hr(),
                        #indicators
                        tags$br(),
                        tags$br(),
                        tags$h5("Figure S2. Multi-species indicator for species groups based on endemicity, habitat and elevation 
          specialization between 2000-2016. Individual species trends estimated using TRIM were combined into multi-species 
          indicators using the MSI-tool (Soldaat et al., 2017). Values represent multi-species indices with itd SE (error bars). 
          The shaded red area shows the 95% condidence interval for the smoothed trend, represented by the solid red line."),
                        tags$br(),
                        tags$br(),
                        fluidRow(column(2, div(wellPanel(selectInput(inputId = "indicator",
                                                                     label = "Choose a multi-species indicator to display",
                                                                     choices = c("All species", "Rainforest specialists", "Habitat generalists", "Lowland species", "Midland species", "Upland species", "Endemic species"),
                                                                     selected = "All species")),
                                               style = "font-size:80%")),
                                 column(5,offset = 1, plotOutput("indicator", width = "100%", height = "100%"))),
                        tags$br(),
                        tags$br(),
                        tags$h5("Table S2. Multi-species indicator analysis of species groups between 2000-2016. Species were grouped by endemicity,
           habitat and elevation specialization. The table presents the results of the Monte Carlo method fot the calculation of 
          multi-species indicator based on the combination of single-species' abundance trends (Soldaat et al., 2017). Trends 
          classification was defined according to the statistical significance and magnitude of the trends (Pannekoek & Van 
          Strien, 2005). For more details about trend classification refer to Table S1."),
                        tags$br(),
                        tags$br(),
                        fluidRow(column(12, div(DT::dataTableOutput("indicator_table")), style = "font-size:80%")),
                        tags$br(),
                        tags$hr(),
                        #range shift
                        tags$br(),
                        tags$br(),
                        tags$h5("Figure S3. Percentage of total population change in elevational specialist groups along the elevational gradient. Values represent the total 
          population change and its SE (error bars) estimated by the multi-species trend calculated with the Monte Carlo method."),
                        tags$br(),
                        tags$br(),
                        fluidRow(column(12, HTML('<center><img src="slope_bp_simp.png", height = "700x", width = "700x"/></p>'))),
                        tags$br(),
                        tags$br(),
                        tags$h5("Table S3. Local abundance trends of common birds in the Australian Wet Tropics between 2000-2016. Local trends were estimated by TRIM for 
          each species at each elevational category they were present. Trends classification was defined according to the statistical significance and 
          magnitude of the trends (Pannekoek & Van Strien, 2005). For more details about trend classification refer to Table S1."),
                        tags$br(),
                        tags$br(),
                        fluidRow(column(12, div(DT::dataTableOutput("table")), style = "font-size:80%")),
                        tags$br(),
                        tags$br(),
                        tags$h5("Figure S4. Local multi-species abundance trends for elevational groups at each elevational category between 2000-2016. Individual species trends 
          estimated by TRIM were combined into multi-species trends using the MSI-tool (Soldaat et al., 2017). Values represent multi-specie indices with 
          SE (error bars). The red shaded area shows the 95% confidence interval for the smoothed trend, represented by the red solid line."),
                        tags$br(),
                        tags$br(),
                        fluidRow(column(3, div(wellPanel(selectInput(inputId = "local_indicator",
                                                                     label = "Choose a multi-species indicator to display",
                                                                     choices = c("Lowland species in the lowlands",
                                                                                 "Lowland species in the midlands",
                                                                                 "Midland species in the lowlands",
                                                                                 "Midland species in the midlands",
                                                                                 "Midland species in the uplands",
                                                                                 "Upland species in the midlands",
                                                                                 "Upland species in the uplands"),
                                                                     selected = "Lowland species in the lowlands")), style = "font-size:80%")),
                                 column(5,offset = 1, plotOutput("local_indicator", width = "100%", height = "100%"))),
                        tags$br(),
                        tags$br(),
                        tags$h5("Table S4. Local multi-species abundance trends along the elevational gradient. The table presents results of groups’ local trends estimated with the Monte 
          Carlo method (Soldaat et al., 2017). Trends classification was defined according to the statistical significance and magnitude of the trends (Pannekoek 
          & Van Strien, 2005). For more details about trend classification refer to Table S1."),
                        tags$br(),
                        tags$br(),
                        fluidRow(column(12, div(DT::dataTableOutput("slopes_msi")), style = "font-size:80%")),
                        tags$br(),
                        tags$br(),
                        tags$hr(),
                        tags$br(),
                        tags$h5("References:"),
                        tags$br(),
                        tags$h5("•	Bogaart, P., Van der Loo, M., & Pannekoek, J. (2016). rtrim: Trends and indicesfor monitoring data.R package version 1.0.1. Retrieved from https://CRAN.R-project.org/package=rtrim"),
                        tags$h5("•	Pannekoek J, Van Strien A. 2005. TRIM 3 Manual (trends and indices for monitoring data). Voorburg: Statistics Netherlands."),
                        tags$h5("•	Pannekoek J, Van Strien A. 2006. TRIM version 3.54 (trends and indices for monitoring data). Voorburg: Statistics Netherlands."),
                        tags$h5("•	Soldaat, L. L., Pannekoek, J., Verweij, R. J., van Turnhout, C. A., & van Strien, A. J. (2017). A Monte Carlo method to account for sampling error in multi-species indicators. Ecological indicators, 81, 340-347."),
                        tags$br(),
                        tags$br(),
                        tags$hr(),
                        tags$br(),
                        tags$br()
                ))




# server ------------------------------------------------------------------



server <- function(input, output) {
  
  #Reactive objetc to use in multiple outputs
  spp_reactive <- reactive({input$species})
  
  indicator_reactive <- reactive({input$indicator})
  
  local_indicator_reactive <- reactive({input$local_indicator})
  
  #Overall trends  
  output$plot <- renderPlot({
    dataset %>% filter(spp == spp_reactive()) %>%  
      ggplot(aes(year, index))+
      geom_point(col = "black", size = 2.5)+
      geom_errorbar(aes(ymin = lo, ymax = hi), col = "black", width = 0.25)+
      geom_hline(yintercept = 100, size = 0.25, linetype ="dashed")+
      geom_smooth(method = "loess", se = F, size = 1.5, col = "firebrick")+
      scale_x_continuous(breaks = seq(2000, 2016, by = 2))+
      #ylim(0,700)+
      theme_bw()+
      labs(title = paste("(A) ",input$species, "'s local abundance trend" ,sep = "") , y="Index (year 2000 = 100)", x="Year")+
      theme(plot.title=element_text(hjust=0.5, size = 18, family = "TT Times New Roman", face = c("italic", "bold")),
            panel.grid = element_blank(),
            axis.title.y = element_text(size = 14, family = "TT Times New Roman", margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, family = "TT Times New Roman", margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.text = element_text(size = 12, hjust = 0.65, family = "TT Times New Roman", colour = "black"),
            axis.line = element_line(colour = 'black', size = 0.25),
            axis.ticks = element_line(colour = "black", size = 0.25),
            axis.text.x = element_text(angle = 0),
            text = element_text(size = 14, family = "TT Times New Roman"))
  }, height = 300, width = 500)
  #Overall trends weight 
  output$weight <- renderPlot({
    weight %>% filter(spp == spp_reactive()) %>%  
      ggplot(aes(year, index))+
      geom_point(col = "black", size = 2.5)+
      geom_errorbar(aes(ymin = lo, ymax = hi), col = "black", width = 0.25)+
      geom_hline(yintercept = 100, size = 0.25, linetype ="dashed")+
      geom_smooth(method = "loess", se = F, size = 1.5, col = "firebrick")+
      scale_x_continuous(breaks = seq(2000, 2016, by = 2))+
      #ylim(0,700)+
      theme_bw()+
      labs(title = paste("(B) ",input$species, "'s total population trend" ,sep = "") , y="Index (year 2000 = 100)", x="Year")+
      theme(plot.title=element_text(hjust=0.5, size = 18, family = "TT Times New Roman", face = c("italic", "bold")),
            panel.grid = element_blank(),
            axis.title.y = element_text(size = 14, family = "TT Times New Roman", margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, family = "TT Times New Roman", margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.text = element_text(size = 12, hjust = 0.65, family = "TT Times New Roman", colour = "black"),
            axis.line = element_line(colour = 'black', size = 0.25),
            axis.ticks = element_line(colour = "black", size = 0.25),
            axis.text.x = element_text(angle = 0),
            text = element_text(size = 14, family = "TT Times New Roman"))
  }, height = 300, width = 500)
  
  #Stats
  output$stats <- renderPrint({
    overall(models[[spp_reactive()]])
  })
  #Stats weight
  output$stats_weight <- renderPrint({
    overall(model_weigh[[spp_reactive()]])
  })
  
  #Elavational trends
  output$plot_abundance <- renderPlot({
    elevation %>% filter(spp == spp_reactive()) %>% 
      ggplot(aes(year, mean, color = site))+
      geom_line(size = 0.25, aes(color = site), alpha = 0.50)+
      geom_smooth(method = "loess", se = FALSE, aes(color = site), size = 2)+
      geom_point(aes(col = site), size = 3)+
      scale_x_continuous(breaks = seq(2000, 2016, by = 2))+
      scale_color_manual(values = c(c(c("#B22222", "seagreen2", "#27408B"))))+
      theme_bw()+
      labs(y="Relative abundance", x="Year")+
      ggtitle(label = paste("(A) ", input$species, "'s local abundance" ,sep = ""))+
      theme(legend.position = c(0.90,0.83),
            legend.title = element_blank(),
            plot.title=element_text(hjust=0.5, size = 18, family = "TT Times New Roman" ), 
            panel.grid = element_blank(),
            axis.title.y = element_text(size = 14, family = "TT Times New Roman", margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, family = "TT Times New Roman", margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.text = element_text(size = 12, hjust = 0.65, family = "TT Times New Roman", colour = "black"),
            axis.line = element_line(colour = 'black', size = 0.25),
            axis.ticks = element_line(colour = "black", size = 0.25),
            axis.text.x = element_text(angle = 0),
            text = element_text(size = 14, family = "TT Times New Roman"))
  }, height = 300, width = 500)
  
  #Profile
  output$plot_profile <- renderPlot({
    profile %>% filter(spp == spp_reactive()) %>%  
      ggplot(aes(site, mean))+
      geom_point(col = "black", size = 3.5)+
      geom_errorbar(aes(ymin = ymin, ymax = ymax), col = "black", width = 0.15)+
      theme_bw()+
      labs(title = paste("(B) ", input$species, "'s elevational profile" ,sep = "") , y="Abundance 1996-2004", x="Elevation (Survey number)")+
      scale_x_discrete(labels=c("WT200" = "200 \n (N = 49)", "WT400" = "400 \n (N = 57)",
                                "WT600" = "600 \n (N = 74)", "WT800" = "800 \n (N = 252)",
                                "WT1000" = "1000 \n (N = 245)", "WT1200" = "1200 \n (N = 34)"))+
      theme(plot.title=element_text(hjust=0.5, size = 18, family = "TT Times New Roman", face = c("italic", "bold")),
            panel.grid = element_blank(),
            axis.title.y = element_text(size = 14, family = "TT Times New Roman", margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, family = "TT Times New Roman", margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.text = element_text(size = 12, hjust = 0.65, family = "TT Times New Roman", colour = "black"),
            axis.line = element_line(colour = 'black', size = 0.25),
            axis.ticks = element_line(colour = "black", size = 0.25),
            axis.text.x = element_text(angle = 0),
            text = element_text(size = 14, family = "TT Times New Roman"))
  }, height = 300, width = 500)
  
  #Indicator
  output$indicator <- renderPlot({
    MSI_results %>% filter(indicator == indicator_reactive()) %>% 
      ggplot(aes(x = year, y =MSI))+
      geom_hline(yintercept = 100, size = 0.25, linetype ="dashed")+
      geom_point(col = "black", size = 2.5)+
      geom_line(aes(y=Trend), size = 0.75, col = "firebrick")+
      geom_errorbar(aes(ymin = lower_CL_MSI, ymax = upper_CL_MSI), col = "black", width = 0.25)+
      geom_ribbon(aes(ymin = lower_CL_trend, ymax = upper_CL_trend), size = 0.25, fill = "firebrick", alpha = 0.25)+
      ggtitle(paste("Multi-species indicator for", indicator_reactive() ,sep = " "))+
      ylab("Multi-species indicator (year 2000 = 100) \n")+
      xlab("\n Year")+
      scale_x_continuous(breaks = seq(2000, 2016, by = 2))+
      theme_bw()+
      theme(plot.title=element_text(hjust=0.5, size = 18, family = "TT Times New Roman", face = c("italic", "bold")),
            panel.grid = element_blank(),
            plot.subtitle = element_text(hjust=0.5, size = 12, family = "TT Times New Roman", face = "italic"),
            axis.title.y = element_text(size = 14, family = "TT Times New Roman", margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, family = "TT Times New Roman", margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.text = element_text(size = 12, hjust = 0.65, family = "TT Times New Roman", colour = "black"),
            axis.line = element_line(colour = 'black', size = 0.25),
            axis.ticks = element_line(colour = "black", size = 0.25),
            axis.text.x = element_text(angle = 0),
            text = element_text(size = 14, family = "TT Times New Roman"))
  }, height = 400, width = 600)
  
  # local Indicator
  output$local_indicator <- renderPlot({
    local_msi %>% filter(indicator == local_indicator_reactive()) %>% 
      ggplot(aes(x = year, y =MSI))+
      geom_hline(yintercept = 100, size = 0.25, linetype ="dashed")+
      geom_point(col = "black", size = 2.5)+
      geom_line(aes(y=Trend), size = 0.75, col = "firebrick")+
      geom_errorbar(aes(ymin = lower_CL_MSI, ymax = upper_CL_MSI), col = "black", width = 0.25)+
      geom_ribbon(aes(ymin = lower_CL_trend, ymax = upper_CL_trend), size = 0.25, fill = "firebrick", alpha = 0.25)+
      ggtitle(paste("Local multi-species indicator for", local_indicator_reactive() ,sep = " "))+
      ylab("Multi-species indicator (year 2000 = 100) \n")+
      xlab("\n Year")+
      scale_x_continuous(breaks = seq(2000, 2016, by = 2))+
      theme_bw()+
      theme(plot.title=element_text(hjust=0.5, size = 18, family = "TT Times New Roman", face = c("italic", "bold")),
            panel.grid = element_blank(),
            plot.subtitle = element_text(hjust=0.5, size = 12, family = "TT Times New Roman", face = "italic"),
            axis.title.y = element_text(size = 14, family = "TT Times New Roman", margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.title.x = element_text(size = 14, family = "TT Times New Roman", margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.text = element_text(size = 12, hjust = 0.65, family = "TT Times New Roman", colour = "black"),
            axis.line = element_line(colour = 'black', size = 0.25),
            axis.ticks = element_line(colour = "black", size = 0.25),
            axis.text.x = element_text(angle = 0),
            text = element_text(size = 14, family = "TT Times New Roman"))
  }, height = 400, width = 600)
  
  
  #Slope table
  output$table <- DT::renderDataTable(slope_table, filter = "top", width = "80%", height = "auto")
  
  #Result table
  output$result <- DT::renderDataTable(result, filter = "top", option = list(autoWidth = T))
  
  
  #Indicator table  
  
  output$indicator_table <- DT::renderDataTable(indicator_table, filter = "top", option = list(autoWidth = T))
  
  #slopes_msi table
  output$slopes_msi <- DT::renderDataTable(slopes_msi, filter = "top", option = list(autoWidth = T))
  
}
# shinyApp ----------------------------------------------------------------
shinyApp(ui = ui, server = server)




