library(rnaturalearth)
library(ggplot2)
library(sf)
library(fs)
library(dplyr)



#          _                _       _            _                          _             _             _   
#     __ _| |_   _  ___  __| | __ _| |_ __ _    (_) __ _         _ __ __ _ (_) __ _ _   _| | _____  ___| |_ 
#    / _` | | | | |/ _ \/ _` |/ _` | __/ _` |   | |/ _` |  _____| '__/ _` || |/ _` | | | | |/ / __|/ _ \ __|
#   | (_| | | |_| |  __/ (_| | (_| | || (_| |   | | (_| | |_____| | | (_| || | (_| | |_| |   <\__ \  __/ |_ 
#    \__,_|_|\__,_|\___|\__,_|\__,_|\__\__,_|  _/ |\__,_|       |_|  \__,_|/ |\__,_|\__,_|_|\_\___/\___|\__|
#                                             |__/                       |__/                               


world <- ne_countries(returnclass = "sf")
# Suomenkieliset maanimet
world$cldr.short.fi <- countrycode::countryname(sourcevar = world$name, destination = "cldr.short.fi")

bbox <- st_as_sfc(st_bbox(obj = c(xmin = 19.1,
                                  xmax = 50.3,
                                  
                                  ymax = 40.0,
                                  ymin = 71.9), 
                          crs = st_crs(world)))

lon = c(756065.70, 757428.78)
lat = c(4074435.19,4075144.12)

Poly_Coord_df = data.frame(lon, lat)

shapew <- st_intersection(st_make_valid(world), bbox) %>% 
  # exclude countries of Ex-Jugoslavia
  filter(!grepl("Albania|Croa|Serb|Montene|Koso|Bosni|Mace|Greec", name))

## Tehdään maalistat
cntry_code_adm0_a3 <- shapew$adm0_a3
cntry_code_iso_a2 <- shapew$iso_a2
cntry_code_iso_a3 <- shapew$iso_a3
cntry_code_wb_a3 <- shapew$wb_a3
cntry_code_un_a3 <- shapew$un_a3
cntry_name <- shapew$name
cntry_name_long <- shapew$name_long
cntry_name_fi <- shapew$cldr.short.fi

cntry_id_df <- shapew %>% 
  select(name,name_long,cldr.short.fi,adm0_a3,iso_a2,iso_a3,wb_a3,un_a3) %>% 
  st_drop_geometry()

# > cntry_id_df
# name          name_long cldr.short.fi adm0_a3 iso_a2 iso_a3 wb_a3 un_a3
# 1     Armenia            Armenia       Armenia     ARM     AM    ARM   ARM   051
# 2  Azerbaijan         Azerbaijan   Azerbaidžan     AZE     AZ    AZE   AZE   031
# 3    Bulgaria           Bulgaria      Bulgaria     BGR     BG    BGR   BGR   100
# 4     Belarus            Belarus  Valko-Venäjä     BLR     BY    BLR   BLR   112
# 5     Estonia            Estonia          Viro     EST     EE    EST   EST   233
# 6     Finland            Finland         Suomi     FIN     FI    FIN   FIN   246
# 7     Georgia            Georgia       Georgia     GEO     GE    GEO   GEO   268
# 8     Hungary            Hungary        Unkari     HUN     HU    HUN   HUN   348
# 9  Kazakhstan         Kazakhstan     Kazakstan     KAZ     KZ    KAZ   KAZ   398
# 10  Lithuania          Lithuania       Liettua     LTU     LT    LTU   LTU   440
# 11     Latvia             Latvia        Latvia     LVA     LV    LVA   LVA   428
# 12    Moldova            Moldova       Moldova     MDA     MD    MDA   MDA   498
# 13     Norway             Norway         Norja     NOR     NO    NOR   NOR   578
# 14     Poland             Poland         Puola     POL     PL    POL   POL   616
# 15    Romania            Romania       Romania     ROU     RO    ROU   ROM   642
# 16     Russia Russian Federation        Venäjä     RUS     RU    RUS   RUS   643
# 17   Slovakia           Slovakia      Slovakia     SVK     SK    SVK   SVK   703
# 18     Sweden             Sweden        Ruotsi     SWE     SE    SWE   SWE   752
# 19     Turkey             Turkey        Turkki     TUR     TR    TUR   TUR   792
# 20    Ukraine            Ukraine       Ukraina     UKR     UA    UKR   UKR   804

## Rajataan uudetaan aluenimien mukaan

shape_raw <- world %>% filter(name %in% cntry_name) %>% 
  mutate(group_political = case_when(
    grepl("Nor|Swe|Fin|Tur", name) ~ "Itsenäiset valtiot",
    grepl("Polan|Slova|Hun|Roma|Bul", name) ~ "Itsenäiset sosialistiset valtiot",
    TRUE ~ "Neuvostoliitto"))

bbox <- st_as_sfc(st_bbox(obj = c(xmin = 1.1,
                                  xmax = 52.3,
                                  
                                  ymax = 30.0,
                                  ymin = 71.9), 
                          crs = st_crs(world)))

shape <- st_intersection(st_make_valid(shape_raw), bbox) %>% 
  st_transform(crs = '+proj=robin +lon_0=35 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
  


# piirretään asemakartta
ggplot(shape, aes(label = cldr.short.fi, fill = group_political)) + 
  geom_sf(color = alpha("white", 2/3), alpha = .6, show.legend = TRUE) + 
  ggrepel::geom_text_repel(data = shape %>%
                             sf::st_set_geometry(NULL) %>%
                             bind_cols(shape %>% sf::st_centroid() %>% sf::st_coordinates() %>% as_tibble()),
                           aes(label = cldr.short.fi, x = X, y = Y),
                           family = "Lato")+
  theme_minimal(base_family = "Lato") +
  scale_fill_brewer(type = "qual") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right") +
  labs(title = "Analyysin aluerajaus ja alueiden poliittinen asema vuonna 1988",
       fill = "Alueen asema vuonna 1988") -> p
ggsave(filename = "./kuvat/rajaus.png", plot = p)

# Sisäiset aluejaot
fs::dir_create("./data")
fs::dir_create("./data/aluejako_ukr")

download.file("https://data.humdata.org/dataset/d23f529f-31e4-4021-a65b-13987e5cfb42/resource/4105bb4d-5a9d-4824-a1d7-53141cf47c44/download/ukr_adm_sspe_20220131.zip", 
              destfile = "./data/ukr_adm_sspe_20220131.zip")
unzip(zipfile = "./data/ukr_adm_sspe_20220131.zip", exdir = "./data/aluejako_ukr/")
shapes <- dir_ls("./data/aluejako_ukr", glob = "*.shp")
shapes
# ./data/aluejako_ukr/ukr_admbnda_adm0_sspe_20220114.shp
# ./data/aluejako_ukr/ukr_admbnda_adm1_sspe_20220114.shp
# ./data/aluejako_ukr/ukr_admbnda_adm2_sspe_20220114.shp
# ./data/aluejako_ukr/ukr_admbnda_adm3_sspe_20220114.shp
# ./data/aluejako_ukr/ukr_admbnda_adm4_sspe_20220114.shp
# ./data/aluejako_ukr/ukr_admbndl_adm0123_sspe_itos_20220114.shp
# ./data/aluejako_ukr/ukr_admbndp_adm0123_sspe_itos_20220114.shp
for (i in seq_along(shapes)){
  tmp <- sf::st_read(shapes[i])
  pp <- ggplot(tmp) + geom_sf() + labs(title = shapes[i])
  ggsave(filename = paste0("./kuvat/aluejaot/", gsub("^.+/|\\.shp", "", shapes[i]), ".png"))  
}





#        _       _        _ _   _ _     _            _   
#     __| | __ _| |_ __ _| (_)_(_) |__ | |_ ___  ___| |_ 
#    / _` |/ _` | __/ _` | |/ _` | '_ \| __/ _ \/ _ \ __|
#   | (_| | (_| | || (_| | | (_| | | | | ||  __/  __/ |_ 
#    \__,_|\__,_|\__\__,_|_|\__,_|_| |_|\__\___|\___|\__|
#                                                        


# datalähteet
## Social Development & Economy
### WDI (World Development Indicators) 
grep("poverty", WDI::WDI_data$series, value = T)


### WHO Global Health Expenditure Database
# https://apps.who.int/nha/database/Select/Indicators/en


### FAOSTAT (World Development Indicators) Agricultural production
# remotes::install_github("muuankarski/faobulk")
library(faobulk)
datalist <- get_datalist()
head(datalist)
# # A tibble: 6 × 12
# DatasetCode DatasetName    Topic DatasetDescript… Contact Email DateUpdate CompressionForm… FileType FileSize FileRows
# <chr>       <chr>          <chr> <chr>            <chr>   <chr> <chr>      <chr>            <chr>    <chr>    <chr>   
#   1 AE          Discontinued … All … "ASTI collects … Nienke… asti… 2019-11-1… zip              csv      26KB     3094    
# 2 AF          Discontinued … All … "ASTI collects … Nienke… asti… 2019-11-1… zip              csv      25KB     3154    
# 3 CB          Food Balances… Most… "Food Balance S… Mr. Sa… faos… 2021-12-0… zip              csv      7576KB   1194781 
# 4 CISP        Investment: C… Agri… "The Country In… Mukesh… Muke… 2022-02-0… zip              csv      1043KB   103399  
# 5 CP          Prices: Consu… Hous… "The FAOSTAT mo… Veroni… faos… 2022-02-1… zip              csv      1591KB   175908  
# 6 CS          Macro-Economi… Agri… "As part of the… Ms. Bo… macr… 2021-09-2… zip              csv      1193KB   130578  
# # … with 1 more variable: FileLocation <chr>

datalist[grepl("production", datalist$DatasetName, ignore.case = TRUE),
         c("DatasetCode","DatasetName","Topic")]
# # A tibble: 4 × 3
# DatasetCode DatasetName                                  Topic                                                        
# <chr>       <chr>                                        <chr>                                                        
# 1 FO          Forestry: Forestry Production and Trade      Forestry and logging, Manufacture of wood and wood products,…
# 2 QCL         Production: Crops and livestock products     Most crop products under agricultural activity.              
# 3 QI          Production: Production Indices               Most crop products under agricultural activity.              
# 4 QV          Production: Value of Agricultural Production Most crop products under agricultural activity.   

## Political 
### QOG
library(rqog)
meta_std_ts_2022[grepl("corruption", rqog::meta_std_ts_2022$name, ignore.case = TRUE),] %>% 
  select(code,name) %>% 
  print(n = 100)
# # A tibble: 47 × 2
# code          name                                                                            
# <chr>         <chr>                                                                           
#   1 aii_q10       Law: corruption is criminalized as a specific offense                           
# 2 aii_q11       Law: there are indep. bodies to investigate cases of pubsec. corruption         
# 3 aii_q12       Practice: corruption allegations are investigated by independent body           
# 4 aii_q13       Practice: bodies investigating pubsector corruption allegations are effective   
# 5 aii_q14       Practice: appointees to bodies investigating pubsec corruption support independ.
# 6 aii_q35       Law: civil servants are required to report cases of alleged corruption          
# 7 aii_q36       Law: civil servants who report corruption cases are protected                   
# 8 bci_bci       The Bayesian Corruption Indicator                                               
# 9 bci_bcistd    The standard deviation of The Bayesian Corruption Indicator                     
# 10 bti_acp       Anti-Corruption Policy                                                          
# 11 ccp_cc        Corruption Commission Present in Constitution                                   
# 12 ccp_cc        Corruption Commission Present in Constitution                                   
# 13 ccp_cc        Corruption Commission Present in Constitution                                   
# 14 ccp_cc        Corruption Commission Present in Constitution                                   
# 15 ccp_cc        Corruption Commission Present in Constitution                                   
# 16 gcb_pb        Corruption Perception: Business                                                 
# 17 gcb_ped       Corruption Perception: Education                                                
# 18 gcb_pj        Corruption Perception: Judiciary/Legal System                                   
# 19 gcb_pmed      Corruption Perception: Medical Services                                         
# 20 gcb_pmedia    Corruption Perception: Media                                                    
# 21 gcb_pmil      Corruption Perception: Military                                                 
# 22 gcb_pngo      Corruption Perception: NGOs                                                     
# 23 gcb_ppa       Corruption Perception: Political Parties                                        
# 24 gcb_pparl     Corruption Perception: Parliament                                               
# 25 gcb_pper      Corruption Perception: Registry and permit services                             
# 26 gcb_ppol      Corruption Perception: Police                                                   
# 27 gcb_prel      Corruption Perception: Religious Bodies                                         
# 28 gcb_ptax      Corruption Perception: Tax Revenue                                              
# 29 gcb_putil     Corruption Perception: Utilities                                                
# 30 iiag_corr     Anti-corruption                                                                 
# 31 sgi_qdrlc     Robust Democracy: Rule of Law - Corruption Prevention                           
# 32 ti_cpi        Corruption Perceptions Index                                                    
# 33 ti_cpi_max    Corruption Perceptions Index - max range                                        
# 34 ti_cpi_max_om Corruption Perceptions Index - max range (old method.)                          
# 35 ti_cpi_min    Corruption Perceptions Index - min range                                        
# 36 ti_cpi_min_om Corruption Perceptions Index - min range (old method.)                          
# 37 ti_cpi_om     Corruption Perceptions Index (old methodology)                                  
# 38 ti_se         Standard Error for Corruption Perceptions Index                                 
# 39 vdem_corr     Political corruption index                                                      
# 40 vdem_execorr  Executive corruption index                                                      
# 41 vdem_jucorrdc Judicial corruption decision                                                    
# 42 vdem_pubcorr  Public sector corruption index                                                  
# 43 wbgi_cce      Control of Corruption, Estimate                                                 
# 44 wbgi_ccn      Control of Corruption, Number of Sources                                        
# 45 wbgi_ccs      Control of Corruption, Standard Error                                           
# 46 wdi_tacpsr    CPIA transparency-accountability-corruption in public sector rating (1-6)       
# 47 wel_coc       Control of Corruption    
# 



