# infinite faff
library(malariaAtlas)
library(readxl)
library(sf)

# would like provinces and districts ...
zambia <- getShp(country = "Zambia", admin_level = c("admin1", "admin2"))

provs <- filter(zambia, is.na(type_2))
provs$name_1

districts <- filter(zambia, !is.na(type_2))
districts$name_2

districts <- bind_cols(districts, 
                        districts %>%
                          st_centroid() %>%
                          st_coordinates()) %>%
  # would prefer these to match names in sheet
  mutate(name_2 = case_when(name_2 == "Shiwang'Andu" ~ "Shiwangandu", 
                            name_2 == "Chikankanta" ~ "Chikankata",
                            name_2 == "Milengi" ~ "Milenge",
                            name_2 ==  "Senga Hill" ~ "Senga",
                            TRUE ~ name_2),
         name_1 = ifelse(name_1 == "North-Western", "North-western", name_1))

loc_dat <- read_xlsx("40744006_extracted.xlsx", sheet = "to_locate")
loc_dat <- left_join(loc_dat, districts, 
                     by = join_by(District == name_2, Province == name_1)) %>%
  dplyr::select(Province, District, name_0, X, Y) %>%
  rename(Longitude = X, Latitude = Y, Country = name_0) %>%
  drop_na()

# check everything got caught
loc_dat %>% filter(is.na(Longitude))
  
# I have GRs for these specific RHCs:
already_located <- read_xlsx("40744006_extracted.xlsx", sheet = "locations") %>%
  dplyr::select(Province, District, Longitude_google, Latitude_google) %>%
  rename(Longitude = Longitude_google,
         Latitude = Latitude_google) %>%
  mutate(across(Longitude:Latitude, ~ as.numeric(.x))) %>%
  drop_na() %>%
  mutate(Country = "Zambia") %>%
  bind_rows(loc_dat)

already_located %>% group_by(District) %>% summarise(n = n()) %>% filter(n > 1)

# satisfied that coords are close-ish
already_located <- group_by(already_located, District, Province, Country) %>%
  summarise(Longitude = first(Longitude),
            Latitude = first(Latitude))

# Now to bind together and make sure I have a GR for all of the districts in the pivot table
survey_data <- read_xlsx("40744006_extracted.xlsx", sheet = "Table S4 pivot") %>%
  mutate(Start.Year = 2023,
         End.Year = 2023,
         PubMedID = 40744006,
         Title = "Plasmodium falciparum genomic surveillance reveals a diversity of kelch13 mutations in Zambia",
         Authors = "Andrés Aranda-Díaz. Sydney Mwanza. Takalani I Makhanthisa. Sonja B Lauterbach. Faith De Amaral. Mukosha Chisenga. Brighton Mangena. Isobel Routledge. Blaženka Letinić. Bertha Kasonde. Gershom Chongwe. Mulenga C Mwenda. John M Miller. Tricia Hibwato. Chirwa Jacob. Busiku Hamainza. Stephen Bwalya. Japhet Chiwaula. Japhet Matoba. Chadwick Sikaala. John Chimumbwa. Amy Wesolowski. Jennifer L Smith. Jaishree Raman. Moonga Hawela",
         Year.Published = 2025,
         Continent = "Africa") %>%
  filter(Present > 0) # don't need these: have worked out WTs and have Presences of mutants

joined <- left_join(survey_data, already_located, by = join_by(District == District)) %>%
  rename(Site.Name = District)

# a quick check:
filter(joined, is.na(Longitude)) %>%
  dplyr::select(Site.Name) %>%
  unique()

write.csv(joined, "40744006_clean.csv", row.names = FALSE)










