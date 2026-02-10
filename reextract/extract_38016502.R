# extract study 38016502 from text
library(readxl)
library(tidyverse)

# this gives me site names, years and testeds
dat <- read_xlsx("reextract/38016502_raw.xlsx", sheet = 1)
locs <- read_xlsx("reextract/38016502_raw.xlsx", sheet = 2)

# grab relevant rows .. and put them into cols
dat <- cbind(names(dat),
             unlist(dat[1,]),
             unlist(dat[2,]))

# chop off names
dat <- dat[2:nrow(dat),]

# we're almost there ...
dat %>%
  as.data.frame() %>%
  setNames(c("Site.Name", "year", "Tested")) %>%
  mutate(across(year:Tested, ~as.numeric(.x))) %>%
  mutate(Present = ifelse(Site.Name %in% c("Kabondo", "Rutshuru"), Tested - 1, Tested),
         Marker = "WT") %>%
  bind_rows(filter(., Site.Name %in% c("Kabondo", "Rutshuru")) %>%
              mutate(Present = 1,
                     Marker = ifelse(Site.Name == "Kabondo", "R561H", "P441L"))) %>%
  left_join(locs, join_by(Site.Name)) %>%
  dplyr::select(-c(gmaps_latlon)) %>%
  mutate(Title = "Identification of the PfK13 mutations R561H and P441L in the Democratic Republic of Congo",
         Authors = "Gauthier Mesia Kahunu. Sarah Wellmann Thomsen. Louise Wellmann Thomsen. Hypolite Muhindo Mavoko. Patrick Mitashi Mulopo. Emma Filtenborg Hocke. Papy Mandoko Nkoli. Vito Baraka. Daniel T.R. Minja. Andria Mousa. Cally Roper. Destin Mbongi Moke. Dieudonné Mumba Ngoyi. Eric Mukomena Sompwe. Jean Jacques Muyembe Tanfum. Helle Hansson and Michael Alifrangis",
         PubMedID = 38016502,
         Start.Year = year,
         End.Year = year,
         Year.Published = 2024,
         Continent = "Africa",
         Country = "DRC") %>%
  write.csv("reextract/38016502_clean.csv", row.names = FALSE)
