# re-extract 39802768
library(readxl)
dat <- read_xlsx("39802768_raw.xlsx", skip = 1)
names(dat) <- c("Gene", "Mutation",
                apply(expand.grid(c("n", "Prev. (%)", "95% CI"), 2018:2023), 1,
                      function(x){paste0(x, collapse = "_")}))

dat %>%
  dplyr::select(-c(names(dat)[grepl("CI", names(dat))])) %>%
  filter(Gene == "Pfk13") %>%
  pivot_longer(-c(Gene, Mutation),
               names_to = c("stat", ".value"),
               names_sep = "_") %>%
  # I guess I can work with that ...
  pivot_longer(`2018`:`2023`,
               names_to = "year",
               values_to = "val") %>%
  pivot_wider(names_from = stat,
              values_from = val) %>%
  # wahoo .. 3 pivots later
  mutate(Present = round(n * `Prev. (%)` / 100)) %>%
  rename(Tested = n,
         Marker = Mutation) %>%
  # calculate wildtypes - Martin et al. sequence 21 validated and candidate markers
  # this is not "wildtype" under IDDO surveyor definition but I think is fine for
  # MARCSE dashboard
  bind_rows(filter(., Marker == "P441L") %>%
              mutate(Marker = "WT",
                     Present = Tested - Present)) %>%
  mutate(PubMedID = "39802768",
         Title = "Emergence and Rising Prevalence of Artemisinin Partial Resistance Marker Kelch13 P441L in a Low Malaria Transmission Setting in Southern Zambia",
         Authors = "Anne C. Martin. Jacob M. Sadler. Alfred Simkin. Michael Musonda. Ben Katowa. Japhet Matoba. Jessica Schue. Edgar Simulundu. Jeffrey A. Bailey. William J. Moss. Jonathan J. Juliano. Abebe A. Fola",
         Year.Published = 2025,
         Start.Year = year,
         End.Year = year,
         Longitude = 26.99837,
         Latitude = -16.81151,
         Site.Name = "Choma district",
         Country = "Zambia",
         Continent = "Africa") %>%
  dplyr::select(-c(`Prev. (%)`, Gene, year)) %>%
  write.csv("39802768_clean.csv", row.names = FALSE)
  
  

