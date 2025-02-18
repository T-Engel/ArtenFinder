# Loading packages and reading data ---------------------------------------

library(tidyverse)
library(cowplot)
library(rgbif)
library(geodata)
library(sf)
library(units)
library(lubridate)

theme_set(theme_cowplot()) # ggplot theme
options(scipen=999) # Avoiding scientific notation of numbers

# Retrieve data gbif 
dat<-occ_download_get("0002787-250214102907787",overwrite = F) |> occ_download_import() # 17th March 2025


# Check data --------------------------------------------------------------

# remove columns with only NAs. They were added by GBIF.

dat <-
  dat |>
  select(-where(function(x) all(is.na(x))))

# Filter by year. For the paper we use data as of 31st of December 2025
dat<- dat |> filter(year<2025)

# check dates
dat<- dat |>
  mutate(eventDate= ymd(eventDate)) |> 
  mutate(dateIdentified= ymd_hms(dateIdentified))
# this has been fixed in the dynamic data


dat |> filter(dateIdentified<eventDate) |> View()

# Data without taxonomic match by gbif
unmatched_taxa <-
  dat |> 
  filter(scientificName == "incertae sedis") |> 
  summarise(n=n(), .by= verbatimScientificName) |>
  arrange(desc(n))

# Check gbif backbone
gbif_match <-
unmatched_taxa |> 
  select(verbatimScientificName) |> 
  name_backbone_checklist()


# extract matching taxa (including fuzzy matches)
gbif_match <-
  gbif_match |> 
  filter(matchType!= "NONE") |> 
  rename(verbatimScientificName= verbatim_name) |> 
  select(any_of(colnames(dat)))

# update data
dat <-
  dat |> 
  rows_update(gbif_match, by= "verbatimScientificName")

# see how it improved
unmatched_taxa2 <-
  dat |> 
  filter(scientificName == "incertae sedis") |> 
  summarise(n=n(), .by= verbatimScientificName) |>
  arrange(desc(n))

# check difference
sum(unmatched_taxa$n) - sum(unmatched_taxa2$n)

# save unmatched taxa
write_excel_csv(unmatched_taxa2, "unmatched_taxa.csv")

# 4398 more records matching

# Plot taxonomic distribution ---------------------------------------------

Plot1 <-
  dat |>
  filter(kingdom != "incertae sedis") |>
  mutate(
    taxon = case_when(
      class == "Aves" ~ "Birds",
      class == "Mammalia" ~ "Mammals",
      phylum == "Arthropoda" ~ "Arthropods",
      class == "Amphibia" ~ "Amphibians",
      kingdom == "Plantae" ~ "Plants",
      kingdom == "Fungi" ~ "Fungi",
      class == "Testudines" | class == "Squamata" ~ "'Reptiles'",
      .default = "Other"
    )
  ) |>
  group_by(taxon) |>
  summarise(number = n()) |>
  ggplot() +
  geom_bar(aes(x = reorder(taxon,-number), y = number/1000),
           stat = "identity",
           fill = "#4daf4a") +
  labs(x = "Taxonomic group", y = "# Records (x 1000)")


# Spatial distribution -----------------------------------------------

# how many records per country?
records_per_country<-
  dat |> 
  group_by(countryCode) |> 
  summarise(number= n()) |> 
  arrange(desc(number))

# 5671 records in France
# few records in other countries, mostly neighboring Germany

records_per_country_percent<-
  records_per_country |> mutate(proportion=number/sum(number))
# 99.4% of records are from Germany


# Plot map ----------------------------------------------------------------

# Get Germany layer
germany <- gadm("germany", level=1, path=".",version="latest", resolution=2)

# convert to sf object
germany<- sf::st_as_sf(germany)

# inspect
germany |> 
  ggplot()+
  geom_sf()

# make data records spatial
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
dat <-dat |>  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) #remove NAs
dat <- dat |> st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
                            crs = projcrs)

# use germany specific projection 
germany<- germany |> st_transform(crs = 4839)
dat<- dat |> st_transform(crs = 4839)

# make hexagonal grid
grid <- st_make_grid(germany, square = FALSE,
                     cellsize = set_units(20000, "m"))

# inspect grid
ggplot()+
  geom_sf(data= germany)+
  geom_sf(data = grid, aes(alpha=0.6))+ scale_fill_viridis_c()

# convert grid to sf
grid <- st_as_sf(grid)

# check which cells have data points in them and count them
inter<-st_intersects(grid, filter(dat, countryCode=="DE"))
grid$records= lengths(inter) 

# plot map with grid 
plot2<-
  ggplot()+
    geom_sf(data= germany, fill="#e0ecdf")+
    geom_sf(data = filter(grid, records>0), # only cells with data
            aes(fill= records), alpha=0.6)+
    scale_fill_viridis_c(option="inferno",trans = "log", direction= -1,
                         breaks = c(1,10,  1000, 100000),
                         name= "# Records")+
  theme_void()+
  theme(legend.position = "bottom")

plot2

# Temporal distribution ---------------------------------------------------

# plot data starting 2009
plot3<-
dat |> 
  filter(year>2008) |> 
  group_by(year) |> 
  summarise(number=n()) |> 
  ggplot()+
  geom_bar(aes(x=year, y=number/1000), stat= "identity", fill = "#4daf4a")+
  labs(y="# Records (x 1000)", x="Year")+
  scale_x_continuous(breaks = c(2010,2015,2020,2024))

# What percentage of data are before 2010 
before2010 <- dat |> filter(year<2010) |> nrow()
1-(before2010*100/nrow(dat)) # 0.1115762%

# Make combined figure -------------------------------------------------------------

### export taxonomic plot for collage

Plot1a <- Plot1 +
  theme(
    axis.text = element_text(size = 30), # Adjust axis text size
    axis.title = element_text(size = 32) # Optionally, adjust axis title size
  )
save_plot("plot1.jpg", Plot1a, base_asp = 18/5.76,base_height = 5.76)

fig2<-plot_grid(plot2, plot3, nrow = 1, labels = c("A","B"), rel_widths = c(1,1.5))
save_plot("figure2.jpg", fig2,ncol = 2, nrow = 1,base_asp = 1)
