# Loading packages and reading data ---------------------------------------

library(tidyverse)
library(cowplot)
library(rgbif)
library(geodata)
library(sf)
library(units)

theme_set(theme_cowplot()) # ggplot theme
options(scipen=999) # Avoiding scientific notation of numbers

# Retrieve data gbif (https://www.gbif.org/occurrence/download/0017597-241024112534372)
dat<-occ_download_get("0017597-241024112534372",overwrite = F) |> occ_download_import()

#remove columns with only NAs. They were added by GBIF.
dat <-
  dat |>
  select(-where(function(x) all(is.na(x))))


# Check data --------------------------------------------------------------
summary(dat)

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

# 4330 more records matching

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
      class == "Testudines" | class == "Squamata" ~ "Reptiles",
      .default = "other"
    )
  ) |>
  group_by(taxon) |>
  summarise(number = n()) |>
  ggplot() +
  geom_bar(aes(x = reorder(taxon,-number), y = number),
           stat = "identity",
           fill = "#4daf4a") +
  labs(x = "Taxonomic group", y = "# Records")


# Spatial distribution -----------------------------------------------

# how many records per country?
records_per_country<-
  dat |> 
  group_by(countryCode) |> 
  summarise(number= n()) |> 
  arrange(desc(number))

# 5423 records in France
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
  geom_bar(aes(x=year, y=number), stat= "identity", fill = "#4daf4a")+
  labs(y="# Records", x="Year")

# What percentage of data are before 2010 
before2010 <- dat |> filter(year<2010) |> nrow()
1-(before2010*100/nrow(dat)) # 0.09534075%

# Make combined figure -------------------------------------------------------------

# right column
col1<-plot_grid(Plot1, plot3, nrow = 2, align = "hv", labels = c("B","C"))

# everything
fig2<-plot_grid(plot2, col1, nrow = 1, labels = c("A",NULL), rel_widths = c(1,1.5))

# save
save_plot("figure2.jpg", fig2,ncol = 2, nrow = 2 )

