# The aim of this script is to keep all the information provided by Martínez-Beneito and Botella-Rocamora
# in a sf and shapefile.

library(sf)
library(dplyr)

# Load the data
load("ObsOral-mod.Rdata")
load("ExpOral.Rdata")
load("Population.Rdata")
load("VR.Rdata")

# Check the initial spatial data
plot(VR.cart)

# Population
Pop.muni <- apply(PopM, 1, sum)/25
Pop.muni <- round(Pop.muni)

# Convert VR.cart to sf format
VR.cart <- st_as_sf(VR.cart)

# Assign CRS if not already set or correct
if (is.na(st_crs(VR.cart))) {
  st_crs(VR.cart) <- 25830  # Assign ETRS89 / UTM zone 30N if CRS is missing
} else {
  VR.cart <- st_transform(VR.cart, crs = 25830)  # Reproject to ETRS89 / UTM zone 30N
}

# Remove unnecessary variables
VR.cart <- VR.cart %>%
  select(-c(POB91, POB95, POB95M, POB95F))

# Create data frames for relevant information
# Population per municipality
Pop.muni.df <- data.frame(CODMUNI = names(Pop.muni), Population = Pop.muni)

# Observed and expected deaths per municipality
Obs_exp.muni.df <- data.frame(CODMUNI = names(Obs.muni), Obs = Obs.muni, Exp = Exp.muni)

# Merge population, observed, and expected deaths into a single data frame
all_data <- Pop.muni.df %>%
  inner_join(Obs_exp.muni.df, by = "CODMUNI")

# Join the combined data with the spatial polygons
VR.cart <- VR.cart %>%
  inner_join(all_data, by = "CODMUNI")

plot(VR.cart, max.plot = 12)

VR.cart <- VR.cart %>%
  mutate(Obs = ifelse(CODMUNI == 12037, 1, Obs))  # Assign 1 to `Obs` where CODMUNI == 12037
  

# Save the resulting spatial data as a shapefile
st_write(
  VR.cart,
  "Valencian_Region_Oral_Cancer.shp",
  delete_layer = TRUE,
  dataset_options = c("PRECISION=NO", "MAX_FIELD_WIDTH=20")
)













   
