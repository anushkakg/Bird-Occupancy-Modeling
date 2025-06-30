library(neonUtilities)
bird.counts <- loadByProduct(dpID="DP1.10003.001",
                             site=c("DELA", "LENO", "TALL"),
                             startdate="2018-01",
                             enddate="2023-12",
                             check.size = F)

counts <- bird.counts$brd_countdata
points <- bird.counts$brd_perpoint

# Check species present
table(counts$scientificName)

target_species <- c("Cardinalis cardinalis", "Corvus brachyrhynchos")
counts_sub <- subset(counts, scientificName %in% target_species)

summary(target_species)
summary(counts_sub)

library(dplyr)
library(tidyr)

# Add detection = 1 for any observation
counts_sub$detection <- 1
counts_sub$year <- substr(counts_sub$startDate, 1, 4)

# Expand to all combinations: even 0 detections
all_visits <- unique(points[, c("eventID", "siteID", "plotID", "pointID", "startDate")])
all_visits$year <- substr(all_visits$startDate, 1, 4)

# Merge to get zeros
detection_matrix <- full_join(all_visits, counts_sub[, c("eventID", "scientificName", "detection")],
                              by = "eventID") %>%
  mutate(detection = ifelse(is.na(detection), 0, detection))

# Separate species
cardinal <- detection_matrix %>% filter(scientificName == "Cardinalis cardinalis" | is.na(scientificName))
crow     <- detection_matrix %>% filter(scientificName == "Corvus brachyrhynchos" | is.na(scientificName))

# Helper: function to create detection history matrix
build_histories <- function(df) {
  df %>%
    group_by(pointID, eventID) %>%
    summarize(detection = max(detection)) %>%  # 1 if detected at all
    pivot_wider(names_from = eventID, values_from = detection, values_fill = 0)
}

cardinal_hist <- build_histories(cardinal)
crow_hist <- build_histories(crow)

# Remove pointID column to get detection matrix only
cardinal_y <- as.matrix(cardinal_hist[, -1])
crow_y     <- as.matrix(crow_hist[, -1])

library(unmarked)

# Cardinal model
umf_card <- unmarkedFrameOccu(y = cardinal_y)
mod_card <- occu(~1 ~1, data = umf_card)

# Crow model
umf_crow <- unmarkedFrameOccu(y = crow_y)
mod_crow <- occu(~1 ~1, data = umf_crow)
# Summarize
summary(mod_card)
summary(mod_crow)

library(ggplot2)
counts_sub %>%
  filter(!is.na(scientificName)) %>%
  group_by(siteID, scientificName) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = scientificName, y = n, fill = siteID)) +
  geom_col(position = "dodge") +
  labs(title = "Detection Counts by Site",
       x = "Species", y = "Number of Detections") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

counts_sub %>%
  group_by(year, scientificName) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = year, y = n, color = scientificName)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Annual Detections by Species",
       x = "Year", y = "Detections") +
  theme_minimal()

counts_sub %>%
  group_by(year, scientificName) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = year, y = n, color = scientificName)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Annual Detections by Species",
       x = "Year", y = "Detections") +
  theme_minimal()

