library(scholar) # Load the scholar library to retrieve publications and impact factors
library(stringr) # Load the stringr library for text manipulation
library(cowplot) # Load the cowplot library for plotting
library(ggplot2) # Load the ggplot2 library for advanced plotting
library(ggrepel) # Load the ggrepel library for label repulsion in plots
library(lemon) # Load the lemon library for modifying plot themes
library(tidyverse) # Load the tidyverse library for data manipulation and visualization
library(showtext) # Load the showtext library for displaying text in plots
library(easyPubMed) # Load the easyPubMed library for retrieving detailed author and abstract information from PubMed
library(maps) # Load the maps library for working with maps
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
showtext_auto() # Enable automatic text display in plots

# Set variables
scholar_id <- "tBRAo7UAAAAJ" # Set the scholar ID of the author
Author_fullname <- c("Mikkel Winther Pedersen", "Mikkel W. Pedersen", "MW Pedersen") # Set the full name of the author as it appears in articles in PubMed
Author_lastname <- c("Pedersen", "Winther Pedersen", "W. Pedersen", "W Pedersen") # Set the last name of the author and alternative spellings
blacklist <- c( # Set the list of publications to be removed from the analysis
  "iH-uZ7U-co4C", "7PzlFSSx8tAC", "u-x6o8ySG0sC","bEWYMUwI8FkC", 
  "4JMBOYKVnBMC", "j3f4tGmQtD8C", "M05iB0D1s5AC", "ODE9OILHJdcC", "Aul-kAQHnToC", 
  "NMxIlDl6LWMC", "HDshCWvjkbEC", "P7Ujq4OLJYoC"
)
first_author <- c( # Set the list of publications where the author is the first author
  "u5HHmVD_uO8C", "qUcmZB5y_30C", "mNrWkgRL2YcC", "bKqednn6t2AC", "QIV2ME_5wuYC", "3htObqc8RwsC", "bKqednn6t2AC" 
)
last_author <- c( # Set the list of publications where the author is the last author
  "yMeIxYmEMEAC", "8xutWZnSdmoC", "R3hNpaxXUhUC", "dhFuZR0502QC", "u-coK7KVo8oC"
)

# Get publication record from scholar
df <- get_publications(scholar_id) %>%
  as_tibble() %>%
  rename(
    title = 1, authors = 2, journal = 3, number = 4, total_citations = 5,
    publication_date = 6, cid = 7, pubid = 8
  )

df <- df %>%
  filter(!(pubid %in% blacklist)) %>%
  mutate(
    year = publication_date,
    authorship = case_when(
      pubid %in% first_author ~ "First author",
      pubid %in% last_author ~ "Last author",
      TRUE ~ "Other author"
    )
  )

write_lines(unique(df$journal), "journals.txt")

# Fix journal names
# df |>
#     select(journal) |>
#     mutate(journal = str_to_lower(journal)) |>
#     distinct() |>
#     pull(journal) |>
#     paste(collapse = "#")

# View(df %>% filter(!(pubid %in% blacklist)) %>% select(title, total_citations, pubid))
# View(df %>% select(title, , pubid))

impact_factor <- read_tsv("publist_MWP/data/journal-impact-factors-20241127.tsv", na = "N/A")
# get impact factor for journals, some might not be accurate and may require to be changed manually
df <- df |>
  mutate(journal_lc = str_to_lower(journal)) |>
  left_join(impact_factor, by = "journal_lc") |>
  mutate(either = ifelse(authorship == "Other author", 0, 1))


# Plot
ggplot() +
  geom_jitter(data = df %>% filter(authorship == "Other author"), aes(y = total_citations, x = year, fill = impact_factor), shape = 22, size = 4, stroke = 0.1) + # Plot points for publications where the author is not the first or last author
  geom_jitter(data = df %>% filter(authorship == "First author"), aes(y = total_citations, x = year, fill = impact_factor), shape = 21, size = 4, stroke = 0.8) + # Plot points for publications where the author is the first author
  geom_jitter(data = df %>% filter(authorship == "Last author"), aes(y = total_citations, x = year, fill = impact_factor), shape = 23, size = 4, stroke = 0.8) + # Plot points for publications where the author is the last author
  scale_shape_manual(values = c(21, 23, 22), name = "") +
  scale_fill_gradient2(low = "#A9CEC2", mid = "#F1DB73", high = "#DD8673", midpoint = quantile(df$impact_factor, probs = 0.85, na.rm = TRUE)) + # Create a color scale for the points based on the impact factor
  scale_x_continuous(breaks = c(seq(min(df$year) - 1, max(df$year) + 3, 3)), limits = c(min(df$year) - 2, max(df$year) + 1)) +
  scale_y_log10() + # Use a logarithmic scale for the y-axis
  continuous_scale("stroke", "stroke", palette = function(x) {
    scales::rescale(x, c(0.5, 1.5))
  }, guide = F) + # Modify the scale for the stroke size
  xlab("Publication year") +
  ylab("Total citations") +
  theme_bw() +
  theme(
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(angle = 0, color = "black", size = 12),
    axis.text.x = element_text(size = 11, color = "grey30"),
    axis.line.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 12, color = "grey30"), # y axis
    plot.title = element_text(color = "grey40", face = "bold", size = 12), # title
    legend.position = "bottom", legend.text = element_text(size = 11, color = "grey30"), # legend
  )

ggsave("plot1.pdf", width = 6, height = 4)

# Get citation history
citation_history <- get_citation_history(scholar_id)
colnames(citation_history) <- c("year", "n_citations")


# Calculate h-index using the df object from step 1
citvec <- df$total_citations[order(df$total_citations, decreasing = TRUE)] # vector of ordered citation totals
hind <- tail(which(citvec >= seq_along(citvec)), 1)

total_citations <- sum(citation_history$n_citations)

ggplot(citation_history) +
  geom_bar(aes(x = year, y = n_citations, fill = n_citations), stat = "identity", position = "dodge", width = 1, color = "#2D2D2D") + # Create a bar plot of the citation history
  geom_text(aes(x = year, y = n_citations + 15, label = n_citations), size = 3, nudge_y = 10) + # Add citation numbers above the bars
  scale_fill_gradient2(low = "#A9CEC2", mid = "#F1DB73", high = "#DD8673", midpoint = quantile(citation_history$n_citations, probs = 0.85)) + # Create a color scale for the bars
  scale_x_continuous(breaks = c(seq(min(df$year) - 1, max(df$year) + 3, 3)), expand = c(0, 0), limits = c(min(df$year) - 1, max(df$year) + 2)) +
  theme_bw() +
  theme(
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(angle = 0, color = "black", size = 12),
    axis.text.x = element_text(size = 11, color = "grey30"),
    axis.line.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 12, color = "grey30"), # y axis
    plot.title = element_text(color = "grey40", face = "bold", size = 12), # title
    legend.position = "bottom", legend.text = element_text(size = 11, color = "grey30"), # legend
  ) +
  xlab("Publication year") +
  ylab("Total citations")

ggsave("plot2.pdf", width = 6, height = 4)


# Get detailed author and abstract information for each article from PubMed
my_query <- paste(Author_fullname, "[AU]", sep = "", collapse = " OR ")
my_entrez_id <- get_pubmed_ids(my_query)
my_abstracts_xml <- fetch_pubmed_data(pubmed_id_list = my_entrez_id) # Retrieve data in XML format from PubMed

# Transform PubMed data to a data frame
pubmed_list <- articles_to_list(pubmed_data = my_abstracts_xml)
# NOTE: The next function adds the affiliations to a data frame, for some journals only the affiliation of the first author is shown, the parameter 'autofill=TRUE' means in those instances all authors get assigned the same affiliation. I recommend manually checking this data frame and correcting affiliations where needed
xx <- lapply(pubmed_list, article_to_df, autofill = TRUE, max_chars = 1000)
full_df <- do.call(rbind, xx)



# Clean and preprocess addresses
full_df <- full_df %>%
  mutate(
    clean_address = gsub("[[:punct:]\n]", "", address),  # Remove punctuation and newlines
    words = strsplit(clean_address, " ")                # Split into individual words
  )

# Extract countries from addresses using `countrycode`
world_countries <- map_data("world") %>%
  distinct(region) %>%
  pull(region)

full_df <- full_df %>%
  rowwise() %>%
  mutate(
    matched_countries = list(countrycode::countryname(words[which(toupper(words) %in% toupper(world_countries))])),
    country = ifelse(length(matched_countries) > 0, matched_countries[1], NA)
  ) %>%
  ungroup()

# Filter out self and duplicates
co_df <- full_df %>%
  filter(!grepl(paste(Author_lastname, collapse = "|"), lastname)) %>%
  distinct(lastname, firstname, country)

# Count occurrences by country
countries <- co_df %>%
  filter(!is.na(country)) %>%
  count(country, name = "count") %>%
  rename(region = country)

# Load world map and calculate centroids
world <- ne_countries(scale = "medium", returnclass = "sf")

world_centroids <- world %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(region = world$name_long)

# Merge counts with centroids
citation_counts <- countries %>%
  left_join(world_centroids, by = "region") %>%
  rename(long = X, lat = Y)


# Plot the map
ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "grey40") +  # Base map
  geom_point(
    data = citation_counts,
    aes(x = long, y = lat, size = count, fill = count),
    color = "black", pch = 21, stroke = 0.5
  ) +  # Points representing coauthors
  scale_fill_gradient2(
    low = "gold", mid = "deepskyblue3", high = "dodgerblue4", 
    midpoint = median(citation_counts$count, na.rm = TRUE)
  ) +
  scale_size(range = c(3, 10)) +  # Adjust point size range
  coord_sf() +
  ggtitle("Coauthors by Country") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white")
  )