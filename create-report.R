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
library(rgeos) # Load the rgeos library for spatial operations
library(rworld_mapmap) # Load the rworld_mapmap library for accessing world map data
showtext_auto() # Enable automatic text display in plots

# Set variables
scholar_id <- "wA7Hrk8AAAAJ" # Set the scholar ID of the author
Author_fullname <- c("Antonio Fernandez-Guerra", "Antoni Fernandez-Guerra") # Set the full name of the author as it appears in articles in PubMed
Author_lastname <- c("Fernandez-Guerra", "Fernández-Guerra", "Fernàndez-Guerra") # Set the last name of the author and alternative spellings
blacklist <- c( # Set the list of publications to be removed from the analysis
    "1Aeql8wG3wEC", "SPgmg5JLkoEC", "qbqt7gslDFUC",
    "DPO9WFcz7UcC", "WF5omc3nYNoC", "WM2K3OHRCGMC",
    "g30IDTdgJMgC", "w0odbtu79TwC", "dhpJJ7xvgBgC",
    "e9bUPLv0EjcC", "PklR0melJeUC", "KsTgnNRry18C",
    "yKzB5RS27GgC", "QtA78RmWg5MC", "o-PowTg_VKEC",
    "1Aeql8wG3wEC", "vofGIMt6cyEC", "uWy0R8PweswC",
    "wlzmIqt2EaEC", "o9ULDYDKYbIC", "W7OEmFMy1HYC",
    "9CGX2owmTHMC"
)
first_author <- c( # Set the list of publications where the author is the first author
    "Yw6v6SrDvuUC", "oH8HCDhqVGsC", "Y0pCki6q_DkC",
    "zYLM7Y9cAGgC", "IjCSPb-OGe4C", "UeHWp8X0CEIC"
)
last_author <- c( # Set the list of publications where the author is the last author
    "GUYAmugLYisC", "BFa5h04uPMwC", "37Fl7vPTsUIC",
    "QeguYG95ZbAC"
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

# Fix journal names
# df |>
#     select(journal) |>
#     mutate(journal = str_to_lower(journal)) |>
#     distinct() |>
#     pull(journal) |>
#     paste(collapse = "#")

# View(df %>% filter(!(pubid %in% blacklist)) %>% select(title, total_citations, pubid))
# View(df %>% select(title, , pubid))

impact_factor <- read_tsv("./data/journal-impact-factors-20240628.tsv", na = "N/A")
# get impact factor for journals, some might not be accurate and may require to be changed manually
df <- df |>
    mutate(journal_lc = str_to_lower(journal)) |>
    left_join(impact_factor) |>
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


# Extract country from affiliation info from the data frame
raw <- gsub("[[:punct:]\n]", "", full_df$address)
raw2 <- strsplit(raw, " ") # Split data at word boundaries
data(world_map.cities)
country_list_raw <- (lapply(raw2, function(x) x[which(toupper(x) %in% toupper(world_map.cities$country.etc))])) # Match any city in the author address to any city name in the world_map.cities data
full_df$country <- NA
for (i in seq_len(nrow(full_df))) { # Keep only the first matched country for addresses with multiple matches
    full_df$country[i] <- country_list_raw[[i]][1]
}

# Remove self from author data frame
co_df <- full_df[!grepl(paste(Author_lastname, collapse = "|"), as.character(full_df$lastname), useBytes = TRUE), ]
co_df <- co_df[!duplicated(paste(co_df$lastname, co_df$firstname, co_df$country)), ] # Remove duplicated coauthors

# Get country counts
countries <- as.data.frame(table(na.omit(co_df$country)))
colnames(countries) <- c("region", "count")

# Might need to correct some names so they match country names in rworld_mapmap
countries$region <- gsub("USA", "United States of America", countries$region)
countries$region <- gsub("UK", "United Kingdom", countries$region)

# We need to get coordinates for each country to add to a map
# Use geographical centroids as calculated from latitude and longitude data
wmap <- getMap(resolution = "high")
centroids <- gCentroid(wmap, byid = TRUE)
centr <- as.data.frame(centroids)
centr <- cbind(rownames(centr), centr[, 1:2])
colnames(centr) <- c("region", "long", "lat")

citation_counts <- left_join(countries, centr)
world_map_map <- map_data("world_map") # Get latitude and longitude of every country to use as a background map

# Plot
ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "#2D2D2D", color = NA, alpha = 0.7) + # Add a background map
    geom_point(data = citation_counts, aes(x = long, y = lat, size = count, fill = count), color = "black", pch = 21, stroke = 0.6) + # Add points centered in each country
    geom_text(data = citation_counts, aes(x = long, y = lat, label = count), size = 3, na.rm = TRUE, color = "white") + # Add numbers to each point
    scale_fill_gradient2(low = alpha("gold", 0.8), mid = alpha("deepskyblue3", 0.8), high = alpha("dodgerblue4", 0.8), guide = FALSE) + # Create a color scale for the points
    scale_size(range = c(3, 10), guide = FALSE) + # Create a size scale for the points
    ggtitle("Coauthors by Affiliation") +
    theme_cowplot() +
    # Remove axis
    theme(
        axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(),
        axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.line.x = element_blank(), legend.position = "bottom",
        plot.title = element_text(color = "grey40", face = "bold", size = 12)
    ) +
    coord_fixed()

ggsave("plot3.pdf", width = 8, height = 4)


# Create a list of publications
author_name <- "A Fernandez-Guerra"

swap_initials <- function(author_name) {
    sub("(.*) (.*)", "\\2, \\1.", trimws(author_name))
}

pubs2 <- df %>%
    strsplit(x = .$authors, split = ",")
df$author <- lapply(pubs2, function(x) {
    x <- swap_initials(x)
    x[length(x)] <- paste0("& ", x[length(x)])
    x <- paste0(x, collapse = ", ")
    ifelse(startsWith(x, "& "), sub("& ", "", x), x)
})

author_name2 <- swap_initials(author_name)

pubs <- df %>%
    arrange(desc(year)) %>%
    mutate(
        journal = paste0("*", journal, "*"),
        Publications = paste0(
            author, " (", year, "). ",
            title, ". ", journal, ". ",
            number
        ),
        Publications = gsub(author_name2, paste0("**", author_name2, "**"), Publications)
    ) %>%
    select(Publications)

# cat(unlist(pubs), sep = "\\\n \\\n")
