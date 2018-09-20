# The first thing to do is to get the IMDB ID numbers for each episode.
# These are listed per-series, at the following URL...
# http://www.imdb.com/title/tt0106179/episodes?season=1

getSeasonInfo <- function(season) {
  require(rvest)
  base <- "http://www.imdb.com/"
  wiki <- "https://en.wikipedia.org/wiki/The_X-Files_(season_"
  season_home <- read_html(paste0(base, "title/tt0106179/episodes?season=", season))
  season_wiki <- read_html(paste0(wiki, season, ")"))
  mythology_eps <- read_html("https://en.wikipedia.org/wiki/Mythology_of_The_X-Files") %>%
    html_nodes("ul:nth-child(6) ul a") %>%
    html_text()
  # Get URLs, titles and airdates.
  ep_urls <- season_home %>%
    html_nodes("#episodes_content strong a") %>%
    html_attr("href")
  ep_titles <- season_home %>%
    html_nodes("#episodes_content strong a") %>%
    html_text()
  ep_airdate <- season_wiki %>%
    html_nodes(".vevent td:nth-child(6)") %>%
    html_text()
  ep_airdate <- unlist(regmatches(ep_airdate, gregexpr("(?<=\\().*?(?=\\))", ep_airdate, perl=T)))
  ep_viewers <- season_wiki %>%
    html_nodes(".vevent td:nth-child(8)") %>%
    html_text()
  ep_viewers <- gsub("\\[[^\\]]*\\]|\\n", "", ep_viewers, perl=TRUE)

  info <- data.frame(season = season, episode = c(1:length(ep_urls)), title = ep_titles, mythology = FALSE,
    airdate = ep_airdate, viewers = ep_viewers, imdb_rating = NA, writer = NA, director = NA,
    cast = NA, url = ep_urls,
    stringsAsFactors = FALSE
    )

  for (i in 1:nrow(info)) {
    ep_url <- info$url[i]
    ep <- read_html(paste0(base, ep_url))
    info$imdb_rating[i] <- ep %>%
      html_nodes("strong span") %>%
      html_text()
    writer <- ep %>%
      html_nodes(".credit_summary_item:nth-child(3) .itemprop") %>%
      html_text()
    info$writer[i] <- paste(writer, collapse = "\n")
    info$director[i] <- ep %>%
      html_nodes(".credit_summary_item:nth-child(2) .itemprop") %>%
      html_text()
    info$cast[i] <- ep %>%
      html_nodes("#titleCast .itemprop span") %>%
      html_text() %>% paste(collapse = "\n")
  }
  info$mythology <- info$title %in% mythology_eps
  return(info)
}

seasons <- c(1:10)
all_seasons <- lapply(seasons, getSeasonInfo)
all_seasons <- do.call(rbind, all_seasons)

# Make a boxplot of imdb rating for each series.
library(ggplot2)
make_num <- function(x) {
  as.numeric(as.character(x))
}
by_season <- ggplot(all_seasons, aes(x = factor(season), y = make_num(imdb_rating))) +
  geom_boxplot()
by_season

# And another for imdb rating for mythology/non-mythology rating.
by_mythos <- ggplot(all_seasons, aes(x = factor(mythology), y = make_num(imdb_rating))) +
  geom_boxplot()
by_mythos

# viewership
view_season <- ggplot(all_seasons, aes(x = factor(season), y = make_num(viewers))) +
  geom_boxplot()
view_season

view_mythos <- ggplot(all_seasons, aes(x = factor(mythology), y = make_num(viewers))) +
  geom_boxplot()
view_mythos

rating <- ggplot(all_seasons, aes(x = c(1:nrow(all_seasons)), y = make_num(imdb_rating))) +
  geom_point()
rating

by_season <- ggplot(all_seasons, aes(x = factor(season), y = make_num(imdb_rating), col=factor(mythology))) +
  geom_boxplot()
by_season
