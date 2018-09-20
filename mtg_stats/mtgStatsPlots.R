# Download data from Scryfall for mtg cards in order to
# take a look at the distribution of various things in each
# set.

# 

setwd("~/Documents/hfg_notwork/projects/mtg_stats")
library(jsonlite)
cds <- fromJSON("AllSets.json")

# This gives a list, with a set per element (also including
# things like duel decks - I don't actually want these.

# What I want to look at is things like the mean price per
# unit power, per colour, across sets through time (i.e.
# arrange sets chronologically). Also things like the
# mean cost of a spell, the number of creatures per colour,
# histograms of mana cost per colour per set etc.

# I can also look at keywords, like find the mean cost of 
# a card that says counter target spell and not the word
# unless, or cards that say token in the name etc.

# exclude the three unsets, since they a) don't have the same
# design philosophy and b) have some cards like gleemax that
# really fuck up the CMC plots!

exclude <- c("UST", "UNH", "UGL")
cds <- cds[!names(cds) %in% exclude]
set_release <- data.frame(set = names(cds),
                          date = sapply(cds, function(x) x$releaseDate))

cmc <- lapply(cds, function(x) x$cards$cmc)

# make this stuff into just one massive table of the things
# that I might be interested in...
cols <- c("artist", "cmc", "colorIdentity", "colors", "flavor", "layout", "manaCost", "name", "power", "toughness", "rarity", "types", "subtypes", "text", "type")
allcards <- lapply(cds, function(x) {
                       if (x$type == "expansion" | x$type == "core") {
                        set <- x$code
                        tmp <- x$cards
                        tmp <- tmp[ , colnames(tmp) %in% cols]
                        tmp$set <- set
                        return(tmp)
                       }
                  })

allcards <- allcards[!sapply(allcards, is.null)]
allcards <- do.call(rbind, allcards)

# Put this into longfom, taking just the cmc, colour, set, and type.


# mana cost is in the form {3}{W}{W}, which doesn't naturally turn into cmc,
# so I need to make sure I can add in a CMC.
# Problenatically, artifacts cards have NULL, rather than colourless. And I 
# also am not sure what gold cards have for colors...
# Gold cards, for colors, have a vector of the two colours. I want to add a new 
# column, where length(colors) == 1, put the colour, where colors == NULL put
# colorless, and where length(colors) > 1 put "gold!".
allcards$colorclass <- NA
.x <- lapply(allcards$colors, length)
allcards$colorclass[.x == 1] <- allcards$colors[.x == 1]
allcards$colorclass[.x == 0] <- "colorless"
allcards$colorclass[.x > 1] <- "gold"



manaCost2CMC <- function(x) {
        x <- strsplit(x, "[{}]")[[1]]
        x <- x[x != ""]
        generic <- x[!is.na(suppressWarnings(as.numeric(x)))]
        if (length(generic) == 0) generic <- 0
        coloured <- x[is.na(suppressWarnings(as.numeric(x)))]
        return(as.numeric(generic) + length(coloured))
}

# Turns out this was actually a waste of time - cmc IS already there! never mind, 
# was a fun exercise.
#allcards$cmc <- sapply(allcards$manaCost, manaCost2CMC)

# histogram of mean cost per set?
