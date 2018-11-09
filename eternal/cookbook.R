# Script to simulate the usage of Cauldron Cookbook.
# Intersted in:
#   a) Average life looss per card drawn
#   b) Probability of firebomb drawn per card drawn
#   c) Average number of firebombs drawn per card drawn (used to derive a)).

# Assumptions:
#   On the draw.
#   Cookbook played on turn 2.
#   Cookbook activated every turn.
#   Standard 75 card deck size.
#   Card draw comes only from start of turn, and cookbook.

# These assumptions represent the best-case scenario, i.e. the scenario that
# will result in the highest number of firebombs in the deck, and therefore the
# maximum amount of potential damage to the owner of the cookbook. Under
# situations where these assumptions are violated, the cookbook will be safer 
# to use.

# The number of previous activations determines the number of firebombs in the
# deck. SO:
# 1) 

# Take a deck (with size, and an n-firebombs). Put a firebomb into it, and draw
# a card out of it, and return the new deck, along with a probability that 
# THE CARD DRAWN THAT TIME was a firebomb.

# This isn't actually right... what I want to do is to determine, at each step,
# the probability that, OF THE CARDS DRAWN SO FAR, how many are firebombs. 
# I think? The problem is, each draw had a different number of firebombs in the
# deck...

cookbook_activation <- function(deck, turn) {
  if (turn == 0) {
    deck$deck_size <- deck$deck_size - 7
    deck$cards_drawn <- deck$cards_drawn + 7
  } else if (turn < 3) {
    deck$deck_size <- deck$deck_size - 1
    deck$cards_drawn <- deck$cards_drawn + 1
  } else if (turn >= 3) {
    deck$deck_size <- deck$deck_size - 1
    deck$cards_drawn <- deck$cards_drawn + 1
    deck$n_firebomb <- deck$n_firebomb + 1
    deck$cards_drawn <- deck$cards_drawn + 1
    deck$p_firebomb <- deck$n_firebomb / deck$deck_size
    deck$cookbook_activations <- deck$cookbook_activations + 1
  }
  return(deck)
}

deck <- list(deck_size = 75, 
  n_firebomb = 0, 
  p_firebomb = 0, 
  cards_drawn = 0,
  cookbook_activations = 0)
turns <- c(1:25)
games <- vector(mode = "list", length = length(turns))
for (i in seq_along(turns)) {
  deck <- cookbook_activation(deck, i)
  games[[i]] <- deck
}



# Make a matrix with the information...
# deck size, normal draws, cookbook draws, nfirebombs.
game <- matrix(ncol = )


# Process.
#   Start with deck size.
#   Remove cards drawn to this point.
#   Activate cookbook:
#     +1 firebombs.
#     Draw card - calculate p of being firebomb.

# Functions needed.



# Do this fancy, with S4 classes?
# Deck is a class
#   Properties:
#     Starting size.
#     Cards drawn
#     Firebombs