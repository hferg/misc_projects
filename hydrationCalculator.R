## Calculate the amount of flour and water in a levain.

# There are two parts of a levain. The mass and hydration of the seed, and then
# the mass and hydration of the finished levain. If I know the specific weights
# of flour and water of each part then I can calculate it one way, but often
# I just know the hydration. Then, I want to know, of a given mass of levain,
# how much of it is flour and how much of it is water.

# Notes.
#   100% hydration = 1:1 flour/water
#   90% hydration = 10:9 flour/water
#   80% hydration = 10:8 flour/water
#     and so on.

#' levainHydration
#' This function calculates the mass of flour and the mass of water in a given
#' quantity of levain.
#' @param seed_mass The mass of mature starter that is used to seed the levain
#' @param seed_hydration The hydration of the mature starter used to seed the
#' levain
#' @param starter_mass The mass of the FRESH flour and water being built into the
#' levain (i.e., the final levain weight, ignoring the seed quantity).
#' @param starter_hydration The hydration of the levain as it is built (i.e., the
#' hydration of the NEW flour and water mix)
#' @param levain_used The mass of levain that will be used in the loaf.

levainHydration <- function(seed_mass, seed_hydration, 
  starter_mass, starter_hydration, levain_used) {

  # first calculate the specifc flour and water of the seed
  seed_flour <- seed_mass / (100 + seed_hydration) * 100
  seed_water <- seed_mass / (100 + seed_hydration) * seed_hydration

  levain_flour <- starter_mass / (100 + starter_hydration) * 100
  levain_water <- starter_mass / (100 + starter_hydration) * starter_hydration

  total_flour <- seed_flour + levain_flour
  total_water <- seed_water + levain_water

  levain_hydration <- (total_water / total_flour) * 100
  levain_flour <- levain_used / (100 + levain_hydration) * 100
  levain_water <- levain_used / (100 + levain_hydration) * levain_hydration

  res <- c(
    total_mass = total_flour + total_water,
    total_flour = total_flour,
    total_water = total_water,
    levain_hydration = levain_hydration,
    levain_flour = levain_flour,
    levain_water = levain_water
  )
  return(res)
}