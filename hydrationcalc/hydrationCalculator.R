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

  res <- matrix(ncol = 2, nrow = 6)
  res[, 1] <- c("Total mass (g)", "Total flour (g)", "Total water (g)",
    "Levain hydration (%)", "Levain flour (g)", "Levain water (g)")
  res[1, 2] <- round(total_flour + total_water, 2)
  res[2, 2] <- round(total_flour, 2)
  res[3, 2] <- round(total_water, 2)
  res[4, 2] <- round(levain_hydration, 2)
  res[5, 2] <- round(levain_flour, 2)
  res[6, 2] <- round(levain_water, 2)
  colnames(res) <- c("", "")
  return(res)
}