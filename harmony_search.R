# Harmony search optimisation

# Assumes uniform distribution of parameters for now

# Keen to add constraints on outputs of f too (e.g. optimise on f1, constrain on f2)


HarmonySearch <- function(f, x.lower, x.upper, ..., hms = 30, hmcr = 0.9, par = 0.3, fw.frac = 0.001, d = 1, itn.max = 100) {

  # Determined consts
  n.x <- length(x.lower)

  # Generate random vectors of decision variables to create harmonic memory (HM)
  u <- matrix(runif(hms * n.x), nrow = hms, ncol = n.x)
  x.hm <- u * matrix(rep(x.upper - x.lower, hms), nrow = hms, ncol = n.x, byrow = TRUE) + matrix(rep(x.lower, hms), nrow = hms, ncol = n.x, byrow = TRUE)

  # Calculate f = f(x) for each vector in HM
  #f.hm <- ????

  # Main iterative loop
  for (i in 1:itn.max) {

    # Generate new vector


    # Perform additonal work for components that came from HM


    # If new vector is better than the worst vector in HM then replace




  }



}

