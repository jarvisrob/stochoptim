# Harmony search optimisation

# Assumes uniform distribution of parameters for now

# Keen to add constraints on outputs of f too (e.g. optimise on f1, constrain on f2)

# TO DO:
# - discrete and continuous decision variables
# - constraints on x and f values
# - convergence tests
# - possibly refactor generation of new vector to avoid so many for and if statements
# - documentation/commenting


HarmonySearch <- function(f, ..., x.type, x.lower, x.upper, fw.d,
                          hms = 30, hmcr = 0.9, par = 0.3, 
                          itn.max = 100, minimize = TRUE) {

  # Determined consts
  n.x <- length(x.type)
  #n.f <- length(f.lower)

  # Generate random vectors of decision variables to create harmonic memory (HM)
  x.hm <- matrix(rep(0, hms * n.x), nrow = hms, ncol = n.x)
  for (j in 1:n.x) {
    if (x.type[j] == "continuous") {
      x.hm[, j] <- runif(hms, min = x.lower[j], max = x.upper[j])
    } else if (x.type[j] == "discrete") {
      x.hm[, j] <- sample(seq(from = x.lower[j], to = x.upper[j], by = fw.d[j]), hms, replace = TRUE)
    } else {
      print("ERROR")
      return
    }
  }
  #print(x.hm)

  # Calculate f = f(x) for each vector in HM
  f.hm <- apply(x.hm, 1, f, ...)
  if (!minimize) {
    f.hm <- -f.hm
  }
  #print(f.hm)

  # Check constraints
  

  # Main iterative loop
  for (i in 1:itn.max) {

    # Generate new vector
    # Start by constructing as random combination of components from vectors in HM
    # Overwrite with random values where component is not to be taken from HM
    comp.from.hm.idx <- sample.int(hms, n.x, replace = TRUE)
    flag.comp.from.hm <- runif(n.x) < hmcr
    x.new <- rep(0, n.x)
    for (j in 1:n.x) {

      if (flag.comp.from.hm[j]) {
        x.new[j] <- x.hm[comp.from.hm.idx[j], j]
        if (runif(1) < par) {
          if (x.type[j] == "continuous") {
            x.new[j] <- x.new[j] + runif(1, min = -fw.d[j], max = fw.d[j])
          } else if (x.type[j] == "discrete") {
            x.new[j] <- x.new[j] + sample(c(-fw.d[j], fw.d[j]), 1)
          } else {
            print("ERROR")
            return
          }
        }

      } else {
        if (x.type[j] == "continuous") {
          x.new[j] <- runif(1, min = x.lower[j], max = x.upper[j])
        } else if (x.type[j] == "discrete") {
          x.new[j] <- sample(seq(from = x.lower[j], to = x.upper[j], by = fw.d[j]), 1)
        } else {
          print("ERROR")
          return
        }
      }
    }

    # Calculate f = f(x) for new vector
    f.new <- f(x.new, ...)
    if (!minimize) {
      f.new <- -f.new
    }
    
    # If f for new vector is better than worst f in HM, replace
    f.worst <- max(f.hm)
    idx.worst <- which.max(f.hm)
    if (f.new < f.worst) {
      x.hm[idx.worst, ] <- x.new
      f.hm[idx.worst] <- f.new
    }

  }

  # Find the best f and its x vector, i.e. the result of the optimisation
  f.best <- min(f.hm)
  idx.best <- which.min(f.hm)
  x.best <- x.hm[idx.best, ]
  if (!minimize) {
    f.hm <- -f.hm
    f.best <- -f.best
  }
  hm <- cbind(x.hm, f.hm)

  # Return best f, its x vector, and all of harmonic memory
  harm.search.result <- list(f.best = f.best, x.best = x.best, hm = hm)
  harm.search.result
}

