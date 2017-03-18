# Harmony search optimisation

# Assumes uniform distribution of parameters for now

# Keen to add constraints on outputs of f too (e.g. optimise on f1, constrain on f2)

# TO DO:
# - convergence tests
# - possibly refactor generation of new vector to avoid so many for and if statements
# - documentation/commenting
# - optimise to meet constraints as higher priority, at the moment when constraints
#   are consistently violated only attempts to remove violation by reducing primary
#   objective
# - option for printing progressive updates to console
# - load HM from previous optimisation as starting point to effectively 'cascade'
#   optimisations where hasn't yet converged


HarmonySearch <- function(f, ..., x.type, x.lower, x.upper, f.lower, f.upper, fw.d,
                          hms = 30, hmcr = 0.9, par = 0.3,
                          itn.max = 100, minimize = TRUE, hm.init = NULL) {

  # Determined consts
  n.x <- length(x.type)
  n.f <- length(f.lower)

  if (is.null(hm.init)) {

    # Generate random vectors of decision variables to create harmonic memory (HM)
    print("Creating harmonic memory ...")
    x.hm <- matrix(rep(0, hms * n.x), nrow = hms, ncol = n.x)
    for (j in 1:n.x) {
      if (x.type[j] == "continuous") {
        x.hm[, j] <- runif(hms, min = x.lower[j], max = x.upper[j])
      } else if (x.type[j] == "discrete") {
        x.hm[, j] <- sample(seq(from = x.lower[j], to = x.upper[j], by = fw.d[j]), hms, replace = TRUE)
      } else {
        print("ERROR in generation of x for HM")
        return
      }
    }
    #print(x.hm)

    # Calculate f = f(x) for each vector in HM
    # First column is always the variable to be optimised, other cols can be constrained
    f.res <- apply(x.hm, 1, f, ...)
    f.hm <- matrix(unlist(f.res), nrow = hms, ncol = n.f, byrow = TRUE)
    #print(f.hm)

    print("... Done")

  } else {
    print("Using provided harmonic memory")
    #print(n.x)
    #print(ncol(hm.init))
    x.hm <- hm.init[, 1:n.x]
    f.hm <- hm.init[, (n.x + 1):ncol(hm.init)]
  }

  if (!minimize) {
    f.hm[, 1] <- -f.hm[, 1]
  }

  # Check constraints
  #flag.x.constr.violated <- matrix(rep(0, hms * n.x), nrow = hms, ncol = n.x)
  f.cv <- matrix(rep(0, hms * n.f), nrow = hms, ncol = n.f)
  #for (j in 1:n.x) {
    #flag.x.constr.violated[, j] <- as.integer(x.hm[, j] < x.lower[j] | x.hm[, j] > x.upper[j])
  #}
  #print(flag.x.constr.violated)
  for (j in 1:n.f) {
    idx.cv.lower <- f.hm[, j] < f.lower[j]
    f.cv[idx.cv.lower, j] <- f.lower[j] - f.hm[idx.cv.lower, j]
    idx.cv.upper <- f.hm[, j] > f.upper[j]
    f.cv[idx.cv.upper, j] <- f.hm[idx.cv.upper, j] - f.upper[j]
  }
  #flag.f.constr.violated <- as.integer(f.hm < f.lower | f.hm > f.upper)
  #print(flag.f.constr.violated)
  f.cv.total <- rowSums(f.cv)
  #print(total.constr.violated)
  #print(f.cv)
  #print(f.cv.total)

  # Main iterative loop
  print("Start main iterative loop ...")
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
            x.new[j] <- x.new[j] + sample(c( - fw.d[j], fw.d[j]), 1)
          } else {
            print("ERROR in taking new x comp from HM")
            return
          }
          if (x.new[j] < x.lower[j]) {
            x.new[j] <- x.lower[j]
          } else if (x.new[j] > x.upper[j]) {
            x.new[j] <- x.upper[j]
          }
        }

      } else {
        if (x.type[j] == "continuous") {
          x.new[j] <- runif(1, min = x.lower[j], max = x.upper[j])
        } else if (x.type[j] == "discrete") {
          x.new[j] <- sample(seq(from = x.lower[j], to = x.upper[j], by = fw.d[j]), 1)
        } else {
          print("ERROR in random new x comp")
          return
        }
      }
    }
    #print(x.new)

    # Calculate f = f(x) for new vector
    f.new <- unlist(f(x.new, ...))
    if (!minimize) {
      f.new[1] <- -f.new[1]
    }
    #print(f.new)

    # Check constraints
    #flag.x.new.constr.violated <- rep(0, n.x)
    f.cv.new <- rep(0, n.f)
    #for (j in 1:n.x) {
      #flag.x.new.constr.violated[j] <- as.integer(x.new[j] < x.lower[j] | x.new[j] > x.upper[j])
    #}
    idx.cv.lower <- f.new < f.lower
    f.cv.new[idx.cv.lower] <- f.lower[idx.cv.lower] - f.new[idx.cv.lower]
    idx.cv.upper <- f.new > f.upper
    f.cv.new[idx.cv.upper] <- f.new[idx.cv.upper] - f.upper[idx.cv.upper]

    #for (j in 1:n.f) {
      #flag.f.new.constr.violated[j] <- as.integer(f.new[j] < f.lower[j] | f.new[j] > f.upper[j])
    #}
    #flag.f.new.constr.violated <- as.integer(f.new < f.lower | f.new > f.upper)
    f.cv.new.total <- sum(f.cv.new)
    #print(flag.x.new.constr.violated)
    #print(flag.f.new.constr.violated)
    #print(total.new.constr.violated)
    #print(f.cv.new)
    #print(f.cv.new.total)

    # If f for new vector is better than worst f in HM, replace
    idx.worst <- order(f.cv.total, f.hm[, 1], decreasing = TRUE)[1]
    f.cv.total.worst <- f.cv.total[idx.worst]
    f.worst <- f.hm[idx.worst, 1]
    #print(cbind(x.hm, f.hm, f.cv.total))
    #print(idx.worst)
    #print(f.worst)
    #print(f.cv.total.worst)
    #f.worst <- max(f.hm)
    #idx.worst <- which.max(f.hm)
    if ((f.cv.new.total < f.cv.total.worst) |
        (f.cv.new.total == f.cv.total.worst) & f.new[1] < f.worst) {
      print(paste0("Replacing | HM worst: cv = ", as.character(f.cv.total.worst),
                   ", f1 = ", as.character(f.worst), " | New: cv = ",
                   as.character(f.cv.new.total), ", f1 = ", as.character(f.new[1])))
      x.hm[idx.worst,] <- x.new
      f.hm[idx.worst, ] <- f.new
      #flag.x.constr.violated[idx.worst, ] <- flag.x.new.constr.violated
      f.cv[idx.worst, ] <- f.cv.new
      f.cv.total <- rowSums(f.cv)
    }

  }
  print("... Main iterative loop done")

  # Find the best f, its x vector, and constraint flags, i.e. the result of the optimisation
  idx.best <- order(f.cv.total, f.hm[, 1])[1]
  f1.best <- f.hm[idx.best, 1]
  #f1.best <- min(f.hm[, 1])
  #idx.best <- which.min(f.hm[, 1])
  if (!minimize) {
    f.hm[, 1] <- -f.hm[, 1]
    f1.best <- -f1.best
  }
  x.best <- x.hm[idx.best, ]
  f.best <- f.hm[idx.best, ]
  hm <- cbind(x.hm, f.hm)
  cv <- cbind(f.cv, f.cv.total)

  # Return best f, its x vector, and all of harmonic memory
  print("Harmony search complete")
  harm.search.result <- list(f1.best = f1.best, x.best = x.best, f.best = f.best, idx.best = idx.best, hm = hm, cv = cv)
  harm.search.result
}

