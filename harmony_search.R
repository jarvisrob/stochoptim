# Harmony search optimisation

# Assumes uniform distribution of parameters for now

# Keen to add constraints on outputs of f too (e.g. optimise on f1, constrain on f2)

# TO DO:
# - constraints on x and f values
# - convergence tests
# - possibly refactor generation of new vector to avoid so many for and if statements
# - documentation/commenting


HarmonySearch <- function(f, ..., x.type, x.lower, x.upper, f.lower, f.upper, fw.d,
                          hms = 30, hmcr = 0.9, par = 0.3,
                          itn.max = 100, minimize = TRUE) {

  # Determined consts
  n.x <- length(x.type)
  n.f <- length(f.lower)

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
  flag.x.constr.violated <- matrix(rep(0, hms * n.x), nrow = hms, ncol = n.x)
  flag.f.constr.violated <- matrix(rep(0, hms * n.f), nrow = hms, ncol = n.f)
  for (j in 1:n.x) {
    flag.x.constr.violated[, j] <- as.integer(x.hm[, j] < x.lower[j] | x.hm[, j] > x.upper[j])
  }
  #print(flag.x.constr.violated)
  #for (j in 1:n.f) {
    #flag.f.constr.violated[j] <- as.integer(f.hm[j] < f.lower[j] | f.hm[j] > f.upper[j])
  #}
  flag.f.constr.violated <- as.integer(f.hm < f.lower | f.hm > f.upper)
  #print(flag.f.constr.violated)
  total.constr.violated <- rowSums(cbind(flag.x.constr.violated, flag.f.constr.violated))
  #print(total.constr.violated)

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
            x.new[j] <- x.new[j] + sample(c( - fw.d[j], fw.d[j]), 1)
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
    #print(x.new)

    # Calculate f = f(x) for new vector
    f.new <- f(x.new, ...)
    if (!minimize) {
      f.new <- -f.new
    }
    #print(f.new)

    # Check constraints
    flag.x.new.constr.violated <- rep(0, n.x)
    flag.f.new.constr.violated <- rep(0, n.f)
    for (j in 1:n.x) {
      flag.x.new.constr.violated[j] <- as.integer(x.new[j] < x.lower[j] | x.new[j] > x.upper[j])
    }
    #for (j in 1:n.f) {
      #flag.f.new.constr.violated[j] <- as.integer(f.new[j] < f.lower[j] | f.new[j] > f.upper[j])
    #}
    flag.f.new.constr.violated <- as.integer(f.new < f.lower | f.new > f.upper)
    total.new.constr.violated <- sum(c(flag.x.new.constr.violated, flag.f.new.constr.violated))
    #print(flag.x.new.constr.violated)
    #print(flag.f.new.constr.violated)
    #print(total.new.constr.violated)

    # If f for new vector is better than worst f in HM, replace
    idx.worst <- order(total.constr.violated, f.hm, decreasing = TRUE)[1]
    total.constr.violated.worst <- total.constr.violated[idx.worst]
    f.worst <- f.hm[idx.worst]
    #print(cbind(x.hm, f.hm, total.constr.violated))
    #print(idx.worst)
    #f.worst <- max(f.hm)
    #idx.worst <- which.max(f.hm)
    if ((total.new.constr.violated < total.constr.violated.worst) |
        (total.new.constr.violated == total.constr.violated.worst) & f.new < f.worst) {
      x.hm[idx.worst, ] <- x.new
      f.hm[idx.worst] <- f.new
      flag.x.constr.violated[idx.worst, ] <- flag.x.new.constr.violated
      flag.f.constr.violated[idx.worst] <- flag.f.new.constr.violated
      total.constr.violated <- rowSums(cbind(flag.x.constr.violated, flag.f.constr.violated))
    }

  }

  # Find the best f, its x vector, and constraint flags, i.e. the result of the optimisation
  f.best <- min(f.hm)
  idx.best <- which.min(f.hm)
  x.best <- x.hm[idx.best, ]
  if (!minimize) {
    f.hm <- -f.hm
    f.best <- -f.best
  }
  hm <- cbind(x.hm, f.hm)
  constr.violated <- cbind(flag.x.constr.violated, flag.f.constr.violated, total.constr.violated)

  # Return best f, its x vector, and all of harmonic memory
  harm.search.result <- list(f.best = f.best, x.best = x.best, hm = hm, constr.violated = constr.violated)
  harm.search.result
}

