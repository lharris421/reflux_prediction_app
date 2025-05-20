odds_to_res_prob <- function(x, y) {
  x / (x + y)
}

# 6 to 1: 0.86 -> 1
# 2 to 1: 0.67 -> 1
# 1 to 1: 0.5 -> 0
# 1 to 3: 0.25 -> 0

