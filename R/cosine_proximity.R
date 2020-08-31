cosine_proximity <- function(x, focal_index, measure = "angle") {
  
  assertthat::assert_that(measure %in% c("angle", "distance"),
                          length(measure) == 1)
  
  cos_prox = rep(NA, ncol(x))
  for (i in 1:ncol(x)) {
    if (focal_index != i) {
      A = x[, focal_index] %>%
        as.matrix(ncol = 1)
      A_norm = (A * A)  %>% sum() %>% sqrt()
      B = x[, i] %>%
        as.matrix(ncol = 1)
      B_norm = (B * B)  %>% sum() %>% sqrt()
      dot_AB = sum(A * B)
      cos_prox[i] = dot_AB / (A_norm * B_norm)
    }
  }
  
  if (measure=="angle")
    return(cos_prox)
  else
    cos_distance = acos(cos_prox) / pi
    return(cos_distance)
  
}