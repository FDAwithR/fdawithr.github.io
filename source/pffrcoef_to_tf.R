pffrcoef_to_tf = function(coef) {

  coef =
    coef %>%
    mutate(
      id = "ID"
    ) %>%
    tf_nest(value:se, .id = id, .arg = yindex.vec) %>%
    select(coef = value, se)

  coef

}
