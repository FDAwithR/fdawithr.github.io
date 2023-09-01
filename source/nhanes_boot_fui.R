nhanes_boot_fui = function(seed, df) {

  set.seed(seed)

  bs_df =
    df %>%
    sample_frac(size = 1, replace = TRUE) %>%
    select(SEQN, age, gender, MIMS_tf)

  X_des =
    model.matrix(
      SEQN ~ gender + age,
      data = bs_df
    )

  Hmat = solve(t(X_des) %*% X_des) %*% t(X_des)

  min_regressions =
    bs_df %>%
    tf_unnest(MIMS_tf) %>%
    rename(epoch = MIMS_tf_arg, MIMS = MIMS_tf_value) %>%
    select(epoch, MIMS) %>%
    nest(data = -epoch) %>%
    mutate(
      data = map(data, as.matrix),
      coef = map(.x = data, ~ Hmat %*% .x)) %>%
    select(epoch, coef) %>%
    unnest(coef)

  min_regressions$term = rep(c("(Intercept)", "genderFemale", "age"), 1440)

  smooth_coefs =
    min_regressions %>%
    select(epoch, term, coef) %>%
    tf_nest(coef, .id = term, .arg = epoch) %>%
    mutate(smooth_coef = tf_smooth(coef)) %>%
    select(term, smooth_coef)

}
