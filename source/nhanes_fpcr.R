nhanes_fpcr = function(df, npc){

  epoch_arg = seq(1/60, 24, length = 1440)

  fpca_obj =
    rfr_fpca("MIMS_tf", data = df, npc = npc)

  B_fpcr = fpca_obj$efunctions * sqrt(60)
  colnames(B_fpcr) = str_c("efunc_", 1:npc)

  num_int_df =
    as_tibble(
      (df$MIMS_mat %*% B_fpcr) * (1/60),
      rownames = "SEQN") %>%
    mutate(SEQN = as.numeric(SEQN))

  fpcr_df =
    left_join(df, num_int_df, by = "SEQN") %>%
    select(BMI, starts_with("efunc_"))

  fit_fpcr_int =
    lm(BMI ~ 1 + ., data = fpcr_df)

  var_basis_coef = vcov(fit_fpcr_int)[-1,-1]
  var_coef_func = B_fpcr %*% var_basis_coef %*% t(B_fpcr)

  inf_df =
    tibble(
      method = str_c("FPCR: ", npc),
      estimate = tfd(t(B_fpcr %*% coef(fit_fpcr_int)[-1]), arg = epoch_arg),
      se = tfd(sqrt(diag(var_coef_func)), arg = epoch_arg)
    ) %>%
    mutate(
      ub = estimate + 1.96 * se,
      lb = estimate - 1.96 * se)

  inf_df
}
