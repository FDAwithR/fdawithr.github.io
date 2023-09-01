fosr_gam_res_to_tfd = function(model, df, arg = seq(1/60, 24, by = 1/60)) {

  pred_obj = predict(model, df, type = "terms", se.fit = TRUE)

  coef_df =
    tibble(
      term = c("(Intercept)", "genderFemale", "age"),
      coef = tfd(t(pred_obj$fit[,1:3]), arg = arg),
      se = tfd(t(pred_obj$se[,1:3]), arg = arg)) %>%
    mutate(coef = coef + c(coef(model)[1], 0, 0))

  coef_df

  }
