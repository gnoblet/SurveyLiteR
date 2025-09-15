# #' @title Student survey tests, wrapper around survey::svyttest()
# #'
# #' @typed design: srvyr::design
# #'   A srvyr::design object.
# #' @typed group: character[1+] | NULL
# #'  Variables to group by. Default to NULL for no group.
# #' @typed na_rm: logical[1]
# #'   Whetehr to remove NAs from `num` and `denom`. Default to TRUE.
# #' @typed var1: character[1]
# #'   Var name 1.
# #' @typed var2: character[1] | NULL
# #'   Var name 2 for two-sample case. Default to NULL.
# #' @typed statistic: character[1]
# #'   See `stats::t.test()` for methods.
# #'
# #' @family test functions
# #'
# #' @inheritParams survey::svyttest
# #'
# #' @typedreturn
# #'   A survey-summarized-interaction tibble
# #'
# #' @export
# svy_test_student <- function(
#   design,
#   var1,
#   var2 = NULL,
#   group = NULL,
#   na_rm = TRUE,
#   statistic = "F",
#   ...
# ) {
#   # Get column names
#   var1_name <- rlang::as_name(rlang::enquo(var1))
#   if (is.null(var2)) {
#     var2_name <- NA_character_
#   } else {
#     var2_name <- rlang::as_name(rlang::enquo(var1))
#   }

#   if (na_rm) {
#     design <- srvyr::drop_na(design, {{ var1 }})
#     if (!is.null(var2)) design <- srvyr::drop_na(design, {{ var2 }})
#   }

#   if (is.na(var2_name)) {
#     formula <- stats::as.formula(paste(var1_name, "~", 0))
#   } else {
#     formula <- stats::as.formula(paste(var1_name, "~", var2_name))
#   }

#   to_return <- survey::svyttest(
#     formula,
#     design,
#     statistic = statistic,
#     na.rm = FALSE,
#     ...
#   )

#   # Outputs of every statistic is not the same
#   boolean_statistic_df <- statistic %in% c("Chisq", "lincom", "saddlepoint")
#   boolean_statistic_value <- statistic %in% c("lincom", "saddlepoint")

#   # Construct the output
#   to_return <- data.frame(
#     statistic = statistic[1],
#     value = if (boolean_statistic_value) NA_real_ else to_return[["statistic"]],
#     p_value = to_return[["p.value"]],
#     ndf = if (boolean_statistic_df) {
#       NA_real_
#     } else {
#       to_return[["parameter"]][["ndf"]]
#     },
#     ddf = if (boolean_statistic_df) {
#       NA_real_
#     } else {
#       to_return[["parameter"]][["ddf"]]
#     },
#     df = if (!(statistic %in% "Chisq")) NA_real_ else to_return[["parameter"]],
#     method = to_return[["method"]]
#   )

#   return(to_return)
# }
