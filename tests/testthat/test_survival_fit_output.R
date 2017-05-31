context("testing survival fit output")

test_that("pad_matrix works",
          {
            orig_matrix <- matrix(1:6, nrow = 3, nc = 2)
            res_matrix <- cbind(orig_matrix, 
                                matrix(NA, nrow = 3, nc = 2))
            dimnames(res_matrix) <- list(NULL, NULL)
            expect_identical(pad_matrix(orig_matrix, 4),
                             res_matrix)
            dimnames(orig_matrix) <- dimnames(res_matrix) <- 
              list(letters[1:3], NULL)
            expect_identical(pad_matrix(orig_matrix, 4),
                             res_matrix)
          }
          )

test_that("get_component works",
          {
            expect_identical(
              get_component(list(a = 1, b = 2), "a"),
              1
            )
            expect_identical(
              get_component(list(a = 1, b = 2), "g"),
              NULL
            )
            expect_identical(
              get_component(list(a = 1, dist = list(b = 2, d = 3)),
                            "d"),
              3
            )
            expect_identical(
              get_component(list(a = 1, dist = list(b = 2, d = 3)),
                            "m"),
              NULL
            )
          })

test_that("extract_fits works",
          {
            x <- 1:5
            expect_error(extract_fits(x),
                         "unrecognized input"
                         )
            fake_flexsurvreg <- 1:5
            class(fake_flexsurvreg) <- "flexsurvreg"
            expect_identical(
              extract_fits(fake_flexsurvreg),
              fake_flexsurvreg
              )
            expect_identical(
              extract_fits(list(a = 1, dist = fake_flexsurvreg)),
              fake_flexsurvreg
              )
              
          })

test_that("fit_plot_tibble catches errors in input",
          {
            fake_fit_tib <- read_file(system.file("tabular/surv",
                                                  "fake_fit_tib.csv", 
                                                  package = "heemod"))
            expect_error(
              plot_fit_tibble(fake_fit_tib,
                              treatment = "wrong_treatment",
                              set_name = "all", type = "PFS"),
              "treatment wrong_treatment not present in entered fit_tibble",
              fixed = TRUE
            )
            expect_error(
              plot_fit_tibble(fake_fit_tib,
                              treatment = "A",
                              set_name = "not_a_set", type = "PFS"),
              "set_name not_a_set not present in entered fit tibble",
              fixed = TRUE
            )
            expect_error(
              plot_fit_tibble(fake_fit_tib,
                              treatment = "A",
                              set_name = "all", type = "PZZ"),
              "type PZZ not present in entered fit_tibble",
              fixed = TRUE
            )
            expect_error(
              plot_fit_tibble(fake_fit_tib[, -c(2,3)],
                              treatment = "A",
                              set_name = "all", type = "PFS"),
              "missing required columns: treatment, set_name",
              fixed = TRUE
            )
            expect_error(plot_fit_tibble(fake_fit_tib,
                                         treatment = c("A", "B"),
                                         set_name = "all", type = "PFS"),
                         "single treatment, set_name, and type",
                         fixed = TRUE
            )
            expect_error(plot_fit_tibble(fake_fit_tib,
                                         treatment = "A",
                                         set_name = "all", 
                                         type = c("PFS", "OS")),
                         "single treatment, set_name, and type",
                         fixed = TRUE
            )
            expect_error(plot_fit_tibble(fake_fit_tib,
                                         treatment = "A",
                                         set_name = "all", 
                                         set_for_km = "wrong",
                                         type = "PFS"),
                         "no Kaplan-Meier curve data using given set_for_km: 'wrong'",
                         fixed = TRUE
            )
          }
          )