# Drives the app's server through a sequence of simulations and checks that the true trajectory follows each one:
# it must not keep a previous simulation's truth. Uses shiny::testServer, so the real reactive graph is exercised.

load_app_server <- function() {
  app_dir <- testthat::test_path("..", "..", "inst", "app")
  if (!file.exists(file.path(app_dir, "server.R"))) return(NULL)
  env <- new.env(parent = globalenv())
  owd <- setwd(app_dir)
  on.exit(setwd(owd), add = TRUE)
  tryCatch({
    source("global.R", local = env, encoding = "UTF-8")
    v <- source("server.R", local = env, encoding = "UTF-8")$value
    if (is.function(v)) v else get("server", envir = env)
  }, error = function(e) NULL)
}

test_that("the simulation the app starts with matches the settings on screen", {
  skip_if_not_installed("shiny")
  srv <- load_app_server()
  skip_if(is.null(srv), "the app could not be loaded in this environment")
  shiny::testServer(srv, {
    # no settings touched: the start-up simulation and the displayed defaults agree, so nothing is pending
    expect_false(toy_settings_pending())
    expect_equal(toy_cfg()$seed, 42)
  })
})

test_that("each new simulation replaces the true trajectory", {
  skip_if_not_installed("shiny")
  srv <- load_app_server()
  skip_if(is.null(srv), "the app could not be loaded in this environment")
  shiny::testServer(srv, {
    ages <- 1:25
    session$setInputs(data_source = "toy", toy_trait = "mass", toy_form = "Cubic", toy_shape = "default")
    session$setInputs(toy_commit = 1)
    expect_identical(truth()$form, "Cubic")
    cubic <- toy_true_curve(truth(), ages)
    expect_match(truth_label(truth()), "Cubic")

    # changing a setting without applying it leaves the simulation in use - and says so
    session$setInputs(toy_form = "Quadratic")
    expect_true(toy_settings_pending())
    expect_identical(truth()$form, "Cubic")

    # applying it replaces the truth: form, label and the curve itself
    session$setInputs(toy_commit = 2)
    expect_false(toy_settings_pending())
    expect_identical(truth()$form, "Quadratic")
    quad <- toy_true_curve(truth(), ages)
    expect_false(isTRUE(all.equal(cubic, quad)))

    session$setInputs(toy_trait = "count_pois", toy_form = "Linear")
    session$setInputs(toy_commit = 3)
    expect_identical(truth()$form, "Linear")
    expect_match(truth_label(truth()), "log scale")
    lin <- toy_true_curve(truth(), ages)
    expect_false(isTRUE(all.equal(quad, lin)))
    # a linear form on the log scale is a straight line in log(trait)
    expect_equal(stats::var(diff(log(lin))), 0, tolerance = 1e-10)
  })
})

test_that("the evidence summary is visible before anything is saved, and says how to start", {
  skip_if_not_installed("shiny")
  srv <- load_app_server()
  skip_if(is.null(srv), "the app could not be loaded in this environment")
  shiny::testServer(srv, {
    g <- evidence_grade()
    expect_null(g$error)
    expect_identical(g$level, "insufficient")
    html <- as.character(output$evidence_summary$html)
    expect_match(html, "No results saved yet")
    expect_match(html, "Save to summary")
  })
})
