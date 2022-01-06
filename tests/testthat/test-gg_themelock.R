test_that("Tests for gg_themelock", {

  red <- ggplot2::theme(text = element_text(color = "red"))
  blue <- ggplot2::theme(text = element_text(color = "blue"))
  ggplt <- ggplot2::ggplot()
  ggthm <- ggplot2::theme()

  # Classes
  expect_true(inherits(gg_themelock(), "ggthemelock"))
  expect_true(inherits(gg_themelock(ggplt), "ggthemelock") && inherits(gg_themelock(ggplt), "ggplot"))
  expect_true(inherits(gg_themelock(ggthm), "ggthemelock") && inherits(gg_themelock(ggthm), "theme"))
  expect_false(inherits(ggplt + red, "ggthemelock"))
  expect_true(inherits(ggplt + red + gg_themelock(), "ggthemelock"))

  # Extract theme
  expect_identical(extract_theme(red), red)
  expect_identical(extract_theme(ggplt + red), red)
  expect_identical(extract_theme(ggplt + red + gg_themelock()), red)

  # Functionality
  expect_identical(extract_theme(ggplt + red + gg_themelock()), red)
  expect_identical(extract_theme(ggplt + red + gg_themelock() + blue), red)
  expect_identical(extract_theme(ggplt + red + gg_themelock() + blue + gg_themelock()), red)

  # Semi-associative property
  expect_identical(extract_theme(ggplt + red + gg_themelock() + blue), extract_theme(ggplt + red + (gg_themelock() + blue)))
  expect_identical(extract_theme(ggplt + red + gg_themelock() + blue), extract_theme((ggplt + red) + gg_themelock() + blue))
})
