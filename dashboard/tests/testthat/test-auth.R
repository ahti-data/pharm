test_that("user_has_app_access grants all users when apps is all", {
  expect_true(user_has_app_access("all", "my_dashboard"))
})

test_that("user_has_app_access grants access for listed app names", {
  expect_true(user_has_app_access("app_a,app_b", "app_a"))
  expect_true(user_has_app_access("app_a,app_b", "app_b"))
})

test_that("user_has_app_access denies access for unlisted app names", {
  expect_false(user_has_app_access("app_a,app_b", "app_c"))
})

test_that("user_has_app_access denies access for empty or null apps", {
  expect_false(user_has_app_access("", "my_dashboard"))
  expect_false(user_has_app_access(NULL, "my_dashboard"))
})
