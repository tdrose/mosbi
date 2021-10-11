test_that("Pest overlap probability", code = {
    expect_true(p_overlap(10, 30, 20, 50) < 1)
    }
)
