context("reg")

test_that("reg for lm, glm and coxph model", {

  # basic models
  reg(data = diabetes, x = c(1:4, 6), y ="diabetes" , factor = c(1, 3, 4), model = 'lm')
  reg(data = diabetes, x = c(1:4, 6), y ="diabetes" , factor = c(1, 3, 4), model = 'glm')
  reg(data = diabetes, x = c(3:4, 6), y ="diabetes",time=2,factor = c(1, 3, 4), model = 'coxph')

  #cov_show
  reg(data = diabetes, x = c(1:4, 6), y ="diabetes" , factor = c(1, 3, 4), model = 'lm',cov_show = TRUE)
  reg(data = diabetes, x = c(1:4, 6), y ="diabetes" , factor = c(1, 3, 4), model = 'glm',cov_show = TRUE)
  reg(data = diabetes, x = c(3:4, 6), y ="diabetes",time=2,factor = c(1, 3, 4), model = 'coxph',cov_show = TRUE)

  #detail_show
  reg(data = diabetes, x = c(1:4, 6), y ="diabetes" , factor = c(1, 3, 4), model = 'lm',detail_show = TRUE)
  reg(data = diabetes, x = c(1:4, 6), y ="diabetes" , factor = c(1, 3, 4), model = 'glm',detail_show = TRUE)
  reg(data = diabetes, x = c(3:4, 6), y ="diabetes",time=2,factor = c(1, 3, 4), model = 'coxph',detail_show = TRUE)

  # y is also in x
  expect_warning(reg(data = diabetes, x = c(1:4, 5), y ="diabetes" , factor = c(1, 3, 4), model = 'lm'))

  })


test_that("reg_y for lm, glm and coxph model", {

  # basic models
  reg_y(data = diabetes, x = c(1:4, 6), y =c("diabetes","C2rs9332739","CFBrs641153") , factor = c(1, 3, 4), model = 'lm')
  reg_y(data = diabetes, x = c(1:4, 6), y =c("diabetes","C2rs9332739","CFBrs641153") , factor = c(1, 3, 4), model = 'glm')
  reg_y(data = diabetes, x = c(3:4, 6), y =c("diabetes","C2rs9332739","CFBrs641153"),time=2,factor = c(1, 3, 4), model = 'coxph')

  #cov_show
  reg_y(data = diabetes, x = c(1:4, 6), y =c("diabetes","C2rs9332739","CFBrs641153") , factor = c(1, 3, 4), model = 'lm',cov_show = TRUE)
  reg_y(data = diabetes, x = c(1:4, 6), y =c("diabetes","C2rs9332739","CFBrs641153") , factor = c(1, 3, 4), model = 'glm',cov_show = TRUE)
  reg_y(data = diabetes, x = c(3:4, 6), y =c("diabetes","C2rs9332739","CFBrs641153"),time=2,factor = c(1, 3, 4), model = 'coxph',cov_show = TRUE)

  #detail_show
  reg_y(data = diabetes, x = c(1:4, 6), y =c("diabetes","C2rs9332739","CFBrs641153") , factor = c(1, 3, 4), model = 'lm',detail_show = TRUE)
  reg_y(data = diabetes, x = c(1:4, 6), y =c("diabetes","C2rs9332739","CFBrs641153") , factor = c(1, 3, 4), model = 'glm',detail_show = TRUE)
  reg_y(data = diabetes, x = c(3:4, 6), y =c("diabetes","C2rs9332739","CFBrs641153"),time=2,factor = c(1, 3, 4), model = 'coxph',detail_show = TRUE)

  # y is also in x
  expect_warning(reg_y(data = diabetes, x = c(1:4, 5), y =c("diabetes","C2rs9332739","CFBrs641153") , factor = c(1, 3, 4), model = 'lm'))

})
