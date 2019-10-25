age = scan("data_raw/egypt_livesdata.txt", skip = 5)
sex = c(rep("male", 82), rep("female", 59))
egypt = tibble::tibble(age = age,
                       sex = sex)

usethis::use_data(egypt)