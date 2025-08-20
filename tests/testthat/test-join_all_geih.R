test_that("join_all_months consolida todos los meses del bundle", {
  # Usamos el bundle incluido en el paquete
  dt <- join_all_months(year = 2024, data_dir = geih_data_base(), verbose = FALSE)

  # Debe ser un data.table
  expect_s3_class(dt, "data.table")

  # Debe tener las claves principales
  expect_true(all(c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "HOGAR") %in% names(dt)))

  # Debe incluir más de un mes
  expect_true("MES" %in% names(dt))
  expect_gt(length(unique(dt$MES)), 1)

  # Debe incluir año
  expect_true("ANO" %in% names(dt) | "PERIODO" %in% names(dt))
})

test_that("join_all_months falla si no existen subcarpetas de meses", {
  tmp <- tempdir()
  dir.create(file.path(tmp, "geih_vacia"))
  expect_error(join_all_months(year = 2025, data_dir = file.path(tmp, "geih_vacia")),
               regexp = "No se encontraron subcarpetas")
})

test_that("join_all_geih es un alias de join_all_months", {
  dt1 <- join_all_months(year = 2024, data_dir = geih_data_base(), verbose = FALSE)
  dt2 <- join_all_geih(year = 2024, data_dir = geih_data_base(), verbose = FALSE)

  expect_equal(dt1, dt2)
})
