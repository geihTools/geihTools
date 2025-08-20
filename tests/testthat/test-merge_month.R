test_that("merge_month carga y combina módulos del bundle", {
  # Usamos el bundle de ejemplo incluido en el paquete
  dt <- merge_month("enero", year = 2024, data_dir = geih_data_base(), verbose = FALSE)

  # Debe ser un data.table
  expect_s3_class(dt, "data.table")

  # Debe contener las claves principales
  expect_true(all(c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "HOGAR") %in% names(dt)))

  # Debe tener al menos una fila
  expect_gt(nrow(dt), 0)

  # Debe incluir variables de los distintos módulos (ejemplo: P6040 edad)
  expect_true("P6040" %in% names(dt))
})

test_that("merge_month falla con mes inválido", {
  expect_error(merge_month("invalido", year = 2024, data_dir = geih_data_base(), verbose = FALSE),
               regexp = "Mes no válido")
})

test_that("merge_month falla si no hay CSV en la carpeta", {
  tmp <- tempdir()
  dir.create(file.path(tmp, "enero"))
  expect_error(merge_month("enero", year = 2024, data_dir = tmp, verbose = FALSE),
               regexp = "Sin CSV")
})
