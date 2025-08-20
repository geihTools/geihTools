test_that("indicadores_basicos funciona con bundle geih_2024", {
  # Cargar datos de ejemplo (usando la lógica del paquete)
  dt <- merge_month("enero", year = 2024)

  # Ejecutar función
  res <- indicadores_basicos(dt, temporalidad = "mensual", division = "nacional", graficar = FALSE)

  # Validaciones básicas
  expect_true("data" %in% names(res))
  expect_s3_class(res$data, "data.table")

  # Columnas esperadas
  expected_cols <- c("ANO","MES","Poblacion","PET","PEA","Ocupados","Desocupados",
                     "PFFT","Informales","Nini","Jovenes",
                     "tasa_participacion","tasa_ocupacion","tasa_desempleo",
                     "tasa_informalidad","tasa_ninis","PERIODO_G")

  expect_true(all(expected_cols %in% names(res$data)))

  # Indicadores deben estar entre 0 y 100 (donde no hay NA)
  for (col in c("tasa_participacion","tasa_ocupacion","tasa_desempleo",
                "tasa_informalidad","tasa_ninis")) {
    vals <- res$data[[col]]
    expect_true(all(is.na(vals) | (vals >= 0 & vals <= 100)))
  }
})
