#' Calcular indicadores básicos del mercado laboral (compatibles con reportes DANE)
#'
#' Esta función calcula y consolida los principales indicadores del mercado laboral
#' a partir de microdatos de la GEIH, de forma coherente y comparable con los
#' reportes oficiales del DANE.
#'
#' Los resultados incluyen tanto las poblaciones de referencia como los indicadores
#' calculados:
#'
#' **Columnas devueltas en `data`:**
#' \itemize{
#'   \item `ANO`: Año de referencia.
#'   \item `MES`: Mes de referencia (si se eligió temporalidad mensual).
#'   \item `Poblacion`: Población total proyectada por el factor de expansión.
#'   \item `PET`: Población en Edad de Trabajar (15 años o más).
#'   \item `PEA`: Población Económicamente Activa.
#'   \item `Ocupados`: Personas ocupadas.
#'   \item `Desocupados`: Personas desocupadas.
#'   \item `PFFT`: Fuerza de Trabajo Potencial.
#'   \item `Informales`: Ocupados en condición de informalidad.
#'   \item `Nini`: Jóvenes que no estudian ni trabajan.
#'   \item `Jovenes`: Total de jóvenes en la población de referencia.
#'   \item `Tasa_participacion`: (PEA / PET) * 100.
#'   \item `Tasa_ocupacion`: (Ocupados / PET) * 100.
#'   \item `Tasa_desempleo`: (Desocupados / PEA) * 100.
#'   \item `Tasa_informalidad`: (Informales / Ocupados) * 100.
#'   \item `Tasa_ninis`: (Nini / Jovenes) * 100.
#'   \item `PERIODO_G`: Variable de periodo estandarizada para visualización
#'         (ejemplo: `"2023"`, `"2023-T2"`, `"2023-05"`).
#' }
#'
#' @param data `data.table` con los microdatos de la GEIH previamente estandarizados.
#' @param temporalidad Nivel de agregación temporal: `"anual"` (default), `"trimestral"` o `"mensual"`.
#' @param division División geográfica: `"nacional"` (default), `"departamento"` o `"area"`.
#' @param desagregar_clase Lógico. Si `TRUE`, agrega la variable `CLASE` (urbano/rural) a la desagregación.
#' @param graficar Lógico. Si `TRUE`, la salida incluye también un gráfico comparativo (`ggplot`).
#'
#' @return Una lista con:
#' \itemize{
#'   \item `data`: `data.table` con todas las columnas descritas arriba.
#'   \item `plot`: gráfico comparativo (`ggplot2`) de los indicadores principales
#'         (si `graficar = TRUE`).
#' }
#'
#' @examples
#' # Calcular indicadores anuales a nivel nacional
#' resultados <- indicadores_basicos(geih_data, temporalidad = "anual", division = "nacional")
#'
#' # Acceder a la tabla consolidada
#' resultados$data
#'
#' # Generar y visualizar el gráfico
#' resultados <- indicadores_basicos(geih_data, temporalidad = "anual",
#'                                   division = "nacional", graficar = TRUE)
#' resultados$plot   # o print(resultados$plot)
#'
#' @export
indicadores_basicos <- function(data,
                                temporalidad = "anual",
                                division = "nacional",
                                desagregar_clase = FALSE,
                                graficar =FALSE ) {
  library(data.table)
  library(ggplot2)

  # 1. Validaciones iniciales
  if (!temporalidad %in% c("anual", "trimestral", "mensual"))
    stop("Temporalidad debe ser 'anual', 'trimestral' o 'mensual'")
  if (!division %in% c("nacional", "departamento", "area"))
    stop("División debe ser 'nacional', 'departamento' o 'area'")

  # 2. Copia y mapeo de variables
  dt <- copy(data)
  dt <- map_all_variables(dt)   # <- 🔑 estandarización previa

  # 3. Definir agrupación temporal
  if (temporalidad == "anual") {
    dt[, ANO := substr(PERIODO, 1, 4)]
    by_vars <- c("ANO")
  } else if (temporalidad == "trimestral") {
    dt[, ANO := substr(PER, 1, 4)]
    dt[, TRIMESTRE := ceiling(as.numeric(MES) / 3)]
    by_vars <- c("ANO", "TRIMESTRE")
  } else {
    dt[, ANO := substr(PER, 1, 4)]
    by_vars <- c("ANO", "MES")
  }

  # 4. División geográfica
  if (division == "departamento") {
    by_vars <- c(by_vars, "DPTO")
  } else if (division == "area") {
    by_vars <- c(by_vars, "AREA")
  }

  # 5. División urbana/rural
  if (desagregar_clase && "CLASE" %in% names(dt)) {
    by_vars <- c(by_vars, "CLASE")
  }

  if (length(by_vars) == 0) stop("No se definieron variables de agrupación")

  # 6. Divisor temporal
  n_meses <- get_month_divisor(dt)

  if (temporalidad == "anual") {
    divisor <- n_meses
  } else if (temporalidad == "trimestral") {
    # si hay n_meses, dividirlos en bloques de 3
    divisor <- min(3, n_meses)
  } else { # mensual
    divisor <- 1
  }


  # 7. Cálculos de población y empleo
  poblacion <- dt[, .(poblacion = sum(FEX_C18, na.rm = TRUE) / divisor), by = by_vars]
  pet <- dt[P6040 >= 15, .(PET = sum(FEX_C18) / divisor), by = by_vars]
  pea <- dt[OCI == 1 | DSI == 1, .(PEA = sum(FEX_C18) / divisor), by = by_vars]
  ocupados <- dt[OCI == 1, .(Ocupados = sum(FEX_C18) / divisor), by = by_vars]
  desocupados <- dt[DSI == 1, .(Desocupados = sum(FEX_C18) / divisor), by = by_vars]
  informales <- dt[EI == "Informal" & OCI == 1, .(Informales = sum(FEX_C18) / divisor), by = by_vars]
  nini <- dt[NiNi == 1, .(nini = sum(FEX_C18) / divisor), by = by_vars]
  jovenes <- dt[Joven == 1, .(jovenes = sum(FEX_C18) / divisor), by = by_vars]
  pfft <- dt[FFT == 1, .(PFFT = sum(FEX_C18) / divisor), by = by_vars]

  # 8. Consolidar todo
  indicadores <- Reduce(function(x, y) merge(x, y, by = by_vars, all = TRUE),
                        list(poblacion,pet, pea, ocupados, desocupados, informales, nini, jovenes, pfft))

  # 9. Calcular indicadores
  indicadores[, tasa_participacion := (PEA / PET) * 100]
  indicadores[, tasa_ocupacion := (Ocupados / PET) * 100]
  indicadores[, tasa_desempleo := (Desocupados / PEA) * 100]
  indicadores[, tasa_informalidad := (Informales / Ocupados) * 100]
  indicadores[, tasa_ninis := (nini / jovenes) * 100]

  # 10. Variable periodo estándar
  if (temporalidad == "anual") {
    indicadores[, PERIODO_G := ANO]
  } else if (temporalidad == "trimestral") {
    indicadores[, PERIODO_G := paste0(ANO, "-T", TRIMESTRE)]
  } else {
    indicadores[, PERIODO_G := paste0(ANO, "-", MES)]
  }

  # 11. Retorno condicional con gráfico
  if (graficar) {
    plot_data <- melt(indicadores,
                      id.vars = by_vars,
                      measure.vars = c("tasa_participacion",
                                       "tasa_ocupacion",
                                       "tasa_desempleo",
                                       "tasa_informalidad",
                                       "tasa_ninis"),
                      variable.name = "Indicador",
                      value.name = "Valor")

    plot_data <- preparar_eje_x(plot_data, temporalidad, by_vars)

    n_periodos <- length(unique(plot_data$eje_x))

    if (n_periodos > 1) {
      p <- ggplot(plot_data, aes(x = eje_x, y = Valor, fill = Indicador)) +
        geom_col(position = "dodge") +
        labs(title = "Indicadores Básicos del Mercado Laboral",
             x = "Periodo / División",
             y = "Porcentaje (%)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      p <- ggplot(plot_data, aes(x = Indicador, y = Valor, fill = Indicador)) +
        geom_col() +
        labs(title = "Indicadores Básicos del Mercado Laboral (Periodo Único)",
             x = "Indicador",
             y = "Porcentaje (%)") +
        theme_minimal()
    }

    return(list(data = indicadores, plot = p))
  } else {
    return(list(data = indicadores))
  }
}
