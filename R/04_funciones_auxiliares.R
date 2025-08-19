# ==========================================================
# Funciones Auxiliares para procesamiento de GEIH
# Archivo: R/auxiliares.R
# ==========================================================

library(data.table)

# ----------------------------------------------------------
# 1. Función para calcular el divisor de meses
# ----------------------------------------------------------

#' Obtener divisor por meses en función de la data
#'
#' @param data data.table con columna `MES`
#' @param year Año de referencia (default: 2024)
#' @param months Vector opcional de meses seleccionados
#' @return Número de meses únicos (para dividir expansores)
#' @export
get_month_divisor <- function(data, year = 2024, months = NULL) {
  if (is.null(months) && "MES" %in% names(data)) {
    n_months <- length(unique(data$MES))
  } else if (!is.null(months)) {
    n_months <- length(unique(months))
  } else {
    n_months <- 1
  }
  return(n_months)
}

# ----------------------------------------------------------
# 2. Mapear Departamentos
# ----------------------------------------------------------

#' Reemplazar códigos de departamentos por nombres
#' @param dt data.table con columna `DPTO`
#' @return data.table con DPTO renombrado
#' @export
map_departamentos <- function(dt) {
  dt[, DPTO := fcase(
    DPTO == "5", "Antioquia",
    DPTO == "8", "Atlántico",
    DPTO == "11", "Bogotá",
    DPTO == "13", "Bolívar",
    DPTO == "15", "Boyacá",
    DPTO == "17", "Caldas",
    DPTO == "18", "Caquetá",
    DPTO == "19", "Cauca",
    DPTO == "20", "Cesar",
    DPTO == "23", "Córdoba",
    DPTO == "25", "Cundinamarca",
    DPTO == "27", "Chocó",
    DPTO == "41", "Huila",
    DPTO == "44", "La Guajira",
    DPTO == "47", "Magdalena",
    DPTO == "50", "Meta",
    DPTO == "52", "Nariño",
    DPTO == "54", "Norte de Santander",
    DPTO == "63", "Quindío",
    DPTO == "66", "Risaralda",
    DPTO == "68", "Santander",
    DPTO == "70", "Sucre",
    DPTO == "73", "Tolima",
    DPTO == "76", "Valle del Cauca",
    DPTO == "81", "Arauca",
    DPTO == "85", "Casanare",
    DPTO == "86", "Putumayo",
    DPTO == "88", "San Andrés y Providencia",
    DPTO == "91", "Amazonas",
    DPTO == "94", "Guainía",
    DPTO == "95", "Guaviare",
    DPTO == "97", "Vaupés",
    DPTO == "99", "Vichada",
    default = NA_character_
  )]
  return(dt)
}

# ----------------------------------------------------------
# 3. Mapear Estado Civil
# ----------------------------------------------------------
map_estado_civil <- function(dt) {
  replacement_map_estado_civil <- c(
    "1" = "Pareja < 2 años", "2" = "Pareja > 2 años", "3" = "Casado(a)",
    "4" = "Separado/Divorciado", "5" = "Viudo(a)", "6" = "Soltero(a)"
  )
  dt[, P6070 := replacement_map_estado_civil[as.character(P6070)]]
  return(dt)
}

# ----------------------------------------------------------
# 4. Mapear Educación
# ----------------------------------------------------------
map_educacion <- function(dt) {
  education_map_short <- c(
    "1" = "Ninguno", "2" = "Preescolar", "3" = "Primaria",
    "4" = "Secundaria", "5" = "Media Académica", "6" = "Media Técnica",
    "7" = "Normalista", "8" = "Técnica Prof.", "9" = "Tecnológica",
    "10" = "Universitaria", "11" = "Especialización",
    "12" = "Maestría", "13" = "Doctorado", "99" = "No sabe"
  )
  dt[, P3042 := education_map_short[as.character(P3042)]]
  return(dt)
}

# ----------------------------------------------------------
# 5. Mapear Acceso a Salud
# ----------------------------------------------------------
map_acceso_salud <- function(dt) {
  acceso_health <- c("9" = "No informa", "2" = "No", "1" = "Sí")
  dt[, P6090 := acceso_health[as.character(P6090)]]
  return(dt)
}

# ----------------------------------------------------------
# 6. Mapear Afiliación a Salud
# ----------------------------------------------------------
map_afiliacion_salud <- function(dt) {
  replacement_map_afiliacion <- c(
    "1" = "Contributivo", "2" = "Especial", "3" = "Subsidiado", "9" = "No informa"
  )
  dt[, P6100 := replacement_map_afiliacion[as.character(P6100)]]
  return(dt)
}

# ----------------------------------------------------------
# 7. Mapear Ocupación / Tipo de trabajo
# ----------------------------------------------------------
map_ocupacion <- function(dt) {
  ocupacion_map_short <- c(
    "1" = "Empleado particular", "2" = "Empleado gobierno", "3" = "Empleado doméstico",
    "4" = "Cuenta propia", "5" = "Empleador", "6" = "Familiar sin pago",
    "7" = "Trabajador sin pago", "8" = "Jornalero", "9" = "Otro"
  )
  dt[, P6430 := ocupacion_map_short[as.character(P6430)]]
  return(dt)
}

# ----------------------------------------------------------
# 8. Mapear Tipo de Vivienda
# ----------------------------------------------------------
map_vivienda <- function(dt) {
  propiedad_map_short <- c(
    "1" = "Propia pagada", "2" = "Propia en pago",
    "3" = "Arriendo", "4" = "Usufructo", "5" = "Posesión sin título",
    "6" = "Propiedad colectiva", "7" = "Otra"
  )
  dt[, P5090 := propiedad_map_short[as.character(P5090)]]
  return(dt)
}

# ----------------------------------------------------------
# 9. Mapear Sexo
# ----------------------------------------------------------
map_sexo <- function(dt) {
  replacement_sexo <- c("1"= "Hombres", "2" = "Mujeres")
  dt[, P3271 := replacement_sexo[as.character(P3271)]]
  return(dt)
}




# ----------------------------------------------------------
# 11. Mapear Áreas Metropolitanas / Ciudades
# ----------------------------------------------------------
map_area <- function(dt) {
  dt[, AREA := fcase(
    AREA == "76", "Cali AM",
    AREA == "73", "Ibagué",
    AREA == "70", "Sincelejo",
    AREA == "68", "Bucaramanga AM",
    AREA == "66", "Pereira AM",
    AREA == "63", "Armenia",
    AREA == "54", "Cúcuta AM",
    AREA == "52", "Pasto",
    AREA == "50", "Villavicencio",
    AREA == "47", "Santa Marta",
    AREA == "44", "Riohacha",
    AREA == "41", "Neiva",
    AREA == "27", "Quibdó",
    AREA == "23", "Montería",
    AREA == "20", "Valledupar",
    AREA == "19", "Popayán",
    AREA == "18", "Florencia",
    AREA == "17", "Manizales AM",
    AREA == "15", "Tunja",
    AREA == "13", "Cartagena",
    AREA == "11", "Bogotá DC",
    AREA == "08", "Barranquilla AM",
    AREA == "05", "Medellín AM",
    default = NA_character_
  )]

  return(dt)
}

# ----------------------------------------------------------
# 12. Mapear CLASE (Urbano/Rural)
# ----------------------------------------------------------
map_CLASE <- function(dt) {
  zona_map <- c(
    "1" = "Urbano",
    "2" = "Rural"
  )
  dt[, CLASE := zona_map[as.character(CLASE)]]
  return(dt)
}


# ----------------------------------------------------------
# 10. Infomalidad laboral
# ----------------------------------------------------------

clasificar_formalidad <- function(dt) {
  dt[, ANIOS := PER - 1]
  dt[, OFICIO_C8_2D := as.numeric(substr(OFICIO_C8, 1, 2))]

  dt[, FORMAL := NA_real_]
  dt[P6430 == 3, FORMAL := NA_real_]
  dt[P6430 == 6, FORMAL := 0]
  dt[RAMA2D_R4 %in% c("84", "99"), FORMAL := 1]
  dt[P6430 == 8, FORMAL := 0]
  dt[P6430 == 2, FORMAL := 1]
  dt[P6430 %in% c(1, 7) & P3045S1 == 1, FORMAL := 1]
  dt[P6430 %in% c(1, 7) & P3045S1 %in% c(2, 9) & P3046 == 1, FORMAL := 1]
  dt[P6430 %in% c(1, 7) & P3045S1 %in% c(2, 9) & P3046 == 2, FORMAL := 0]
  dt[P6430 %in% c(1, 7) & P3045S1 %in% c(2, 9) & P3046 == 9 & P3069 >= 4, FORMAL := 1]
  dt[P6430 %in% c(1, 7) & P3045S1 %in% c(2, 9) & P3046 == 9 & P3069 <= 3, FORMAL := 0]

  dt[, sin_negocio := P6765 %in% 1:6 | P6765 == 8]
  dt[P6430 %in% c(4, 5) & sin_negocio & P3065 == 1, FORMAL := 1]
  dt[P6430 %in% c(4, 5) & sin_negocio & P3065 %in% c(2, 9) & P3066 == 1, FORMAL := 1]
  dt[P6430 %in% c(4, 5) & sin_negocio & P3065 %in% c(2, 9) & P3066 == 2, FORMAL := 0]
  dt[P6430 == 5 & sin_negocio & P3065 %in% c(2, 9) & P3066 == 9 & P3069 >= 4, FORMAL := 1]
  dt[P6430 == 5 & sin_negocio & P3065 %in% c(2, 9) & P3066 == 9 & P3069 <= 3, FORMAL := 0]
  dt[P6430 == 4 & sin_negocio & P3065 %in% c(2, 9) & P3066 == 9 & OFICIO_C8_2D <= 20, FORMAL := 1]
  dt[P6430 == 4 & sin_negocio & P3065 %in% c(2, 9) & P3066 == 9 & OFICIO_C8_2D >= 21, FORMAL := 0]

  dt[, con_negocio := P6765 == 7]
  dt[P6430 %in% c(4, 5) & con_negocio & P3067 == 1 & P3067S1 == 1 & P3067S2 >= ANIOS, FORMAL := 1]
  dt[P6430 %in% c(4, 5) & con_negocio & P3067 == 1 & P3067S1 == 1 & P3067S2 < ANIOS, FORMAL := 0]
  dt[P6430 %in% c(4, 5) & con_negocio & P3067 == 1 & P3067S1 == 2 & P6775 == 1, FORMAL := 1]
  dt[P6430 %in% c(4, 5) & con_negocio & P3067 == 1 & P3067S1 == 2 & P6775 == 3 & OFICIO_C8_2D <= 20, FORMAL := 1]
  dt[P6430 %in% c(4, 5) & con_negocio & P3067 == 1 & P3067S1 == 2 & P6775 == 3 & OFICIO_C8_2D >= 21, FORMAL := 0]
  dt[P6430 %in% c(4, 5) & con_negocio & P3067 == 1 & P3067S1 == 2 & P6775 == 2, FORMAL := 0]
  dt[P6430 == 4 & con_negocio & P3067 == 1 & P3067S1 == 2 & P6775 == 9 & OFICIO_C8_2D <= 20, FORMAL := 1]
  dt[P6430 == 4 & con_negocio & P3067 == 1 & P3067S1 == 2 & P6775 == 9 & OFICIO_C8_2D >= 21, FORMAL := 0]
  dt[P6430 == 5 & con_negocio & P3067 == 1 & P3067S1 == 2 & P6775 == 9 & P3069 >= 4, FORMAL := 1]
  dt[P6430 == 5 & con_negocio & P3067 == 1 & P3067S1 == 2 & P6775 == 9 & P3069 <= 3, FORMAL := 0]

  dt[P6430 %in% c(4, 5) & con_negocio & P3067 == 2 & P6775 == 1 & P3068 == 1, FORMAL := 1]
  dt[P6430 %in% c(4, 5) & con_negocio & P3067 == 2 & P6775 == 1 & P3068 == 2, FORMAL := 0]
  dt[P6430 %in% c(4, 5) & con_negocio & P3067 == 2 & P6775 == 3 & OFICIO_C8_2D <= 20, FORMAL := 1]
  dt[P6430 %in% c(4, 5) & con_negocio & P3067 == 2 & P6775 == 3 & OFICIO_C8_2D >= 21, FORMAL := 0]
  dt[P6430 %in% c(4, 5) & con_negocio & P3067 == 2 & P6775 == 1 & P3068 %in% c(3, 9), FORMAL := 0]
  dt[P6430 %in% c(4, 5) & con_negocio & P3067 == 2 & P6775 == 2, FORMAL := 0]
  dt[P6430 == 5 & con_negocio & P3067 == 2 & P6775 == 9 & P3069 >= 4, FORMAL := 1]
  dt[P6430 == 5 & con_negocio & P3067 == 2 & P6775 == 9 & P3069 <= 3, FORMAL := 0]
  dt[P6430 == 4 & con_negocio & P3067 == 2 & P6775 == 9 & OFICIO_C8_2D <= 20, FORMAL := 1]
  dt[P6430 == 4 & con_negocio & P3067 == 2 & P6775 == 9 & OFICIO_C8_2D >= 21, FORMAL := 0]

  # Salud
  dt[, SALUD := 0]
  dt[P6430 %in% c(1, 3, 7) & P6100 %in% c(1, 2) & P6110 %in% c(1, 2, 4), SALUD := 1]
  dt[P6430 %in% c(1, 3, 7) & P6100 == 9 & P6450 == 2, SALUD := 1]
  dt[P6430 %in% c(1, 3, 7) & P6110 == 9 & P6450 == 2, SALUD := 1]

  # Pensión
  dt[, PENSION := 0]
  dt[P6430 %in% c(1, 3, 7) & P6920 == 3, PENSION := 1]
  dt[P6430 %in% c(1, 3, 7) & P6920 == 1 & P6930 %in% 1:3 & P6940 %in% c(1, 3), PENSION := 1]

  # Ocupación informal
  dt[, EI := 0]
  dt[P6430 == 2, EI := 1]
  dt[P6430 %in% c(4, 5) & !is.na(FORMAL), EI := FORMAL]
  dt[P6430 %in% c(1, 2, 3, 7) & SALUD == 1 & PENSION == 1, EI := 1]
  dt[P6430 %in% c(4, 5) & RAMA2D_R4 %in% c("84", "99"), EI := 1]
  dt[!(P6430 %in% c(6, 8)) & RAMA2D_R4 %in% c("84", "99"), EI := 1]

  invisible(dt)
}

map_informalidad <- function(dt) {

  informal_map <- c(
    "0" ="Informal",
    "1" = "Formal"
  )
  dt[, EI := informal_map[as.character(EI)]]
  return(dt)
}

clasificar_nini <- function(dt) {
  dt[, Joven := fifelse(P6040 >= 15 & P6040 <= 28, 1L, 0L)]

  dt[, InactivosDesocupados := fifelse(DSI == 1 | FFT == 1, 1L, 0L)]

  dt[, NoEstudia := fifelse(P6170 == 2, 1L, 0L)]

  dt[, NiNi := fifelse(
    Joven == 1 & InactivosDesocupados == 1 & NoEstudia == 1, 1L,
    fifelse(Joven == 1, 0L, NA_integer_)
  )]

  return(dt)
}

# 4. Wrapper final
map_all_variables <- function(dt) {
  dt <- clasificar_formalidad(dt)
  dt <- map_informalidad(dt)
  dt <- clasificar_nini(dt)
  dt <- map_departamentos(dt)
  dt <- map_estado_civil(dt)
  dt <- map_educacion(dt)
  dt <- map_acceso_salud(dt)
  dt <- map_afiliacion_salud(dt)
  dt <- map_ocupacion(dt)
  dt <- map_vivienda(dt)
  dt <- map_sexo(dt)
  dt <- map_area(dt)
  dt <- map_CLASE(dt)
  return(dt)
}

# Auxiliar para definir eje del gráfico
preparar_eje_x <- function(dt, temporalidad, by_vars) {
  if (temporalidad == "anual") {
    dt[, eje_x := ANO]
  } else if (temporalidad == "trimestral") {
    dt[, eje_x := paste0(ANO, "-T", TRIMESTRE)]
  } else if (temporalidad == "mensual") {
    dt[, eje_x := paste0(ANO, "-", sprintf("%02d", MES))]
  }

  # Si también hay división geográfica, pegarla
  if (any(c("DPTO", "AREA") %in% by_vars)) {
    dt[, eje_x := do.call(paste, c(.SD, sep = "-")),
       .SDcols = intersect(c("eje_x", "DPTO", "AREA"), names(dt))]
  }

  return(dt)
}

