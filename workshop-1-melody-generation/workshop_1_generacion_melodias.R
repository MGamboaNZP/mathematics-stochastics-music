

# ============================================================
# ¿PODEMOS TRANSFORMAR LAS MATEMÁTICAS EN MÚSICA?
# GENERACIÓN DE MELODÍAS MEDIANTE DISTRIBUCIONES DE PROBABILIDAD
# ============================================================

# En este taller vamos a explorar cómo podemos utilizar las
# matemáticas y la estadística para generar música.
#
# Primero utilizaremos diferentes distribuciones de probabilidad
# para seleccionar notas musicales al azar.
#
# Después modificaremos las probabilidades para conseguir
# melodías con determinadas características musicales.
#
# Finalmente, guardaremos las melodías en archivos MIDI
# para poder escucharlas en Ableton Live.


# ============================================================
# 1. PREPARACIÓN
# ============================================================


# Creamos una carpeta específica para los archivos MIDI
# de la segunda parte del taller.

carpeta_midi <- "MIDI_Parte_1"

dir.create(carpeta_midi, showWarnings = FALSE) 
# Fijamos una semilla para que los resultados sean reproducibles.
# Si ejecutamos de nuevo el script, obtendremos las mismas melodías.
set.seed(2026)

# Definimos una escala de Do mayor con siete notas.
notas <- c("C", "D", "E", "F", "G", "A", "B")

# Número MIDI correspondiente a cada nota.
# Utilizamos las notas de la octava C4-B4.
midi_notas <- c(60, 62, 64, 65, 67, 69, 71)

# Número de notas que tendrá cada melodía.
n <- 32

# Duración de cada nota en segundos.
duracion <- 0.5


# ============================================================
# 2. FUNCIÓN PARA CREAR ARCHIVOS MIDI
# ============================================================

# Esta función convierte una melodía en un archivo MIDI.
#
# Recibe:
# - melodia: secuencia de notas o números MIDI.
# - archivo: nombre del archivo que queremos crear.
# - notas: nombres de las notas de nuestra escala.
# - midi_notas: números MIDI correspondientes a cada nota.
# - duracion: duración de cada nota en segundos.
#
# Puede recibir tanto nombres de notas como números MIDI.
# Esto nos permite trabajar con diferentes registros musicales.


crear_midi <- function(melodia, archivo, notas = NULL,
                       midi_notas = NULL, duracion = 0.5) {
  
  # Si recibimos nombres de notas, los convertimos a números MIDI.
  if (!is.null(notas) && !is.null(midi_notas)) {
    
    notas_midi_generadas <- midi_notas[match(melodia, notas)]
    
  } else {
    
    # Si recibimos números MIDI, los utilizamos directamente.
    notas_midi_generadas <- melodia
  }
  
  # Comprobamos que la melodía no esté vacía.
  if (length(notas_midi_generadas) == 0) {
    stop("La melodía no puede estar vacía.")
  }
  
  # Comprobamos que todas las notas sean números MIDI válidos.
  if (!is.numeric(notas_midi_generadas) ||
      anyNA(notas_midi_generadas) ||
      any(!is.finite(notas_midi_generadas)) ||
      any(notas_midi_generadas != as.integer(notas_midi_generadas)) ||
      any(notas_midi_generadas < 0 | notas_midi_generadas > 127)) {
    
    stop("La melodía contiene notas MIDI no válidas.")
  }
  
  # Comprobamos que la duración sea un número positivo.
  if (length(duracion) != 1 ||
      !is.numeric(duracion) ||
      !is.finite(duracion) ||
      duracion <= 0) {
    
    stop("La duración debe ser un número positivo.")
  }
  
  # Parámetros del archivo MIDI.
  ppq <- 480
  bpm <- 120
  
  # Convertimos la duración de las notas en unidades MIDI (ticks).
  ticks <- round(duracion * ppq * bpm / 60)
  
  # Comprobamos que la duración se pueda representar en ticks.
  if (ticks < 1) {
    stop("La duración es demasiado pequeña para el formato MIDI.")
  }
  
  # Función auxiliar para escribir números enteros en formato MIDI.
  escribir_entero <- function(x, bytes) {
    
    resultado <- raw(bytes)
    
    for (i in seq(bytes, 1)) {
      resultado[bytes - i + 1] <-
        as.raw((x %/% 256^(i - 1)) %% 256)
    }
    
    resultado
  }
  
  # Función auxiliar para escribir los tiempos en formato MIDI.
  escribir_variable <- function(x) {
    
    resultado <- as.raw(x %% 128)
    x <- x %/% 128
    
    while (x > 0) {
      resultado <- c(
        as.raw((x %% 128) + 128),
        resultado
      )
      
      x <- x %/% 128
    }
    
    resultado
  }
  
  # Aquí iremos almacenando los eventos musicales.
  eventos <- raw()
  
  # Indicamos el tempo: 120 pulsaciones por minuto.
  eventos <- c(eventos, as.raw(c(
    0x00, 0xFF, 0x51, 0x03, 0x07, 0xA1, 0x20
  )))
  
  # Añadimos las notas de la melodía.
  for (nota in notas_midi_generadas) {
    
    # Inicio de la nota (NOTE ON).
    eventos <- c(
      eventos,
      escribir_variable(0),
      as.raw(c(0x90, nota, 80))
    )
    
    # Final de la nota (NOTE OFF).
    eventos <- c(
      eventos,
      escribir_variable(ticks),
      as.raw(c(0x80, nota, 0))
    )
  }
  
  # Indicamos que la pista MIDI ha terminado.
  eventos <- c(eventos, as.raw(c(0x00, 0xFF, 0x2F, 0x00)))
  
  # Construimos la cabecera del archivo MIDI.
  cabecera <- c(
    charToRaw("MThd"),
    escribir_entero(6, 4),
    escribir_entero(0, 2),
    escribir_entero(1, 2),
    escribir_entero(ppq, 2)
  )
  
  # Construimos la pista musical.
  pista <- c(
    charToRaw("MTrk"),
    escribir_entero(length(eventos), 4),
    eventos
  )
  
  # Guardamos el archivo MIDI.
  writeBin(c(cabecera, pista), file.path(carpeta_midi, archivo))
  
  message("Archivo MIDI creado: ", archivo)
}


# ============================================================
# EXPERIMENTO 1. ¿CÓMO SUENAN LAS MATEMÁTICAS?
# ============================================================

# En este primer experimento utilizaremos distribuciones
# de probabilidad conocidas para generar nuestras melodías.
#
# Cada distribución asignará una probabilidad diferente
# a las notas musicales.


# ------------------------------------------------------------
# 1.1. DISTRIBUCIÓN UNIFORME DISCRETA
# ------------------------------------------------------------

# Todas las notas tienen exactamente la misma probabilidad
# de aparecer en la melodía.

prob_uniforme <- rep(1 / length(notas), length(notas))

# Seleccionamos 32 notas al azar.
melodia_uniforme <- sample(
  notas,
  size = n,
  replace = TRUE,
  prob = prob_uniforme
)

# Mostramos la melodía y sus probabilidades.
melodia_uniforme
prob_uniforme

# Guardamos la melodía en un archivo MIDI.
crear_midi(
  melodia_uniforme,
  "experimento_1_1_uniforme.mid",
  notas,
  midi_notas,
  duracion
)


# ------------------------------------------------------------
# 1.2. DISTRIBUCIÓN BINOMIAL
# ------------------------------------------------------------

# La distribución binomial cuenta el número de éxitos
# obtenidos en un número fijo de ensayos independientes.
#
# Elegimos una distribución binomial con:
# - 6 ensayos.
# - Probabilidad de éxito igual a 0.6.
#
# Los valores posibles son 0, 1, 2, 3, 4, 5 y 6.
# Asociamos cada uno de estos valores a una nota musical.

prob_binomial <- dbinom(
  0:6,
  size = 6,
  prob = 0.6
)

# Generamos una melodía siguiendo las probabilidades binomiales.
melodia_binomial <- sample(
  notas,
  size = n,
  replace = TRUE,
  prob = prob_binomial
)

# Mostramos la melodía y sus probabilidades.
melodia_binomial
prob_binomial

# Guardamos la melodía en un archivo MIDI.
crear_midi(
  melodia_binomial,
  "experimento_1_2_binomial.mid",
  notas,
  midi_notas,
  duracion
)


# ------------------------------------------------------------
# 1.3. DISTRIBUCIÓN DE POISSON
# ------------------------------------------------------------

# La distribución de Poisson permite modelizar el número
# de sucesos que ocurren en un intervalo.
#
# Esta distribución puede tomar cualquier valor entero
# no negativo: 0, 1, 2, 3, ...
#
# Elegimos una distribución de Poisson con lambda = 5.
# Su media y su varianza son iguales a 5.
#
# Generamos directamente una muestra de la distribución
# completa, sin limitar sus posibles valores.

valores_poisson <- rpois(
  n,
  lambda = 5
)

# Transformamos los valores obtenidos en notas musicales.
#
# Utilizamos tres octavas iniciales de la escala de Do mayor:
# Do3, Re3, ..., Si3, Do4, ..., Si4, Do5, ..., Si5.
#
# Cada valor entero corresponde a una nota diferente.
# Si aparece un valor superior a 20, continuamos en
# octavas superiores.
#
# De esta forma no truncamos ni modificamos la distribución.

midi_escala <- c(0, 2, 4, 5, 7, 9, 11)

midi_poisson <- 48 +
  12 * (valores_poisson %/% 7) +
  midi_escala[(valores_poisson %% 7) + 1]

# Mostramos los valores generados y sus notas MIDI.
valores_poisson
midi_poisson

# Guardamos la melodía en un archivo MIDI.
crear_midi(
  midi_poisson,
  "experimento_1_3_poisson.mid",
  duracion = duracion
)


# ------------------------------------------------------------
# 1.4. DISTRIBUCIÓN GEOMÉTRICA
# ------------------------------------------------------------

# La distribución geométrica permite modelizar el número
# de fracasos antes de obtener el primer éxito.
#
# Esta distribución puede tomar cualquier valor entero
# no negativo: 0, 1, 2, 3, ...
#
# Elegimos una distribución geométrica con probabilidad
# de éxito p = 0.4.
#
# Generamos directamente una muestra de la distribución
# completa, sin limitar sus posibles valores.

valores_geometricos <- rgeom(
  n,
  prob = 0.4
)

# Transformamos los valores obtenidos en notas musicales.
#
# Utilizamos la misma correspondencia que en Poisson:
# cada valor entero corresponde a una nota de la escala
# de Do mayor, extendida a diferentes octavas.
#
# De esta forma no truncamos ni modificamos la distribución.

midi_geometrica <- 48 +
  12 * (valores_geometricos %/% 7) +
  midi_escala[(valores_geometricos %% 7) + 1]

# Mostramos los valores generados y sus notas MIDI.
valores_geometricos
midi_geometrica

# Guardamos la melodía en un archivo MIDI.
crear_midi(
  midi_geometrica,
  "experimento_1_4_geometrica.mid",
  duracion = duracion
)


# ------------------------------------------------------------
# 1.5. COMPROBACIÓN DE LOS VALORES GENERADOS
# ------------------------------------------------------------

# Comprobamos los valores máximos obtenidos.
# Los valores entre 0 y 20 corresponden al registro inicial
# de tres octavas: Do3-Si5.

cat("\nValor máximo de Poisson:", max(valores_poisson), "\n")
cat("Valor máximo de geométrica:", max(valores_geometricos), "\n")

# Comprobamos si alguna secuencia supera las tres octavas iniciales.

if (any(valores_poisson > 20)) {
  cat("Poisson: hay notas por encima del registro inicial.\n")
} else {
  cat("Poisson: todas las notas están dentro de las tres octavas iniciales.\n")
}

if (any(valores_geometricos > 20)) {
  cat("Geométrica: hay notas por encima del registro inicial.\n")
} else {
  cat("Geométrica: todas las notas están dentro de las tres octavas iniciales.\n")
}


# ============================================================
# EXPERIMENTO 2. AÑADIMOS UN POCO DE MUSICALIDAD
# ============================================================

# En este segundo experimento vamos a diseñar las probabilidades
# teniendo en cuenta algunas características musicales.
#
# Queremos explorar cómo cambia una melodía cuando damos
# más importancia a determinadas notas de la escala de Do mayor.


# ------------------------------------------------------------
# 2.1. DO MAYOR SUAVE
# ------------------------------------------------------------

# Asignamos probabilidades diferentes a las siete notas.
# Todas pueden aparecer, pero algunas tienen más posibilidades
# de aparecer que otras.

prob_do_mayor <- c(
  0.20, 0.10, 0.15, 0.10, 0.15, 0.15, 0.15
)

# Generamos una melodía.
melodia_do_mayor <- sample(
  notas,
  size = n,
  replace = TRUE,
  prob = prob_do_mayor
)

# Mostramos la melodía y sus probabilidades.
melodia_do_mayor
prob_do_mayor

# Guardamos la melodía en un archivo MIDI.
crear_midi(
  melodia_do_mayor,
  "experimento_2_1_do_mayor_suave.mid",
  notas,
  midi_notas,
  duracion
)


# ------------------------------------------------------------
# 2.2. TRÍADA DE DO MAYOR
# ------------------------------------------------------------

# Damos mayor probabilidad a Do, Mi y Sol.
# Estas tres notas forman el acorde de Do mayor.

prob_triada <- c(
  0.30, 0.05, 0.25, 0.05, 0.25, 0.05, 0.05
)

# Generamos una melodía.
melodia_triada <- sample(
  notas,
  size = n,
  replace = TRUE,
  prob = prob_triada
)

# Mostramos la melodía y sus probabilidades.
melodia_triada
prob_triada

# Guardamos la melodía en un archivo MIDI.
crear_midi(
  melodia_triada,
  "experimento_2_2_triada_do_mayor.mid",
  notas,
  midi_notas,
  duracion
)


# ------------------------------------------------------------
# 2.3. TONALIDAD FUERTE
# ------------------------------------------------------------

# Todas las notas pueden aparecer, pero asignamos
# probabilidades diferentes para destacar algunas notas
# características de la tonalidad de Do mayor.
#
# Los pesos se normalizan para que las probabilidades sumen 1.

pesos_tonalidad <- c(
  0.30, 0.08, 0.20, 0.07, 0.20, 0.06, 0.04
)

prob_tonalidad <- pesos_tonalidad / sum(pesos_tonalidad)

# Generamos una melodía.
melodia_tonalidad <- sample(
  notas,
  size = n,
  replace = TRUE,
  prob = prob_tonalidad
)

# Mostramos la melodía y sus probabilidades.
melodia_tonalidad
prob_tonalidad

# Guardamos la melodía en un archivo MIDI.
crear_midi(
  melodia_tonalidad,
  "experimento_2_3_tonalidad_fuerte.mid",
  notas,
  midi_notas,
  duracion
)


# ------------------------------------------------------------
# 2.4. PERFIL KRUMHANSL-KESSLER
# ------------------------------------------------------------

# Krumhansl y Kessler propusieron perfiles que representan
# la estabilidad o importancia tonal de cada nota.
#
# Utilizamos los valores correspondientes a las notas
# de la tonalidad de Do mayor.
#
# Estos valores no son probabilidades originales.
# Para utilizarlos en la generación de melodías,
# los normalizamos y los transformamos en probabilidades.
#
# Se trata de una adaptación del perfil original de doce
# clases cromáticas a las siete notas diatónicas de Do mayor.

perfil_C_mayor <- c(
  6.35, 2.23, 4.38, 2.33, 5.19, 2.52, 2.29
)

# Transformamos los valores del perfil en probabilidades.
prob_krumhansl <- perfil_C_mayor / sum(perfil_C_mayor)

# Generamos una melodía.
melodia_krumhansl <- sample(
  notas,
  size = n,
  replace = TRUE,
  prob = prob_krumhansl
)

# Mostramos la melodía y sus probabilidades.
melodia_krumhansl
prob_krumhansl

# Guardamos la melodía en un archivo MIDI.
crear_midi(
  melodia_krumhansl,
  "experimento_2_4_krumhansl_kessler.mid",
  notas,
  midi_notas,
  duracion
)


# ============================================================
# 3. ESCUCHAR Y COMPARAR LOS RESULTADOS
# ============================================================

# Hemos generado ocho melodías utilizando diferentes
# distribuciones y perfiles de probabilidad.
#
# Ahora podemos escuchar los archivos MIDI y reflexionar:
#
# ¿Suenan diferentes las melodías?
# ¿Qué notas aparecen con mayor frecuencia?
# ¿Qué distribución produce una melodía que nos gusta más?
# ¿Qué diferencias encontramos entre los dos experimentos?
# ¿Tiene que ser musical una melodía para ser matemáticamente
# interesante?


# ============================================================
# FIN DEL TALLER
# ============================================================
