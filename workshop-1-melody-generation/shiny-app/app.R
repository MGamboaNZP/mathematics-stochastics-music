
# ============================================================
# MATHEMATICS, STOCHASTICS & MUSIC
# WORKSHOP 1: STOCHASTIC MELODIES
#
# Experiment 1: Probability distributions
# Experiment 2: Musical probability profiles
#
# Features:
# - Interactive probability distributions
# - Adjustable distribution parameters
# - Interactive sliders for discrete weights
# - Fixed musical probability profiles
# - 32-note melodies
# - Musical note labels
# - Theoretical probabilities vs observed frequencies
# - Audio playback (WAV)
# - MIDI download
# ============================================================


# ============================================================
# 1. PACKAGES AND GENERAL SETTINGS
# ============================================================

library(shiny)
library(base64enc)

n_notes <- 32
note_duration <- 0.5
bpm <- 120

# C major scale
midi_scale <- c(0, 2, 4, 5, 7, 9, 11)

note_names <- c(
  "C", "D", "E", "F", "G", "A", "B"
)

midi_notes <- 60 + midi_scale

# Extended range for Poisson and Geometric
# C2 to B8
extended_base <- 36
max_scale_degree <- 47


# ============================================================
# 2. MUSICAL PROBABILITY PROFILES
# ============================================================

profile_defaults <- list(
  
  "C major" = c(
    20, 10, 15, 10, 15, 15, 15
  ),
  
  "C major triad" = c(
    30, 5, 25, 5, 25, 5, 5
  ),
  
  "Strong tonality" = c(
    30, 8, 20, 7, 20, 6, 4
  ),
  
  "Krumhansl-Kessler" = c(
    6.35, 2.23, 4.38, 2.33,
    5.19, 2.52, 2.29
  )
  
)


# ============================================================
# 3. GENERAL FUNCTIONS
# ============================================================

normalise_weights <- function(weights) {
  
  if (
    any(!is.finite(weights)) ||
    any(weights < 0) ||
    sum(weights) <= 0
  ) {
    stop(
      "Weights must be non-negative and at least one must be positive."
    )
  }
  
  weights / sum(weights)
  
}


midi_to_note <- function(midi) {
  
  notes <- c(
    "C", "C#", "D", "D#", "E", "F",
    "F#", "G", "G#", "A", "A#", "B"
  )
  
  note_index <- (midi %% 12) + 1
  octave <- floor(midi / 12) - 1
  
  paste0(
    notes[note_index],
    octave
  )
  
}


scale_degree_to_midi <- function(
    degree,
    base = 60
) {
  
  base +
    12 * (degree %/% 7) +
    midi_scale[(degree %% 7) + 1]
  
}


format_probability <- function(x) {
  
  formatC(
    x,
    format = "g",
    digits = 7
  )
  
}


# ============================================================
# 4. MELODY GENERATION
# ============================================================

generate_melody <- function(
    
  experiment,
  
  distribution = NULL,
  
  profile = NULL,
  
  discrete_weights = NULL,
  
  binomial_n = 6,
  
  binomial_p = 0.6,
  
  poisson_lambda = 5,
  
  geometric_p = 0.4
  
) {
  
  # ----------------------------------------------------------
  # EXPERIMENT 1: PROBABILITY DISTRIBUTIONS
  # ----------------------------------------------------------
  
  if (experiment == "distribution") {
    
    # --------------------------------------------------------
    # DISCRETE DISTRIBUTION
    # --------------------------------------------------------
    
    if (distribution == "Discrete") {
      
      probabilities <- normalise_weights(
        discrete_weights
      )
      
      support <- 0:6
      
      category_midi <- midi_notes
      
      sampled_values <- sample(
        support,
        size = n_notes,
        replace = TRUE,
        prob = probabilities
      )
      
      melody <- category_midi[
        sampled_values + 1
      ]
      
    }
    
    
    # --------------------------------------------------------
    # BINOMIAL DISTRIBUTION
    # --------------------------------------------------------
    
    if (distribution == "Binomial") {
      
      support <- 0:binomial_n
      
      probabilities <- dbinom(
        support,
        size = binomial_n,
        prob = binomial_p
      )
      
      category_midi <- scale_degree_to_midi(
        support,
        base = 60
      )
      
      sampled_values <- rbinom(
        n_notes,
        size = binomial_n,
        prob = binomial_p
      )
      
      melody <- scale_degree_to_midi(
        sampled_values,
        base = 60
      )
      
    }
    
    
    # --------------------------------------------------------
    # POISSON DISTRIBUTION
    # --------------------------------------------------------
    
    if (distribution == "Poisson") {
      
      support_max <- min(
        qpois(
          0.999,
          lambda = poisson_lambda
        ),
        max_scale_degree
      )
      
      support <- 0:support_max
      
      probabilities <- dpois(
        support,
        lambda = poisson_lambda
      )
      
      # Conditional probabilities within the available
      # musical range
      probabilities <- normalise_weights(
        probabilities
      )
      
      category_midi <- scale_degree_to_midi(
        support,
        base = extended_base
      )
      
      # Sample from the same truncated distribution
      sampled_values <- sample(
        support,
        size = n_notes,
        replace = TRUE,
        prob = probabilities
      )
      
      melody <- scale_degree_to_midi(
        sampled_values,
        base = extended_base
      )
      
    }
    
    
    # --------------------------------------------------------
    # GEOMETRIC DISTRIBUTION
    # --------------------------------------------------------
    
    if (distribution == "Geometric") {
      
      support_max <- min(
        qgeom(
          0.999,
          prob = geometric_p
        ),
        max_scale_degree
      )
      
      support <- 0:support_max
      
      probabilities <- dgeom(
        support,
        prob = geometric_p
      )
      
      # Conditional probabilities within the available
      # musical range
      probabilities <- normalise_weights(
        probabilities
      )
      
      category_midi <- scale_degree_to_midi(
        support,
        base = extended_base
      )
      
      # Sample from the same truncated distribution
      sampled_values <- sample(
        support,
        size = n_notes,
        replace = TRUE,
        prob = probabilities
      )
      
      melody <- scale_degree_to_midi(
        sampled_values,
        base = extended_base
      )
      
    }
    
    
    # --------------------------------------------------------
    # THEORETICAL AND OBSERVED PROBABILITIES
    # --------------------------------------------------------
    
    observed_categories <- match(
      melody,
      category_midi
    )
    
    observed_counts <- tabulate(
      observed_categories,
      nbins = length(category_midi)
    )
    
    observed_frequencies <- (
      observed_counts / n_notes
    )
    
    probability_table <- data.frame(
      
      Note = midi_to_note(category_midi),
      
      Theoretical_probability = probabilities,
      
      Observed_frequency = observed_frequencies,
      
      Observed_count = observed_counts
      
    )
    
  }
  
  
  # ----------------------------------------------------------
  # EXPERIMENT 2: MUSICAL PROBABILITY PROFILES
  # ----------------------------------------------------------
  
  if (experiment == "profile") {
    
    # The selected profile is fixed internally.
    # Students cannot modify individual note weights.
    
    probabilities <- normalise_weights(
      profile_defaults[[profile]]
    )
    
    support <- 0:6
    
    category_midi <- midi_notes
    
    sampled_values <- sample(
      support,
      size = n_notes,
      replace = TRUE,
      prob = probabilities
    )
    
    melody <- category_midi[
      sampled_values + 1
    ]
    
    observed_counts <- tabulate(
      sampled_values + 1,
      nbins = length(category_midi)
    )
    
    observed_frequencies <- (
      observed_counts / n_notes
    )
    
    probability_table <- data.frame(
      
      Note = midi_to_note(category_midi),
      
      Theoretical_probability = probabilities,
      
      Observed_frequency = observed_frequencies,
      
      Observed_count = observed_counts
      
    )
    
  }
  
  
  list(
    
    melody = melody,
    
    note_names = midi_to_note(melody),
    
    probability_table = probability_table
    
  )
  
}


# ============================================================
# 5. AUDIO GENERATION
# ============================================================

create_audio <- function(melody) {
  
  sample_rate <- 22050
  
  duration <- note_duration
  
  audio <- numeric(0)
  
  for (midi in melody) {
    
    frequency <- 440 * 2^(
      (midi - 69) / 12
    )
    
    n_samples <- round(
      sample_rate * duration
    )
    
    time <- seq(
      0,
      by = 1 / sample_rate,
      length.out = n_samples
    )
    
    wave <- sin(
      2 * pi * frequency * time
    )
    
    # Short fade-in and fade-out
    fade_length <- min(
      500,
      floor(length(wave) / 4)
    )
    
    envelope <- rep(
      1,
      length(wave)
    )
    
    if (fade_length > 0) {
      
      envelope[1:fade_length] <- seq(
        0,
        1,
        length.out = fade_length
      )
      
      envelope[
        (length(wave) - fade_length + 1):length(wave)
      ] <- seq(
        1,
        0,
        length.out = fade_length
      )
      
    }
    
    wave <- wave * envelope * 0.3
    
    audio <- c(
      audio,
      wave
    )
    
  }
  
  audio <- pmax(
    -1,
    pmin(1, audio)
  )
  
  con <- rawConnection(
    raw(),
    open = "wb"
  )
  
  on.exit(close(con))
  
  # WAV header
  
  writeBin(
    charToRaw("RIFF"),
    con
  )
  
  total_size <- 36 + length(audio) * 2
  
  writeBin(
    as.integer(total_size),
    con,
    size = 4,
    endian = "little"
  )
  
  writeBin(
    charToRaw("WAVE"),
    con
  )
  
  writeBin(
    charToRaw("fmt "),
    con
  )
  
  writeBin(
    as.integer(16),
    con,
    size = 4,
    endian = "little"
  )
  
  writeBin(
    as.integer(1),
    con,
    size = 2,
    endian = "little"
  )
  
  writeBin(
    as.integer(1),
    con,
    size = 2,
    endian = "little"
  )
  
  writeBin(
    as.integer(sample_rate),
    con,
    size = 4,
    endian = "little"
  )
  
  writeBin(
    as.integer(sample_rate * 2),
    con,
    size = 4,
    endian = "little"
  )
  
  writeBin(
    as.integer(2),
    con,
    size = 2,
    endian = "little"
  )
  
  writeBin(
    as.integer(16),
    con,
    size = 2,
    endian = "little"
  )
  
  writeBin(
    charToRaw("data"),
    con
  )
  
  writeBin(
    as.integer(length(audio) * 2),
    con,
    size = 4,
    endian = "little"
  )
  
  writeBin(
    as.integer(round(audio * 32767)),
    con,
    size = 2,
    endian = "little"
  )
  
  raw_audio <- rawConnectionValue(con)
  
  base64enc::base64encode(
    raw_audio
  )
  
}


# ============================================================
# 6. MIDI GENERATION
# ============================================================

write_integer <- function(x, bytes) {
  
  result <- raw(bytes)
  
  for (i in seq_len(bytes)) {
    
    result[i] <- as.raw(
      bitwAnd(
        bitwShiftR(
          as.integer(x),
          8 * (bytes - i)
        ),
        255
      )
    )
    
  }
  
  result
  
}


write_variable <- function(value) {
  
  value <- as.integer(value)
  
  buffer <- bitwAnd(
    value,
    127
  )
  
  while (
    value <- bitwShiftR(value, 7)
  ) {
    
    buffer <- bitwOr(
      bitwShiftL(buffer, 8),
      bitwOr(
        bitwAnd(value, 127),
        128
      )
    )
    
  }
  
  result <- raw()
  
  repeat {
    
    result <- c(
      result,
      as.raw(
        bitwAnd(buffer, 255)
      )
    )
    
    if (bitwAnd(buffer, 128) == 0) {
      break
    }
    
    buffer <- bitwShiftR(
      buffer,
      8
    )
    
  }
  
  result
  
}


create_midi <- function(melody) {
  
  ticks_per_beat <- 480
  
  note_ticks <- ticks_per_beat
  
  track <- raw()
  
  # Tempo: 120 BPM
  
  track <- c(
    track,
    as.raw(c(
      0x00,
      0xFF,
      0x51,
      0x03,
      0x07,
      0xA1,
      0x20
    ))
  )
  
  for (note in melody) {
    
    note_on <- as.raw(c(
      0x90,
      as.integer(note),
      0x64
    ))
    
    note_off <- as.raw(c(
      0x80,
      as.integer(note),
      0x00
    ))
    
    track <- c(
      track,
      write_variable(0),
      note_on,
      write_variable(note_ticks),
      note_off
    )
    
  }
  
  # End of track
  
  track <- c(
    track,
    as.raw(c(
      0x00,
      0xFF,
      0x2F,
      0x00
    ))
  )
  
  header <- c(
    charToRaw("MThd"),
    write_integer(6, 4),
    write_integer(0, 2),
    write_integer(1, 2),
    write_integer(ticks_per_beat, 2)
  )
  
  track_header <- c(
    charToRaw("MTrk"),
    write_integer(length(track), 4)
  )
  
  c(
    header,
    track_header,
    track
  )
  
}


# ============================================================
# 7. USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  titlePanel(
    "Mathematics, Stochastics & Music"
  ),
  
  p(
    "Workshop 1: Stochastic melodies",
    style = "font-size: 18px; font-weight: bold;"
  ),
  
  tabsetPanel(
    
    # ========================================================
    # EXPERIMENT 1
    # ========================================================
    
    tabPanel(
      
      "Experiment 1 · Probability distributions",
      
      br(),
      
      sidebarLayout(
        
        sidebarPanel(
          
          selectInput(
            "distribution",
            "Probability distribution",
            
            choices = c(
              "Discrete",
              "Binomial",
              "Poisson",
              "Geometric"
            )
            
          ),
          
          
          # --------------------------------------------------
          # DISCRETE DISTRIBUTION
          # --------------------------------------------------
          
          conditionalPanel(
            
            condition = "input.distribution == 'Discrete'",
            
            h4("Weights for each musical note"),
            
            helpText(
              "Move the sliders to change the relative weights of each note. ",
              "The corresponding probabilities are automatically normalised."
            ),
            
            sliderInput(
              "weight_C",
              "C4",
              min = 0,
              max = 20,
              value = 11,
              step = 1
            ),
            
            sliderInput(
              "weight_D",
              "D4",
              min = 0,
              max = 20,
              value = 2,
              step = 1
            ),
            
            sliderInput(
              "weight_E",
              "E4",
              min = 0,
              max = 20,
              value = 3,
              step = 1
            ),
            
            sliderInput(
              "weight_F",
              "F4",
              min = 0,
              max = 20,
              value = 2,
              step = 1
            ),
            
            sliderInput(
              "weight_G",
              "G4",
              min = 0,
              max = 20,
              value = 1,
              step = 1
            ),
            
            sliderInput(
              "weight_A",
              "A4",
              min = 0,
              max = 20,
              value = 1,
              step = 1
            ),
            
            sliderInput(
              "weight_B",
              "B4",
              min = 0,
              max = 20,
              value = 2,
              step = 1
            )
            
          ),
          
          
          # --------------------------------------------------
          # BINOMIAL DISTRIBUTION
          # --------------------------------------------------
          
          conditionalPanel(
            
            condition = "input.distribution == 'Binomial'",
            
            sliderInput(
              "binomial_n",
              "Number of trials (n)",
              min = 6,
              max = 13,
              value = 6,
              step = 1
            ),
            
            sliderInput(
              "binomial_p",
              "Success probability (p)",
              min = 0,
              max = 1,
              value = 0.6,
              step = 0.05
            )
            
          ),
          
          
          # --------------------------------------------------
          # POISSON DISTRIBUTION
          # --------------------------------------------------
          
          conditionalPanel(
            
            condition = "input.distribution == 'Poisson'",
            
            sliderInput(
              "poisson_lambda",
              "Rate parameter (lambda)",
              min = 0.1,
              max = 20,
              value = 5,
              step = 0.1
            )
            
          ),
          
          
          # --------------------------------------------------
          # GEOMETRIC DISTRIBUTION
          # --------------------------------------------------
          
          conditionalPanel(
            
            condition = "input.distribution == 'Geometric'",
            
            sliderInput(
              "geometric_p",
              "Success probability (p)",
              min = 0.1,
              max = 1,
              value = 0.4,
              step = 0.05
            )
            
          ),
          
          br(),
          
          actionButton(
            "generate_1",
            "Generate melody",
            class = "btn-primary"
          )
          
        ),
        
        
        # ----------------------------------------------------
        # EXPERIMENT 1 OUTPUTS
        # ----------------------------------------------------
        
        mainPanel(
          
          h4("Generated melody"),
          
          uiOutput("notes_1"),
          
          br(),
          
          h4("Melodic sequence"),
          
          plotOutput(
            "plot_1",
            height = "300px"
          ),
          
          br(),
          
          h4(
            "Theoretical probabilities and observed frequencies"
          ),
          
          tableOutput("table_1"),
          
          br(),
          
          h4("Listen to your melody"),
          
          uiOutput("audio_1"),
          
          br(),
          
          downloadButton(
            "download_midi_1",
            "Download MIDI"
          )
          
        )
        
      )
      
    ),
    
    
    # ========================================================
    # EXPERIMENT 2
    # ========================================================
    
    tabPanel(
      
      "Experiment 2 · Musical probability profiles",
      
      br(),
      
      sidebarLayout(
        
        sidebarPanel(
          
          selectInput(
            "profile",
            "Choose a musical probability profile",
            
            choices = c(
              "C major",
              "C major triad",
              "Strong tonality",
              "Krumhansl-Kessler"
            ),
            
            selected = "C major"
          ),
          
          helpText(
            "Each profile defines a fixed probability distribution over the seven notes of the C major scale."
          ),
          
          br(),
          
          actionButton(
            "generate_2",
            "Generate melody",
            class = "btn-primary"
          )
          
        ),
        
        
        # ----------------------------------------------------
        # EXPERIMENT 2 OUTPUTS
        # ----------------------------------------------------
        
        mainPanel(
          
          h4("Generated melody"),
          
          uiOutput("notes_2"),
          
          br(),
          
          h4("Melodic sequence"),
          
          plotOutput(
            "plot_2",
            height = "300px"
          ),
          
          br(),
          
          h4(
            "Theoretical probabilities and observed frequencies"
          ),
          
          tableOutput("table_2"),
          
          br(),
          
          h4("Listen to your melody"),
          
          uiOutput("audio_2"),
          
          br(),
          
          downloadButton(
            "download_midi_2",
            "Download MIDI"
          )
          
        )
        
      )
      
    )
    
  )
  
)


# ============================================================
# 8. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # EXPERIMENT 1
  # ----------------------------------------------------------
  
  melody_1 <- eventReactive(
    
    input$generate_1,
    
    {
      
      weights <- c(
        input$weight_C,
        input$weight_D,
        input$weight_E,
        input$weight_F,
        input$weight_G,
        input$weight_A,
        input$weight_B
      )
      
      if (
        input$distribution == "Discrete" &&
        (
          any(weights < 0) ||
          sum(weights) <= 0
        )
      ) {
        
        showNotification(
          "At least one weight must be positive, and all weights must be non-negative.",
          type = "error"
        )
        
        return(NULL)
        
      }
      
      generate_melody(
        
        experiment = "distribution",
        
        distribution = input$distribution,
        
        discrete_weights = weights,
        
        binomial_n = input$binomial_n,
        
        binomial_p = input$binomial_p,
        
        poisson_lambda = input$poisson_lambda,
        
        geometric_p = input$geometric_p
        
      )
      
    },
    
    ignoreInit = TRUE
    
  )
  
  
  # ----------------------------------------------------------
  # EXPERIMENT 1: MELODY NOTES
  # ----------------------------------------------------------
  
  output$notes_1 <- renderUI({
    
    result <- melody_1()
    
    req(result)
    
    div(
      
      style = paste(
        "display: flex;",
        "flex-wrap: wrap;",
        "gap: 6px;"
      ),
      
      lapply(
        
        seq_along(result$note_names),
        
        function(i) {
          
          span(
            
            result$note_names[i],
            
            style = paste(
              "padding: 7px 10px;",
              "border: 1px solid #777;",
              "border-radius: 5px;",
              "font-weight: bold;"
            )
            
          )
          
        }
        
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # EXPERIMENT 1: MELODIC SEQUENCE
  # ----------------------------------------------------------
  
  output$plot_1 <- renderPlot({
    
    result <- melody_1()
    
    req(result)
    
    melody <- result$melody
    
    plot(
      
      seq_along(melody),
      
      melody,
      
      type = "b",
      
      pch = 19,
      
      xaxt = "n",
      
      yaxt = "n",
      
      xlab = "Note position",
      
      ylab = "Musical note",
      
      main = "Melodic sequence",
      
      ylim = range(melody) + c(-2, 2)
      
    )
    
    axis(
      1,
      at = seq_along(melody),
      labels = seq_along(melody)
    )
    
    unique_notes <- sort(
      unique(melody)
    )
    
    axis(
      2,
      at = unique_notes,
      labels = midi_to_note(unique_notes),
      las = 1
    )
    
    grid()
    
  })
  
  
  # ----------------------------------------------------------
  # EXPERIMENT 1: PROBABILITY TABLE
  # ----------------------------------------------------------
  
  output$table_1 <- renderTable({
    
    result <- melody_1()
    
    req(result)
    
    table <- result$probability_table
    
    table$Theoretical_probability <- vapply(
      table$Theoretical_probability,
      format_probability,
      character(1)
    )
    
    table$Observed_frequency <- vapply(
      table$Observed_frequency,
      format_probability,
      character(1)
    )
    
    table
    
  })
  
  
  # ----------------------------------------------------------
  # EXPERIMENT 1: AUDIO
  # ----------------------------------------------------------
  
  output$audio_1 <- renderUI({
    
    result <- melody_1()
    
    req(result)
    
    audio_data <- create_audio(
      result$melody
    )
    
    tags$audio(
      
      controls = NA,
      
      src = paste0(
        "data:audio/wav;base64,",
        audio_data
      ),
      
      type = "audio/wav",
      
      style = "width: 100%;"
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # EXPERIMENT 1: MIDI DOWNLOAD
  # ----------------------------------------------------------
  
  output$download_midi_1 <- downloadHandler(
    
    filename = function() {
      
      "stochastic_melody_experiment_1.mid"
      
    },
    
    content = function(file) {
      
      result <- melody_1()
      
      req(result)
      
      writeBin(
        create_midi(result$melody),
        file
      )
      
    }
    
  )
  
  
  # ----------------------------------------------------------
  # EXPERIMENT 2
  # ----------------------------------------------------------
  
  melody_2 <- eventReactive(
    
    input$generate_2,
    
    {
      
      generate_melody(
        
        experiment = "profile",
        
        profile = input$profile
        
      )
      
    },
    
    ignoreInit = TRUE
    
  )
  
  
  # ----------------------------------------------------------
  # EXPERIMENT 2: MELODY NOTES
  # ----------------------------------------------------------
  
  output$notes_2 <- renderUI({
    
    result <- melody_2()
    
    req(result)
    
    div(
      
      style = paste(
        "display: flex;",
        "flex-wrap: wrap;",
        "gap: 6px;"
      ),
      
      lapply(
        
        seq_along(result$note_names),
        
        function(i) {
          
          span(
            
            result$note_names[i],
            
            style = paste(
              "padding: 7px 10px;",
              "border: 1px solid #777;",
              "border-radius: 5px;",
              "font-weight: bold;"
            )
            
          )
          
        }
        
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # EXPERIMENT 2: MELODIC SEQUENCE
  # ----------------------------------------------------------
  
  output$plot_2 <- renderPlot({
    
    result <- melody_2()
    
    req(result)
    
    melody <- result$melody
    
    plot(
      
      seq_along(melody),
      
      melody,
      
      type = "b",
      
      pch = 19,
      
      xaxt = "n",
      
      yaxt = "n",
      
      xlab = "Note position",
      
      ylab = "Musical note",
      
      main = paste(
        "Melodic sequence:",
        input$profile
      ),
      
      ylim = range(melody) + c(-2, 2)
      
    )
    
    axis(
      1,
      at = seq_along(melody),
      labels = seq_along(melody)
    )
    
    unique_notes <- sort(
      unique(melody)
    )
    
    axis(
      2,
      at = unique_notes,
      labels = midi_to_note(unique_notes),
      las = 1
    )
    
    grid()
    
  })
  
  
  # ----------------------------------------------------------
  # EXPERIMENT 2: PROBABILITY TABLE
  # ----------------------------------------------------------
  
  output$table_2 <- renderTable({
    
    result <- melody_2()
    
    req(result)
    
    table <- result$probability_table
    
    table$Theoretical_probability <- vapply(
      table$Theoretical_probability,
      format_probability,
      character(1)
    )
    
    table$Observed_frequency <- vapply(
      table$Observed_frequency,
      format_probability,
      character(1)
    )
    
    table
    
  })
  
  
  # ----------------------------------------------------------
  # EXPERIMENT 2: AUDIO
  # ----------------------------------------------------------
  
  output$audio_2 <- renderUI({
    
    result <- melody_2()
    
    req(result)
    
    audio_data <- create_audio(
      result$melody
    )
    
    tags$audio(
      
      controls = NA,
      
      src = paste0(
        "data:audio/wav;base64,",
        audio_data
      ),
      
      type = "audio/wav",
      
      style = "width: 100%;"
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # EXPERIMENT 2: MIDI DOWNLOAD
  # ----------------------------------------------------------
  
  output$download_midi_2 <- downloadHandler(
    
    filename = function() {
      
      "musical_profile_experiment_2.mid"
      
    },
    
    content = function(file) {
      
      result <- melody_2()
      
      req(result)
      
      writeBin(
        create_midi(result$melody),
        file
      )
      
    }
    
  )
  
}


# ============================================================
# 9. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
