# ======================================================================
#  CHATBOT LMB - Versión Final Actualizada (Corregida)
#  - UI tipo burbujas
#  - Respuestas naturales del modelo
#  - Tablas como texto dentro de la burbuja
#  - Código ejecutado en sandbox (tidyverse)
#  - RAG + aliases de equipos + normalize_team
#  - Código solo visible si el usuario lo pide
# ======================================================================

library(shiny)
library(httr2)
library(dplyr)
library(glue)

# ======================================================================
#  OpenAI: Chat
# ======================================================================

openai_chat <- function(messages, model = "gpt-4o-mini", temperature = 0.1) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("OPENAI_API_KEY no configurada")
  
  url <- "https://api.openai.com/v1/chat/completions"
  
  max_retries <- 5
  wait_seconds <- 2
  
  for (i in 1:max_retries) {
    cat("🔥 LLAMANDO A OPENAI (intento", i, ")\n")
    
    req <- request(url) |>
      req_headers(
        Authorization = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(list(
        model = model,
        messages = messages,
        temperature = temperature
      ))
    
    resp <- tryCatch(req_perform(req), error = function(e) e)
    
    if (inherits(resp, "error")) {
      if (grepl("429", resp$message)) {
        message(glue("⚠️ 429 Too Many Requests — reintentando en {wait_seconds}s (intento {i}/{max_retries})"))
        Sys.sleep(wait_seconds)
        wait_seconds <- wait_seconds * 2
        next
      }
      stop(resp$message)
    }
    
    res <- resp_body_json(resp)
    return(res$choices[[1]]$message$content)
  }
  
  stop("❌ OpenAI falló después de varios reintentos.")
}

# ======================================================================
#  OpenAI: Embeddings (para RAG)
# ======================================================================

openai_embed <- function(texts, model = "text-embedding-3-large") {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("OPENAI_API_KEY no configurada")
  
  url <- "https://api.openai.com/v1/embeddings"
  
  req <- request(url) |>
    req_headers(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(list(
      model = model,
      input = texts
    ))
  
  resp <- req_perform(req)
  res  <- resp_body_json(resp)
  
  do.call(rbind, lapply(res$data, function(e) unlist(e$embedding)))
}

# ======================================================================
#  RAG: índice y recuperación
# ======================================================================

.rag_cache <- NULL

load_rag_index <- function(path = "data/rag_embeddings.rds") {
  if (!file.exists(path)) {
    message("ℹ️ Archivo RAG no encontrado: ", path)
    return(NULL)
  }
  x <- readRDS(path)
  
  emb_mat <- NULL
  if (is.matrix(x$embeddings)) {
    emb_mat <- x$embeddings
  } else if (is.list(x$embeddings)) {
    emb_mat <- do.call(rbind, lapply(x$embeddings, function(e) unlist(e$embedding)))
  } else {
    stop("Formato de embeddings no soportado en rag_embeddings.rds")
  }
  
  list(corpus = x$corpus, emb = emb_mat)
}

get_rag_index <- function() {
  if (is.null(.rag_cache)) {
    .rag_cache <<- load_rag_index()
  }
  .rag_cache
}

cosine_sim <- function(a, b) {
  a <- as.numeric(a)
  if (is.null(dim(b))) {
    b <- matrix(as.numeric(b), nrow = 1)
  } else {
    b <- apply(b, 1, as.numeric)
  }
  num <- b %*% a
  den <- sqrt(sum(a * a)) * sqrt(rowSums(b * b))
  as.numeric(num / den)
}

rag_retrieve <- function(query, k = 5) {
  idx <- get_rag_index()
  if (is.null(idx)) return(NULL)
  
  q_emb <- openai_embed(query)
  sims  <- cosine_sim(q_emb[1, ], idx$emb)
  ord   <- order(sims, decreasing = TRUE)
  top_k <- head(ord, k)
  
  idx$corpus[top_k]
}

# ======================================================================
#  ALIASES DE EQUIPOS + normalize_team
# ======================================================================

team_aliases <- list(
  "JAL"   = c("Charros de Jalisco", "JAL", "Charros", "Jalisco"),
  "OAX"   = c("Guerreros de Oaxaca", "OAX", "Guerreros", "Oaxaca"),
  "PUE"   = c("Pericos de Puebla", "PUE", "Pericos", "Puebla"),
  "CAM"   = c("Piratas de Campeche", "CAM", "Piratas", "Campeche"),
  "MVA"   = c("Acereros de Monclova", "MVA", "Acereros", "Monclova"),
  "AGS"   = c("Rieleros de Aguascalientes", "AGS", "Rieleros", "Aguascalientes"),
  "YUC"   = c("Leones de Yucatan", "YUC", "Leones", "Yucatan"),
  "TIJ"   = c("Toros de Tijuana", "TIJ", "Toros", "Tijuana"),
  "MTY"   = c("Sultanes de Monterrey", "MTY", "Sultanes", "Monterrey"),
  "LEO"   = c("Bravos de Leon", "LEO", "Bravos", "Leon"),
  "MEX"   = c("Diablos Rojos del Mexico", "MEX", "Diablos Rojos", "Mexico", "Diablos"),
  "SLT"   = c("Saraperos de Saltillo", "SLT", "Saraperos", "Saltillo"),
  "LAR"   = c("Tecos de los Dos Laredos", "LAR", "Tecos", "Dos Laredos"),
  "TIG"   = c("Tigres de Quintana Roo", "TIG", "Tigres", "Quintana Roo"),
  "TAB"   = c("Olmecas de Tabasco", "TAB", "Olmecas", "Tabasco"),
  "DUR"   = c("Caliente de Durango", "DUR", "Caliente", "Durango"),
  "QRO"   = c("Conspiradores de Queretaro", "QRO", "Conspiradores", "Queretaro"),
  "VER"   = c("El Aguila de Veracruz", "VER", "El Aguila", "Veracruz", "Águila de Veracruz", "Aguila de Veracruz"),
  "CHI"   = c("Dorados de Chihuahua", "CHI", "Dorados", "Chihuahua"),
  "LAG"   = c("Algodoneros Union Laguna", "LAG", "Algodoneros", "Union Laguna", "Unión Laguna", "Laguna")
)

normalize_team <- function(x, aliases = team_aliases) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NA_character_)
  q <- tolower(trimws(x))
  for (canon in names(aliases)) {
    vals <- tolower(trimws(unlist(aliases[[canon]])))
    if (q %in% vals) return(canon)
  }
  x
}

team_aliases_text <- paste(
  "Catálogo de aliases de equipos (canon => aliases):",
  paste(
    vapply(
      names(team_aliases),
      function(canon) paste0("- ", canon, " => ", paste(team_aliases[[canon]], collapse = ", ")),
      FUN.VALUE = character(1)
    ),
    collapse = "\n"
  ),
  sep = "\n"
)

# ======================================================================
#  SANDBOX: dplyr + base + stats + utils
# ======================================================================

make_sandbox_env <- function(datasets) {
  env <- new.env(parent = emptyenv())
  
  # dplyr
  allowed_dplyr <- c(
    "filter","select","mutate","arrange","group_by","summarise","summarize","ungroup",
    "left_join","inner_join","full_join",
    "slice","slice_head","slice_max","slice_min","rename","distinct"
  )
  for (fn in allowed_dplyr) {
    assign(fn, get(fn, envir = asNamespace("dplyr")), envir = env)
  }
  assign("%>%", dplyr::`%>%`, envir = env)
  
  # base
  base_fns <- list(
    sum = base::sum,
    min = base::min,
    mean = base::mean,
    max = base::max,
    abs = base::abs,
    round = base::round,
    floor = base::floor,
    ceiling = base::ceiling,
    c = base::c,
    ":" = base::":",
    "+" = base::"+",
    "-" = base::"-",
    "*" = base::"*",
    "/" = base::"/",
    "==" = base::"==",
    "!=" = base::"!=",
    "<" = base::"<",
    ">" = base::">",
    "<=" = base::"<=",
    ">=" = base::">=",
    as.numeric = base::as.numeric,
    as.character = base::as.character,
    as.integer = base::as.integer,
    is.na = base::is.na,
    ifelse = base::ifelse
  )
  for (nm in names(base_fns)) assign(nm, base_fns[[nm]], envir = env)
  
  # stats
  stats_fns <- list(
    median = stats::median,
    sd = stats::sd,
    var = stats::var,
    quantile = stats::quantile
  )
  for (nm in names(stats_fns)) assign(nm, stats_fns[[nm]], envir = env)
  
  # utils
  utils_fns <- list(
    head = utils::head,
    tail = utils::tail
  )
  for (nm in names(utils_fns)) assign(nm, utils_fns[[nm]], envir = env)
  
  # Aliases tools
  assign("team_aliases", team_aliases, envir = env)
  assign("normalize_team", normalize_team, envir = env)
  
  # Data
  for (nm in names(datasets)) assign(nm, datasets[[nm]], env)
  
  env
}

safe_eval_code <- function(code, sandbox_env) {
  blacklist <- c("system", "file", "unlink", "download", "socket", "open.connection", "setwd")
  if (any(grepl(paste(blacklist, collapse = "|"), code))) {
    stop("⚠️ Código inseguro detectado.")
  }
  eval(parse(text = code), envir = sandbox_env)
}

# ======================================================================
#  EXTRACTOR ROBUSTO DE CÓDIGO + SANITIZADOR
# ======================================================================

extract_r_code <- function(txt) {
  pattern <- "```+\\s*(r|R)?[^\\n]*\\n([\\s\\S]*?)```"
  m <- regexpr(pattern, txt, perl = TRUE)
  if (m[1] == -1) return(NULL)
  
  captures <- attr(m, "capture.start")
  lengths  <- attr(m, "capture.length")
  
  start <- captures[1, 2]
  len   <- lengths[1, 2]
  
  code <- substr(txt, start, start + len - 1)
  trimws(code)
}

sanitize_code <- function(code) {
  code <- gsub("library\\([^\\)]+\\)", "", code)
  code <- gsub("require\\([^\\)]+\\)", "", code)
  code <- gsub("^[ ]*[A-Za-z0-9_\\.]+[ ]*<-.*$", "", code, perl = TRUE)
  code <- gsub("\n{2,}", "\n", code)
  trimws(code)
}

# ======================================================================
#  generate_llm_reply(): RAG + OpenAI + ejecución en sandbox
# ======================================================================

generate_llm_reply <- function(user_msg, datasets) {
  
  # ----------------------------------------------------------
  # 1. Detectar si el usuario pide explícitamente CÓDIGO
  # ----------------------------------------------------------
  wants_code <- grepl(
    "c[oó]digo|code|script|en r\\b|en r\\?|como hacerlo en r|cómo hacerlo en r|mu[eé]strame.*c[oó]digo|ens[eñ]ame.*c[oó]digo",
    tolower(user_msg)
  )
  
  # ----------------------------------------------------------
  # 2. RAG (si existe índice)
  # ----------------------------------------------------------
  rag_chunks <- NULL
  rag_text   <- "Sin contexto adicional."
  if (exists("rag_retrieve")) {
    try({
      rag_chunks <- rag_retrieve(user_msg, k = 5)
      if (!is.null(rag_chunks)) {
        rag_text <- paste(paste0("- ", rag_chunks), collapse = "\n")
      }
    }, silent = TRUE)
  }
  
  # ----------------------------------------------------------
  # 3. Esquema real de tablas
  # ----------------------------------------------------------
  schema_parts <- c()
  add_schema <- function(name) {
    if (!is.null(datasets[[name]])) {
      cols <- colnames(datasets[[name]])
      schema_parts <<- c(schema_parts, paste0("- ", name, ": ", paste(cols, collapse = ", ")))
    }
  }
  add_schema("hitting")
  add_schema("pitching")
  add_schema("fielding")
  add_schema("team_hitting")
  add_schema("team_pitching")
  add_schema("team_fielding")
  add_schema("rosters")
  add_schema("trans")
  add_schema("game_logs")
  
  schema_text <- paste(
    "Esquema de tablas disponibles (columnas reales):",
    paste(schema_parts, collapse = "\n"),
    "\nREGLA: Usa SOLO estos nombres de columnas. Si falta algo, dilo claramente.",
    sep = "\n"
  )
  
  # ----------------------------------------------------------
  # 4. System prompt ÚNICO (claro y compacto)
  # ----------------------------------------------------------
  sys_prompt <- glue("
Eres un asistente experto en estadísticas de la Liga Mexicana de Béisbol (LMB).
Responde SIEMPRE en tono natural en español.

Tienes acceso directo a estas tablas en R dentro de la app Shiny:
{paste(names(datasets), collapse = ', ')}

Y este es el esquema REAL de las columnas:
{schema_text}

Además tienes:
- Contexto aproximado RAG (no siempre actualizado): 
{rag_text}

- Catálogo de aliases de equipos (canon => aliases):
{team_aliases_text}

REGLAS IMPORTANTES (OBLIGATORIAS):

1) Cuando la pregunta requiera un cálculo con datos (líderes, top N, promedios, etc.):
   - DEBES generar INTERNAMENTE un único bloque de código R tidyverse entre ```r y ``` 
     para obtener la respuesta a partir de las tablas.
   - Ese bloque debe usar SIEMPRE el operador %>%.
   - NO uses library() ni require().
   - NO uses asignaciones (<- ni =) dentro del bloque; el último paso debe devolver un data.frame o vector.
   - Usa SIEMPRE los nombres de columnas reales del esquema. 
     Ejemplos típicos: Year, Team, Name, HR, etc.
   - Si el usuario menciona un equipo por alias (\"Toros\", \"Tijuana\", \"TIJ\"):
       * Usa normalize_team(\"ALIAS\", team_aliases) para obtener el código canónico (ej. \"TIJ\").
       * Luego filtra por la columna de equipo adecuada, por ejemplo Team == \"TIJ\".

2) Para responder al usuario:
   - Primero calcula usando las tablas.
   - Después responde con un TEXTO NATURAL que explique el resultado (quién fue líder, cuántos HR, con qué equipo, etc.).
   - SÓLO debes mostrar el bloque ```r ... ``` al usuario si él lo pide explícitamente
     (por ejemplo: \"muéstrame el código\", \"dame el código en R\", \"¿cómo se hace en R?\", etc.).

3) Si el usuario NO pide código:
   - En tu mensaje de respuesta SIGUE INCLUYENDO el bloque ```r ... ``` para que el sistema pueda extraerlo.
   - PERO ese bloque se considera INTERNO: el sistema lo ocultará al usuario.
   - Asegúrate de que tu texto natural explique el resultado usando los datos calculados.

4) Si el usuario SÍ pide código:
   - Incluye un único bloque válido ```r ...``` con el código.
   - Explica brevemente qué hace el código.

5) Si los datos de la app contradicen tu conocimiento previo o tu contexto RAG:
   - SIEMPRE confía en las tablas de la app (hitting, pitching, etc.) como fuente de verdad.
   - No inventes estadísticas; si no puedes calcular algo con las columnas disponibles, dilo explícitamente.

6) Fechas y temporadas:
   - Asume que hay datos al menos hasta la temporada 2025 en las tablas de la app.
   - No digas que solo tienes datos hasta 2023 a menos que realmente falten filas en las tablas.

Responde ahora la siguiente pregunta del usuario siguiendo TODAS estas reglas.
")
  
  # ----------------------------------------------------------
  # 5. Construir mensajes para OpenAI
  # ----------------------------------------------------------
  messages <- list(
    list(role = "system", content = sys_prompt),
    list(role = "user",   content = user_msg)
  )
  
  # ----------------------------------------------------------
  # 6. Llamada al modelo
  # ----------------------------------------------------------
  reply_raw <- openai_chat(messages)
  reply_clean <- gsub("\\\\n", "\n", reply_raw)
  reply_clean <- gsub("\\\\t", "\t", reply_clean)
  
  # ----------------------------------------------------------
  # 7. Extraer bloque de código R del mensaje del modelo
  # ----------------------------------------------------------
  code_raw <- extract_r_code(reply_clean)
  
  # Si no hay código, devolvemos solo el texto natural tal cual
  if (is.null(code_raw)) {
    return(list(
      text       = reply_clean,
      table_text = NULL,
      code       = NULL
    ))
  }
  
  # Sanitizar código (sin library, sin asignaciones, etc.)
  code_sanitized <- sanitize_code(code_raw)
  
  cat("=============================================\n")
  cat("CÓDIGO R RECIBIDO DEL MODELO:\n", code_raw, "\n\n")
  cat("CÓDIGO R SANITIZADO QUE SE EJECUTARÁ:\n", code_sanitized, "\n")
  cat("=============================================\n")
  
  # ----------------------------------------------------------
  # 8. Preparar sandbox y ejecutar código
  # ----------------------------------------------------------
  sandbox <- make_sandbox_env(datasets)
  
  result <- tryCatch(
    safe_eval_code(code_sanitized, sandbox),
    error = function(e) e
  )
  
  # ----------------------------------------------------------
  # 9. Preparar texto natural para el usuario
  #    - Si NO quiere código, ocultamos el bloque ```r ... ```
  #    - Si SÍ quiere código, devolvemos el mensaje completo
  # ----------------------------------------------------------
  if (wants_code) {
    text_for_user <- reply_clean
  } else {
    text_for_user <- gsub("```+\\s*[rR]?[^\\n]*\\n[\\s\\S]*?```", "", reply_clean, perl = TRUE)
    text_for_user <- trimws(text_for_user)
  }
  
  # ----------------------------------------------------------
  # 10. Manejo de error en ejecución
  # ----------------------------------------------------------
  if (inherits(result, "error")) {
    msg <- paste0(
      text_for_user,
      if (nzchar(text_for_user)) "\n\n" else "",
      "❌ Hubo un error al ejecutar el cálculo con los datos de la app:\n",
      result$message
    )
    return(list(
      text       = msg,
      table_text = NULL,
      code       = if (wants_code) code_sanitized else NULL
    ))
  }
  
  # ----------------------------------------------------------
  # 11. Si el resultado es un data.frame o vector/lista
  #     lo convertimos a texto de tabla
  # ----------------------------------------------------------
  if (is.data.frame(result) || is.list(result) || is.atomic(result)) {
    table_text <- paste(capture.output(print(result)), collapse = "\n")
    return(list(
      text       = text_for_user,
      table_text = table_text,
      code       = if (wants_code) code_sanitized else NULL
    ))
  }
  
  # ----------------------------------------------------------
  # 12. Caso raro: resultado no legible
  # ----------------------------------------------------------
  return(list(
    text       = if (nzchar(text_for_user)) text_for_user else "La consulta se ejecutó pero no produjo un resultado legible.",
    table_text = NULL,
    code       = if (wants_code) code_sanitized else NULL
  ))
}


# ======================================================================
#  UI: chatbot tipo burbujas
# ======================================================================

chatbotUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      id = ns("chat-container"),
      style = "padding:15px;",
      
      div(
        style = "height:420px; overflow-y:auto; border:1px solid #ddd; padding:10px; border-radius:6px; background:white;",
        uiOutput(ns("chat_history"))
      ),
      
      br(),
      
      # Sugerencias simples
      div(
        style="margin-bottom:10px;",
        tags$span("Sugerencias:", style="font-weight:bold;"),
        div(
          style="display:flex; gap:8px; flex-wrap:wrap; margin-top:6px;",
          actionButton(ns("sug1"), "Top 5 HR en 2024", class="btn btn-outline-primary btn-sm"),
          actionButton(ns("sug2"), "Líder de HR 2023", class="btn btn-outline-primary btn-sm"),
          actionButton(ns("sug3"), "Mejores pitchers 2023", class="btn btn-outline-primary btn-sm"),
          actionButton(ns("sug4"), "wOBA líderes 2024", class="btn btn-outline-primary btn-sm")
        )
      ),
      
      div(
        style = "display:flex; gap:10px;",
        textAreaInput(
          ns("msg"), 
          NULL,
          placeholder = "Escribe tu pregunta aquí...",
          width = "100%", 
          rows = 2
        ),
        actionButton(ns("send"), "Enviar", class = "btn btn-primary")
      )
    )
  )
}

# ======================================================================
#  SERVER: chatbot
# ======================================================================

chatbotServer <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    history <- reactiveVal(list())
    
    bubble <- function(text, role = "assistant") {
      if (role == "user") {
        div(
          style="
            background:#d7e3ff;
            margin:8px 0;
            padding:10px 14px;
            border-radius:12px;
            max-width:80%;
            margin-left:auto;
            font-size:15px;
            white-space:pre-wrap;
          ",
          HTML(text)
        )
      } else {
        div(
          style="
            background:white;
            margin:8px 0;
            padding:10px 14px;
            border-radius:12px;
            max-width:80%;
            margin-right:auto;
            border:1px solid #eee;
            font-size:15px;
            white-space:pre-wrap;
          ",
          HTML(text)
        )
      }
    }
    
    output$chat_history <- renderUI({
      msgs <- history()
      if (length(msgs) == 0) return(div("Bienvenido al asistente LMB."))
      tagList(lapply(msgs, function(m) bubble(m$text, m$role)))
    })
    
    # Sugerencias
    observeEvent(input$sug1, { updateTextAreaInput(session, "msg", value = "Top 5 HR en 2024") })
    observeEvent(input$sug2, { updateTextAreaInput(session, "msg", value = "¿Quién fue el líder de HR en 2023?") })
    observeEvent(input$sug3, { updateTextAreaInput(session, "msg", value = "Top 5 en ponches de 2023") })
    observeEvent(input$sug4, { updateTextAreaInput(session, "msg", value = "Top 10 wOBA en 2024") })
    
    throttled <- shiny::throttle(reactive(input$send), 1200)
    
    observeEvent(throttled(), {
      req(input$msg)
      user_msg <- input$msg
      
      # Burbuja del usuario
      history( append(history(), list(list(role="user", text=user_msg))) )
      
      # Limpiar input
      updateTextAreaInput(session, "msg", value = "")
      
      # Llamar a generate_llm_reply
      reply <- generate_llm_reply(user_msg, datasets)
      
      # Texto natural (si hay)
      if (!is.null(reply$text) && nzchar(reply$text)) {
        history( append(history(), list(list(role="assistant", text=reply$text))) )
      }
      
      # Tabla en texto (si hay)
      if (!is.null(reply$table_text)) {
        history( append(history(), list(list(
          role = "assistant",
          text = paste0("<pre>", reply$table_text, "</pre>")
        ))) )
      }
      
      # Log de código en consola
      if (!is.null(reply$code)) {
        cat("=============================================\n")
        cat("CÓDIGO EJECUTADO POR EL CHATBOT:\n")
        cat(reply$code, "\n")
        cat("=============================================\n")
      }
    })
    
  })
}

# ======================================================================
# FIN DEL ARCHIVO
# ======================================================================
