make_detailed_teaching_undergraduate  <- function(..., detailVector = "Details", bulletVector1 = "Escuela", bulletVector2 = "Cursos") { 
 
 # ... --->  Formato para cada Estudio
 # Curso = list(
 #  "Details" = c(
 #   Fecha,
 #   Universidad
 #  ),
 #  "Escuela" = c(),
 #  "Cursos" = c(), # Los cursos de una misma escuela van en un mismo caracter.   
 #  )
 # )
 
 args <- list(...)
 if (length(args) == 0) stop("Function requires arguments")
 
 for (i in length(args)) {
  if (!is.list(args[[i]])) stop(paste("Argument", i, "is not a list."))
 }
 
 df <- vector("list")
 
 for(i in 1:length(args)) {
  df[[i]] <- tibble(
   What = NA, # NA
   When = NA, # Aquí normalmente va la fecha pero se mueva para que quede más compacto
   With = paste(args[[i]][[detailVector]][2], "\\vspace{-0.6cm}"), # Universidad
   Where = paste(args[[i]][[detailVector]][1], "\\vspace{-0.6cm}"), # Fecha (para que quede más arriba)
   # Why = list(paste0(args[[i]][[bulletVector1]], ": ", # Escuela
   #              "\\text{", args[[i]][[bulletVector2]], "}")) # Cursos
   Why = args[[i]][[bulletVector1]] # Escuela 
  )
 }
 
 cat("\\vspace{-0.6cm}")
 
 do.call(rbind.data.frame, df) |>
  detailed_entries(What, When, With, Where, Why, .protect = FALSE)
 
}

make_detailed_teaching_graduate  <- function(..., detailVector = "Details", bulletVector1 = "Escuela", bulletVector2 = "Cursos") { 
 
 # ... --->  Formato para cada Estudio
 # Curso = list(
 #  "Details" = c(
 #   Fecha,
 #   Universidad,
 #   Carrera,
 #  ),
 #  "Cursos u otros" = c(), # Los cursos de una misma escuela van en un mismo caracter.   
 #  )
 # )
 
 args <- list(...)
 if (length(args) == 0) stop("Function requires arguments")
 
 for (i in length(args)) {
  if (!is.list(args[[i]])) stop(paste("Argument", i, "is not a list."))
 }
 
 df <- vector("list")
 
 for(i in 1:length(args)) {
  df[[i]] <- tibble(
   What = args[[i]][[detailVector]][3], # NA
   When = NA, # Aquí normalmente va la fecha pero se mueva para que quede más compacto
   With = args[[i]][[detailVector]][2], # Universidad
   Where = args[[i]][[detailVector]][1], # Fecha (para que quede más arriba)
   Why = args[[i]][[bulletVector2]] # Cursos u otros
  )
 }
 
 do.call(rbind.data.frame, df) |>
  detailed_entries(What, When, With, Where, Why, .protect = FALSE)
 
}