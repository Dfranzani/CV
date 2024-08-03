make_detailed_section <- function(..., order = c(3, 2, 1, 4), detailVector = "Details", bulletVector = "Bullets") { 
 
 # ... --->  Formato para cada Estudio
 # Estudio = list(
 #  "Details" = c(
 #   Grado o título,
 #   Fechas,
 #   Universidad,
 #   Locación
 #  ),
 #  "Bullets" = NA # En caso de querer agregar descriptivos en un vector separado por comas.
 # )
 
 args <- list(...)
 if (length(args) == 0) stop("Function requires arguments")
 
 for (i in length(args)) {
  if (!is.list(args[[i]])) stop(paste("Argument", i, "is not a list."))
 }
 
 df <- vector("list")
 
 for(i in 1:length(args)) {
  df[[i]] <- tibble(
   What = args[[i]][[detailVector]][[order[[1]]]], 
   When = args[[i]][[detailVector]][[order[[2]]]], 
   With = args[[i]][[detailVector]][[order[[3]]]], 
   Where = args[[i]][[detailVector]][[order[[4]]]], 
   Why = args[[i]][[bulletVector]][seq_along(args[[i]][[bulletVector]])]
  )
 }
 
 do.call(rbind.data.frame, df) |>
  detailed_entries(What, When, With, Where, Why, .protect = FALSE)
 
}