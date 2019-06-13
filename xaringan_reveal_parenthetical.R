# reveal lines up to `upto` and highlight lines `highlight`
reveal <- function(name, upto, highlight = upto) {
  content <- knitr:::knit_code$get(name)
  content[upto] <- gsub("%>%\\s*(#.+)?$", "\\1", content[upto])
  content[upto] <- gsub("\\+\\s*(#.+)?$", "\\1", content[upto])
  content[upto] <- gsub("->\\s*(#.+)?$", "\\1", content[upto])	
  content[highlight] <- paste(content[highlight], "#<<")
  content[1:upto]
}

partial_knit_chunks <- function(chunk_name) {
  # Create slide for lines 1:N for each line N in the given chunk
  # idx_lines <- seq_along(knitr:::knit_code$get(chunk_name))
  
  code_split <- knitr:::knit_code$get(chunk_name)
  
  idx_lines <- sort(
    c(grep("\\)\\s?[a-z]|\\+|->|%>%\\s?$", code_split),
      grep("\\)\\s?$", code_split))
  )
  
  
  highlight <- list()
  
  for (i in 1:length(idx_lines)) {
    if (i == 1) {  
      highlight[[i]] <- 1
    } else {
      highlight[[i]] <- (idx_lines[i-1]+1):idx_lines[i]
    }
  }
  
  partial_knit_steps <- glue::glue(
    "class: split-40",
    "count: false",
    "",
    ".column[.content[",
    "```{r plot_{{chunk_name}}_{{idx_lines}}, eval=FALSE, code=reveal('{{chunk_name}}', {{idx_lines}}, {{highlight}})}",
    "```",
    "]]",
    ".column[.content.center[",
    "```{r output_{{chunk_name}}_{{idx_lines}}, echo=FALSE, code=reveal('{{chunk_name}}', {{idx_lines}}, {{highlight}})}",
    "```",
    "]]",
    .open = "{{", .close = "}}", .sep = "\n"
  )
  glue::glue_collapse(partial_knit_steps, "\n---\n")
}

apply_reveal <- function(chunk_name){
  paste(knitr::knit(text = partial_knit_chunks(chunk_name)), collapse = "\n")
}



# 
# code = "ggplot(gapminder) +
#   aes(x = lifeExp,
#       y = gdpPercap) +
#   geom_point(col = blue,
#              alpha = .2)
# "
# 
# code_split <- strsplit(code, "\\n")[[1]]
# 
# upto_for_wrapped <-
#   c(grep("\\)\\s?\\+|->|%>%\\s?$", code_split),
#       grep("\\)\\s?$", code_split))
# 
# 
# highlight <- list()
# 
# for (i in 1:length(upto_for_wrapped)) {
#   if(i == 1){
#   highlight[[i]] <- 1
#   }else{
#   highlight[[i]] <- (upto_for_wrapped[i-1]+1):upto_for_wrapped[i]
#   }
# }
# 
