eval(parse(text = "2 + "))

eval(parse(text = "2 + 2"))

eval(parse(text = "q()"))

shiny::validate(
  need(try(calc_expression <- validinp_calculator("q()")), "Please provide a valid calculator expression (+ - / *)")
)
shiny::validate(
  need(try(calc_expression <- validinp_calculator("2 +")), "Please provide a valid calculator expression (+ - / *)")
)
shiny::validate(
  need(try(calc_expression <- validinp_calculator("2 + 2")), "Please provide a valid calculator expression (+ - / *)")
)

need(try(calc_evaluation <- eval(parse(text = "2 + "))), "Please provide a valid calculator expression")



validinp_calculator("hello")
validinp_calculator("2 + 2")
validinp_calculator("2 - 2")
validinp_calculator("2 / 2")
validinp_calculator("2 * 2")
validinp_calculator("q")

validinp_calculator("log(3)")

#' validate calculator input
#' 
#' validate calculator: textInput
#' 
#' @param x input
#' @param pattern that input has to match (regexp)
#' @param many TRUE if more than 1 string (checkboxGroupInput)
#' @return validated input
#' @export
#' @examples
#' \dontrun{validinp_character(input$txt)}
#' \dontrun{validinp_character(input$radiobox, pattern="^((ab)|(cd))$")}
#' \dontrun{validinp_character(input$chkboxgrp, many=TRUE}
validinp_calculator <- function(x, pattern="^[[:digit:]\\. _+-/* log\\(\\)]*$", many=FALSE) {
  if(many && is.null(x)) return(character(0))  ## hack for checkboxGroupInput
  if(!( is.character(x) && (many || length(x)==1) && 
        all(!is.na(x)) && all(grepl(pattern,x)) )) {
    stop("Invalid input from shiny UI")
  }
  x
}