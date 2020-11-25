library(dplyr)
library(rlang)
library(palmerpenguins)

# what the heck is !! !!! {{ }} :=
# why do i see the ~ in places other than model fitting?

# data(package = 'palmerpenguins')

penguins


# selecting columns ----

penguins[, "species"]
penguins[, c("species", "island", "bill_length_mm")]

penguins$species
penguins[, "species", drop = TRUE]

base::subset(penguins,
             select = species)
base::subset(x = penguins,
             select = c(species, island, bill_length_mm))

dplyr::select(penguins, species)
dplyr::select(penguins, species, island, bill_length_mm)

penguins[, 1]
penguins[, c(1, 3, 5)]

# understand how expressions work

3 + 3

quote(3 + 3)
ex <- quote(3 + 3)
ex
eval(ex)

rlang::expr(3 + 3)
ex <- rlang::expr(3 + 3)
ex
eval(ex)

ex
str(ex)

ex <- quote(3)
str(ex)

ex <- quote(species)
str(ex)

# expressions is either a: call, symbol, constant, or pairlist


# setup select

col <- "species"
penguins[, col]
penguins[, "species"]
penguins[, species]

# tibbles only
penguins[, as.name("species")]
penguins[, quote(species)]

cars[, as.name("speed")]
cars[, quote(sp)]

my_select <- function(data, col) {
  return(
    data[, col, drop = FALSE]
  )
}

my_select(penguins, "species")
my_select(penguins, species)
my_select(penguins, quote(species))

my_select <- function(data, col) {
  return(
    data[, quote(col), drop = FALSE]
  )
}

my_select(penguins, species) # quote passed in col, not species

# enexpr
# enriched expr
# works with function arguments
# quote the expression i gave you, not the expression of the function param name / signature
# "automatically quotes its ... argument)

my_select <- function(data, col) {
  return(
    data[, rlang::enexpr(col), drop = FALSE]
  )
}

my_select(penguins, species)

my_select <- function(data, col) {
  col <- rlang::enexpr(col)
  return(
    data[, col, drop = FALSE]
  )
}

my_select <- function(data, col) {
  col <- rlang::enexpr(col)
  idx <- which(names(data) %in% as.character(col))
  return(
    data[, idx, drop = FALSE]
  )
}

my_select(penguins, species)

my_select <- function(data, ...) {
  cols <- rlang::enexprs(...)
  cols_char <- as.vector(cols, mode = "character")
  idx <- which(names(data) %in% cols_char)
  return(
    data[, idx, drop = FALSE]
  )
}

my_select(penguins, species)
my_select(penguins, species, year, sex, island)

# https://vctrs.r-lib.org/reference/vec_as_location.html

# Learned about expressions
# Creating expressions
# Capturing expressions within a function

# user passes an expression, to be used in a function does not use expressions


# Quasiquotation
# Quasiquotation is one of the three pillars of tidy evaluation, quosures, and the data mask


# quoting and unquoting

my_select <- function(data, ...) {
  cols <- rlang::enexprs(...)
  cols_char <- as.vector(cols, mode = "character") # unquoting the arguments
  idx <- which(names(data) %in% cols_char)
  return(
    data[, idx, drop = FALSE]
  )
}

col_name <- "species"
penguins[, c(col_name, "island", "year")]

dplyr::select(penguins, col_name, island, year)



my_select(penguins, island, year)
my_select(penguins, col_name, island, year)

# need a way to treat the variable as the variable not as the quoted term
# i.e., we want to unquote col_name since the input is automatically quoted using `enexprs`
# i.e., i want to replace col_name with species

my_select(penguins, "island", "year")
my_select(penguins, col_name, "island", "year")
my_select(penguins, !!col_name, island, year)

# how do we know what the function needs?
# pass the arguemnt outside the function on its own
my_select(penguins, island)
island

# Unquoting allows you to selectively evaluate parts of the expression that would otherwise be quoted

# think of !!! as the !! for ...
# in python this is similar to quoting + "unpacking"

cols <- exprs(species, island, year)
my_select(penguins, cols)
my_select(penguins, !!!cols)

# dplyr::select has an additional check
dplyr::select(penguins, col_name, island, year)
dplyr::select(penguins, !!col_name, island, year) ## !!unquotes a single argument


# quosures + data mask
# evaluation

# another way to call a function
# build the code (call) and then evaluate the call

quote(my_select(penguins, species, island, year)) %>%
  eval()

cols <- exprs(species, island, year)

quote(my_select(penguins, !!!cols)) %>%
  eval()

base::call("my_select", penguins, !!!cols) %>%
  eval()

# call2 allows for tidydots
rlang::call2(my_select, penguins, !!!cols) %>%
  eval()

# quosures = expression + envrionment -----

# environments

e <- new.env()
e$x <- 3
x
e$x
eval(quote(3 + x))
eval(quote(3 + x), envir = e)

## formula = expression + environment

~ 3 + 3
form <- ~ 3 + 3
form
form[[1]]
form[[2]]
eval(form[[2]])

attributes(form)

form <- ~ 3 + x
form
form[[1]]
form[[2]]
environment(form)

e <- new.env()
e$x <- 10
environment(form) <- e
eval(expr = form[[2]],
     envir = environment(form))

lin <- y ~ mx + b
lin[[1]]
lin[[2]]
lin[[3]]

## rlang quosures
# usually ussed in a function: enquo and enquos
# you can create quosures outside of a function using quo and quos,
# but they are usually not used (only kept for consistency)
# and you can manually create a quosure from it's parts using new_quosure()

#> mean[1]
#Error in mean[1] : object of type 'closure' is not subsettable

# quosure = quoting + clousures
# clousure = "thing" + environment
# quosure = quoting + environment

form <- ~ 3 + x
form
rlang::get_expr(form)
rlang::get_env(form)

e <- new.env()
e$x <- 10
environment(form) <- e
eval(expr = form[[2]],
     envir = environment(form))

q <- rlang::new_quosure(expr = rlang::expr(3 + x),
                        env = e)
rlang::is_quosure(q)
rlang::eval_tidy(q)

class(q) # subclass of formula, allows quasiquotation
# in a formula you woule normally create the string and pass into as.formula

# data mask -----


# quote = rlang::expr
# substitute = rlang::enexpr
# alist = rlang::exprs
# as.list(substitute(x)) = rlang::enexprs


my_select <- function(data, ...) {
  dots <- enquos(...)

  vars <- as.list(set_names(seq_along(data), names(data)))
  cols <- unlist(purrr::map(dots, eval_tidy, vars))

  data[, cols, drop = FALSE]
}

my_select <- function(data, ...) {
  cols <- rlang::enexprs(...)
  cols_char <- as.vector(cols, mode = "character") # unquoting the arguments
  idx <- match(cols_char, names(data))

  # if there are any missing values for the column index
  if (any(is.na(idx))){
    # iterate through the column index
    for (i in seq_along(idx)) {
      # if the column index is mising
      if (is.na(idx[[i]])) {
        # evaluate the expression in the caller environment
        unquoted_name <- rlang::eval_tidy(cols[[i]], env = rlang::caller_env())
        # find the index position of the evaluated variable
        idx[[i]] <- match(unquoted_name, names(data))
      }
    }
  }

  return(
    data[, idx, drop = FALSE]
  )
}

col_name <- "species"
my_select(penguins, !!col_name, island, year)
my_select(penguins, col_name, island, year)

# dplyr::select example

df <- data.frame(a = 1, b = 2, c = 3, d = 4, e = 5)

df <- data.frame(a = 1:10, b = LETTERS[1:10], c = 101:110, d = 51:60, e = letters[11:20])

select2 <- function(data, ...) {
  dots <- enquos(...)

  vars <- as.list(set_names(seq_along(data), names(data)))
  browser()
  cols <- unlist(purrr::map(dots, eval_tidy, vars))

  data[, cols, drop = FALSE]
}

select2(df, b:d)


# :=
# as used in dplyr::mutate

# tl;dr ----

# pass a quoted term into another quoted term, enquo + !! or use {{ }}
# pass a quoted term into a string (e.g., formula, aes), construnt string: enquo + as.character, as.formula, aes_string
my_func <- function(data, col) {
  q <- rlang::enexpr(col)  # capture expression
  s <- rlang::as_string(q) # convert to string
  return(
    ggplot(data = data, aes_string(x = s)) +
      geom_histogram() +
      ggtitle(s)
  )
}

my_func(cars, speed)


my_func <- function(data, col) {
  q <- rlang::enexpr(col)  # capture expression
  s <- rlang::as_string(q) # convert to string
  print(q)
  return(data[, s, drop=FALSE])
}
my_func(cars, speed)

# pass a string into a quoted term, enquo
# pass a string into a string, normally what we've done, pass it along like normal!
