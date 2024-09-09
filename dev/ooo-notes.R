# What do i want from an object oriented R class system?
# opt-in public immutability - neither. Accomplished with private property with active binding in R6
# interior mutability - R6
# type safety - S7
# self-referential methods - R6

# private methods don't have any type safety they can be whatever you want. 
# immutables can only be set at creation and class doesn't matter

# Each .public & .private element must be named 
#   and must be an S7 Object or a function

# .public is put in the private part of R6 has a getter (active binding) 
# and has a setter `set_{arg}`


# helper function to create setters 

library(R6)
library(S7)

make_setter <- function(.name) {
  setter_fmt <- "function(.x) {
    check_is_S7(.x, r67_env[['pub_props']][['%s']])
    private$.%s <- .x
    self
  }"
  rlang::eval_bare(rlang::parse_expr(sprintf(setter_fmt, .name, .name)))
}

new_r67 <- function(
  class, 
  .public = list(),
  .immutable = list(),
  .private = list()
) {

  # we check that all of the elements are named 
  if (!rlang::is_named2(.public) || !rlang::is_named2(.private) || !rlang::is_named2(.immutable)) {
    cli::cli_abort("all properties must be named")
  }

  # we ensure that they are all functions or S7 classes
  for (cls in c(.public, .private)) {
    if (!inherits(cls, c("S7_class", "function"))) {
      cli::cli_abort("Each property must be an {.cls S7} class or a function")
    }
  }

  # we identify which ones are s7 classes
  pub_s7_idx <- vapply(.public, inherits, logical(1), "S7_class")

  # subet to only the s7 classes and methods respectively
  pub_props <- .public[pub_s7_idx]
  pub_methods <- .public[!pub_s7_idx]

  # we store the public s7 classes in an evironment
  r67_env <- rlang::new_environment()
  r67_env[["pub_props"]] <- pub_props

  # store the immutables in the environment as well they'll be accessed via active binding
  r67_env[["immutables"]] <- .immutable

  # make getters for immutable objects
  immut_getters <- lapply(names(.immutable), function(.nm) {
    rlang::eval_bare(
      rlang::parse_expr(
        sprintf("function() r67_env[['immutables']][['%s']]", .nm)
      )
    )
  })

  names(immut_getters) <- names(.immutable)

  # extract the names of these props
  pub_prop_names <- names(pub_props)

  # create a list of setters 
  .pub_setters <- Map(
    make_setter,
    pub_prop_names
  )
  # modify the names to include set_{}
  names(.pub_setters) <- paste0("set_", pub_prop_names)

  # create a named list of functions to act as the getters
  # this will be put into active bindings
  .pub_getters <- Map(function(.name) {
    rlang::eval_bare(
      rlang::parse_expr(
        sprintf("function() private$.%s", .name)
      )
    )
  }, pub_prop_names)

  R6Class(
    class,
    public = c(.pub_setters, pub_methods),
    active = c(.pub_getters, immut_getters),
    private = rlang::new_list(
      length(.pub_getters),
      paste0(".", pub_prop_names)
    )
  )
}


# create some sample s7 object
pet <- S7::new_class("pet")
book <- S7::new_class("book")
person <- S7::new_class("person")
secret <- S7::new_class("secret")


# create lists 
.public <- list(pet = pet, book = book, me = \(){})
.private <- list(internal = function(.x) .x)


my_class <- new_r67(
  "my_r67",
  .public,
  # immutable x vector
  list(x = rnorm(100)),
  .private
)

# create a new instance
x <- my_class$new()

# view immutable value
x$x

# try setting immutable
x$x <- 1L

# try setting mutable prop w/ wrong class
x$set_book(list('x'))

# try setting with s7 object
x$set_book(book())

# now get it 
x$book


