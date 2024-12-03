# legend:
    # error message for now
    ## proposition for new error message

  #When length variable types > 1
expect_snapshot(elic_start(var_names = "var1",
                           var_types = c("p", "N"),
                           elic_types = "3"),
                error = TRUE)
    # x The value provided for `var_types` is a character vector of length 2 but should be a single string with short codes.
    ## x The value provided for `var_types` should be a single string of short codes (i.e. "pN" and not "p","N").

  # When length elicitation types > 1
expect_snapshot(elic_start(var_names = "var1",
                           var_types = "p",
                           elic_types = c("4", "3")),
                error = TRUE)
    # x The value provided for `elic_types` is a character vector of length 2 but should be a single string with short codes.
    ## x The value provided for `elic_types` should be a single string of short codes (i.e. "43" and not "4","3").

# When 1 variable type is not allowed
expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                           var_types = "pqR",
                           elic_type = "1"), #<- why is it singular "type"?
                error = TRUE)
    # ! argument "experts" is missing, with no default
    ## ! Incorrect value for `var_types`:
    ### x "q" not amongst the list of available values for `var_types`.
    ## i See "Variable Types" in `elicitr::elic_start()` for the list of variables available.

# When 2 variable types are not allowed but have the same short code
expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                           var_types = "apa",
                           elic_type = "1"), #<- why is it singular "type"?
                error = TRUE)
    # ! argument "experts" is missing, with no default
    ## ! Incorrect value for `var_types`:
    ## x "a" not amongst the list of available values for `var_types`.
    ## i See "Variable Types" in `elicitr::elic_start()` for the list of variables available.

# When 2 variable types are not allowed and have different short codes
expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                           var_types = "pqG",
                           elic_type = "1"), #<- why is it singular "type"?
                error = TRUE)
    # ! argument "experts" is missing, with no default
    ## ! Incorrect value for `var_types`:
    ## x "q" and "G" not amongst the list of available values for `var_types`.
    ## i See "Variable Types" in `elicitr::elic_start()` for the list of variables available.

# When 1 estimate type is not allowed
expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                           var_types = "p",
                           elic_type = "123"), #<- why is it singular "type"?
                error = TRUE)
    # ! argument "experts" is missing, with no default
    ## ! Incorrect value for `elic_types`:
    ## x "2" not amongst the list of available values for `elic_types`.
    ## i See "Elicitation Types" in `elicitr::elic_start()` for the list of types available.

# When 2 estimate types are not allowed but have the same short code
expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                           var_types = "p",
                           elic_type = "232"), #<- why is it singular "type"?
                error = TRUE)
    # ! argument "experts" is missing, with no default
    ## ! Incorrect value for `elic_types`:
    ## x "2" not amongst the list of available values for `elic_types`.
    ## i See "Elicitation Types" in `elicitr::elic_start()` for the list of variables available.

# When 2 estimate types are not allowed and have different short codes
expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                           var_types = "p",
                           elic_type = "1237"), #<- why is it singular "type"?
                error = TRUE)
    # ! argument "experts" is missing, with no default
    ## ! Incorrect value for `elic_types`:
    ## x "2" and "7" not amongst the list of available values for `elic_types`.
    ## i See "Elicitation Types" in `elicitr::elic_start()` for the list of variables available.

# check_arg_mism()----
# When there are less variables than variable types
expect_snapshot(elic_start(var_names = c("var1"),
                           var_types = c("pR"),
                           elic_type = c("1")), #<- why is it singular "type"?
                error = TRUE)
    # ! argument "experts" is missing, with no default
    ## ! Mismatch between function arguments:
    ## x Number of values in `var_types` should be 1 or same as in `var_names`
    ## i See `elicitr::elic_start()`.

# When there are less variables than estimate types
expect_snapshot(elic_start(var_names = c("var1"),
                           var_types = c("p"),
                           elic_type = c("13")), #<- why is it singular "type"?
                error = TRUE)
    # ! argument "experts" is missing, with no default
    ## ! Mismatch between function arguments:
    ## x Number of values in `elic_types` cannot be 3 if number of values in `var_names` is 1.
    ## i See `elicitr::elic_start()`.

# When there are less variables than variable types and estimate types
expect_snapshot(elic_start(var_names = c("var1"),
                           var_types = c("pR"),
                           elic_type = c("13")), #<- why is it singular "type"?
                error = TRUE)
    # ! argument "experts" is missing, with no default
    ## I would have 2 error messages here instead of one combining both

# When there are more variables than variable types (elic_types is recycled)
expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                           var_types = c("pN"),
                           elic_type = c("1")), #<- why is it singular "type"?
                error = TRUE)
    # ! argument "experts" is missing, with no default
    ## ! Mismatch between function arguments:
    ## x Number of values in `var_types` should be 1 or same as in `var_names`.
    ## i See `elicitr::elic_start()`.

# When there are more variables than estimate types (var_types is recycled) #<- I think this was wrong (you had elic_types = c("13"))
expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                           var_types = c("p"),
                           elic_type = c("111")), #<- why is it singular "type"?
                error = TRUE)
    # ! argument "experts" is missing, with no default
    ## ! `var_types` "p" is recycled for all variables in var_names

# When there are more variables than elicitation types (elic_types is recycled)
expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                           var_types = c("ppp"),
                           elic_type = c("1")), #<- why is it singular "type"?
                error = TRUE)
# ! argument "experts" is missing, with no default
## ! `elic_types` "1" is recycled for all variables in var_names

# When there are more variables than elicitation types
expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                           var_types = c("ppp"),
                           elic_type = c("13")), #<- why is it singular "type"?
                error = TRUE)
    # ! argument "experts" is missing, with no default
    ## ! Mismatch between function arguments:
    ## x Number of values in `elic_types` should be 1 or same as in `var_names`.
    ## i See `elicitr::elic_start()`.











