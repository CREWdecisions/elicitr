# elic_cont class

The `elic_cont` class is a container for the elicitation data. It is
used to store the data collected during the elicitation process and
their matadata.

## Object elements

There are 5 elements in the `elic_cont` object:

- `var_names`: character vector with the name of the estimated
  variables.

- `var_types`: character string with short codes indicating the variable
  type.

- `elic_types`: character string with short codes indicating the
  elicitation type.

- `experts`: numeric indicating the number of experts participating in
  the elicitation process.

- `data`: list with the data collected during the elicitation process.
  The list has two elements, `round_1` and `round_2`, which store the
  data collected during the first and second round of the elicitation
  process, respectively.

Moreover, the object has a `title` attribute that binds a name to the
object.
