# The elic_cont class: storing continuous elicitation metadata and data

An `elic_cont` object is an object of class `elic_cont`. It keeps the
information about your elicitation and the experts' responses together.
It acts as a folder for one elicitation project.

## Details

The object holds two kinds of information:

- \**Metadata* describe the elicitation: what the experts are estimating
  (the names of the variables), the expected type of variables (which
  values are expected), the elicitation type of each variable (one-,
  three- or four-point elicitations) and how many experts are expected
  to participate.

- \**Data* are the responses provided by the experts, stored separately
  in a maximum of two elicitation rounds (round 1 and round 2).

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

## Creating the object and adding responses

First, use
[cont_start()](https://crewdecisions.github.io/elicitr/reference/cont_start.md)
to create the object by providing the metadata (aka the description of
expected responses). Once this is done, the object describes your
elicitation but contains no responses. Then use
[cont_add_data()](https://crewdecisions.github.io/elicitr/reference/cont_add_data.md)
to add the responses for the first round. If your elicitation includes a
second round, add those responses to the same object with another call
to
[cont_add_data()](https://crewdecisions.github.io/elicitr/reference/cont_add_data.md).

Save the result of each call using `<-`. For example,
`my_elicitation <- cont_add_data(my_elicitation, responses, round = 1)`
updates the round `1` of the `elic_cont` object called `my_elicitation`
with the responses stored in the data file `responses`. Use these
functions to create and update the object rather than editing its
contents manually, so that the package can check the information.

## Why keep metadata and data together?

The metadata tell the package what responses are expected for each
variable. This allows
[elicitr](https://crewdecisions.github.io/elicitr/reference/elicitr-package.md)
to check whether data subsequently added in the `elic_cont` object fit
the expectations. This way, data verification happens automatically
instead of having to be done manually by the user.

For example, if a variable is defined in the metadata as a probability
four-point elicitation,
[elicitr](https://crewdecisions.github.io/elicitr/reference/elicitr-package.md)
knows that estimates must fall between zero for that variable and that
minimum, maximum, best-guess and confidence estimates must be provided.
[elicitr](https://crewdecisions.github.io/elicitr/reference/elicitr-package.md)
will check that data loaded in the `elic_cont` object fit that
description and provide errors or warnings if that is not the case.

Keeping this information together means you do not need to enter it
again for every analysis. You can pass the same object to
[cont_get_data()](https://crewdecisions.github.io/elicitr/reference/cont_get_data.md),
[plot()](https://crewdecisions.github.io/elicitr/reference/plot.elic_cont.md),
[cont_sample_data()](https://crewdecisions.github.io/elicitr/reference/cont_sample_data.md),
[cont_summary()](https://crewdecisions.github.io/elicitr/reference/summary.cont_sample.md),
and
[plot()](https://crewdecisions.github.io/elicitr/reference/plot.cont_sample.md)
to retrieve responses, plot raw data, generate samples, summarise
results, and create plots. Keeping both rounds in the same object also
helps you compare responses before and after experts have had an
opportunity to review feedback and revise their estimates.
