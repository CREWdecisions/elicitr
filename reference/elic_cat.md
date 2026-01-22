# elic_cat class

The `elic_cat` class is a container for the elicitation data. It is used
to store the data collected during the elicitation process and their
metadata.

## Object elements

There are 6 elements in the `elic_cat` object:

- `categories`: character vector with the names of the categories.

- `options`: character vector with the names of the options
  investigated.

- `experts`: numeric, indicating the maximum number of experts
  participating in the elicitation process for one topic.

- `topics`: character vector with the names of the topics investigated.

- `data`: list with the data collected during the elicitation process.
  The list has multiple elements, corresponding to the topics
  investigated.

Moreover, the object has a `title` attribute that binds a name to the
object.
