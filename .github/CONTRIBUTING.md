# Contributing to elicitr

Thank you for taking the time to contribute to our project. Please, read our [Contributor Code of Conduct](./CODE_OF_CONDUCT.md) to help keep our community friendly and respectful and review this document before filing an issue or opening a PR.

## Questions

If you have a question, please do not file an issue on GitHub but use other channels.

## Issues

Please, before filing and issue check whether it already exists.

Before reporting a **bug** make sure you have the latest version of `elicitr` installed. It is very important to provide a **minimal reproducible example**.
The example should only include code relevant to the problem, so please remove any lines of code not relevant to it.

## Pull Requests

We follow a coding convention. Check out the code in this repository to see the style we use and refer to the [tidyverse guidelines](https://style.tidyverse.org/) for a detailed description of coding conventions.

Before you open a PR it is important that all the following steps can be executed successfully (see the [R Packages](https://r-pkgs.org/) book for details):

1. Start a new R session

2. Load `devtools` with:

    ```r
    library(devtools)
    ```

3. Load the package with:

    ```r
    load_all()
    ```

4. Rebuild the documentation with:

    ```r
    document()
    ```

5. Rebuild the README file with:

    ```r
    build_readme()
    ```

6. Run R CMD check:

    ```r
    check()
    ```

7. Run spell check:

    ```r
    spell_check()
    ```

8. Lint the package:

    ```r
    lintr::lint_package()
    ```
