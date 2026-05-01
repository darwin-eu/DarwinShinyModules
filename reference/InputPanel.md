# InputPanel Module Class

InputPanel module that handles inputs based on an input function like:
[`shiny::selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html),
[`shinyWidgets::pickerInput()`](https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html),
etc.

## Details

The assigned input values are accessible in the reactive values
`inputValues` field. Other modules may trigger off these reactive values
with i.e.
[`shiny::observeEvent()`](https://rdrr.io/pkg/shiny/man/observeEvent.html).

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `InputPanel`

## Active bindings

- `parentNamespace`:

  (`character(1)`) Namespace of the parent module.

- `funs`:

  ([`list()`](https://rdrr.io/r/base/list.html)) Named list of xInput
  functions used `list(funA = shiny::selectInput)`.

- `args`:

  ([`list()`](https://rdrr.io/r/base/list.html)) Named list of arguments
  used by xInput functions
  `list(funA = list(inputId = "name", label = "name"))`.

- `inputValues`:

  (`reactiveValues`) Values passed from the input fields.

## Methods

### Public methods

- [`InputPanel$new()`](#method-InputPanel-new)

- [`InputPanel$validate()`](#method-InputPanel-validate)

- [`InputPanel$update()`](#method-InputPanel-update)

- [`InputPanel$clone()`](#method-InputPanel-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)

------------------------------------------------------------------------

### Method `new()`

Initializer method

#### Usage

    InputPanel$new(funs, args, growDirection = "vertical", ...)

#### Arguments

- `funs`:

  ([`list()`](https://rdrr.io/r/base/list.html)) Named list of xInput
  functions used `list(funA = shiny::selectInput)`.

- `args`:

  ([`list()`](https://rdrr.io/r/base/list.html)) Named list of arguments
  used by xInput functions
  `list(funA = list(inputId = "name", label = "name"))`

- `growDirection`:

  The direction in which this component will be placed, either
  "horizontal" or "vertical" (default)

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

(`invisible(self)`)

------------------------------------------------------------------------

### Method `validate()`

Validation method

#### Usage

    InputPanel$validate()

#### Returns

(`self`)

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Updates the input variables using the provided update functions supplied
in `updateFuns`

#### Usage

    InputPanel$update(fun, name, ...)

#### Arguments

- `fun`:

  (`funciton`) Update function to use i.e.
  [`shiny::updateSelectInput`](https://rdrr.io/pkg/shiny/man/updateSelectInput.html)

- `name`:

  (`character(1)`) Name of the update function and argument set to use.

- `...`:

  Arguments that are used by the supplied function. `inputId` should now
  be provided, as it is derived from the `name` argument.

#### Returns

(`invisible(self)`)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    InputPanel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(DarwinShinyModules)

inputPanel <- InputPanel$new(
  funs = list(
    select = shiny::selectInput,
    text = shiny::textInput
  ),
  args = list(
    select = list(inputId = "select", choices = c("a", "b"), label = "select"),
    text = list(inputId = "text", label = "text")
  )
)

if (interactive()) {
  preview(inputPanel)
}
```
