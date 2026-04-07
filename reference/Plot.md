# Plot Decorator Class

This class is a `decorator` and is not meant to be directly used, but to
be inherited by other modules, like `PlotStaic`, `PlotWidget`, and
`PlotPlotly`.

## Details

The inherited `Plot` modules evaluate the provided function with a
provided data object.

To add a new plot type it is required to inherit from the `Plot` class,
and to override the private `.UI()` and `.server()` methods.

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `Plot`

## Active bindings

- `title`:

  (`character(1)`) Title to use for the plot.

- `fun`:

  Plotting function.

- `reactiveArgs`:

  (`reactive`) Arguments used for plot.

- `args`:

  (`list`) Arguments used for plot.

- `plot`:

  Plot object.

## Methods

### Public methods

- [`Plot$new()`](#method-Plot-new)

- [`Plot$validate()`](#method-Plot-validate)

- [`Plot$clone()`](#method-Plot-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)

------------------------------------------------------------------------

### Method `new()`

initialize

#### Usage

    Plot$new(fun, args, title = NULL, ...)

#### Arguments

- `fun`:

  (`function()`) Function to plot with.

- `args`:

  (`list`) Named list of arguments to pass to `fun`.

- `title`:

  (`character(1)`: `NULL`) Title of the plot. When set to `NULL`, no
  title is shown.

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### Method `validate()`

Validator method

#### Usage

    Plot$validate()

#### Returns

(`self`)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Plot$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
