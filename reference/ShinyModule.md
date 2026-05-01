# Module Decorator Class

This class is a `decorator` and is not meant to be directly used, but to
be inherited by other Modules.

## Details

**Namespacing**  
The `ShinyModule` class manages namespacing with the `moduleName` and
`instanceId`, to create a `moduleId`. The `moduleId` and
`parentNamespace` (when a module is nested in another module) make up
the `namespace` field.


      moduleId = moduleName-instanceId
      namespace = [parentNamespace-]moduleId

**Server method**  
When creating a new module, the `id` in
[`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html)
is set to the `moduleId` field.

Besides setting the `id`, the `initServer()` method is called at the
start of
[`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html).
This initializes a reactive environment to be used freely when
developing modules. This method may be expanded upon to initialize other
namespace dependant features.

All of this is done by the class it self, in the public `server()`
method. The `server()` method calls the private `private$.server()`,
which should overridden when creating a module.

As an example:


    ...
    private = list(
      .server = function(input, output, session) {
        output$table <- shiny::renderTable(iris)
      }
    )
    ...

If the public `server()` method is overridden an error will be thrown:


    ...
    public = list(
      server = function(input, output, session) {
        output$table <- shiny::renderTable(iris)
      }
    )
    ...

    myModule <- MyModule$new()
    #> `self$server()` was overridden in `public = list(...)` override `private$.server()` instead in `private = list(.server = function(input, output, session) {})`

**UI method**  
When accessing an `outputId` in the UI, the `namespace` field is used to
reference the correct namespace with
[`shiny::NS()`](https://rdrr.io/pkg/shiny/man/NS.html).

It is also expected that the `UI()` method returns all contents to be
shown, so if multiple things should be shown, they should be nested in,
as an exmaple, `shiny::taglist()`.

As an example:


    ...
    private = list(
      .UI = function() {
        # `private$.namespace` would also be valid.
        shiny::tableOutput(outputId = shiny::NS(self$namespace, "table"))
      }
    )

If the public `UI()` method is overridden an error will be thrown:


    ...
    public = list(
      server = function(input, output, session) {
        output$table <- shiny::renderTable(iris)
      }
    )
    ...

    myModule <- MyModule$new()
    #> `self$UI()` was overridden in `public = list(...)` override `private$.UI()` instead in `private = list(.UI = function() {})`

## Active bindings

- `instanceId`:

  (`character(1)`) Random ID of 10 capitalized letters.

- `parentNamespace`:

  (`character(1)`) Namespace of the parent module.

- `moduleName`:

  (`character(1)`) Name of the module.

- `moduleId`:

  (`character(1)`) Module identifier, composed like:
  `moduleName-instanceId`

- `namespace`:

  (`character(1)`) Namespace, composed like:
  `[parentNamespace-]moduleName-instanceId` where `parentNamespace` is
  optional

- `reactiveValues`:

  (`reactivevalues`) Reactive values. use
  [`shiny::isolate()`](https://rdrr.io/pkg/shiny/man/isolate.html) to
  get a non-reactive item from the reactive environment.

- `async`:

  (`logical(1)`: `FALSE`) Logical parameter to switch asynchronous mode
  on or off.

## Methods

### Public methods

- [`ShinyModule$new()`](#method-ShinyModule-new)

- [`ShinyModule$validate()`](#method-ShinyModule-validate)

- [`ShinyModule$getReactiveValues()`](#method-ShinyModule-getReactiveValues)

- [`ShinyModule$UI()`](#method-ShinyModule-UI)

- [`ShinyModule$server()`](#method-ShinyModule-server)

- [`ShinyModule$clone()`](#method-ShinyModule-clone)

------------------------------------------------------------------------

### Method `new()`

Initializer method

#### Usage

    ShinyModule$new(...)

#### Arguments

- `...`:

  Additional parameters to set fields.

#### Returns

(`self`)

------------------------------------------------------------------------

### Method `validate()`

Validator method

#### Usage

    ShinyModule$validate()

#### Returns

(`self`)

------------------------------------------------------------------------

### Method `getReactiveValues()`

Method to get reactive values for a specific session.

#### Usage

    ShinyModule$getReactiveValues(session = getDefaultReactiveDomain())

#### Arguments

- `session`:

  (`session`) Session from the server function.

#### Returns

`reactiveValues`

------------------------------------------------------------------------

### Method `UI()`

Method to include a
[tagList](https://rstudio.github.io/htmltools/reference/tagList.html) to
include the body.

#### Usage

    ShinyModule$UI()

#### Returns

(`tagList`)

------------------------------------------------------------------------

### Method `server()`

Method to handle the back-end.

#### Usage

    ShinyModule$server(input, output, session)

#### Arguments

- `input`:

  (`input`) Input from the server function.

- `output`:

  (`output`) Output from the server function.

- `session`:

  (`session`) Session from the server function.

#### Returns

(`NULL`)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ShinyModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
MyModule <- R6::R6Class(
  classname = "MyModule",
  inherit = ShinyModule,
  private = list(
    .UI = function() {
      # `private$.namespace` would also be valid.
      shiny::tableOutput(outputId = shiny::NS(self$namespace, "table"))
    },

    # Override server()
    .server = function(input, output, session) {
      output$table <- shiny::renderTable(iris)
    }
  )
)

if (interactive()) {
  myModule <- MyModule$new()
  preview(myModule)
}

# The following would throw an error for overwritnig the public UI() and server() methods:
MyModule <- R6::R6Class(
  classname = "MyModule",
  inherit = ShinyModule,
  public = list(
    UI = function() {
      # `private$.namespace` would also be valid.
      shiny::tableOutput(outputId = shiny::NS(self$namespace, "table"))
    },

    # Override server()
    server = function(input, output, session) {
      output$table <- shiny::renderTable(iris)
    }
  )
)

tryCatch(
  {
    myModule <- MyModule$new()
  },
  error = function(e) {
    message(e)
  }
)
#> Error in private$checkMethodOverrides(): `self$server()` was overridden in `public = list(...)` override `private$.server()` instead in `private = list(.server = function(input, output, session) {})`
#>   `self$UI()` was overridden in `public = list(...)` override `private$.UI()` instead in `private = list(.UI = function() {})`
#> `self$server()` was overridden in `public = list(...)` override
#> `private$.server()` instead in
#> `private = list(.server = function(input,output, session) {})`

#> `self$UI()` was overridden in `public = list(...)` override
#> `private$.UI()` instead in `private = list(.UI = function() {})`
```
