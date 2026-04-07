# Text

Text Module

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `Text`

## Active bindings

- `markdown`:

  (`character(n)`) Lines read from the markdown-file supplied.

## Methods

### Public methods

- [`Text$new()`](#method-Text-new)

- [`Text$validate()`](#method-Text-validate)

- [`Text$clone()`](#method-Text-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)

------------------------------------------------------------------------

### Method `new()`

initialize

#### Usage

    Text$new(markdown, ...)

#### Arguments

- `markdown`:

  (`character(n)`) Markdown.

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### Method `validate()`

Validator method

#### Usage

    Text$validate()

#### Returns

(`self`)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Text$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(DarwinShinyModules)

md <- c(
  "# H1
  ## H2
  ### H3",
  "**bold text**",
  "*italicized text*",
  "> blockquote",
  "1. First item
  2. Second item
  3. Third item",
  "- First item
  - Second item
  - Third item",
  "`code`",
  "---",
  "[link](https://www.markdownguide.org/cheat-sheet/)",
  "![alt text](https://mdg.imgix.net/assets/images/
   san-juan-mountains.jpg?auto=format&fit=clip&q=40&w=1080)",
  "| Syntax | Description |
  | ----------- | ----------- |
  | Header | Title |
  | Paragraph | Text |",
  "```r
    foo <- function(bar, baz) {
      return(bar ** baz)
    }

    foo(2, 3)
  ```",
  "  Here's a sentence with a footnote. [^1]

  [^1]: This is the footnote.",
  "### My Great Heading {#custom-id}",
  "term
  : definition",
  "~~The world is flat.~~",
  "- [x] Write the press release
  - [ ] Update the website
  - [ ] Contact the media",
  "That is so funny! :joy:",
  "I need to highlight these ==very important words==.",
  "H~2~O",
  "X^2^"
)

text <- Text$new(markdown = md)

if (interactive()) {
  preview(module = text)
}
```
