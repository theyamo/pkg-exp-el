# pkg-exp --- Explore your installed packages using a Transient menu

Small package that allows you to explore your installed Emacs packages using a Transient menu. It provides an efficient way to navigate package-related commands and perform common tasks such as describing packages, finding definitions, and uninstalling packages.

## Features

- Browse installed packages and their commands using a Transient menu.
- Execute commands associated with a package.
- View documentation, find function definitions, and debug functions interactively.
- Describe packages and visit their source files.
- Customize and uninstall packages.

## Installation

Ensure you have Emacs version 28 or later. Clone the repository and add it to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/pkg-exp-el")
(load "pkg-exp.el")
```

## Usage

To explore a package, run the `pkg-exp` command and select a package from the list. This opens a Transient menu displaying available actions and commands for the selected package.

## License

`pkg-exp` is free software licensed under the GNU General Public License v3.0 or later. See the [LICENSE](http://www.gnu.org/licenses/) for details.

For more information, visit the [GitHub repository](https://github.com/theyamo/pkg-exp-el).
