# Python Docstring Inserter

This repository enables the automatic generation of function docstrings in
Python according to the [Google style
guide](https://google.github.io/styleguide/pyguide.html#383-functions-and-methods).
Here is an example:

```python
def fetch_data(url: str, keys: List[int]) -> Mapping[int, str]:
    """This is a short description of this function and can span over multiple
    lines.

    This is a long description of the function with more details about it.

    Args:
        url: This is an argument description, which can span over multiple
            lines too.
        keys: This is a description of the second argument.

    Returns:
        This is a description of the returned data and can be detailed too.

    """
```

The text gets automatically *indented* and *split* on multiple lines.

## Install

### With [use-package](https://github.com/jwiegley/use-package) and [quelpa](https://github.com/quelpa/quelpa)

```elisp
(use-package insert-docstring
  :ensure nil
  :quelpa (insert-docstring
           :fetcher github
           :repo "macurovc/insert-docstring"))
```

### Manually

Place [insert-docstring.el](insert-docstring.el) in your Emacs `load-path`. E.g.:

```elisp
(add-to-list 'load-path (expand-file-name "~/{path/to}/insert-docstring"))
(require 'insert-docstring)
```

## Usage

Set in the `~/.emacs` file a keybinding such as:

```elisp
(defun set-python-keybindings ()
  (local-set-key (kbd "C-c i") 'insert-docstring-at-point-with-google-style)
  )
(add-hook 'python-mode-hook 'set-python-keybindings)
```

Now, in a python file, place the cursor on a function, type `C-c i` and follow
the instructions.

## Contributing

Any contribution is welcome. The file
[insert-docstring-tests.el](insert-docstring-tests.el) contains tests for the
functions that don't require user inputs and don't modify a buffer. The tests
can be run with the following command:

```bash
emacs -batch -l insert-docstring.el -l insert-docstring-tests.el -f ert-run-tests-batch-and-exit
```

Each contribution must respect the following requirements:
* pass the validation of [package-lint](https://github.com/purcell/package-lint)
* byte-compilation without errors
* no errors with `M-x checkdoc`
