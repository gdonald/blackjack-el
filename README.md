[![MELPA](https://melpa.org/packages/blackjack-badge.svg)](https://melpa.org/#/blackjack)

## Blackjack for Emacs

This package provides the [Blackjack](https://en.wikipedia.org/wiki/Blackjack) card game for the [Emacs](https://www.gnu.org/software/emacs/) text editor.

### Install

#### If you use [use-package](https://github.com/jwiegley/use-package):

Assuming you have [MELPA configured](https://melpa.org/#/getting-started), you can add a new entry to your .emacs or init.el:

    (use-package blackjack
      :ensure t)

#### Or for a manual install:

Download and load `blackjack.el` in your .emacs or init.el:

    (load "~/path/to/blackjack.el")

#### Running blackjack:

Once installed you can run the game like this:

    [M-x](https://www.gnu.org/software/emacs/manual/html_node/emacs/M_002dx.html) blackjack

#### Running tests:

Comands `make test` and `make test-coverage` are convenience commands available.  Tests are written using [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup).  Test coverage is generated using [undercover](https://github.com/undercover-el/undercover.el).  Both packages are available from MELPA.

You will need to install [Ruby](https://www.ruby-lang.org/) and [Simplecov](https://github.com/simplecov-ruby/simplecov) for test coverage to build.

#### Going broke:

If you run out of money, delete `blackjack.txt` and restart blackjack.  You will get a free $100 for another try.

### Screenshots:

![Blackjack](https://raw.githubusercontent.com/gdonald/blackjack-el/main/imgs/ss1.png)

![Blackjack](https://raw.githubusercontent.com/gdonald/blackjack-el/main/imgs/ss2.png)

### Bugs / Issues / Feature Requests

Please report any bugs or issues you find:

[https://github.com/gdonald/blackjack-el/issues](https://github.com/gdonald/blackjack-el/issues)

### License

[![GitHub](https://img.shields.io/github/license/gdonald/blackjack-el?color=aa0000)](https://github.com/gdonald/blackjack-el/blob/main/LICENSE)

### Alternative Implementations:

Code for Blackjack in other programming languages:

- [Ruby](https://github.com/gdonald/console-blackjack-ruby)
- [Rust](https://github.com/gdonald/console-blackjack-rust)
- [Ruku](https://github.com/gdonald/Console-Blackjack)
- [Typescript](https://github.com/gdonald/blackjack-js)
- [Perl](https://github.com/gdonald/console-blackjack-perl)
- [C](https://github.com/gdonald/blackjack-c)
- [C++](https://github.com/gdonald/blackjack-cpp)
- [Crystal](https://github.com/gdonald/blackjack-cr)
- [Go](https://github.com/gdonald/blackjack-go)
- [Elixir](https://github.com/gdonald/blackjack-ex)
- [Python](https://github.com/gdonald/blackjack-py)
- [C with SDL](https://github.com/gdonald/blackjack-c-sdl)
