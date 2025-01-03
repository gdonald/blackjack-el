[![MELPA](https://melpa.org/packages/blackjack-badge.svg)](https://melpa.org/#/blackjack)

## Blackjack for Emacs

This package provides the [Blackjack](https://en.wikipedia.org/wiki/Blackjack) card game for your [Emacs](https://www.gnu.org/software/emacs/) text editor.

### Install

#### If you use [use-package](https://github.com/jwiegley/use-package):

Assuming you have [MELPA configured](https://melpa.org/#/getting-started), you can add a new entry to your .emacs or init.el:

    (use-package blackjack
      :ensure t)

#### Or for a manual install:

Download and load [blackjack.el](https://raw.githubusercontent.com/gdonald/blackjack-el/main/blackjack.el) in your .emacs or init.el:

    (load "~/path/to/blackjack.el")

#### Running blackjack:

Once installed you can run the game like this:

    M-x blackjack

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

### Other Blackjack Implementations:

I've written Blackjack in [some other programming languages](https://github.com/gdonald?tab=repositories&q=blackjack&type=public&language=&sort=stargazers) too.  Check them out!

