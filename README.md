## mu-wizard
Shell script to auto-configure email accounts for `mu4e` similar in function to
mutt-wizard. It uses `isync` to synchronize mail accounts, `msmtp` to send mail
and creates individual Lisp profiles for each account. It is still WIP.

## Dependencies
* `isync` (for offline mail storage)
* `mu` (or maildir-utils depending on your distribution)
* `msmtp` (for sending mails)
* Password manager (`pass`, `pash`, and `pm` is supported)

## Installation
In order to install mu-wizard, clone this repository and build mu-wizard.

    git clone https://github.com/cemkeylan/mu-wizard.git
    cd mu-wizard
    make install

Users of Arch Linux based distributions can install mu-wizard through the AUR package [mu-wizard-git](https://aur.archlinux.org/packages/mu-wizard-git/):

    yay -S mu-wizard-git

## Usage
The wizard is run with `muw`. The options bellow are availabe for usage with `muw`:
- `muw`: Show usage help.
- `muw a`: **Add** and aoutoconfigure an email address.
- `muw d`: Pick an account to **delete**.
- `muw l`: **List** configured accounts.
- `muw p`: **Purge** all configuration.
- `muw s`: See your **share** directory: command not found.

## Emacs Configuration
Emacs will not be loading the configurations, you will need to set it manually.
In your init file, you may choose to load the configuration in the following
ways.

``` elisp
(load-file "~/.config/mu4e/mu4e-config.el")
```

``` elisp
(add-to-list 'load-path "~/.config/mu4e")
(require 'mu4e-config)
```

``` elisp
(use-package mu4e-config
  :after mu4e
  :load-path "~/.config/mu4e")
```

## `Domains.csv` file
`mu-wizard` doesn't come with a predefined `domains.csv` file, but it can use
one if it is found on `/usr/share/mu-wizard/domains.csv`. `mu-wizard` also saves
the domain information that you use when creating an account on your
configuration directory, so you don't have to retype every detail when creating
a second account with the same domain.

## Overrides
Domain-level overrides are possible by adding a shell file to either the share
directory (`/usr/local/share/mu-wizard/overrides`) or the user configuration
directory (`~/.config/mu4e/overrides`). See `overrides/protonmail.com` for an
example override. You can run `muw share` to learn your share directory.

### Protonmail users
`mu-wizard` supports protonmail. If you are using one of the default domains,
you don't have to do anything. If you are an alternative domain, you can link
the protonmail.com override to your personal domain. Here is an example:

``` sh
ln -sf /usr/share/mu-wizard/overrides/protonmail.com $HOME/.config/mu4e/overrides/example.com
```
