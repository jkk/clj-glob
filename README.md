# glob

Finds files based on glob patterns like "*.jpg" or "/usr/*/se*". Similar to glob in Perl, Ruby, and PHP.

## Usage

    (glob "*.{jpg,gif}")

    (glob ".*")  ; dot files are not included by default

    (glob "/usr/*/se*")  ; works on directories and subdirectories

## Installation

    (ns foo.bar
      (:use org.satta.glob))

## License

EPL

