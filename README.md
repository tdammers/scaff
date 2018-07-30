# scaff

## Introduction

Scaff is a small, unopinionated software project scaffolding tool.

As such, it is suitable for any programming language, build toolchain, or
framework, as long as a "project" consists of textual files in a directory
tree.

## Installing

Currently, only installing from source is supported.

### Using `cabal`:

#### Prerequisites

- Cabal 2.0 or newer
- GHC 8.0 or newer

Both these tools should come with a reasonable GHC install, to be found from
https://www.haskell.org/ghc.

1. Check out `git@github.com:tdammers/scaff`
2. `cd scaff`
3. `cabal sandbox init`
4. `cabal install`
5. (optionally) `sudo install ./.cabal-sandbox/bin/scaff /usr/local/bin/scaff`

### Using `stack`:

#### Prerequisites

- stack (https://haskellstack.org/)

1. Check out `git@github.com:tdammers/scaff`
2. `cd scaff`
3. `stack setup` (this will also install GHC if needed)
4. `stack install`
5. (optionally) copy the `scaff` binary somewhere global like
   `/usr/local/bin/`.

### Configuring Scaff

Before you can use `scaff`, you need to give it a little bit of extra
information.

1. Create a directory `~/.scaff/`
2. Copy the `config.example.yaml` file into `~/.scaff/config.yaml`, and edit it
   to taste
3. Create a file `~/.scaff/repos.list`, containing the following line:

```
http https://raw.githubusercontent.com/tdammers/scaff/master/templates/
```

This defines a repository of project templates using the HTTP(S) protocol
(i.e., scaff will pull in templates over HTTP or HTTPS), and any templates you
specify will be looked for below the given URL.

If you prefer getting templates from a local directory, you can use the `local`
repository type:

```
local /home/yourname/src/scaff/templates/
```

(Assuming that you checked out the scaff source repo into `~/src/scaff`)

You can add as many repositories you like; scaff will try them in order, until
it finds the desired template.

## Usage

The full CLI specification goes like this:

    scaff TEMPLATE PROJECT_NAME [OUTPUT_DIR]

Where:

- `TEMPLATE` is the project template you want to use
- `PROJECT_NAME` is the name of your project
- `OUTPUT_DIR` is the root directory for your project; if not given, the
  project name is used, below the CWD

Depending on the template being used, `scaff` may now ask you a number of
questions, and then generate the project into `OUTPUT_DIR`.

## Authoring Templates

Scaff templates use the [Ginger](https://ginger.tobiasdammers.nl/) template
language.

The two most important files in a template are `./files` and `./questions`; any
other files are used based on what these two say.

### `./files`

`./files` is a list of files to generate; each line may follow one of two
formats:

- `PATH` - take the file at `./PATH`, run it as a Ginger template, and
  save it as `OUTPUT_DIR/PATH`
- `INPUT:OUTPUT` - take the file at `./INPUT`, run it as a Ginger template, and
  save is as `OUTPUT_DIR/OUTPUT`.

Now, the fun part is that `./files` itself is also a Ginger template, so it
gets run through Ginger before reading the individual lines. This means that
you can use Ginger loops to make it generate multiple files from the same
template, to generate files conditionally, or to dynamically determine the
names of output files.

### `./questions`

`./questions` is a list of question definitions that will be asked in the same
order they are given in this file. Each line defines one question, following
the format:

`KEY:QUESTION_TEXT:DEFAULT:QUESTION_TYPE`

These elements work as follows:

- `KEY` defines a variable name ("key") under which the answer will be
  available in template files.
- `QUESTION_TEXT` is the text that will be presented to the user when asking
  the question.
- `DEFAULT` is the default value to be used when the user answers with a blank
  line.
- `QUESTION_TYPE` defined the type of question, and can be one of the
  following:
  - `.`, a single-entry question: the user can enter one free-form answer,
    which gets exposed as a string. This is the default.
  - `?`, a Yes/No question: the user can enter "y" or "n", the answer gets
    exposed as a boolean.
  - `*`, a multi-entry question: the user can enter as many entries as they
    like, until they enter a blank line; answers are exposed as an array.
  - A list of comma-separated labels: the user can pick one of the labels,
    which is exposed as a string.

### Variables Available In Template Files

When running a template file through ginger, the following variables will be
available:

- All environment variables from the process environment that start with
  `SCAFF_` will be exposed under the `env` variable, converted to "kebab case".
  That is, if you provide an environment variable
  `SCAFF_SOMETHING_INTERESTING=olives`, then the template construct `{{
  env['something-interesting'] }}` will render `olives`.
- The `author` key from `~/.scaff/config` will be available as `{{ author }}`
- The current project name will be available as `{{ project }}`
- Any answer to questions from the `./questions` file will be exposed under
  their defined keys.

### Publishing Templates

The easiest way to publish templates is to host them somewhere public, such
that each template `foo/bar` is situated somewhere under your root URL, e.g.
`https://scaff-templates.yourname.com/templates/`. Code hosting services like
github, gitlab, bitbucket, etc., will all offer a way to list and access raw
code files directly over HTTPS for public repos, so for example for github, you
can do something like this in your `repos.list`:

    http https://raw.githubusercontent.com/yourname/scaff-templates/master/templates/
