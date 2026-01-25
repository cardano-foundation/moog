# Installation

## Prerequisites

The Moog command-line-interface (CLI) can run on Linux and MacOS.

## Installing Moog's CLI

The recommended way for end-users to install the Moog CLI is to download the
pre-built binaries for your platform from the [releases page](https://github.com/cardano-foundation/moog/releases)

Alternatively, if you are a Nix user you may run the CLI directly from the official repository:

```bash
nix shell github:cardano-foundation/moog?dir=cli#moog
```

For additional ways, see the CONTRIBUTING files in the [source repository](https://github.com/cardano-foundation/moog).

After installation, you should be able to run:

```
moog --help
```

If this prints out Moog's CLI help, you have successfully installed the CLI.

### Optional CLI features

You can enable bash completion for the `moog` command by adding the following
line to your `.bashrc` or `.bash_profile` file:

```bash
source <(moog --bash-completion-script "$(which moog)")
```

```asciinema-player
{ "file": "assets/video/shell-completions.cast"
, "mkap_theme": "none"
, "cols": 100
}
```

You can have a pretty output (not valid JSON,  but easier to read) by passing
`--pretty` switch or setting the `MOOG_PRETTY` environment variable to any
value:

> Be careful that, in general, `| jq`, will not work with this setting.

```bash
export MOOG_PRETTY=1
```

For scripting purposes you can disable the pretty effect of the env-var by
passing `--no-pretty` switch.
