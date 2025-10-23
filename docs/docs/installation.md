# Installation Guide

The Moog command-line-interface (CLI) can run on Linux and MacOS.

## Installing Moog's CLI

The recommended way for end-users to install the Moog CLI is to download the
pre-built binaries for your platform from the [releases page](https://github.com/cardano-foundation/moog/releases)

For additional ways, see the CONTRIBUTING files in the [source repository](https://app.radicle.xyz/nodes/ash.radicle.garden/rad:z2a7Te5b28CX5YyPQ7ihrdG2EEUsC)

After unpacking the binary tarball, you should be able to run:

```
./moog --help
```

## Optional CLI features

You can enable bash completion for the `moog` command by adding the following
line to your `.bashrc` or `.bash_profile` file:

```bash
source <(moog --bash-completion-script "$(which moog)")
```

You can have a pretty output (not valid JSON,  but easier to read) by passing
`--pretty` switch or setting the `MOOG_PRETTY` environment variable to any
value:

```bash
export MOOG_PRETTY=1
```

For scripting purposes you can disable the pretty effect of the env-var by
passing `--no-pretty` switch.

Now you can progress with the [manual for your role](manual/any-role.md).