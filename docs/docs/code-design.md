# Code Design Decisions

##  opt-env-conf vs optparse-applicative

Optparse-applicative is a popular library for parsing command-line options in Haskell. It provides a declarative way to define command-line interfaces and supports features like subcommands, flags, and arguments.

Opt-env-conf is a library that extends optparse-applicative by adding support for environment variables and configuration files. It allows developers to define options that can be set via command-line arguments, environment variables, or configuration files, providing more flexibility in how applications can be configured.

We chose to use opt-env-conf over optparse-applicative for the following reasons:
1. Some commands require some env vars to be set, while others don't. opt-env-conf allows us to handle this complexity more gracefully, wihout passing around Maybe values.
2. We need to handle both encrypted and unencrypted mnemonics. Depending on that the settings are different. opt-env-conf allows us to define different settings based on the context, making it easier to manage these variations. In particular after parsing the env var ANTI_WALLET_FILE we insert a read-only operation (checkMapIO) that brings in the fields of the wallet file as settings. Depending on the field we require different settings for the passphrase.
3. We want to support interactive prompts for sensitive information like passwords. opt-env-conf `mapIO` allows us to introduce interactive prompts as part of the settings parsing process, making it easier to handle user input securely.


## Commands as GADTs

All commands are reified as GADTs where
- each constructor represents a command
- the fields of the constructor are the arguments of the command
- the return type of the constructor is the return type of the command

By using GADTs it's possible to have a different output type for each command.

To structure the commands we use subcommands. Higher level commands just wraps the lower level ones. For example the `anti wallet` command is a subcommand of the `anti` command and the `anti wallet create` command is a subcommand of the `anti wallet` command.

Thanks to GADTs the high level commands can just demandate the output type of the lower level commands and return it as their own output type.

## Parsing commands

The parsing of the commands is done using the `opt-env-conf` library. See the [opt-env-conf documentation](https://hackage.haskell.org/package/opt-env-conf) for more details and the code decision [here](./code-design-decisions.md#opt-env-conf).

Command parsers for the same GADTs compose via the `Alternative` typeclass. This means that each command parser can be defined independently and then combined together to form the full command parser.

Arguments parsers compose via the `Applicative` typeclass. This means that each argument parser can be defined independently and then combined together to form the full command parser for a command.

### Existential boxing of the command results

Because each command can have a different return type, we need to box the result of the command in an existential type. This is done using the `Box` type. The `Box` allows only for results that can be rendered as canonical JSON.

`Box` has a form of fmapping called `fmapBox` that allows to transform the inner value of the box while keeping the box type. This is useful when creating options for high level commands that just wrap lower level commands.
