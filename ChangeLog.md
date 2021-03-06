# 0.4.2 — 2021.06.24

- Add `Alternative`, `MonadFix`, `MonadPlus`, `MonadFail`, `MonadThrow`, and `MonadCatch` instances for `Cmd`

# 0.4.1 — 2020.11.01

- Allow no options for `[a]`

# 0.4.0 — 2020.11.01

## Breaking changes

- Change in behavior when the same option is specified multiple times [#8](https://github.com/tanakh/optparse-declarative/pull/8)

## Other changes

- Support for list types [#8](https://github.com/tanakh/optparse-declarative/pull/8)

# 0.3.1

- Allow False as a default value for Bool arguments [#2](https://github.com/tanakh/optparse-declarative/pull/2)
- Make `[]` a `IsCmd` instance [#6](https://github.com/tanakh/optparse-declarative/pull/6)
- Fix typo in README [#1](https://github.com/tanakh/optparse-declarative/pull/1)

# 0.3.0

- Simplify API
- Support -vv -vvv verbosity
- Improve help message

# 0.2.0

- Verbosity support

# 0.1.0

- First release
