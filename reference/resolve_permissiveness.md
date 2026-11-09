# Resolve permissiveness: fill NULLs from preset, keep explicit values

Resolve permissiveness: fill NULLs from preset, keep explicit values

## Usage

``` r
resolve_permissiveness(permissiveness, ...)
```

## Arguments

- permissiveness:

  Character: "loosest", "looser", "tighter", "tightest"

- ...:

  Named parameter values (NULL = use preset, non-NULL = override)

## Value

Named list of all resolved parameter values
