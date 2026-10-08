# Marginal Cox proportional hazards model for the treatment effect in RCT

Marginal Cox proportional hazards model for the treatment effect in RCT

## Usage

``` r
est_phreg(
  response = "Surv(time, status)",
  treatment = "a",
  level = 0.95,
  id = NULL
)
```

## Arguments

- response:

  Response variable (character or formula). Default: "Surv(time,
  status)"

- treatment:

  Treatment variable (character)

- level:

  Confidence interval level

- id:

  Optional subject id variable (character)

## Value

function

## See also

[Trial](https://novonordisk-opensource.github.io/carts/reference/Trial.md)
[est_adj](https://novonordisk-opensource.github.io/carts/reference/est_adj.md)

## Author

Klaus Kähler Holst
