# S4 class to represent weather factors

`weather` is a data-carrying class with exogenous shocks used to
influence operation of processes in the model.

## Details

Weather factors are separated from the model parameters and can be added
or replaced for different scenarios.

## Slots

- `name`:

  character. Name of the weather factor, used in sets.

- `desc`:

  character. Description of the weather factor.

- `units`:

  data.frame. Unit of the weather series. A weather factor is
  DIMENSIONLESS by default and that is the common case; a unit is
  declared only when the series carries a real measure, and then it is
  used for reporting and for a cross-check against what a process
  expects – never for conversion.

- `geoframe`:

  character. Optional. The geoscale level this object's regions are
  expressed at; unset (the default) derives it from the region codes.
  See `technology@geoframe` for the full rule, including the consistency
  checks it turns on. A weather factor is multiplicative and balances
  nothing, so no commodity rule applies. This is the class where
  declaring the level matters most: left unset, a series whose data
  names no region spans every region of every level, where naming the
  level scopes it to that level alone.

- `region`:

  character. Region(s) the weather factor is declared at. May be a
  COARSER region of the model's geoscale than the processes that use it:
  a process reads the series at its own region when the object serves
  it, otherwise at the NEAREST ancestor that does. One profile declared
  at a parent therefore feeds every child region from a single stored
  series instead of being copied per child. Unset means every region. A
  process whose region the object neither serves nor has an ancestor for
  is an error – the factor is multiplicative and defaults to 0, so an
  unresolved link would shut the process down silently.

- `timeframe`:

  character. Timeframe of the weather factor.

- `defVal`:

  data.frame. Optional per-object override of a parameter's DEFAULT,
  keyed by the short column name the value is written under –
  `defVal$af.up <- 1` on a technology, `defVal$wval <- 3` on a weather
  series. Empty by default, and one column per override. A parameter
  name also works and is resolved (`pTechAf.up`, or `pTechAf` for both
  bounds). A key that names no column or parameter of this class is an
  error, not a silent no-op. This is the last of three layers: the
  catalogue (`data-raw/parameters.yml`), then `config@defVal`, then
  this; a value equal to what the layer before it says records nothing.

- `cluster`:

  data.frame. Declaration of the object's parallel series. For weather a
  cluster is a RESOURCE TIER – one profile per grade of the same
  resource, so a single object covers a whole tier set instead of one
  hand-named object per tier. This slot declares WHAT the tiers are; the
  per-tier values live in the `cluster` column of `weather`. It is
  REQUIRED whenever that column carries a label, and authoritative:
  labels are never harvested from the series, so a misspelled tier is an
  error rather than a series nobody asked for. At interpolation each
  tier referenced by a process is cloned into an ordinary weather object
  named with the model's cluster prefix (`WWIN` tier `T3` -\>
  `WWIN_CLT3`), so the backends only ever see plain series. A consumer
  finds its tier BY CLUSTER NAME: a `weather` link row of a technology,
  storage or supply carrying `cluster = "T3"` reads this object's `T3`
  rows. A link row with no cluster reads an unclustered object whole, as
  before, and is an ERROR against a clustered one – the factor is
  multiplicative, so "every tier at once" has no reading. LIMITATION –
  there are deliberately no share columns. A share ties parallel parts'
  capacity or activity to fixed proportions, and a capacity-factor
  series has neither. Size the tiers on the processes that read them.

  cluster

  : character. Tier label, used in the expanded object name.

  desc

  : character. Description of the tier.

  region

  : character. Region(s) the tier's series is declared for, NA for
    everywhere. Intersected with the object's own `region` slot. A
    process whose region no tier series covers (directly or through the
    nearest ancestor) is an error, not a silent zero.

  order

  : integer. Reporting order, 1 = the best tier. Without it labels sort
    alphabetically and "T10" would precede "T2".

- `weather`:

  data.frame. Weather factor values.

  cluster

  : character. Tier this row belongs to, NA for every tier. See the
    `cluster` slot.

  region

  : character. Region name to apply the parameter, NA for every region.

  year

  : integer. Year to apply the parameter, NA for every year.

  timeslice

  : character. Time timeslice to apply the parameter, NA for every
    timeslice.

  wval

  : numeric. Weather factor value.

- `interpolation`:

  data.frame. Optional per-object override of a parameter's
  INTERPOLATION RULE, keyed exactly as `defVal` above:
  `interpolation$af.up <- "inter"`. Any accepted spelling of a rule
  works and is normalised, so `bwd.mid.fwd` is `back.inter.forth`. The
  override applies to THIS object's rows only: every other object of the
  class keeps the rule the catalogue or `config` gives it.

- `misc`:

  list. Additional information.
