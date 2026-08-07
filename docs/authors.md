# Authorship and contributions

`DESCRIPTION` carries the cumulative record of who holds rights and
responsibility in the package, and is what CRAN reads. `inst/CITATION`
carries the per-release citation. **This file records how roles and
contributions have changed over time**, which neither of those can
express.

Roles follow R’s [`person()`](https://rdrr.io/r/utils/person.html)
conventions: `aut` = author, `cre` = maintainer, `ctb` = contributor.

## Roles by release series

| Series      | Dates          | `aut`                | `cre`   | `ctb`  |
|-------------|----------------|----------------------|---------|--------|
| 0.50 – 0.71 | 2023 – present | Lugovoy, Potashnikov | Lugovoy | Sharma |
| 0.01 – 0.11 | 2019 – 2023    | Lugovoy, Potashnikov | Lugovoy | —      |
| 0.00.x      | 2016 – 2019    | Lugovoy, Potashnikov | Lugovoy | —      |

Roles have been stable since 2016; what has changed is who is *active*.
Both authors are named in every citation entry because the joint work
remains in the shipped package.

## Periods of activity

- **Oleg Lugovoy** — 2016-06-30 to present. Author and maintainer
  throughout.
- **Vladimir Potashnikov** — 2016-06-30 to 2022-11-24. Co-founder;
  present from the second commit of the repository. Both founders were
  listed as maintainers in the initial `DESCRIPTION`; this was reduced
  to a single maintainer the same day, and the split has stood since.
- **Tarun Sharma** — contributor, added 2024-08-31.

## Contributions

*Contribution descriptions to be agreed between the authors — see the
comment in the source of this file.*

**Tarun Sharma** contributed the design of the revised multi-hour
storage formulation (March 2024, recorded in `dev/changelog.txt`), which
changed how storage capacity is defined for `cap2stg != 1` cases. This
is a design contribution with no commits of its own, and is noted here
because the git history cannot show it.

## Other acknowledgements

Single-commit contributions, not listed in `DESCRIPTION`:

- **Michaja Pehl** (PIK) — 2020-01-17
- **Vitaliya Zhikhareva** — 2020-04-16
- **@AboodaA** — reported path handling issues, credited in
  `dev/changelog.txt` (April 2024)

## Development tools

Parts of the package have been developed with AI assistance. Consistent
with the policy in
[`.github/CONTRIBUTING.md`](https://energyRt.org/CONTRIBUTING.md), AI
tools are disclosed but are not listed as authors: authorship implies
accountability, which a tool cannot carry.

------------------------------------------------------------------------

*Maintained by the energyRt authors. Last updated 2026-08-01.*
