# energyRt: Energy Systems Modeling Toolbox

A macro-language for energy system modeling in R. Describe the system as
reusable building blocks (commodities, technologies, supply, demand,
storage, trade); energyRt compiles them into a least-cost
capacity-expansion and dispatch optimization model, solves it in
'GLPK'/'MathProg', 'Julia'/'JuMP', 'Python'/'Pyomo' or 'GAMS', or
through a direct LP matrix, and returns tidy results. Time resolution,
geography and solver are separate choices, so one can change without
rewriting the model. Built-in tools cover structural validation,
levelized costs, reports, and plotting.

## See also

Useful links:

- <https://energyRt.org>

- <https://github.com/optimal2050/energyRt>

- Report bugs at <https://github.com/optimal2050/energyRt/issues>

## Author

**Maintainer**: Oleg Lugovoy <olugovoy@optimalsolution.dev>
([ORCID](https://orcid.org/0000-0001-5546-9875))

Authors:

- Oleg Lugovoy <olugovoy@optimalsolution.dev>
  ([ORCID](https://orcid.org/0000-0001-5546-9875))

- Vladimir Potashnikov <potashnikov.vu@gmail.com> (contributed
  2016-2022)

Other contributors:

- Tarun Sharma <tarunsharma@ms.iitr.ac.in>
  ([ORCID](https://orcid.org/0000-0003-3636-2858)) \[contributor\]
