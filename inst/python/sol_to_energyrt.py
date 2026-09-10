#!/usr/bin/env python3
"""Turn an external solver's solution file into energyRt's `output/` directory.

For solutions produced by the `pyomo_mps` preset, where the model was written
to MPS and solved elsewhere (cuOpt on a rented GPU) and no symbol map was kept.
Reconstructs the per-variable Arrow files that `read_solution()` expects, using
only what the solver directory already holds.

Needs, from the solver directory that produced the matrix:
  output.py    the generated writer -- names every variable, its column names
               and the mapping set it iterates. Authoritative for this model.
  input/*.arrow  the mapping sets themselves.

Labels are never parsed. Pyomo flattens an index tuple into the label with an
underscore convention that varies by version, and energyRt's own member names
contain underscores (`E_BIOMASS`, `AT0_0`), so `vTechCap(E_BIOMASS_AT0_0__2050_)`
cannot be split. Instead both sides are reduced to an underscore-free key --
the label's index becomes `EBIOMASSAT002050`, and so does the tuple
('E_BIOMASS','AT0_0','2050') -- which is invariant to whatever separator
convention the writer used. Key collisions within a mapping set are checked and
fatal, as is any unmatched label.

Usage:
  sol_to_energyrt.py SOLVER_DIR SOL_FILE [--out DIR] [--round-sig N]
"""
from __future__ import annotations

import argparse
import csv
import os
import re
import sys
import time

import pyarrow as pa
import pyarrow.feather as feather

# `flist.write("vTechInv\n")` then `f.write("tech,region,year,value\n")`
# then `for t, r, y in mTechInv:` -- the generated writer is fully regular.
RE_VAR = re.compile(r'flist\.write\("(\w+)\\n"\)')
RE_OPEN = re.compile(r'f = open(?:_var)?\("output/(\w+)\.csv"(?:, "w")?\)')
RE_COLS = re.compile(r'f\.write\("([\w,]+)\\n"\)')
RE_LOOP = re.compile(r'for\s+(.+?)\s+in\s+(\w+):')


class Fatal(RuntimeError):
    pass


def parse_output_py(path):
    """-> (declared order, {variable: ([columns...], mapping_set or None)}).

    Blocks are keyed on `f = open_var("output/<VAR>.csv")`, which names the
    variable explicitly. Pairing by proximity to `flist.write` does not work:
    the writer emits runs of several names before their blocks.
    """
    src = open(path, encoding="utf-8").read().splitlines()
    declared, blocks = [], {}
    for i, line in enumerate(src):
        s = line.strip()
        m = RE_VAR.match(s)
        if m and m.group(1) != "value":       # the variable_list.csv header
            declared.append(m.group(1))
            continue
        o = RE_OPEN.match(s)
        if not o:
            continue
        name, cols, mset = o.group(1), None, None
        for j in range(i + 1, min(i + 8, len(src))):
            t = src[j].strip()
            if cols is None:
                c = RE_COLS.match(t)
                if c:
                    cols = c.group(1).split(",")
            if mset is None:
                lp = RE_LOOP.match(t)
                if lp:
                    mset = lp.group(2)
            if RE_OPEN.match(t):
                break
        blocks[name] = (cols, mset)
    if not blocks:
        raise Fatal(f"parsed no variable blocks from {path}")
    return declared, blocks


def load_index(input_dir, mset):
    """Mapping set -> (dict of underscore-free key -> tuple, column names)."""
    p = os.path.join(input_dir, mset + ".arrow")
    if not os.path.exists(p):
        return None, None
    t = feather.read_table(p)
    cols = [c.to_pylist() for c in t.columns]
    idx, dup = {}, 0
    for tup in zip(*cols):
        k = "".join(str(x) for x in tup).replace("_", "")
        if k in idx:
            dup += 1
        idx[k] = tup
    if dup:
        # Two distinct members collapsing to one key would silently misassign
        # values; it has not been observed, but it must never pass unnoticed.
        raise Fatal(f"{mset}: {dup} key collisions after underscore removal")
    return idx, t.column_names


# Dimension column name -> the base set that holds its members. read.R applies
# the same aliasing when it reads the files back.
ALIAS = {"src": "region", "dst": "region", "regionp": "region",
         "yearp": "year", "acomm": "comm", "commp": "comm",
         "timeslicep": "timeslice"}


_MEMBER_CACHE: dict = {}


def base_members(input_dir, dim):
    """All members of a dimension.

    Prefers `input/<set>.arrow`. Some dimensions (`imp`, `expp`, `group`, ...)
    have no standalone set file because they only ever appear inside mapping
    sets, so fall back to the union of every `input/*.arrow` column with that
    name.
    """
    key = (input_dir, dim)
    if key in _MEMBER_CACHE:
        return _MEMBER_CACHE[key]
    name = ALIAS.get(dim, dim)
    p = os.path.join(input_dir, name + ".arrow")
    if os.path.exists(p):
        out = feather.read_table(p).column(0).to_pylist()
    else:
        out = set()
        for f in sorted(os.listdir(input_dir)):
            if not f.endswith(".arrow"):
                continue
            fp = os.path.join(input_dir, f)
            try:
                with pa.ipc.open_file(fp) as r:
                    if name not in r.schema.names:
                        continue
                    tbl = r.read_all()
            except (pa.ArrowInvalid, OSError):
                continue
            out.update(str(v) for v in tbl.column(name).to_pylist())
        out = sorted(out)
    _MEMBER_CACHE[key] = out
    return out


class PrefixDecoder:
    """Decompose a label index using the base sets, for variables whose mapping
    set is built inside `energyRt.py` rather than shipped in `input/`.

    Works on the same underscore-free keys as the mapping-set path, matching one
    dimension at a time. An index that decomposes more than one way is fatal --
    it would otherwise assign a value to an arbitrary member.
    """

    def __init__(self, input_dir, dims):
        self.dims = dims
        self.tabs = []
        for d in dims:
            members = base_members(input_dir, d)
            if not members:
                raise Fatal(f"no members found for dimension {d!r}")
            by_key = {str(m).replace("_", ""): str(m) for m in members}
            self.tabs.append((by_key, sorted({len(k) for k in by_key}, reverse=True)))
        self.cache = {}

    def __call__(self, key):
        hit = self.cache.get(key)
        if hit is not None:
            return hit
        out = []
        self._walk(key, 0, 0, [], out)
        if len(out) != 1:
            return None
        self.cache[key] = tuple(out[0])
        if len(self.cache) > 2_000_000:
            self.cache.clear()
        return self.cache.get(key, tuple(out[0]))

    def _walk(self, key, pos, k, acc, out):
        if len(out) > 1:
            return
        if k == len(self.tabs):
            if pos == len(key):
                out.append(list(acc))
            return
        by_key, lens = self.tabs[k]
        remaining = len(key) - pos
        for L in lens:
            if L > remaining:
                continue
            m = by_key.get(key[pos:pos + L])
            if m is not None:
                acc.append(m)
                self._walk(key, pos + L, k + 1, acc, out)
                acc.pop()


class VarWriter:
    """Buffered Arrow IPC writer for one variable."""

    def __init__(self, path, columns, chunk=500_000):
        self.path, self.columns, self.chunk = path, columns, chunk
        self.buf = [[] for _ in columns]
        self.n = 0
        self._w = None
        self._schema = pa.schema(
            [(c, pa.string()) for c in columns[:-1]] + [(columns[-1], pa.float64())])

    def add(self, tup, value):
        for i, v in enumerate(tup):
            self.buf[i].append(str(v))
        self.buf[-1].append(value)
        self.n += 1
        if len(self.buf[-1]) >= self.chunk:
            self.flush()

    def flush(self):
        if not self.buf[-1]:
            return
        arrs = [pa.array(b, type=pa.string()) for b in self.buf[:-1]]
        arrs.append(pa.array(self.buf[-1], type=pa.float64()))
        batch = pa.record_batch(arrs, schema=self._schema)
        if self._w is None:
            self._w = pa.ipc.new_file(self.path, self._schema)
        self._w.write_batch(batch)
        self.buf = [[] for _ in self.columns]

    def close(self):
        self.flush()
        if self._w is not None:
            self._w.close()
        elif self.n == 0:
            # An empty variable is legitimate; in Arrow mode read_solution()
            # skips a missing file, so write a valid empty table for clarity.
            feather.write_feather(pa.table(
                {c: pa.array([], type=pa.string()) for c in self.columns[:-1]}
                | {self.columns[-1]: pa.array([], type=pa.float64())}), self.path)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("solver_dir")
    ap.add_argument("solfile")
    ap.add_argument("--out", default=None, help="output dir (default SOLVER_DIR/output)")
    ap.add_argument("--round-sig", type=int, default=None,
                    help="round values to N significant digits (vObjective kept exact)")
    a = ap.parse_args()

    sd = a.solver_dir
    outdir = a.out or os.path.join(sd, "output")
    os.makedirs(outdir, exist_ok=True)
    t0 = time.time()

    declared, blocks = parse_output_py(os.path.join(sd, "output.py"))
    print(f"output.py: {len(declared)} variables declared, "
          f"{len(blocks)} with a write block", flush=True)

    inp = os.path.join(sd, "input")
    resolve, columns, unsupported = {}, {}, {}
    via = {"mapping": 0, "prefix": 0, "scalar": 0}
    for name, (cols, mset) in blocks.items():
        if not cols:
            continue
        columns[name] = cols
        dims = cols[:-1]
        if not dims:                                   # scalar, e.g. vObjective
            via["scalar"] += 1
            continue
        idx = mcols = None
        if mset:
            idx, mcols = load_index(inp, mset)
        if idx is not None:
            if len(mcols) != len(dims):
                raise Fatal(f"{name}: output.py declares {dims} but {mset} "
                            f"has {mcols}")
            resolve[name] = idx.get
            via["mapping"] += 1
        else:
            # Built inside energyRt.py, not shipped: fall back to the base sets.
            try:
                resolve[name] = PrefixDecoder(inp, dims)
                via["prefix"] += 1
            except Fatal as e:
                # Losing one variable is better than losing the whole solution;
                # in Arrow mode read_solution() skips a file that is not there.
                unsupported[name] = str(e)
    print(f"  resolvers: {via['mapping']} from mapping sets, "
          f"{via['prefix']} from base sets, {via['scalar']} scalar", flush=True)
    if unsupported:
        print(f"  NOT RECONSTRUCTABLE ({len(unsupported)}): "
              f"{', '.join(sorted(unsupported))}", flush=True)

    writers, stats, members, skipped = {}, {}, {}, {}
    header, unmatched, nonfinite, seen = {}, {}, 0, 0
    scal_vals = {}
    sig = a.round_sig

    with open(a.solfile, "r", errors="replace") as fh:
        for line in fh:
            if not line or line[0] == "#":
                k, _, v = line[1:].partition(":")
                header[k.strip()] = v.strip()
                continue
            lab, _, raw = line.rpartition(" ")
            if not lab:
                continue
            try:
                val = float(raw)
            except ValueError:
                continue
            seen += 1
            var, _, idx = lab.partition("(")
            if not idx:
                # A scalar variable (vObjective), or one of the solver's own
                # helper columns -- Pyomo emits `fornontriv` for trivial rows.
                if var in blocks:
                    scal_vals[var] = val
                continue
            if val == 0.0:                                # energyRt writes non-zeros
                continue
            if val != val or val in (float("inf"), float("-inf")):
                nonfinite += 1
                continue
            fn = resolve.get(var)
            if fn is None:
                if var in unsupported:
                    skipped[var] = skipped.get(var, 0) + 1
                else:
                    unmatched[var] = unmatched.get(var, 0) + 1
                continue
            tup = fn(idx.rstrip(")").replace("_", ""))
            if tup is None:
                unmatched[var] = unmatched.get(var, 0) + 1
                continue
            cols = columns[var]
            for pos, dim in enumerate(cols[:-1]):
                members.setdefault(dim, set()).add(str(tup[pos]))
            if sig and var != "vObjective" and val:
                from math import floor, log10
                val = round(val, -int(floor(log10(abs(val)))) + (sig - 1))
            w = writers.get(var)
            if w is None:
                w = writers[var] = VarWriter(
                    os.path.join(outdir, var + ".arrow"), columns[var])
            w.add(tup, val)

    for v, w in writers.items():
        w.close()
        stats[v] = w.n

    # An index mismatch yields a clean, plausible, empty result -- make it fatal.
    if unmatched:
        tot = sum(unmatched.values())
        worst = sorted(unmatched.items(), key=lambda kv: -kv[1])[:5]
        raise Fatal(f"{tot:,} labels did not match any mapping set: {worst}. "
                    f"The solution and the solver directory are not from the "
                    f"same model.")
    if not stats:
        raise Fatal("no values were written")

    # --- meta files ------------------------------------------------------
    with open(os.path.join(outdir, "variable_list.csv"), "w", newline="") as fh:
        fh.write("value\n")
        for name in declared:
            if name in stats or name in scal_vals:
                fh.write(name + "\n")

    # raw_data_set.csv: read_solution() needs every dimension column name to
    # resolve to a known set. Built from the members actually written, so it
    # cannot disagree with the files. Aliases are folded onto their base set --
    # read.R resolves the alias name itself, and emitting a set literally named
    # `src` would not help it.
    sets = {}
    for dim, vals in members.items():
        sets.setdefault(ALIAS.get(dim, dim), set()).update(vals)
    with open(os.path.join(outdir, "raw_data_set.csv"), "w", newline="") as fh:
        w = csv.writer(fh, lineterminator="\n")
        w.writerow(["set", "value"])
        for s in sorted(sets):
            for v in sorted(sets[s]):
                w.writerow([s, v])

    # Scalars go out in the same format as everything else: `open_var()` in the
    # generated writer redirects "output/<v>.csv" to <v>.arrow, so a scalar
    # written as CSV would simply not be found.
    for name, val in scal_vals.items():
        feather.write_feather(
            pa.table({"value": pa.array([val], type=pa.float64())}),
            os.path.join(outdir, name + ".arrow"))

    status = (header.get("Status") or "unknown").strip()
    ok = int(status.lower() == "optimal")
    now = time.strftime("%H:%M:%S")
    with open(os.path.join(outdir, "log.csv"), "w", newline="") as fh:
        fh.write("parameter,value,time\n")
        fh.write(f'"model language",PyomoConcrete,"{now}"\n')
        fh.write(f'"reconstructed from",{os.path.basename(a.solfile)},"{now}"\n')
        if "Objective value" in header:
            fh.write(f'"objective",{header["Objective value"]},"{now}"\n')
        fh.write(f'"solution status",{ok},"{now}"\n')
        fh.write(f'"termination","{status}","{now}"\n')
        fh.write(f'"done",,"{now}"\n')

    # --- report ----------------------------------------------------------
    print(f"\nread {seen:,} solution entries in {time.time() - t0:.1f}s")
    print(f"status={status}  objective={header.get('Objective value')}")
    if nonfinite:
        print(f"WARNING: {nonfinite} non-finite values skipped")
    if skipped:
        tot = sum(skipped.values())
        print(f"WARNING: {tot:,} values dropped for {len(skipped)} variables "
              f"with no reconstructable index: {', '.join(sorted(skipped))}")
    print(f"\nwrote {len(stats)} variables to {outdir}")
    for v in sorted(stats, key=lambda k: -stats[k]):
        print(f"  {v:<26}{stats[v]:>12,} rows")
    if scal_vals:
        print("  scalars: " + ", ".join(f"{k}={v!r}" for k, v in scal_vals.items()))
    if not ok:
        print(f"\nNOTE: status is {status!r}, not Optimal -- log.csv reports "
              f"solution status 0 and read_solution() will not treat this as solved.")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Fatal as e:
        print(f"\nFATAL: {e}", file=sys.stderr)
        sys.exit(2)
