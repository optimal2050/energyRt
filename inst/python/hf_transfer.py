#!/usr/bin/env python3
"""Move a payload to and from a Hugging Face job volume.

This is the only part of the cloud backend that is not R. Hugging Face moves
bytes over Xet -- content-defined chunking with dedup, implemented as a Rust
extension inside `huggingface_hub` -- so re-implementing the transfer in httr2
is not practical. Job *control* stays in R; this script only carries data.

Commands, each reading a JSON request on argv[1] and printing a JSON result:

  up    sync a local directory to a job volume, report the volume's bucket and
        folder so the caller can mount it and, later, find the results
  down  fetch named files out of a bucket folder

Both print a single line of JSON prefixed with HFJSON so the caller can pick it
out of whatever progress output the hub library writes to stdout.
"""
from __future__ import annotations

import json
import sys


def _emit(obj):
    print("HFJSON " + json.dumps(obj), flush=True)


def cmd_up(req):
    from huggingface_hub import sync_job_volume

    vol = sync_job_volume(
        req["local_dir"],
        req["mount_path"],
        read_only=bool(req.get("read_only", True)),
        namespace=req.get("namespace"),
        token=req.get("token"),
    )
    # `source` is the bucket ("<namespace>/jobs-artifacts"); `path` is the folder
    # within it, randomised per upload. Passing `path` where `source` is wanted
    # is a 403, so both are reported and the caller keeps them apart.
    _emit({"source": vol.source, "path": vol.path,
           "mount_path": req["mount_path"],
           "read_only": bool(req.get("read_only", True))})


def cmd_down(req):
    from huggingface_hub import download_bucket_files

    folder = req.get("path") or ""
    pairs = [((f"{folder}/{n}" if folder else n), dest)
             for n, dest in req["files"].items()]
    download_bucket_files(
        req["source"], pairs,
        raise_on_missing_files=bool(req.get("required", True)),
        token=req.get("token"),
    )
    _emit({"downloaded": list(req["files"].values())})


def main(argv):
    if len(argv) != 3 or argv[1] not in ("up", "down"):
        sys.exit("usage: hf_transfer.py {up|down} <request.json>")
    with open(argv[2], encoding="utf-8") as fh:
        req = json.load(fh)
    try:
        (cmd_up if argv[1] == "up" else cmd_down)(req)
    except Exception as exc:                      # reported, never a traceback
        _emit({"error": f"{type(exc).__name__}: {exc}"})
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
