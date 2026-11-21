# Slot pool: single-slot runs can stall a waiting four-slot run

Observed 2026-11-21 (jobs strict-par and tnorm-limit). `/tmp/umvl-cpu/run4` takes slots 0-3 in order with
blocking locks; `/tmp/umvl-cpu/run` tries slots 5, 4, ... and falls back to the lower slots when the high ones
are busy. tnorm-limit's `run4` held slots 0-2 for about 90 minutes while waiting for slot 3, which a 2.4-hour
single-slot replay of strict-par had taken because slots 4 and 5 were busy at that moment: three reserved cores
idle, and the four-slot job blocked behind a single-threaded one.

Options:
- A single-slot run never takes slots 0-3 while a `run4` is waiting (a waiting marker file, or a ticket lock in
  front of the low slots), at the cost of single runs queueing when slots 4-5 are busy.
- `run4` takes its four slots all at once or none (try-lock all four, release and back off on failure), so it
  never sits on idle partial reservations; needs a fairness rule so it is not starved by single runs.
- Long single-threaded runs (the strict replay before speed-up D) use a separate "long" class limited to the
  high slots.

Source: `~/.local/share/umvl-cpu/` (`run`, `run4`, `ensure.sh`).

**Done 2026-11-21 (Claude)**, combining the first two options: `run4` try-locks slots 0-3 all at once or none
(releasing partial reservations and retrying with backoff up to 2 s), and while it waits it holds `wait4.lock`
shared; single-slot runs that see a waiting `run4` use only slots 4 and 5. Checked by hand: with slots 3-5 held by
single runs and a `run4` waiting, a fourth single run waited for slot 4 or 5 instead of taking 0-2, the `run4` held
no slot while waiting and started as soon as slot 3 was free; with no `run4` waiting, single runs still fall back to
the low slots. Previous wrapper kept as `umvl-cpu-wrapper.py.bak-20261121`.

**Memory, 2026-11-21 13:10.** The slots bound cores, not memory. A job's hand-written R test script ran testthat
with four parallel workers on an uncompiled package copy with `MAKEFLAGS=-j4`; each worker compiled the unity
build, 16 compilers took about 10 GB, and the kernel OOM killer hit the desktop session; the user manager then
stopped all four job units. Since then every job and batch-check unit runs in `~/.config/systemd/user/umvl-work.slice`
(`MemoryHigh=14G`, `MemoryMax=16G`, `CPUWeight=20`), so a runaway build is killed inside the slice, and the job brief
requires the recipes for builds and tests, a compiled package before any parallel testthat run, and `MAKEFLAGS` no
larger than the slots held.
