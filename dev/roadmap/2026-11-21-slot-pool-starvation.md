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
