#!/usr/bin/env python3
"""Plot benchmark results from the generated CSV.

This script reads the CSV written by benchmark.ml and produces:
- one graph for stack implementations
- one graph for queue implementations

The y-axis is operations per microsecond and the x-axis is thread count.
"""

from __future__ import annotations

import argparse
import csv
from collections import defaultdict
from pathlib import Path
from typing import Dict, List

import matplotlib.pyplot as plt


STACK_IMPLEMENTATIONS = [
    "locked_stack",
    "lockfree_stack_builtin_list",
    "lf_universal_stack",
    "wf_universal_stack",
]

QUEUE_IMPLEMENTATIONS = [
    "locked_queue",
    "lockfree_queue",
    "lf_universal_queue",
    "wf_universal_queue",
]


def load_rows(csv_path: Path) -> List[dict[str, str]]:
    with csv_path.open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def group_throughput(rows: List[dict[str, str]], object_kind: str) -> Dict[str, Dict[int, float]]:
    grouped: Dict[str, Dict[int, float]] = defaultdict(dict)
    for row in rows:
        if row["object"] != object_kind:
            continue
        implementation = row["implementation"]
        threads = int(row["threads"])
        ops_per_sec = float(row["ops_per_sec"])
        ops_per_us = ops_per_sec / 1_000_000.0
        grouped[implementation][threads] = ops_per_us
    return grouped


def plot_group(
    grouped: Dict[str, Dict[int, float]],
    implementations: List[str],
    title: str,
    output_path: Path,
) -> None:
    plt.figure(figsize=(10, 6))

    for implementation in implementations:
        thread_map = grouped.get(implementation, {})
        if not thread_map:
            continue
        threads = sorted(thread_map)
        values = [thread_map[thread] for thread in threads]
        plt.plot(threads, values, marker="o", linewidth=2, label=implementation)

    plt.title(title)
    plt.xlabel("Number of threads")
    plt.ylabel("Operations per microsecond")
    plt.yscale("log")
    plt.grid(True, alpha=0.3)
    plt.legend()
    plt.tight_layout()
    plt.savefig(output_path, dpi=200)
    plt.close()


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Plot stack and queue benchmark results from a CSV file."
    )
    parser.add_argument(
        "csv_file",
        nargs="?",
        default="benchmark_vs_threads_1_to_8.csv",
        help="Path to the benchmark CSV file",
    )
    parser.add_argument(
        "--output-dir",
        default=None,
        help="Directory where plots will be written. Defaults to the CSV folder.",
    )
    args = parser.parse_args()

    csv_path = Path(args.csv_file).expanduser().resolve()
    output_dir = Path(args.output_dir).expanduser().resolve() if args.output_dir else csv_path.parent

    rows = load_rows(csv_path)
    stack_grouped = group_throughput(rows, "stack")
    queue_grouped = group_throughput(rows, "queue")

    output_dir.mkdir(parents=True, exist_ok=True)
    plot_group(
        stack_grouped,
        STACK_IMPLEMENTATIONS,
        "Stack throughput vs threads",
        output_dir / "stack_throughput.png",
    )
    plot_group(
        queue_grouped,
        QUEUE_IMPLEMENTATIONS,
        "Queue throughput vs threads",
        output_dir / "queue_throughput.png",
    )

    print(f"Wrote {output_dir / 'stack_throughput.png'}")
    print(f"Wrote {output_dir / 'queue_throughput.png'}")


if __name__ == "__main__":
    main()
