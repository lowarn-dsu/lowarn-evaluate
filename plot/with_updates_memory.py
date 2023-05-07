import numpy as np
import matplotlib.pyplot as plt
import csv
from collections import defaultdict
from .plot_utils import *

set_plot_options(True)

memories = defaultdict(lambda: [])

with open(
    f"{data_directory}/memory-over-time.csv",
    newline="",
) as file:
    reader = csv.reader(file)
    for row in reader:
        memories[int(row[0])].append(int(row[3]))

memory_pairs = np.array(
    [
        np.array([b, c])
        for _, b, c in sorted(
            ((number, np.mean(m), np.std(m)) for number, m in memories.items()),
            key=lambda x: x[0],
        )
    ]
)

discontinuityIndices = np.where(np.abs(np.diff(memory_pairs[:, 0])) >= 400000)
memory_pairs[discontinuityIndices, 0] = np.nan
memory_pairs[discontinuityIndices, 1] = np.nan

fig = plt.figure(layout="constrained", figsize=(6, 8))
ax = fig.add_subplot(111)

x = np.arange(len(memory_pairs))

ax.plot(x, memory_pairs[:, 0] / 1024, color="#5367DB")
ax.fill_between(
    x,
    (memory_pairs[:, 0] - memory_pairs[:, 1] * 2) / 1024,
    (memory_pairs[:, 0] + memory_pairs[:, 1] * 2) / 1024,
    alpha=1,
    facecolor="#CDEBF6",
)

ax.set_xlabel("Number of updates performed")
ax.set_ylabel("Average PSS (MiB)")

plt.savefig("memory-with-updates.pgf")
# plt.show()
