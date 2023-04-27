import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec
import csv
from collections import defaultdict

data_directory = "./data/"

experiment_types = ["normal", "lowarn"]
versions = [1147, 1163, 1223, 1234, 1271, 1326, 1363]
releases = [0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17]

averages = {experiment_type: [] for experiment_type in experiment_types}
standard_deviations = {experiment_type: [] for experiment_type in experiment_types}

mpl.rcParams.update({"text.usetex": True, "font.family": "serif", "font.size": 12})

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
