import numpy as np
import matplotlib.pyplot as plt
from .plot_utils import *

set_plot_options(True)

memory_usages = {
    experiment_type: memory_usage / 1024
    for experiment_type, memory_usage in get_memory_usages(smp_server_versions).items()
}

fig, gs, legend_ax = get_legend_grid_spec(6, 4.5, 10)
ax = fig.add_subplot(gs[0, :])

x = np.arange(len(smp_server_versions))
width = 0.35

plot_bars(ax, memory_usages, width, x)

ax.set_xlabel("Milestone version")
ax.set_ylabel("Average PSS (MiB)")
ax.set_xticks(x + width / 2, smp_server_releases)

add_legend(ax, legend_ax)

plt.savefig("memory-smp-server.pgf")
# plt.show()
