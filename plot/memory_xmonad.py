import numpy as np
import matplotlib.pyplot as plt
from .plot_utils import *

set_plot_options(True)

memory_usages = {
    experiment_type: memory_usage / 1024
    for experiment_type, memory_usage in get_memory_usages(xmonad_versions).items()
}

fig, gs, legendAx = get_legend_grid_spec(6, 4.5, 10)
ax = fig.add_subplot(gs[0, :])

x = np.arange(len(xmonad_versions))
width = 0.35

plot_bars(ax, memory_usages, width, x)

ax.set_xlabel("Milestone version")
ax.set_ylabel("Average PSS (MiB)")
ax.set_xticks(x + width / 2, xmonad_releases)

add_legend(ax, legendAx)

plt.savefig("memory-xmonad.pgf")
# plt.show()
