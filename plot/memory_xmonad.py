import numpy as np
import matplotlib.pyplot as plt
from .plot_utils import *

set_plot_options(True)

memory_usages = {
    experiment_type: memory_usage / 1024
    for experiment_type, memory_usage in get_memory_usages(xmonad_versions).items()
}

fig, gs, legend_ax = get_legend_grid_spec(3.075, 2.8, 8)
inner_gs = gs[0].subgridspec(
    1,
    1,
    wspace=0,
    hspace=0,
)

ax = fig.add_subplot(inner_gs[0])
hidden_ax = fig.add_subplot(inner_gs[:], frameon=False)

hidden_ax.tick_params(
    labeltop=False,
    top=False,
    labelbottom=False,
    bottom=False,
    labelleft=False,
    left=False,
    labelright=False,
    right=False,
)

x = np.arange(len(xmonad_versions))
width = 0.39

plot_bars(ax, memory_usages, width, x, 5)

hidden_ax.set_xlabel("Milestone version", labelpad=20)
hidden_ax.set_ylabel("Average PSS (MiB)", labelpad=26)
ax.set_xticks(x + width / 2, xmonad_releases)
ax.set_ylim(0, 160)

add_legend(ax, legend_ax)

plt.margins(0)
plt.savefig("memory-xmonad.pgf")
# plt.show()
