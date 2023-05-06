from matplotlib import ticker
import numpy as np
import matplotlib.pyplot as plt
from .plot_utils import *

set_plot_options(False)

memory_usages = get_times(smp_server_versions)

fig, gs, legendAx = get_legend_grid_spec(6, 4.5, 10)
ax = fig.add_subplot(gs[0, :])

x = np.arange(len(smp_server_versions))
width = 0.35

plot_bars(ax, memory_usages, width, x)

ax.yaxis.set_major_locator(ticker.MultipleLocator(5))

ax.set_xlabel("Milestone version")
ax.set_ylabel("Time taken to complete benchmark (s)")
ax.set_xticks(x + width / 2, smp_server_releases)

add_legend(ax, legendAx)

# plt.savefig("time-smp-server.pgf")
plt.show()
