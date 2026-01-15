hs.window.animationDuration = 0
hs.loadSpoon("SpoonInstall")
Install = spoon.SpoonInstall
Install.repos.Caffeine = {
   url = "https://github.com/Hammerspoon/Spoons/tree/master/Source/Caffeine.spoon",
   desc = "Caffeine.spoon repository",
   branch = "master",
}
Install:andUse("Caffeine", {
				  start = true,
})
Install.repos.PaperWM = {
   url = "https://github.com/mogenson/PaperWM.spoon",
   desc = "PaperWM.spoon repository",
   branch = "release",
}
Install:andUse("PaperWM", {
				  repo = "PaperWM",
				  config = {
					 window_gap = { top = 20, bottom = 20, left = 20, right = 20 },
					 window_ratios = { 1/3, 1/2, 2/3, 1 }
				  },
				  fn = function(spoon)
					 spoon.window_filter:rejectApp("Finder")
					 spoon.window_filter:rejectApp("Terminal")
					 spoon.window_filter:rejectApp("QuickTime Player")
					 spoon.window_filter:rejectApp("iPhone Mirroring")
					 spoon.window_filter:rejectApp("Shortcuts")
					 spoon.window_filter:rejectApp("Messages")
					 spoon.window_filter:rejectApp("Facetime")
					 spoon.window_filter:rejectApp("Stickies")
					 spoon.window_filter:rejectApp("Minecraft")
					 spoon.window_filter:rejectApp("IINA")
					 spoon.window_filter:rejectApp("Elmedia Video Player")
					 spoon.window_filter:rejectApp("Color Picker")
					 spoon.window_filter:rejectApp("LuLu")
					 spoon.window_filter:rejectApp("java")
					 spoon.window_filter:rejectApp("Keynote")
				  end,
				  start = true,
				  hotkeys = {
					 -- position and resize focused window
					 cycle_width = {"alt", "c"},
					 -- move the focused window into / out of the tiling layer
					 toggle_floating = { "alt", "t" },
					 focus_floating = { "alt", "r" },
					 -- switch windows by cycling forward/backward
					 -- (forward = down or right, backward = up or left)
					 focus_prev = {"alt", "p"},
					 focus_next = {"alt", "n"},
					 -- Switch to a new focused window in tiled grid
					 swap_left = {"alt", "b"},
					 swap_right = {"alt", "f"},
					 -- focus the first / second / etc window in the current space
					 focus_window_1 = {"alt", "1"},
					 focus_window_2 = {"alt", "2"},
					 focus_window_3 = {"alt", "3"},
					 focus_window_4 = {"alt", "4"},
					 focus_window_5 = {"alt", "5"},
					 focus_window_6 = {"alt", "6"},
					 focus_window_7 = {"alt", "7"},
					 focus_window_8 = {"alt", "8"},
					 focus_window_9 = {"alt", "9"},
				  }
})
-- Install:asyncUpdateAllRepos()
