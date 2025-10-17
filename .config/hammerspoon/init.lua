hs.window.animationDuration = 0
shortcut = {"alt", "ctrl"}
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
					 spoon.window_filter:rejectApp("Shortcuts")
					 spoon.window_filter:rejectApp("Messages")
					 spoon.window_filter:rejectApp("Facetime")
					 spoon.window_filter:rejectApp("Stickies")
					 spoon.window_filter:rejectApp("LuLu")
					 spoon.window_filter:rejectApp("IINA")
					 spoon.window_filter:rejectApp("java")
				  end,
				  start = true,
				  hotkeys = {
					 -- switch to a new focused window in tiled grid
					 focus_left = {shortcut, "left"},
					 focus_right = {shortcut, "right"},
					 -- Switch to a new focused window in tiled grid
					 swap_left = {shortcut, "`"},
					 swap_right = {shortcut, "tab"},
					 -- position and resize focused window
					 center_window = {shortcut, "c"},
					 cycle_width = {shortcut, "r"},
					 -- move the focused window into / out of the tiling layer
					 toggle_floating = {shortcut, "escape"},
					 -- focus the first / second / etc window in the current space
					 focus_window_1 = {shortcut, "1"},
					 focus_window_2 = {shortcut, "2"},
					 focus_window_3 = {shortcut, "3"},
					 focus_window_4 = {shortcut, "4"},
					 focus_window_5 = {shortcut, "5"},
					 focus_window_6 = {shortcut, "6"},
					 focus_window_7 = {shortcut, "7"},
					 focus_window_8 = {shortcut, "8"},
					 focus_window_9 = {shortcut, "9"},
				  }
})
-- Install:asyncUpdateAllRepos()
