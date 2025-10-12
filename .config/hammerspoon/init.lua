hotkey = {"cmd", "control"}
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
					 focus_left = {hotkey, "p"},
					 focus_right = {hotkey, "n"},
					 -- Switch to a new focused window in tiled grid
					 swap_left = {hotkey, "b"},
					 swap_right = {hotkey, "f"},
					 -- position and resize focused window
					 center_window = {hotkey, "c"},
					 cycle_width = {hotkey, "r"},
					 -- move the focused window into / out of the tiling layer
					 toggle_floating = {hotkey, "escape"},
					 -- move focused window to a new space and tile
					 move_window_1 = {hotkey, "1"},
					 move_window_2 = {hotkey, "2"},
					 move_window_3 = {hotkey, "3"},
					 move_window_4 = {hotkey, "4"},
					 move_window_5 = {hotkey, "5"},
					 move_window_6 = {hotkey, "6"},
					 move_window_7 = {hotkey, "7"},
					 move_window_8 = {hotkey, "8"},
					 move_window_9 = {hotkey, "9"}
				  }
})
Install:asyncUpdateAllRepos()
