meh = {"ctrl", "alt", "shift"}
hyper = {"ctrl", "alt", "cmd", "shift"}
hs.loadSpoon("SpoonInstall")
spoon.SpoonInstall:andUse("Caffeine", {
    start = true,
})
spoon.SpoonInstall.repos.PaperWM = {
    url = "https://github.com/mogenson/PaperWM.spoon",
    desc = "PaperWM.spoon repository",
    branch = "release",
}
spoon.SpoonInstall:andUse("PaperWM", {
    repo = "PaperWM",
    config = {
		window_ratios = { 0.3, 0.5, 0.68, 1 },
		rejectApp = {"IINA", "java"},
		swipe_fingers = 4,
		swipe_gain = 2.0
	},
	start = true,
    hotkeys = {
		-- Switch to a new focused window in tiled grid
		focus_left  = {meh, "left"},
		focus_right = {meh, "right"},
		focus_up    = {meh, "up"},
		focus_down  = {meh, "down"},
	
		-- move windows around in tiled grid
		swap_left  = {hyper, "left"},
		swap_right = {hyper, "right"},
		swap_up    = {hyper, "up"},
		swap_down  = {hyper, "down"},
	
		-- position and resize focused window
		center_window        = {meh, "return"},
		cycle_width          = {meh, "space"},
	
		-- move focused window into / out of a column
		slurp_in = {meh, "-"},
		barf_out = {meh, ";"},
	
		-- move the focused window into / out of the tiling layer
		toggle_floating = {meh, "escape"},
	
		-- switch to a new Mission Control space
		switch_space_1 = {meh, "["},
		switch_space_2 = {meh, "]"},
		switch_space_3 = {meh, "/"},
	
		-- move focused window to a new space and tile
		move_window_1 = {hyper, "["},
		move_window_2 = {hyper, "]"},
		move_window_3 = {hyper, "/"}
    }
})
