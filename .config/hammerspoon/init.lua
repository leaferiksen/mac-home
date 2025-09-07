primary = {"cmd", "alt"}
secondary = {"cmd", "alt", "shift"}
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
		window_gap  =  { top = 20, bottom = 60, left = 20, right = 20 },
		window_ratios = { 0.3, 0.5, 0.68, 1 },
		rejectApp = {"LuLu", "IINA", "java"}
	},
	start = true,
    hotkeys = {
		-- Switch to a new focused window in tiled grid
		focus_left  = {primary, "left"},
		focus_right = {primary, "right"},
		focus_up    = {primary, "up"},
		focus_down  = {primary, "down"},
	
		-- move windows around in tiled grid
		swap_left  = {secondary, "left"},
		swap_right = {secondary, "right"},
		swap_up    = {secondary, "up"},
		swap_down  = {secondary, "down"},
	
		-- position and resize focused window
		center_window        = {primary, "return"},
		cycle_width          = {primary, "space"},
	
		-- move focused window into / out of a column
		slurp_in = {primary, "-"},
		barf_out = {primary, ";"},
	
		-- move the focused window into / out of the tiling layer
		toggle_floating = {primary, "escape"},
	
		-- switch to a new Mission Control space
		switch_space_1 = {primary, "["},
		switch_space_2 = {primary, "]"},
		switch_space_3 = {primary, "/"},
	
		-- move focused window to a new space and tile
		move_window_1 = {secondary, "["},
		move_window_2 = {secondary, "]"},
		move_window_3 = {secondary, "/"}
    }
})
