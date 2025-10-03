primary = {"alt", "control", "shift"}
secondary = {"cmd", "alt", "control", "shift"}
right = {"i"}
left = {"n"}
up = {"u"}
down = {"e"}
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
		window_gap  =  { top = 20, bottom = 20, left = 20, right = 20 },
		-- bottom = 60
		window_ratios = { 1/3, 1/2, 2/3 },
		rejectApp = {"Facetime", "Stickies", "LuLu", "IINA", "java"},
		swipe_fingers = 4,
		swipe_gain = 2
		-- ,
		-- drag_window = { primary },
		-- lift_window = { secondary }
	},
	start = true,
    hotkeys = {
		-- switch windows by cycling forward/backward
		-- (forward = down or right, backward = up or left)
		focus_prev = {primary, "l"},
		focus_next = {primary, "y"},

		-- Switch to a new focused window in tiled grid
		swap_left  = {primary, "n"},
		swap_right = {primary, "i"},
		swap_up    = {primary, "u"},
		swap_down  = {primary, "e"},
				
		-- position and resize focused window
		center_window        = {primary, "c"},
		full_width           = {primary, "f"},
		cycle_width          = {primary, "r"},
		cycle_height         = {secondary, "r"},	
		
		-- move focused window into / out of a column
		slurp_in = {primary, "j"},
		barf_out = {primary, "m"},
		
		-- move the focused window into / out of the tiling layer
		toggle_floating = {primary, "escape"},
		
		-- switch to a new Mission Control space
		switch_space_l = {primary, "h"},
		switch_space_r = {primary, ","},
		switch_space_1 = {primary, "1"},
		switch_space_2 = {primary, "2"},
		switch_space_3 = {primary, "3"},
		switch_space_4 = {primary, "4"},
		switch_space_5 = {primary, "5"},
		switch_space_6 = {primary, "6"},
		switch_space_7 = {primary, "7"},
		switch_space_8 = {primary, "8"},
		switch_space_9 = {primary, "9"},
		
		-- move focused window to a new space and tile
		move_window_1 = {secondary, "1"},
		move_window_2 = {secondary, "2"},
		move_window_3 = {secondary, "3"},
		move_window_4 = {secondary, "4"},
		move_window_5 = {secondary, "5"},
		move_window_6 = {secondary, "6"},
		move_window_7 = {secondary, "7"},
		move_window_8 = {secondary, "8"},
		move_window_9 = {secondary, "9"}
    }
})
