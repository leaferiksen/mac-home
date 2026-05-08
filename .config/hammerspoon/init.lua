hs.loadSpoon("SpoonInstall")
Install = spoon.SpoonInstall
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
		window_ratios = { .3, .5, .7, 1 },
		infinite_loop_window = true
	},
	fn = function(spoon)
		-- ignore a specific app
		spoon.window_filter:rejectApp("Finder")
		spoon.window_filter:rejectApp("Terminal")
		spoon.window_filter:rejectApp("QuickTime Player")
		spoon.window_filter:rejectApp("iPhone Mirroring")
		spoon.window_filter:rejectApp("Shortcuts")
		spoon.window_filter:rejectApp("Messages")
		-- spoon.window_filter:rejectApp("Facetime")
		spoon.window_filter:rejectApp("Stickies")
		spoon.window_filter:rejectApp("Minecraft")
		spoon.window_filter:rejectApp("IINA")
		spoon.window_filter:rejectApp("Elmedia Video Player")
		spoon.window_filter:rejectApp("Color Picker")
		spoon.window_filter:rejectApp("LuLu")
		spoon.window_filter:rejectApp("java")
		spoon.window_filter:rejectApp("Keynote")
		-- ignore a specific window of an app
		spoon.window_filter:setAppFilter("Music", { rejectTitles = "MiniPlayer" })
		-- list of screens to tile (use % to escape string match characters, like -)
		-- spoon.window_filter:setScreens({ "Built%-in Retina Display" })
	end,
	start = true,
	hotkeys = {
		-- position and resize focused window
		cycle_width = {"alt", "."},
		reverse_cycle_width = {"alt", ","},
		-- move focused window into / out of a column
		slurp_in = {"alt", "i"},
		barf_out = {"alt", "o"},
		-- move the focused window into / out of the tiling layer
		toggle_floating = {"alt", "t"},
		focus_floating = {"alt", "s"},
		-- switch windows by cycling forward/backward
		-- (forward = down or right, backward = up or left)
		focus_prev = {"alt", "a"},
		focus_next = {"alt", "e"},
		-- Switch to a new focused window in tiled grid
		swap_left = {"alt", "b"},
		swap_right = {"alt", "f"},
		swap_up = {"alt", "p"},
		swap_down = {"alt", "n"},
	}
})
