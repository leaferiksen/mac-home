paneru.setup({
    options = { preset_column_widths = { 0.3, 0.5, 0.7, 1 }, sliver_width = 1 },
    padding = { left = 10, right = 10 },
    swipe = { gesture = { fingers_count = 3, vertical = true } },
    bindings = {
	-- In-Workspace Focus (Emacs style)
	["window focus north"] = "cmd + ctrl + alt - p",
	["window focus south"] = "cmd + ctrl + alt - n",
	["window focus west"]  = "cmd + ctrl + alt - a",
	["window focus east"]  = "cmd + ctrl + alt - e",

	-- Window Swapping (Emacs style)
	["window swap west"]   = "cmd + ctrl + alt - b",
	["window swap east"]   = "cmd + ctrl + alt - f",

	-- Virtual Workspaces Focus (Arrow Keys)
	["window virtualfocus north"] = "cmd + ctrl + alt - leftarrow",  -- Navigate to previous/upper workspace
	["window virtualfocus south"] = "cmd + ctrl + alt - rightarrow", -- Navigate to next/lower workspace

	-- Virtual Workspace Movement (Arrow Keys)
	["window virtualmove north"]  = "cmd + ctrl + alt - uparrow",    -- Move window to previous/upper workspace
	["window virtualmove south"]  = "cmd + ctrl + alt - downarrow",  -- Move window to next/lower workspace

	-- Stacking & Layout Controls
	["window stack"]   = "cmd + ctrl + alt - s",
	["window unstack"] = "cmd + ctrl + alt - r",

	-- Resizing & Positioning
	["window grow"]             = "cmd + ctrl + alt - g",
	["window center"]           = "cmd + ctrl + alt - c",
	["window togglefloatlayer"] = "cmd + ctrl + alt - t",
    },
    decorations = { inactive = { dim = { opacity = -0.1, opacity_night = -0.1 } } },
    restore = { enabled = true, startup_grace_ms = 2000 },
    windows = {
	all = { title = ".*", vertical_padding = 20, horizontal_padding = 10 },
	colors = { title = "^Colors$", floating = true },
	fonts = { title = "^Fonts$", floating = true },
	facetime = { title = ".*", bundle_id = "com.apple.Facetime", floating = true },
	devicehub = { title = ".*", bundle_id = "com.apple.dt.Devices", floating = true },
	iina = { title = ".*", bundle_id = "com.colliderli.iina", floating = true },
    },
})
