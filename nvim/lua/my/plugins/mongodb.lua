return {
	"donus3/mongo.nvim",
	dependencies = {
		"ibhagwan/fzf-lua"
	},
	opts = {
		---default mongo url show on start up
		default_url = "mongodb://localhost:27017",
		---execute query on collection selected
		find_on_collection_selected = false
	},
	cmd = { "Mongo" },
}
