local settings = {
	settings = {
		["js/ts"] = {
			inlayHints = {
				parameterNames = {
					enabled = "literals",
					suppressWhenArgumentMatchesName = true,
				},

				parameterTypes = {
					enabled = false,
				},

				variableTypes = {
					enabled = false,
				},

				propertyDeclarationTypes = {
					enabled = false,
				},

				functionLikeReturnTypes = {
					enabled = false,
				},

				enumMemberValues = {
					enabled = false,
				},
			},
		},
	},
}

return settings
