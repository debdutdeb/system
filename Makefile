nvim:
	cd nvim && nix build . && sudo rm -rf /opt/nvim && sudo cp -r result /opt/nvim
	
.PHONY: nvim
