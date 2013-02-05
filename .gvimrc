set guifont=Source\ Code\ Pro:h14
if has("gui_macvim")
	macmenu &File.New\ Tab key=<D-T>
	map <D-t> :CommandT<CR>
endif
