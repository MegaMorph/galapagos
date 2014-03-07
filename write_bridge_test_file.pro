pro write_bridge_test_file
  openw, 1, '~/IDL_bridge_test_file'
  printf, 1, 'success'
  close,1
end
